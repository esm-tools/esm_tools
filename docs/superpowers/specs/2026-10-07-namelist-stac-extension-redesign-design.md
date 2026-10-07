# Namelist STAC extension: nested + consistently named, with collection-level filtering

## Problem

Running `asterix-001`'s live Collection JSON (stac-dev.awi.de) through a STAC
browser's metadata renderer produces two cards: an untitled one with a single
`components` row, and a card titled "Nml" holding `nml:files` (12 entries),
`nml:groups` (63 entries), and `nml:parameters` — 499 entries as one flat
definition list, each labeled with the full
`component__file__group__key` string title-cased
(`"Fesom Namelist Oce Oce Dyn C D"`), repeating the file and group on every
row. Confirmed against the live collection
(`asterix-001-8542a77e`, fetched 2026-10-07):

```
nml:parameters: 499 entries, e.g. "fesom__namelist_oce__oce_dyn__c_d": 0.0025
nml:files:      12 entries
nml:groups:     63 entries
components:     ["echam", "fesom", "oasis3mct"]   (bare extra_fields key, no card)
description:    "asterix-001"                      (repeats title)
license:        "proprietary"                       (true default, nothing configured)
providers:      None
keywords:       None
```

The flat `component__file__group__key` scheme
(`src/esm_catalog/namelist.py`) is deliberate, not an oversight: pgstac builds
an unquoted JSON path from an *unregistered* property name, so a name
containing `.` (a filename like `namelist.echam`) or `[]` (a repeated-group
index) breaks the path and the CQL2 filter silently matches nothing. Flat,
punctuation-free keys dodge that with zero registration required — see the
module docstring. That exact mechanism is why the Collection body is
unreadable: the same flattening used for Item-level filtering is also applied
to the Collection body, which has nothing to do with per-item registration
and everything to do with how a browser renders a JSON object.

Separately, the actual goal is broader than cosmetics: filter **collections**
(whole experiments) and **items** (file groups) both, by the simulation
settings they ran under (namelist values) — a capability that doesn't exist
at the collection level today (`nml:parameters` there is pure display data;
grep confirms zero in-repo consumer and no queryables registration for it).

## Research: is "nest it" compatible with "filter it"?

Checked against upstream pgstac/stac-fastapi, not assumed:

- pgstac's `queryable()` resolution **correctly splits a registered dotted
  name** into a nested JSON path (`foo:detail.value` →
  `properties->'foo:detail'->'value'`). Nesting is filterable, *if
  registered*.
- A known open upstream bug,
  [pgstac#483](https://github.com/stac-utils/pgstac/issues/483), hits only
  the **unregistered** fallback (`indexdef()`): it treats a dotted name as
  one literal key, builds an index that's NULL for every row, so the filter
  silently matches nothing. This is almost certainly the exact failure this
  module's docstring is already warning about, and why flat keys were chosen
  originally — it's the one scheme guaranteed to filter without registration.
- `/collections?filter=...&filter-lang=cql2-text` is a real, documented
  stac-fastapi-pgstac capability — collection-level CQL2 filtering doesn't
  need to be invented.

Conclusion: nesting is safe **only if every leaf is explicitly registered**
as a dotted-path queryable. Never rely on the unregistered fallback for a
nested property.

## Browser rendering, confirmed empirically

A separate session ran `asterix-001`'s live collection JSON through STAC
Browser 5.1's actual renderer
(`@radiantearth/stac-fields` ~1.6.1, the library that does essentially all of
the Metadata section's work — card grouping, labels, value formatting), not
just reasoning about the API shape. Findings that confirm or bound this
design:

- **Card title**: confirmed independently — an unregistered prefix is
  title-cased by `formatKey`. `namelist` isn't one of the library's 47
  pre-registered extension labels (`cube`, `sci`, `proj`, ... ), so it
  renders as plain "Namelist". Corroborates the rename.
- **Native field placement**: `keywords`, `providers`, `license`, `assets`,
  `item_assets` are drawn in fixed page sections outside Metadata entirely —
  confirms moving `components` to `keywords` removes the untitled card, and
  that `providers` entries take `name`/`roles` (`licensor`/`producer`/
  `processor`/`host`)/`url`.
- **Two cosmetic limits nesting does NOT fix** — both are
  `stac-fields`/`fields.config.js` build-time concerns (a STAC Browser
  rebuild we don't control), explicitly out of scope for this spec:
  - **Number formatting**: values are locale-formatted by the generic
    fallback, so `delta_min = 1e-11` displays as `0` and `yr_perp = 1850`
    gets a thousands separator. Storing values as strings to dodge this
    would break `queryable_value`'s numeric typing and CQL2 numeric
    comparison — not worth it for a display quirk.
  - **Flat repeated-record arrays**: `io_list`-style values (Fortran
    output-interval tuples) render as one flat bullet list (116 items)
    instead of a 29×4 table. Fixing this generically would mean guessing a
    stride per array by name — fragile, whack-a-mole, not attempted.

## Design

### One vocabulary: `nml` → `namelist`, both scopes

No split vocabulary. The item-level flat-key prefix and the collection-level
field prefix both change from `nml` to `namelist`, in the same change, with a
full grep sweep for the old name (code, tests, docs, CLI help) — no
transition shim, no dual-prefix period.

### Item level: same mechanism, renamed prefix

`src/esm_catalog/namelist.py`: `_ITEM_PREFIX = "nml"` → `"namelist"`. Flattened
keys become `namelist__{component}__{file}__{group}__{key}`. The flattening
function (`_flatten`), the queryables generator (`namelist_queryables`), and
the caching (`_namelist_props_once`) are otherwise unchanged — this mechanism
already works, is already deployed, and already does exactly what "filter
items by simulation setting" asks for. Touching it is a rename, not a
redesign.

### Collection level: real nesting, explicitly registered

Replace the single flat `nml:parameters` dict with a real nested
`namelist:parameters`:

```json
{
  "echam": {
    "namelist_echam": {
      "radctl": { "io3": 4 }
    }
  },
  "fesom": {
    "namelist_oce": {
      "oce_dyn": { "c_d": 0.0025, "spp": false, "redi": true }
    }
  }
}
```

Each leaf gets registered as a dotted-path queryable
(`namelist:parameters.echam.namelist_echam.radctl.io3`) through a new
collection-scope counterpart to the existing `namelist_queryables()` /
`pypgstac load-queryables` pipeline (`scan/ingest.py`, `scan/workspace.py`,
the `esm-catalog-load-queryables` operator recipe in `cli.py`) — extended to
emit collection-scope entries alongside the existing item-scope ones. Segment
sanitization (stripping `.`/`[]` from an individual filename or repeated-group
name) stays exactly as `_flatten` already does it — only the *joining*
changes, from string concatenation to real dict nesting.

`nml:files` and `nml:groups` are dropped outright: both fully derivable by
walking `namelist:parameters`, and grep confirms neither has a consumer or a
queryables registration today. This alone removes the bulk of the flat
definition-list mess.

### Native STAC fields (bundled — same card, low risk)

- `components` moves from a bare, unprefixed `extra_fields["components"]`
  (the cause of the untitled card — it matches no STAC field and no extension
  prefix) to `collection.keywords` — a native `pystac.Collection` field that
  browsers render as tags under the description. One-line change in
  `collection.py`.
- `providers`: currently never set. Proposed default: AWI as
  `producer`/`host` when nothing more specific is configured — **flagging
  this as a one-line call to confirm**, since a future externally-run
  experiment might need a different provider and nothing today distinguishes
  that case.
- `description` falling back to repeating the title
  (`exp_metadata.description or exp_metadata.experiment_id` in
  `collection.py`) is accurate given no `Description` key exists in
  `asterix-001`'s config — a data-population gap in that experiment's
  `general_metadata`, not a code bug. No change proposed here.
- `license` defaulting to `"proprietary"` is the honest default when nothing
  is configured. No change proposed.

### Filesystem paths in parameter values (considered, no change)

Several parameters are absolute HPC paths (`meshpath`, `resultpath`,
`out_datapath`, tidal-forcing files, `ifile_transit`), one including an
account/project path segment, published on `stac-dev.awi.de`'s
unauthenticated-readable API (confirmed: a plain unauthenticated `curl` to
`/api/collections` returns `200` with full collection bodies). Raised
explicitly and decided: these are internal cluster paths, not sensitive —
left as-is. No redaction/stripping added by this change.

### Schema version bump

`configs/stac-extensions/namelist/v1.0.0/schema.json` is our own,
locally-hosted schema (not a third-party upstream) — both branches of its
`oneOf` change (the Item branch's `patternProperties` prefix, the Collection
branch's required fields and structure), so this is a breaking version bump:
new `configs/stac-extensions/namelist/v2.0.0/schema.json`,
`registry.py`'s `EXTENSION_URLS[Extension.namelist]` repointed to match.

### Blast radius / migration

- Collection-level `namelist:*` fields: net-new structure, nothing to
  migrate (today's collection-level fields are unregistered and unconsumed).
- Item-level: the already-deployed catalog has 32k+ items carrying
  `nml__...` properties under already-registered queryables. Renaming the
  prefix means newly-scanned/pushed experiments emit `namelist__...`; already
  -ingested experiments elsewhere in the catalog keep the old name until
  re-scanned. No dual-prefix shim is being built to paper over that gap —
  re-scanning the rest of the live catalog, if wanted, is a separate,
  later decision.

### Validation: asterix-001, live, before/after

`asterix-001-8542a77e`'s current Collection JSON is saved
(`/Users/pgierz/.claude/jobs/01bcf8f7/tmp/asterix-001-before.json`) as the
"before" snapshot. Plan: implement, delete the live `asterix-001` collection,
re-scan and re-push it under the new code, compare the rendered metadata
cards against this snapshot.

**Open gap**: `asterix-001`'s source experiment tree isn't in this checkout
(`grep`/`find` for "asterix" turns up nothing locally) — it must live on
whatever filesystem it was originally scanned from. Needs locating before the
re-scan step; not resolved in this spec.

## Testing

- `namelist.py` unit tests: rebuild around `namelist__` prefix and the nested
  collection structure; cover the repeated-group `[N]`-suffix case nested
  (not just flattened).
- Schema conformance tests (`stac_ext.apply_extension`'s validation path)
  against the new `v2.0.0` schema, both Item and Collection branches.
- The live asterix-001 before/after comparison above stands in for an
  end-to-end check a unit test can't give: does the actual STAC browser
  render it well.
