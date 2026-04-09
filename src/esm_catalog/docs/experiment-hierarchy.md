# Experiment Hierarchy

## Overview

ESM-Catalog organises STAC collections into a two-level tree:

```
Root catalog  /
└── Experiment catalog  /experiments/{id}
    └── Collection       /collections/{experiment}-{component}
        └── Items        /collections/{id}/items
```

Previously `GET /collections` returned a flat list with no grouping. STAC
Browser had no way to navigate to "all components of experiment X". This
feature adds a virtual experiment layer derived from the `experiment` field
already present on every collection, without any schema changes to DuckDB.

---

## STAC Object Model

```
Root (type=Catalog)
│   links: child → /experiments/basic-001
│           child → /experiments/basic-002
│           ...
│
├── Experiment Catalog (type=Catalog)  /experiments/basic-001
│       id: "basic-001"
│       stac_version: "1.0.0"
│       links: self   → /experiments/basic-001
│               root   → /
│               parent → /
│               child  → /collections/basic-001-echam
│               child  → /collections/basic-001-fesom
│
├── Collection  /collections/basic-001-echam
│       links: parent → /experiments/basic-001   ← fixed (was /)
│               ...
│
└── Collection  /collections/basic-001-fesom
        links: parent → /experiments/basic-001
                ...
```

---

## Endpoints

### `GET /experiments`

Returns a paginated list of all experiments across all registered catalogs.

**Query parameters:**

| Parameter | Default | Description |
|-----------|---------|-------------|
| `limit`   | 100     | Max experiments to return (1–1000) |
| `offset`  | 0       | Skip first N experiments |

**Example response:**

```json
{
  "experiments": [
    {
      "id": "basic-001",
      "title": "basic-001",
      "collection_count": 2,
      "href": "http://localhost:8000/experiments/basic-001"
    }
  ],
  "numberMatched": 1,
  "numberReturned": 1,
  "links": [
    {"rel": "self",  "href": "http://localhost:8000/experiments?limit=100&offset=0"},
    {"rel": "root",  "href": "http://localhost:8000/"},
    {"rel": "first", "href": "http://localhost:8000/experiments?limit=100&offset=0"}
  ]
}
```

Pagination links `next` and `prev` are added when applicable.

---

### `GET /experiments/{id}`

Returns a STAC Catalog object for one experiment with child links pointing to
each of its component collections.

**Path parameters:**

| Parameter | Description |
|-----------|-------------|
| `id`      | Experiment identifier (e.g. `basic-001`) |

**Returns 404** if no collections with `experiment == id` exist.

**Example response:**

```json
{
  "type": "Catalog",
  "id": "basic-001",
  "stac_version": "1.0.0",
  "description": "Experiment basic-001",
  "title": "basic-001",
  "links": [
    {"rel": "self",   "href": "http://localhost:8000/experiments/basic-001"},
    {"rel": "root",   "href": "http://localhost:8000/"},
    {"rel": "parent", "href": "http://localhost:8000/"},
    {"rel": "child",  "title": "basic-001-echam",
     "href": "http://localhost:8000/collections/basic-001-echam"},
    {"rel": "child",  "title": "basic-001-fesom",
     "href": "http://localhost:8000/collections/basic-001-fesom"}
  ]
}
```

---

## Federation

When multiple catalog files are registered (one per experiment or per HPC
facility), experiments are merged across catalogs:

- `GET /experiments` deduplicates experiment IDs across all catalogs.
  `collection_count` reflects the total number of unique collections for that
  experiment across all catalogs.
- `GET /experiments/{id}` returns child links for all collections belonging to
  the experiment, regardless of which catalog file they live in.

---

## Collection Parent Links

The `parent` link on each collection now points to its experiment catalog
instead of the API root:

| Before | After |
|--------|-------|
| `{"rel": "parent", "href": "/"}` | `{"rel": "parent", "href": "/experiments/basic-001"}` |

**Fallback:** collections without an `experiment` field still use `/` as their
parent, ensuring backwards compatibility with manually inserted collections.

---

## Landing Page Child Links

The root landing page (`GET /`) now advertises experiments as children instead
of collections. This enables STAC Browser Browse mode to show the experiment
tree:

```
/
├── experiments/basic-001  (child link)
├── experiments/basic-002  (child link)
└── ...
```

STAC Browser follows these child links to show the two-level tree.
