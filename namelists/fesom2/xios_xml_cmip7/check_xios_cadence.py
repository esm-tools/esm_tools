#!/usr/bin/env python3
"""Check the FESOM XIOS XML for field ids that are written at more than one cadence.

Under XIOS, FESOM sends a field whenever any file using it wants a sample, and
each send is the mean since the previous send. A field id referenced by files
of different freq_op leaves the coarser file with the last short mean only
(monthly m_ice was the mean of the last day of the month). Every field id must
therefore have one freq_op; a second cadence, a surface level or a snapshot
needs its own alias id (def_xios_aliases in FESOM's io_meandata.F90).

file_def_fesom.xml.j2 is rendered for every combination of its boolean
switches. With --fesom-src, the alias table in io_meandata.F90 is checked too:
every alias id the XML uses must exist there, and every instantaneous file must
read an 'inst' alias.

Usage:
    check_xios_cadence.py [--fesom-src <fesom>/src/io_meandata.F90]
Exit status 1 if anything is wrong.
"""
import argparse
import itertools
import re
import sys
import xml.etree.ElementTree as ET
from pathlib import Path

import jinja2

HERE = Path(__file__).resolve().parent


class Ns(dict):
    """Attribute access for the template's fesom.* / xios.* names."""

    __getattr__ = dict.__getitem__


def fesom_aliases(path):
    src = Path(path).read_text()
    lists = {}
    for name in ("alias_id", "alias_of", "alias_mode"):
        m = re.search(r"::\s*%s\(\*\)\s*=\s*\(/(.*?)/\)" % name, src, re.S)
        if not m:
            sys.exit(f"{path}: no {name} table")
        lists[name] = re.findall(r"'([^']*)'", m.group(1))
    return {i.strip(): (o.strip(), mo.strip()) for i, o, mo in zip(lists["alias_id"], lists["alias_of"], lists["alias_mode"])}


def defined_fields():
    root = ET.parse(HERE / "field_def_fesom.xml").getroot()
    return {f.get("id"): f for f in root.iter("field") if f.get("id")}


def file_fields(rendered):
    """(file id, field id, freq_op, operation) for every field of every enabled file."""
    root = ET.fromstring(rendered)
    out = []

    def walk(el, inh):
        inh = dict(inh)
        for k in ("enabled", "freq_op", "operation"):
            if k in el.attrib:
                inh[k] = el.attrib[k]
        if el.tag == "file":
            inh["file"] = el.get("id")
        if el.tag == "field" and inh.get("enabled", "true") != "false":
            out.append((inh.get("file"), el.get("field_ref") or el.get("id"), inh.get("freq_op"), inh.get("operation")))
        for c in el:
            walk(c, inh)

    walk(root, {})
    return out


def main():
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    ap.add_argument("--fesom-src", help="FESOM src/io_meandata.F90 with the def_xios_aliases table")
    args = ap.parse_args()

    template = (HERE / "file_def_fesom.xml.j2").read_text()
    jinja = " ".join(re.findall(r"\{[{%](.*?)[%}]\}", template, re.S))
    switches = sorted(set(re.findall(r"\bfesom\.([A-Za-z_0-9]+)", jinja)))
    fields = defined_fields()
    aliases = fesom_aliases(args.fesom_src) if args.fesom_src else None
    errors = set()

    for values in itertools.product((False, True), repeat=len(switches)):
        ctx = {"fesom": Ns(zip(switches, values)), "xios": Ns(compression_level=3)}
        rendered = jinja2.Template(template, undefined=jinja2.StrictUndefined).render(**ctx)
        cadence = {}
        for fid, ref, fop, op in file_fields(rendered):
            cadence.setdefault(ref, set()).add(fop)
            if ref not in fields:
                errors.add(f"{fid}: field_ref {ref} is not in field_def_fesom.xml")
            elif fields[ref].get("field_ref"):
                errors.add(f"field_def: {ref} has a field_ref, so XIOS takes its data from {fields[ref].get('field_ref')}, not from FESOM")
            if aliases is not None:
                if ref.endswith(("_day", "_3h", "_sfc", "_inst")) and ref not in aliases and ref not in fields:
                    errors.add(f"{fid}: {ref} is neither an alias in io_meandata.F90 nor a FESOM field")
                if op == "instant" and aliases.get(ref, (None, None))[1] != "inst":
                    errors.add(f"{fid}: operation=instant on {ref}, which is not an 'inst' alias: every sample is a mean")
        for ref, fops in cadence.items():
            if len(fops) > 1:
                on = ", ".join(s for s, v in zip(switches, values) if v) or "none"
                errors.add(f"{ref}: freq_op {sorted(fops)} (switches on: {on})")

    if aliases is not None:
        for alias, (base, _) in aliases.items():
            if alias not in fields:
                errors.add(f"alias {alias} (on {base}) has no <field> in field_def_fesom.xml")

    for e in sorted(errors):
        print(e)
    n = 2 ** len(switches)
    print(f"{'FAIL' if errors else 'ok'}: {n} switch combinations ({', '.join(switches)}), {len(errors)} problem(s)")
    return 1 if errors else 0


if __name__ == "__main__":
    sys.exit(main())
