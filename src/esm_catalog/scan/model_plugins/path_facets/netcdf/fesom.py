"""FESOM's path-facet templates: ``<stream>.fesom.<date>[.nc variant]``.

Confirmed live: ``a_ice.fesom.1850.nc`` (plain year-stamped) and
``a_ice.fesom.185001.01.nc`` (AWI-CM3 v3.2's variant -- month fixed at 01,
FESOM writes one file per year; the trailing literal ".01" is an
output-chunk index not observed to vary). Neither matched the other's
filename shape; every AWI-CM3 FESOM file fell through to a full read before
the second entry was added.
"""

from __future__ import annotations

from esm_catalog.scan.path_facets.templates import template_data_hookimpl


@template_data_hookimpl
def get_path_facet_templates():
    return {
        "fesom": [
            {"pattern": "{{ stream }}.fesom.{{ date }}.nc", "date_format": "%Y"},
            {
                "pattern": "{{ stream }}.fesom.{{ date }}.01.nc",
                "date_format": "%Y%m",
            },
        ]
    }
