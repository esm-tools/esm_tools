"""ECHAM's path-facet template: ``<expid>_<date>_<stream>``.

Confirmed live: ``historical_c14_init_185001.01_echam`` (outdata:
echam/co2/accw/ism/...).
"""

from __future__ import annotations

from esm_catalog.scan.path_facets.templates import template_data_hookimpl


@template_data_hookimpl
def get_path_facet_templates():
    return {
        "echam": [
            {"pattern": "{{ expid }}_{{ date }}_{{ stream }}", "date_format": "%Y%m.%d"},
        ]
    }
