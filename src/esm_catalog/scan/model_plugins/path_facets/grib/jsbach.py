"""JSBACH's path-facet template: couples through ECHAM's own convention.

Confirmed live: ``historical_c14_init_185001.01_yasso`` (also
jsbach/land/surf/veg) -- same convention as ECHAM outdata (jsbach couples
through it). No enrichment quirk of its own, so unlike echam/oifs/fesom this
component has no corresponding ``model_plugins/enrichers/...`` module -- a
path-facet template is the only model-specific contribution it has ever
needed.
"""

from __future__ import annotations

from esm_catalog.scan.path_facets.templates import template_data_hookimpl


@template_data_hookimpl
def get_path_facet_templates():
    return {
        "jsbach": [
            {"pattern": "{{ expid }}_{{ date }}_{{ stream }}", "date_format": "%Y%m.%d"},
        ]
    }
