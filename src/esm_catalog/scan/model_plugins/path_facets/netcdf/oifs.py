"""OIFS/XIOS path-facet templates: ``<grid>_<freq>_<var>_<freq>_<start>-<end>.nc``.

Confirmed live against a real AWI-CM3 (develop-is) run's actual ``outdata/oifs/``
listing (288 files): ``atm_1d_10u_1d_1900-1900.nc`` (2D),
``atm_remapped_1m_lai_hv_1m_1900-1900.nc`` (2D, variable name itself
underscore-bearing), and ``atm_remapped_1m_pl_q_1m_pl_1900-1900.nc``
(pressure-level 3D -- the literal ``pl`` marker brackets the variable name
the same way the frequency code does for 2D fields). The pressure-level
entry is listed first since it is the more specific pattern: tried against
the 2D one first, "pl" would be swallowed into ``{{ stream }}`` as part of
the variable name, which is wrong.

The file bundles a whole year of steps, so its real end (observed always
equal to start in this sample, but not assumed to always be) comes from the
filename's own second year -- frequency alone cannot reconstruct it the way
it can for ECHAM/FESOM's single-step files.
"""

from __future__ import annotations

from esm_catalog.scan.path_facets.templates import template_data_hookimpl


@template_data_hookimpl
def get_path_facet_templates():
    return {
        "oifs": [
            {
                "pattern": (
                    "{{ expid }}_{{ ignore }}_pl_{{ stream }}_{{ ignore }}_pl_"
                    "{{ date }}-{{ end_date }}.nc"
                ),
                "date_format": "%Y",
                "end_date_format": "%Y",
            },
            {
                "pattern": (
                    "{{ expid }}_{{ ignore }}_{{ stream }}_{{ ignore }}_"
                    "{{ date }}-{{ end_date }}.nc"
                ),
                "date_format": "%Y",
                "end_date_format": "%Y",
            },
        ]
    }
