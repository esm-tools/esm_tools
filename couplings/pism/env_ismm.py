"""Environment for the OIFS <-> ISM-mapper coupling.

Kept out of env_fesom.py: the SMB coupling is between OIFS and the ice sheet
and FESOM is not a party to it. It is spliced into the ice2fesom environment
only because the atmosphere<->ice weights are built from the same regenerated
A096 mask, in the same oasis_regen directory, in the same job. That mask moves
with the FESOM coastline, so the ice links cannot be generated separately or
they would be built against a stale atmosphere.
"""


def ismm_environment(config, environment_dict=None):
    """ISM-mapper variables for the OASIS regeneration.

    ``environment_dict`` is the caller's environment so far; used to rewrite
    OCP_TEMPLATE_CONFIG, which must point at a config carrying an ``ice:``
    section or ocp-tool writes no ice grid for the links to remap from.
    """
    general = config.get("general", {})
    fesom = config.get("fesom", {})
    ismm = config.get("ismm", {})

    if not general.get("smb_coupled", False):
        return {"SMB_COUPLED": 0}

    env = {
        "SMB_COUPLED": 1,
        "ISM_GRID": ismm.get("ice_grid", "ismp"),
        "ISM_PREFIX": ismm.get("ice_prefix", "antar"),
        # BILINEAR and CONSERV abort on a pole-centred grid, see
        # ocp_tool.oasis_weights.awiesm3_ismp_links.
        "ISM_REMAP_METHOD": ismm.get("ism_remap_method", "gauswgt"),
    }

    if environment_dict and not fesom.get("ocp_template_config"):
        template = str(environment_dict.get("OCP_TEMPLATE_CONFIG", ""))
        if template.endswith(".yaml"):
            env["OCP_TEMPLATE_CONFIG"] = template[: -len(".yaml")] + "_ICE.yaml"

    return env
