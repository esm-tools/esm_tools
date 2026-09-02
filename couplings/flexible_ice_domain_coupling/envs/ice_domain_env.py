"""Shared helpers for the coupling ``env_*.py`` scripts.

An "ice domain" is one ice sheet model instance taking part in the coupling.
The old setups either had exactly one (and wrote everything flat into the
couple directory) or exactly two, hard coded as "pism_nh"/"pism_sh". Here the
list is read from the runscript, so any number of them works.

How the list is determined, in this order:

1. ``general.ice_domains`` -- an explicit list (or a comma separated string) of
   setup names, e.g.::

       general:
           ice_domains: [pism_nh, pism_sh, pism_gris]

2. the ``modelN`` sections of the iterative coupling that carry
   ``ice_domain: True``::

       model2:
           setup_name: pism_nh
           runscript: pism_nh.yaml
           chunk_size: 20
           chunk_unit: years
           ice_domain: True
           ice_domain_box: "0,360,0,90"      # optional
           ice_domain_weight: 1              # optional

3. nothing of the above -- the experiment then runs in the flat single ice
   sheet layout of the original scheme, and the domain is called ".", which
   maps ``ICE_COUPLE_DIR`` back onto ``COUPLE_DIR``.

The values produced here are consumed by ``general/ice_domains.functions``.
ESM-Tools exports the environment as unquoted ``export KEY=VALUE`` lines, so
none of the values may contain a space -- hence the comma separated list.
"""

FLAT_DOMAIN = "."


def _model_sections(config):
    """Yield (section name, section) of the iterative coupling models, in order."""
    index = 1
    while "model" + str(index) in config:
        yield "model" + str(index), config["model" + str(index)]
        index += 1


def ice_domain_names(config):
    """The ordered list of ice domain names, or ``["."]`` for the flat layout."""
    explicit = config["general"].get("ice_domains")
    if explicit:
        if isinstance(explicit, str):
            return [name.strip() for name in explicit.replace(":", ",").split(",") if name.strip()]
        return [str(name) for name in explicit]

    marked = [
        str(section["setup_name"])
        for _, section in _model_sections(config)
        if section.get("ice_domain", False)
    ]
    if marked:
        return marked

    return [FLAT_DOMAIN]


def ice_domain_section(config, name):
    """The ``modelN`` section belonging to an ice domain, or ``{}``."""
    for _, section in _model_sections(config):
        if str(section.get("setup_name")) == name:
            return section
    return {}


def _sanitize(name):
    return "".join(
        character if character.isalnum() else "_" for character in name
    ).upper()


def ice_domain_environment(config):
    """The ``ICE_DOMAIN*`` variables describing the whole set of ice domains.

    Returns a dict ready to be merged into an ``env_*.py`` environment dict.
    """
    names = ice_domain_names(config)
    environment = {
        "ICE_DOMAINS": ",".join(names),
        "ICE_EXP_DIR": config["general"]["experiment_couple_dir"] + "/..",
    }

    for name in names:
        if name == FLAT_DOMAIN:
            continue
        section = ice_domain_section(config, name)
        # Per domain settings may live in the modelN section of the coupling
        # runscript or in the domain's own runscript section.
        domain_config = config.get(name, {})
        box = section.get("ice_domain_box", domain_config.get("ice_domain_box"))
        weight = section.get(
            "ice_domain_weight", domain_config.get("ice_domain_weight")
        )
        if box:
            environment[f"ICE_DOMAIN_{_sanitize(name)}_BOX"] = str(box).replace(" ", "")
        if weight is not None:
            environment[f"ICE_DOMAIN_{_sanitize(name)}_WEIGHT"] = weight

    return environment


def this_ice_domain(config):
    """The domain of the component this env script is being run for."""
    setup_name = config["general"]["setup_name"]
    if setup_name in ice_domain_names(config):
        return setup_name
    return FLAT_DOMAIN


def this_chunk_size(config):
    """Chunk size of the component this env script is being run for.

    ``config["general"]["this_chunk_size"]`` is set by ``chunky_parts.py`` for
    exactly this purpose; the ``config["model2"]["chunk_size"]`` of the old
    scripts only happened to be right while the second model was the only ice
    sheet.
    """
    if "this_chunk_size" in config["general"]:
        return config["general"]["this_chunk_size"]
    for _, section in _model_sections(config):
        if str(section.get("setup_name")) == config["general"]["setup_name"]:
            return section["chunk_size"]
    return config["general"].get("nyear", 1)


def esm_chunk_size(config):
    """Chunk size of the driving ESM, i.e. of ``model1``."""
    if "model1" in config:
        return config["model1"]["chunk_size"]
    return config["general"].get("nyear", 1)
