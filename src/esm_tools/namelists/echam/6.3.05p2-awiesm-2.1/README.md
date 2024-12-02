> **NOTE THAT THIS REPOSITORY IS STILL UNDER CONSTRUCTION**

# Namelists for AWI-ESM Simulations

This repository contains namelists which you can use for the following scenarios:

* **Spinup Simulations** 

    In this case, output is reduced to the bare minimum to save disk space, yet still give you reasonable output to check model state. Use the output type `spinup`

    NOTE: **IN PROGRESS**

* **Production Simulations**

    Standard output of monthly means. An example for `echam.namelist` is already provided. Use the output type `production`

* **High Frequency Production Simulations**

    Select variables are produced in sub-daily temporal frequency to enable examination of, e.g. extreme events. Use the output type `high_freq_production`

    NOTE: **IN PROGRESS**

* **PMIP/CMIP/IPCC Conform**

    Output configured such that we can produce CMOR-izable files. Use the output type `cmor`
    
    NOTE: **IN PROGRESS**

# Shared Pool

These namelists are also provided in a shared pool.

On **Ollie** under:
```
/home/ollie/pgierz/shared/gitlab.awi.de/paleodyn/models/namelists
```

And on **mistral** under:
```
/pf/a/a270077/shared/gitlab.awi.de/paleodyn/models/namelists
```

# Binding in to the ESM Tools

To include these namelists into your run configuration, you can set the following (in this example, for echam):

```yaml
echam:
    namelist_dir: <path/to/namelist_repo/output_type>
```

More concretely, this could look like this:

```yaml
echam:
    namelist_dir: /work/ba0989/a270077/PalModII/namelists/production/echam
```

In that example, it is assumed you have done `git clone https://gitlab.awi.de/paleodyn/models/namelists` in the directory `/work/ba0989/a270077/PalModII`


# Further Discussion

Please see the [Issues section](https://gitlab.awi.de/paleodyn/Models/namelists/-/issues) for additional aspects in need of improvement or discussion. 
