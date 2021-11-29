#!/usr/bin/env python3
"""
ESM Environment
===============

This module provides classes to generate headers for runscripts and compile
scripts based upon machine settings and model specifications.
"""
import pathlib
import socket
from typing import List

import jinja2

import esm_tools

# import esm_parser

TEMPLATE_ENV = jinja2.Environment(
    loader=jinja2.PackageLoader("esm_environment"),
    autoescape=jinja2.select_autoescape(),
    trim_blocks=True,
)
"""jinja2.Environment : The central jinja2 object to get templates from"""


class EsmEnvironmentError(Exception):
    """Raise this when something goes wrong constructing the script stub"""


class ComputerSpecifications:
    """
    General computer specifications, independent of runtime or compiletime.

    This class contains information for module actions and export variables of
    a particular computer. They are accessible as properties on the initialized
    object.

    Parameters
    ----------
    computer : str
        The computer to configuration to read, e.g. ``ollie``. If none is
        passed, uses the socket module to determine the current hostname.

    Example
    -------
    The class might be used as follows:
        >>> comp_spec = ComputerSpecifications("ollie")
        >>> comp_spec.module_actions
        [
            "purge",
            "load cmake",
            "load udunits",
            "load gribapi/1.28.0",
            "unload intel.compiler",
            "load intel.compiler",
            "unload netcdf",
            "load hdf5",
            "load centoslibs cdo nco netcdf/4.6.2_intel",
            "load automake",
            "load python3/3.7.7_intel2020u2",
            "load git",
            "list",
        ]
        >>> comp_spec.export_vars
        {
            "LC_ALL": "en_US.UTF-8",
            "FC": "${fc}",
            "F77": "${f77}",
            "MPIFC": "${mpifc}",
            "MPICC": "${mpicc}",
            "CC": "${cc}",
            "CXX": "${cxx}",
            "HDF5RO OT": "$HDF5_ROOT",
            "NETCDFFROOT": "$NETCDF_DIR",
            "NETCDFROOT": "$NETCDF_DIR",
            "NETCDF_Fortran_INCLUDE_DIRECTORIES": "$NETCDFROOT/include",
            "NETCDF_CXX_INCLUDE_DIRECTORIES": "$NETCDFROOT/include",
            "NETCDF_CXX_LIBRARIES": "$NETCDFROOT/lib",
            "PERL5LIB": "/usr/lib64/perl5",
            "LAPACK _LIB": '"-lmkl_intel_lp64 -lmkl_core -mkl=sequential -lpthread -lm -ldl"',
            "LAPACK_LIB_DEFAULT": '"-L/global/AWIsoft/intel/2018/compilers_a nd_libraries_2018.5.274/linux/mkl/lib/intel64 -lmkl_intel_lp64 -lmkl_core -lmkl_sequential"',
            "XML2ROOT": "/usr",
            "ZLIBROOT": "/usr",
            "MPIR OOT": "$(${mpifc} -show | perl -lne 'm{ -I(.*?)/include } and print $1')",
            "MPI_LIB": "$(${mpifc} -show |sed -e 's/^[^ ]*//' -e 's/-[I][^ ] *//g')",
            "PATH": "/work/ollie/jhegewal/sw/cmake/bin:$PATH",
        }
    """

    def __init__(self, computer=None):
        self._computer = computer or socket.getfqdn()
        try:
            computer_config = esm_tools.read_config_file(f"machines/{self._computer}")
        except FileNotFoundError as exception:
            raise EsmEnvironmentError(
                f"A configuration for {computer} could not be located!"
            ).with_traceback(exception.__traceback__)
        self._computer_config = (
            computer_config  # TODO: esm_parser.resolve_dict(computer_config)
        )

    def __repr__(self):
        return f"{self.__class__.__name__}(computer={self._computer!r})"

    @property
    def module_actions(self) -> List[str]:
        """The module actions associated with these computer specifications"""
        module_actions = self._computer_config.get("module_actions")
        if module_actions:
            return [f"module {action}" for action in module_actions]
        return []

    @property
    def export_vars(self) -> dict[str, str]:
        """The export variables associated with these computer specifications"""
        return self._computer_config.get("export_vars")

    @property
    def shebang(self) -> str:
        return f"#!{self._computer_config.get('sh_interpreter', '/bin/bash')}"


class RuntimeComputerSpecifications(ComputerSpecifications):
    """Compute specifications that may be specific to generating a runtime environment"""


class CompiletimeComputerSpecifications(ComputerSpecifications):
    """Compute specifications that may be specific to generating a compiletime environment"""


class ModelSpecifications:
    def __init__(self, name: str, cfg_type: str) -> None:
        if cfg_type not in ["component", "setup"]:
            raise EsmEnvironmentError("You must specify either component or setup!")
        try:
            model_config = esm_tools.read_config_file(f"{cfg_type}s/{name}")
        except FileNotFoundError as exception:
            raise EsmEnvironmentError(
                f"A configuration for the {cfg_type} {name} could not be located!"
            ).with_traceback(exception.__traceback__)
        self._model_config = model_config

    @property
    def module_actions(self):
        pass

    @property
    def export_vars(self):
        pass


class RuntimeModelSpecifications(ModelSpecifications):
    pass


class CompiletimeModelSpecifications(ModelSpecifications):
    pass


class ScriptHeader:
    jobtype = "generic"
    _tmpl_file = "script.base.jinja"

    def __init__(self, comp_spec, model_spec=None):
        self.comp_spec = comp_spec
        self.model_spec = model_spec
        self._template = TEMPLATE_ENV.get_template(self._tmpl_file)
        self.template_vars = dict(
            jobtype=self.jobtype,
            comp_spec=self.comp_spec,
            model_spec=self.model_spec,
            version=esm_tools.__version__,
            script_generator=self,
        )

    def __repr__(self):
        if self.model_spec is None:
            return f"{self.__class__.__name__}(comp_spec={self.comp_spec!r})"
        return f"{self.__class__.__name__}(comp_spec={self.comp_spec!r}, model_spec={self.model_spec!r})"

    def write(self, fname: str or pathlib.Path) -> pathlib.Path:
        with open(fname, "w", encoding="utf-8") as fout:
            fout.write(self._template.render(**self.template_vars))
        return pathlib.Path(fname)


class RuntimeScriptHeader(ScriptHeader):
    jobtype = "run"
    _tmpl_file = "script.run.jinja"


class CompiletimeScriptHeader(ScriptHeader):
    jobtype = "compile"
    _tmpl_file = "script.comp.jinja"
