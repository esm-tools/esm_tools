#!/usr/bin/env python3
import pathlib
import re
from collections import UserDict
from dataclasses import dataclass, field
from typing import Any, List

import dpath.util
from loguru import logger
from ruamel.yaml import YAML


@dataclass
class ConfigAddress:
    """Address of a config value."""

    path: str = ""
    """Path to the config value."""

    sep: str = "."
    """The separator used to separate the path components."""

    @property
    def parts(self) -> List[str]:
        """List of parts of the path."""
        return self.path.split(self.sep)

    def __str__(self):
        return self.path


@dataclass
class ConfigConstant:
    """A representation of a constant within the configuration"""

    name: str
    address: ConfigAddress
    value: Any

    def __str__(self):
        return f"{self.name} = {self.value} (defined at {self.address})"


@dataclass
class ConfigVariable:
    """A representation of a variable within the configuration"""

    name: str
    address: ConfigAddress
    value: Any = "unresolved"
    is_resolved: bool = False
    resolved_from: ConfigAddress = ConfigAddress()
    # NOTE(PG): Should this maybe be a ConfigAddress instead?
    should_resolve_from: str = "Unknown"
    dependencies: List[Any] = field(default_factory=list)
    start_mark: str = "${"
    end_mark: str = "}"
    sep_mark: str = "."

    def __str__(self) -> str:
        return str(self.value)

    def value_clean_marks(self) -> str:
        """Remove the start and end marks from the variable's ``value`` attribute."""
        return self.value.replace(self.start_mark, "").replace(self.end_mark, "")

    def should_resolve_from_clean_marks(self) -> str:
        """Remove the start and end marks from the variable's ``should_resolve_from`` attribute."""
        return self.should_resolve_from.replace(self.start_mark, "").replace(
            self.end_mark, ""
        )

    @property
    def resolution_info(self) -> str:
        if self.is_resolved:
            return f"{self.name} = {self.value} (defined at {self.address}, resolved from {self.resolved_from})"
        return f"{self.name} = {self.value} (defined at {self.address}, wants value of {self.should_resolve_from})"


class ChooseBlock(UserDict):
    """Represents a Choose block in the configuration"""

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.choices = {}

    def __getitem__(self, key):
        if key in self.choices:
            return self.choices[key]
        return super().__getitem__(key)

    def __setitem__(self, key, value):
        if key in self.choices:
            raise KeyError(f"{key} is already a choice")
        self.choices[key] = value

    def __delitem__(self, key):
        if key in self.choices:
            raise KeyError(f"{key} is a choice")
        super().__delitem__(key)

    def __contains__(self, key):
        return key in self.choices or key in self

    def __str__(self):
        return f"ChooseBlock({self.data})"

    @property
    def default_choice(self):
        """The default choice for this block"""
        return self.get("*", {})


class EsmConfig(UserDict):
    """Configuration as a dictionary"""

    def __init__(self, config_file=None, config=None, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.config_file = config_file
        self.data = config or {}

    @classmethod
    def from_yaml(cls, config_file):
        """Load configuration from a YAML file"""
        yaml = YAML()
        with open(config_file, "r") as f:
            config = yaml.load(f)
        return cls(config_file=config_file, config=config)

    def dump_yaml(self, fpath: str or pathlib.Path) -> None:
        """Writes config to fpath

        Parameters
        ----------
        fpath : str or pathlib.Path
           Location to write the output to
        """
        yaml = YAML()
        with open(fpath, "w") as f:
            yaml.dump(self.data, f)


class EsmParser:
    """A main class for parsing configuration files"""

    def __init__(self):
        pass

    @staticmethod
    def resolve_variable(
        config: EsmConfig,
        variable: ConfigVariable,
    ) -> ConfigVariable:
        """Resolve a variable

        Parameters
        ----------
        config : EsmConfig
            The configuration to resolve the variable in
        variable : ConfigVariable
            The variable to resolve

        Returns
        -------
        ConfigVariable
            The resolved variable
        """
        if variable.is_resolved:
            return variable
        value = dpath.util.get(
            config,
            variable.should_resolve_from_clean_marks(),
            separator=variable.sep_mark,
        )
        variable.value = value
        variable.is_resolved = True
        return variable

    @staticmethod
    def find_variables(
        config: EsmConfig,
        variable_start: str = "${",
        variable_end: str = "}",
        variable_separator: str = ".",
    ) -> List[ConfigVariable]:
        """In the config, find all variables

        This function can be used to find all variable values in the
        configuration. It accepts arguments for variable start, end and
        separator.

        Parameters
        ----------
        config : EsmConfig
            The configuration to search
        variable_start : str
            The start of a variable. Defaults to ``${``
        variable_end : str
            The end of a variable. Defaults to ``}``
        variable_separator : str
            The separator between the variable name and the elements of
            thevariable address. Defaults to ``.``

        Returns
        -------
        List[ConfigVariable]
            A list of all variables found in the configuration

        """

        def _filter_variable(value):
            """Filter function for in value side of key/value pair"""
            if isinstance(value, str):
                return variable_start in value and variable_end in value
            return False

        variables = []
        search_results = dpath.util.search(
            config,
            "**",
            separator=variable_separator,
            afilter=_filter_variable,
            yielded=True,
        )
        for result in search_results:
            logger.debug(f"Full result: {result}")
            re_pattern = re.escape(variable_start) + r"(.*?)" + re.escape(variable_end)
            logger.debug(rf"Pattern to search with: {re_pattern}")
            logger.debug(f"Result: {result[1]}")
            regex_results = re.findall(re_pattern, result[1])
            logger.debug(regex_results)
            for regex_result in regex_results:
                variables.append(
                    ConfigVariable(
                        name=result[0].split(variable_separator)[-1],
                        address=ConfigAddress(result[0]),
                        should_resolve_from=regex_result,
                        is_resolved=False,
                        start_mark=variable_start,
                        end_mark=variable_end,
                        sep_mark=variable_separator,
                    )
                )
        return variables
