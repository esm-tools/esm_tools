import sys

from ruamel.yaml import YAML, CommentedMap

top = CommentedMap()
top["sub_key1"] = data = CommentedMap()

data["a"] = 1
data["b"] = "asdf"
data["c"] = 3.3333
data.yaml_add_eol_comment("comment 1", "a")
data.yaml_add_eol_comment("comment 2", "b")
data.yaml_add_eol_comment("comment 3", "c")

top["sub_key2"] = data = CommentedMap()

data["a"] = "long text"
data["b"] = "an even longer text"
data.yaml_add_eol_comment("comment 4", "a")
data.yaml_add_eol_comment("comment 5", "b")

YAML().dump(top, sys.stdout)
