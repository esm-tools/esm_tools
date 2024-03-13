import sys

from ruamel.yaml import YAML
from ruamel.yaml.comments import CommentedMap


class CustomData(CommentedMap):
    def __init__(self):
        super().__init__()


# Load the YAML file
yaml = YAML()
data = yaml.load(open("input.yaml"))


# Add comments at the end of each line
def add_comments(node):
    if isinstance(node, CommentedMap):
        for key in node.keys():
            value = node[key]
            if isinstance(value, CommentedMap) or isinstance(value, list):
                add_comments(value)
            else:
                breakpoint()
                value.yaml_add_eol_comment("Woo", key)
    elif isinstance(node, list):
        for item in node:
            add_comments(item)


add_comments(data)

# Dump the modified data with comments to stdout
yaml.dump(data, sys.stdout)
