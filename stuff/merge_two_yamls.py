#!/usr/bin/env python3

import argparse
import yaml

def merge(a, b, path=None):
    "merges b into a"
    if path is None: path = []
    for key in b:
        if key in a:
            if isinstance(a[key], dict) and isinstance(b[key], dict):
                merge(a[key], b[key], path + [str(key)])
            elif a[key] == b[key]:
                pass # same leaf value
            else:
                if isinstance(a[key], list) and isinstance(b[key], list):
                    a[key] = list(set(a[key] + b[key]))
                else:
                    print('Conflict at %s' % '.'.join(path + [str(key)]))
                    print(f"a: {a[key]}")
                    print(f"b: {b[key]}")
                    a_or_b = input("Please choose a or b: ")
                    print(f"Ok, setting: {a_or_b}[{key}]")
                    d_to_use = locals()[a_or_b]
                    a[key] = d_to_use[key]
                    #raise Exception('Conflict at %s' % '.'.join(path + [str(key)]))
        else:
            a[key] = b[key]
    return a

def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument("yaml1")
    parser.add_argument("yaml2")
    parser.add_argument("yaml_out")
    return parser.parse_args()

def yaml_file_to_dict(filepath):
    """
    Given a yaml file, returns a corresponding dictionary.

    If you do not give an extension, tries again after appending one.

    Parameters
    ----------
    filepath : str
        Where to get the YAML file from

    Returns
    -------
    dict
        A dictionary representation of the yaml file.
    """
    with open(filepath) as yaml_file:
        return yaml.load(yaml_file, Loader=yaml.FullLoader)


class quoted(str):
    pass

def quoted_presenter(dumper, data):
    return dumper.represent_scalar('tag:yaml.org,2002:str', data, style='"')
yaml.add_representer(quoted, quoted_presenter)

class literal(str):
    pass

def literal_presenter(dumper, data):
    return dumper.represent_scalar('tag:yaml.org,2002:str', data, style='|')
yaml.add_representer(literal, literal_presenter)

def ordered_dict_presenter(dumper, data):
    return dumper.represent_dict(data.items())


def transform_strs(d, path=None):
    if path is None: path = []
    for k in d:
        if isinstance(d[k], dict):
            transform_strs(d[k], path + [str(k)])
        elif isinstance(d[k], list):
            new_list = []
            for item in d[k]:
                if isinstance(item, str):
                    new_item = literal(item) if "\n" in item or ";" in item else quoted(item)
                else:
                    new_item = item
                new_list.append(item)
            d[k] = new_list
        elif isinstance(d[k], str):
            item = d[k]
            d[k] =  literal(item) if "\n" in item or ";" in item else quoted(item)
        elif isinstance(d[k], int):
            continue
        else:
            print(d[k], type(d[k]))
            raise Exception("Do not understand...")
    return d

def main():
    args = parse_args()
    d1 = yaml_file_to_dict(args.yaml1)
    d2 = yaml_file_to_dict(args.yaml2)
    d1 = merge(d1, d2)

    with open(args.yaml_out, "w") as yaml_out:
        yaml.dump(transform_strs(d1), yaml_out)

if __name__ == "__main__":
    main()
