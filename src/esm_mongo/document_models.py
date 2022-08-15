#!/usr/bin/env python3
"""
Provides an abstraction for Documents (description of possible entries) in the
esm-tools database
"""

import getpass

import mongoengine
import questionary
import yaml
from loguru import logger
from mongoengine import (DictField, Document, EmailField, FloatField,
                         ListField, ReferenceField, StringField, connect,
                         disconnect)
from mongoengine.context_managers import switch_collection

from esm_parser import yaml_file_to_dict

from . import auth_commands


def float_converter(number: str):
    if number == "":
        return float('nan')
    return float(number)

def list_converter(list_in: str):
    return list_in.split(",")

_CONVERT_TYPES = {
    mongoengine.fields.FloatField: float_converter,
    mongoengine.fields.ListField: list_converter,

}

class InteractiveDocument(Document):
    meta = {'abstract': True}

    @classmethod
    def mk_interactively(cls):
        fields = {}
        logger.debug(cls._fields.items())
        for field_name, field in cls._fields.items():
            if field_name in ["id", "_cls"]:
                continue  # PG: id should be automatically created
            if field.required:
               extra_text = "This field is required!"
            else:
                extra_text = "This field can by skipped (just push enter)..."
            if field.default:
                extra_text = f"Default is {field.default()}. If that's correct, just push enter..."
            # Special handling is required for some types of fields:
            if isinstance(field, ReferenceField):
                if field_name == "reference_simulation":
                    answer = questionary.confirm("You can add a reference simulation. Would you like to do this?").ask()
                    if not answer:
                        continue
                answer = cls.mk_interactively_with_existing(field)
            elif isinstance(field, ESMToolsConfigField):
                path = questionary.path(f"Please enter a path to your simulation's {field_name}. Tab completion will work. {extra_text}").ask()
                with open(path, "r") as f:
                    config = yaml.safe_load(f)
                answer = _fixup_dict(config)
            else:
                converter = _CONVERT_TYPES.get(type(field))
                answer = questionary.text(f"Please input a value for the {cls.__name__} {field_name}. {extra_text}").ask()
                if converter:
                    answer = converter(answer)
                logger.debug(f"Using for {field_name}: {answer} ({type(answer)})")
            fields[field_name] = answer
        return cls(**fields)

    @classmethod
    def mk_interactively_with_existing(cls, referenced_field):
        connect("esm_tools", auth_commands.HOST)
        with switch_collection(cls, referenced_field.document_type.__name__):
            check_existing = questionary.confirm(f"Do you want to view existing entries for {referenced_field.document_type.__name__}?").ask()
            if check_existing:
                already_existing = referenced_field.document_type_obj.objects
                choices = ["None of these, please help me to make a new one"]
                choices += [thing.name for thing in already_existing]
                selected = questionary.select("Which would you like to use?", choices=choices).ask()
                if selected == choices[0]:
                    new =  referenced_field.document_type_obj.mk_interactively()
                    new.save()
                    return new
                all_matches = already_existing(name=selected)
                for match in all_matches:
                    return match  # Yes, we return the first one. I don't like this.
            else:
                new = referenced_field.document_type_obj.mk_interactively()
                new.save()
                return new



def _fixup_dict(d):
    new = {}
    for k, v in d.items():
        if isinstance(v, dict):
            v = _fixup_dict(v)
        if isinstance(k, str):
            new[k.replace(".", "-dot-")] = v
        else:
            new[str(k)] = v
    return new


class ESMToolsConfigField(DictField):
    pass

class Scenario(InteractiveDocument):
    """Describes a scenario for a particular simulation."""
    name = StringField(required=True)
    description = StringField(required=True)
    paleo_time_period = StringField()
    # Greenhouse gas values:
    co2vmr = FloatField()  # Maybe these needs to be lists of values to handle ramps?
    ch4vmr = FloatField()
    n2ovmr = FloatField()
    # Orbital Values
    cecc = FloatField()
    cobld = FloatField()
    clonp = FloatField()
    # Other Metadata
    mip = ListField()  # Which MIPs this run might belong to, CMIP, PMIP,
                       # DeepMIP, etc.
    project = ListField()



class Supercomputer(InteractiveDocument):
    """Minimally describes a supercomputer"""
    name = StringField(required=True)
    # NOTE(PG): We can probably just dump the machine yaml in here...?
    #esm_tools_config = ESMToolsConfigField()





class Simulation(InteractiveDocument):
    """Describes a simulation"""
    name = StringField(required=True)
    description = StringField(required=True)
    path = StringField(required=True) 
    scientist = StringField(required=True)
    email = EmailField(required=True)
    department = StringField()
    username = StringField(required=True, default=getpass.getuser)
    supercomputer = ReferenceField("Supercomputer", required=True)
    scenario = ReferenceField("Scenario")
    features = ListField()  # PG: e.g. Hosing, Isotopes, usw.
    esm_tools_config = ESMToolsConfigField(required=True)
    original_runscript = ESMToolsConfigField(required=True)
    model_name = StringField(required=True)
    model_components = ListField(required=True)  # Needs to be a list, e.g. ECHAM, FESOM, VILMA
    project = ListField()
    reference_simulation = ReferenceField("Simulation")  # PG: This is recursive and might bite me in the ass
    # Needs a CLI to easily modify
    quality_status = ListField()  # Something like "benchmarked", "use_with_caution", "published", "stupid_test", ...

class Model(Document):
    name = StringField(require=True)
    version = StringField(required=True)
    realm = ReferenceField("Realm") 
    description = StringField()
    key_publications = ListField()
    esm_config = DictField()

class Realm(InteractiveDocument):
    name = StringField(required=True)

class Publication(InteractiveDocument):
    # This could of course be much more expansive
    name = StringField(required=True)
    authors = ListField(required=True)
    doi = StringField(required=True)
