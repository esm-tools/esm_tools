import hashlib
import os
import time

import yaml


class AuditFile:
    def __init__(self, file_path, checksum=None, times=None):
        self.file_path = file_path
        self._checksum = checksum  # Optional precomputed checksum
        self._times = times  # Optional precomputed file times

        # Verify if the file exists if no precomputed data is provided
        if not os.path.isfile(self.file_path) and not (checksum and times):
            raise FileNotFoundError(f"{self.file_path} does not exist.")

    def get_checksum(self, algorithm="md5"):
        """Calculates and returns the checksum of the file using the specified algorithm."""
        if self._checksum:
            return self._checksum

        hash_func = hashlib.new(algorithm)
        try:
            with open(self.file_path, "rb") as file:
                while chunk := file.read(8192):
                    hash_func.update(chunk)
            return hash_func.hexdigest()
        except FileNotFoundError:
            return None

    def get_file_times(self, recompute=False):
        """Returns the file's access, modification, and (if available) creation times on Linux."""
        if self._times and not recompute:
            return self._times

        file_stats = os.stat(self.file_path)

        access_time = time.ctime(file_stats.st_atime)
        modification_time = time.ctime(file_stats.st_mtime)

        # Creation time (birth time) is not always available on Linux filesystems
        if hasattr(file_stats, "st_birthtime"):
            creation_time = time.ctime(file_stats.st_birthtime)
        else:
            creation_time = "Not available on this machine"

        return {
            "access_time": access_time,
            "modification_time": modification_time,
            "creation_time": creation_time,
        }

    def to_dict(self):
        """Returns the file's checksum and times as a dictionary."""
        file_info = {
            "file_path": self.file_path,
            "checksum": self.get_checksum(),
            "times": self.get_file_times(),
        }
        return file_info

    def to_yaml(self):
        """Converts the file's information into a YAML formatted string."""
        file_info_dict = self.to_dict()
        return yaml.dump(file_info_dict, default_flow_style=False)

    @classmethod
    def from_dict(cls, data):
        """Creates an AuditFile instance from a dictionary."""
        file_path = data.get("file_path")
        checksum = data.get("checksum")
        times = data.get("times")
        return cls(file_path, checksum=checksum, times=times)

    @classmethod
    def from_yaml(cls, yaml_string):
        """Creates an AuditFile instance from a YAML formatted string."""
        data = yaml.safe_load(yaml_string)
        return cls.from_dict(data)

    def __eq__(self, other):
        """Checks if two AuditFile objects are equal by comparing checksums and modification times."""
        if not isinstance(other, AuditFile):
            return False

        return (
            self.get_checksum() == other.get_checksum()
            and self.get_file_times()["modification_time"]
            == other.get_file_times()["modification_time"]
        )

    def is_younger_than(self, other):
        """Checks if the current file was modified after another file."""
        if not isinstance(other, AuditFile):
            raise ValueError("Can only compare with another AuditFile object.")

        self_mod_time = time.strptime(self.get_file_times()["modification_time"])
        other_mod_time = time.strptime(other.get_file_times()["modification_time"])

        return self_mod_time > other_mod_time

    def is_older_than(self, other):
        """Checks if the current file was modified before another file."""
        if not isinstance(other, AuditFile):
            raise ValueError("Can only compare with another AuditFile object.")

        self_mod_time = time.strptime(self.get_file_times()["modification_time"])
        other_mod_time = time.strptime(other.get_file_times()["modification_time"])

        return self_mod_time < other_mod_time


# Example Usage
if __name__ == "__main__":
    # Creating an instance from the file
    file_audit = AuditFile("example.txt")

    # Convert to dict and YAML
    file_dict = file_audit.to_dict()
    file_yaml = file_audit.to_yaml()

    print("File Info as Dict:", file_dict)
    print("File Info as YAML:\n", file_yaml)

    # Create an instance from the dict
    new_file_audit_from_dict = AuditFile.from_dict(file_dict)
    print("Created from Dict:", new_file_audit_from_dict.to_dict())

    # Create an instance from the YAML string
    new_file_audit_from_yaml = AuditFile.from_yaml(file_yaml)
    print("Created from YAML:", new_file_audit_from_yaml.to_dict())
