import os
import time
import pytest
from esm_runscripts.helpers import CachedFile


class TestCachedFile:
    @pytest.fixture(autouse=True)
    def setup_and_teardown(self):
        self.test_file_1 = "test1.txt"
        self.test_file_2 = "test2.txt"
        self.test_file_3 = "test.txt"
        self.test_file_4 = "test.yaml"
        yield
        os.remove(self.test_file_1)
        os.remove(self.test_file_2)
        os.remove(self.test_file_3)
        os.remove(self.test_file_4)

    def test_init(self):
        cf = CachedFile(self.test_file_3)
        assert cf.path == self.test_file_3

    def test_is_younger_than(self):
        with open(self.test_file_1, "w") as f:
            f.write("test")
        time.sleep(1)  # delay for 1 second
        with open(self.test_file_2, "w") as f:
            f.write("test")
        cf1 = CachedFile(self.test_file_1)
        cf2 = CachedFile(self.test_file_2)
        assert not cf1.is_younger_than(cf2.path)

    def test_is_older_than(self):
        with open(self.test_file_1, "w") as f:
            f.write("test")
        time.sleep(1)  # delay for 1 second
        with open(self.test_file_2, "w") as f:
            f.write("test")
        cf1 = CachedFile(self.test_file_1)
        cf2 = CachedFile(self.test_file_2)
        assert cf1.is_older_than(cf2.path)

    def test_read(self):
        with open(self.test_file_3, "w") as f:
            f.write("test")
        time.sleep(1)  # delay for 1 second
        cf = CachedFile(self.test_file_3)
        assert cf.read() == "test"

    def test_load_cache(self):
        with open(self.test_file_4, "w") as f:
            f.write("test: test")
        time.sleep(1)  # delay for 1 second
        cf = CachedFile(self.test_file_4)
        assert cf.load_cache() == {"test": "test"}
