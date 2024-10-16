#!/usr/bin/env python

""" Tests for ``esm_runscripts.jinja``"""
import copy
import sys
import tempfile
import unittest
from io import StringIO

from esm_runscripts import jinja

config = {
    "xios": {
        "ni_glo": 400,
        "nj_glo": 600,
    },
}

template = """<domain_definition>
  <!-- Definition of the native domain of the model -->
  <domain id="reduced_gaussian" long_name="reduced Gaussian grid" type="gaussian" />

  <!-- Definition of regular Gaussian domains -->
  <domain_group id="regular_domains" type="rectilinear" >
    <domain id="regular" long_name="regular grid" ni_glo="{{ xios['ni_glo'] }}" nj_glo="{{ xios['nj_glo'] }}" >
      <generate_rectilinear_domain />
      <interpolate_domain order="1" write_weight="true" />
    </domain>
  </domain_group>
</domain_definition>"""

rendered_file = """<domain_definition>
  <!-- Definition of the native domain of the model -->
  <domain id="reduced_gaussian" long_name="reduced Gaussian grid" type="gaussian" />

  <!-- Definition of regular Gaussian domains -->
  <domain_group id="regular_domains" type="rectilinear" >
    <domain id="regular" long_name="regular grid" ni_glo="400" nj_glo="600" >
      <generate_rectilinear_domain />
      <interpolate_domain order="1" write_weight="true" />
    </domain>
  </domain_group>
</domain_definition>"""


class Capturing(list):
    """Taken from https://stackoverflow.com/questions/16571150/how-to-capture-stdout-output-from-a-python-function-call"""

    def __enter__(self):
        self._stdout = sys.stdout
        sys.stdout = self._stringio = StringIO()
        return self

    def __exit__(self, *args):
        self.extend(self._stringio.getvalue().splitlines())
        del self._stringio  # free up some memory
        sys.stdout = self._stdout


class TestJinja(unittest.TestCase):
    """Test the various static methods of the jinja implementation"""

    def setUp(self):
        """Prepares everything to might be needed"""
        self.config = config
        self._test_dir = tempfile.mkdtemp()
        with open(f"{self._test_dir}/template.j2", "w") as f:
            f.write(template)

    def test_jinja(self):
        """Tests whether jinja is working correctly"""
        jinja.render_template(
            self.config, f"{self._test_dir}/template.j2", f"{self._test_dir}/output.xml"
        )

        with open(f"{self._test_dir}/output.xml", "r") as f:
            output = f.read()

        assert output == rendered_file

    def test_jinja_remove_j2_extension(self):
        """
        Checks that the error is raised when the rendered file still contains the j2
        extension
        """
        jinja.render_template(
            self.config,
            f"{self._test_dir}/template.j2",
            f"{self._test_dir}/output.xml.j2",
        )

        with open(f"{self._test_dir}/output.xml", "r") as f:
            output = f.read()

        assert output == rendered_file

    def test_jinja_variable_not_defined_error(self):
        """
        Checks that the non-defined variables in the template are catched and reported
        to the user
        """
        cconfig = copy.deepcopy(self.config)
        del cconfig["xios"]["ni_glo"]

        with Capturing() as output:
            try:
                jinja.render_template(
                    cconfig,
                    f"{self._test_dir}/template.j2",
                    f"{self._test_dir}/output.xml",
                )
            except SystemExit as e:
                error = e

        assert isinstance(error, SystemExit)
        assert any(["ERROR: Jinja" in line for line in output])
