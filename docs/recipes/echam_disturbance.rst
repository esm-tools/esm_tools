Applying a temporary disturbance to ECHAM
=========================================

.. use = for sections, ~ for subsections and - for subsubsections

**Feature available since version:** ``esm_runscripts v4.2.1``

From time to time, the ``ECHAM`` family of models run into an error resulting
from too high wind speeds. This may look like this in your log files::

       30: ================================================================================
       30: 
       30: FATAL ERROR in cuadjtq (1):  lookup table overflow
       30:  FINISH called from PE: 30

To overcome this problem, you can apply a small change to the air viscosity via
the namelist parameter ``ensdiff`` in the ``dynctl`` section of the ``ECHAM``
namelist. As this is a common problem, there is a way to have the run do this
automatically for some specific years of your simulation.

1. Generate a file to list years you want distrubed.
   In your experiment script folder (**not** the one specific for each run),
   you can create a file called ``disturb_years.dat``. An abbreviated file tree
   would look like::

      * EXPID
      |
      | * scripts
      | | - run_config.yaml
      | | - distrub_years.dat
      | |
      | * outdata
      | | - echam
      | ... other files and folders ...


2. Add years you want disturbed
   The file should contain a list of years the distrubance should be applied
   to, seperated by new lines.


Example
~~~~~~~

.. subsection including examples with particular things in the recipe for specific models
   and coupled setups.
   Recommendation: use ``tabs`` for including examples for different models
   Note: numbering of the general recipe steps can be handy to reference the steps to modify


In this example, we disturb the years 2005, 2007, and 2008 of an experiment
called ``EXAMPLE`` running on ``ollie``::

  $ cat /work/ollie/pgierz/test_esmtools/EXAMPLE/scripts/disturb_years.dat 
  2005
  2007
  2008

You can also set the distrubance strength in your configuration under
``echam.disturbance``. The default is ``1.000001``. Here, we apply a 200%
distrubance whenever a "distrub_year" is encountered.

.. code-block:: yaml

         echam:
             disturbance: 2.0

See also
~~~~~~~~

.. links to relevant parts of the documentation

- `Relevant source code <https://github.com/esm-tools/esm_runscripts/blob/103d0f3d614688efb839aa9292d843da49bd3788/esm_runscripts/namelists.py#L182-L217>`_
