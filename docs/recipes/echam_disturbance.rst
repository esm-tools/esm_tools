Applying a temporary disturbance to ECHAM to overcome numeric instability (lookup table overflows of various kinds)
===================================================================================================================

.. use = for sections, ~ for subsections and - for subsubsections

**Feature available since version:** ``esm_runscripts v4.2.1``

From time to time, the ``ECHAM`` family of models runs into an error resulting
from too high wind speeds. This may look like this in your log files::

       30: ================================================================================
       30: 
       30: FATAL ERROR in cuadjtq (1):  lookup table overflow
       30:  FINISH called from PE: 30

To overcome this problem, you can apply a small change to the factor "by which
stratospheric horizontal diffussion is increased from one level to the next
level above." (``mo_hdiff.f90``), that is the namelist parameter ``enstdif``,
in the ``dynctl`` section of the ``ECHAM`` namelist. As this is a common problem,
there is a way to have the run do this for specific years of your simulation. Whenever
a model year crashes due to numeric instability, you have to apply the method outlined
below.

1. Generate a file to list years you want disturbed.

   In your experiment script folder (**not** the one specific for each run),
   you can create a file called ``disturb_years.dat``. An abbreviated file tree
   would look like::

      * EXPID
      |
      | * scripts
      | | - run_config.yaml (sometimes called "EXPID.yaml")
      | | - disturb_years.dat
      | |
      | * outdata
      | | - echam
      | ... other files and folders ...


2. Add years you want disturbed.

   The file should contain a list of years the disturbance should be applied
   to, seperated by new lines. In practice, you will add a new line with the
   value of the model year during which the model crashes whenever such a crash
   occurs.


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

You can also set the disturbance strength in your configuration under
``echam.disturbance``. The default is ``1.000001``. Here, we apply a 200%
disturbance whenever a "disturb_year" is encountered.

.. code-block:: yaml

         echam:
             disturbance: 2.0

See also
~~~~~~~~

- `ECHAM6 User Handbook <https://icdc.cen.uni-hamburg.de/fileadmin/user_upload/icdc_Dokumente/ECHAM/echam6_userguide.pdf>`_, Table 2.4, dynctl

- `Relevant source code <https://github.com/esm-tools/esm_runscripts/blob/103d0f3d614688efb839aa9292d843da49bd3788/esm_runscripts/namelists.py#L182-L217>`_
