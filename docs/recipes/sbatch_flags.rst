Change/Add Flags to the sbatch Call
===================================


**Feature available since version:** 4.2

If you are using `SLURM` batch system together with `ESM-Tools` (so far the default
system), you can modify the ``sbatch`` call flags by modifying the following variables
from your runscript, inside the ``computer`` section:

.. csv-table::
   :header: Key, Description
   :widths: 15, 85

   "mail_type, mail_user",      Define these two variables to get updates about your slurm-job through email.
   single_proc_submit_flag,     "By default defined as ``--ntasks-per-node=1``"
   additional_flags,            "To add any additional flag that is not predefined in `ESM-Tools`"

Example
~~~~~~~

Assume you want to run a simulation using the Quality of Service flag (``--qos``) of
`SLURM` with value ``24h``. Then, you'll need to define the ``additional_flags`` inside
the ``computer`` section of your runscript. This can be done by adding the following to
your runscript:

.. code-block:: yaml

   computer:
       additional_flags: "--qos=24h"

Adding more than one flag
~~~~~~~~~~~~~~~~~~~~~~~~~

Alternatively, you can include a list of additional flags:

.. code-block:: yaml

    computer:
        additional_flags:
            - "--qos=24h"
            - "--comment='My Slurm Comment'"

See the documentation for the batch scheduler on your HPC system to see the allowed options.
