Recieve batch notifications via e-mail
======================================

It is possible to define the following variables in your runscript either in `computer`
or in `general` (up to you which one) to recieve e-mail notifications about the status
of your job:

.. code-block:: yaml

   general/computer:
       mail_type: <type_of_notification>
       mail_user: <your_e-mail>

Find more about the possible values of ``mail_type`` in SLURM documentation (https://slurm.schedmd.com/sbatch.html#OPT_mail-type)
