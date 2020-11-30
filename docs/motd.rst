ESM MOTD
========

The package ``esm_motd`` is an `ESM-Tools` integrated `message-of-the-day` system,
intended as a way for the `ESM-Tools Development Team` to easily announce new releases
and bug fixes to the users without the need of emailing.

It checks the versions of the different `ESM-Tools` packages installed by the user, and
reports back to the user (writing to `stdout`) about packages that have available
updates, and what are the new improvements that they provide (i.e. reports back that a
bug in a certain package has been solved).

This check occurs every time the user uses ``esm_runscripts``.

The messages, their corresponding versions and other related information is stored
online in GitHub and accessed by `ESM-Tools` also online to produce the report. The user
can look at this file if necessary here:
`<https://github.com/esm-tools/esm_tools/tree/release/esm_tools/motd/motd.yaml>_`.

.. warning::

   The ``motd.yaml`` file is to be modified exclusively by the ESM-Tools Core
   Development Team, so... stay away from it ;-)
