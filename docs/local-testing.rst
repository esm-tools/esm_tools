=============
Local Testing
=============

There are a few ways to test locally. One way is to leverage ``docker``. The
github CI workflow ``dummy-model.yml`` can be used as reference. You can use it
together with the ``act`` (https://github.com/nektos/act) tool to set up the ``esm-tools``,
and install the ``dummy-model``. That is a good starting point for futher tests::

    act -w "./.github/workflows/dummy-model.yml" -r

The ``-r`` flag keeps the container afterwards. You can then use ``docker exec`` to enter
the container and do further experimentation::

    docker container ls
    docker exec -it <container_id> /bin/bash

By default, you have the following structure:
* ``/work/esm-bot/my-project/model-codes``
* ``/work/esm-bot/my-project/run-configs``
* ``/work/esm-bot/my-project/experiments``

Note that you need to switch to this user first when you start the
interactive docker session!::

    su - esm-bot
