Build steps
===========

1. Run lint checks

.. code-block:: bash

  make lint

  # may want to pipe the output to less to parse it since there are a lot
  # of errors that are not really errors
  make lint 2>&1 | less

2. Run checkdoc (docstring checker)

.. code-block:: bash

  make checkdoc

3. Run tests

.. code-block:: bash

  make test

4. Install any new dependencies

.. code-block:: bash

  make install

5. Byte compile

.. code-block:: bash

  make build

Drafting a New Release
======================

1. Bump the version in the :code:`rigpa.el` header and commit the changes

.. code-block:: elisp

  ;; Version: i.j.k [use MAJOR.MINOR.PATCH]

2. Tag the release commit

.. code-block:: bash

  git tag -n  # list existing tags and annotations
  git tag -a <new version number> -m "<release message>"

3. Push the new tag to origin:

.. code-block:: bash

  git push --follow-tags  # push new tag to remote
