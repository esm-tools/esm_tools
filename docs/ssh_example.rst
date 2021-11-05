Security settings for the people who are using GitHub for the first time:
=========================================================================

1. On your terminal eg. on Mistral::

       ssh-keygen -t ed25519 -C "your e-mail that you use for GitHub"

2. Copy the content of the ``*.pub`` file to your GitHub settings. This will be
   something like::

        ssh-ed25519 <combination of characters> <your e-mail that you use for GitHub>

3. Go to your ``GitHub page -> click your avatar -> settings -> SSH and GPG keys.`` Or: https://github.com/settings/keys

    * click "new SSH key"
    * For the title add a meaningful name, eg. name of the computer
    * paste the contents of the ``*.pub`` file to the key section and click "Add SSH key"

4. Verify that authentication is successful by typing ssh -T git@github.com.
   You receive see something like: Hi <your username>! You've successfully
   authenticated, but GitHub does not provide shell access.

5. Modify the git config for the repository:
    5.1) Inside the top directory of the repository (where .git directory is found), open the file .git/config
    5.2) change the value of the url on the line::

        [remote "origin"]
            url = git@github.com:esm-tools/<esm package you are working on>.git

    as an example::

        [remote "origin"]
            url = git@github.com:esm-tools/esm_tools>.git

    Alternatively, you can use the command ``git remote set-url origin git@github.com:esm-tools/esm_tools.git``

6. Now, you should be able to push to the remote repository with the git push command.
