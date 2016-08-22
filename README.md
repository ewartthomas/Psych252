252 Course Website
==============

## Making changes to the website:

### Dependencies

To update the website, you'll need to have the following on your computer:

- a Github account
- git installed on your computer (or GitHub Mac)
- Sphinx (in terminal: `easy_install sphinx`)
- Sphinx bootstrap themes (in terminal: `easy_install sphinx-bootstrap-theme`)

### Generating the html files

Once you've made changes to the files in the WWW folder (e.g., adding new .Rmds or .htmls into the section folder; updating an index.rst file), you'll need to generate the new html files. To do this, type the following into your terminal:

```
cd ~/yourfilepath/Psych253/WWW
make clean html
./upload_website.sh <sunetid>
```

Check out more details in the README in the WWW folder!

### Updating Github

Now that you've made changes on your computer, you'll need to sync the changes with Github so everyone else can access them (make sure you've pulled any changes that others might have made before you make edits!). Do to this, you can type in the following:

```
git pull origin master
git add -A :/
git commit -m 'Type a brief message about what you've changed'
git push origin master
```

