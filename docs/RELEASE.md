# How to make a release.

- Update the [changelog][CHANGES.md]
- Ensure the doc builds (`make -C docs`)
- Run `dune-release`
- In [ocsigen.org-data](https://github.com/ocsigen/ocsigen.org-data)
  - Run the instructions from [how](https://github.com/ocsigen/html_of_wiki)
  - copy `tyxml/dev` to the new version number.
