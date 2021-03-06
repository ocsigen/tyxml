# How to update the dev documentation

- install wikidoc with
```
opam pin add https://github.com/ocsigen/wikidoc.git
```
- Run `make wikidoc`

For more detail about wikidoc, see [how](https://github.com/ocsigen/html_of_wiki)

# How to make a release.

- Update the [changelog][CHANGES.md]
- Ensure the doc builds (`make -C docs`)
- Make sure the dev doc is up-to-date (see above)
- In the wikidoc branch, copy dev to VERSION

- Use dune-release:
```
dune-release tag
dune-release distrib
DUNE_RELEASE_DELEGATE=github-dune-release-delegate dune-release publish distrib
dune-release opam pkg
dune-release opam submit
```
