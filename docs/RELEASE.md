# How to make a release

## Documentation

The documentation is built with odoc and themed with wodoc; see
[`doc/README.md`](../doc/README.md) for the sources, the build command and a
local preview. Publishing is automated:

- every push to `master` rebuilds and deploys the `dev` documentation;
- a release is published by running the **Documentation** workflow manually
  (GitHub, Actions, *Documentation*, "Run workflow") with the **version**
  input. That job freezes the current `dev` documentation as `/<version>/`,
  repoints the `latest` symlink and refreshes `versions.json`. It does not
  rebuild: the documentation of a release is exactly the `dev` documentation at
  that point.

The details, including how to do it by hand on a `gh-pages` checkout, are in
[`doc/README.md`](../doc/README.md).

## Release

- Update the [changelog](../CHANGES.md). Its first heading is the version
  number: `dune-release tag` reads it, and no `.opam` file or `dune-project`
  carries a version field.
- Check that the build, the tests and the documentation are clean:
  `dune build @check`, `dune runtest` and `dune build @doc`.
- Update the packages that depend on TyXML, so that a release never lands
  before its reverse dependencies can accept it.
- Use dune-release:

```
dune-release tag
dune-release distrib
DUNE_RELEASE_DELEGATE=github-dune-release-delegate dune-release publish distrib
dune-release opam pkg
dune-release opam submit
```

- Publish the documentation of the new version with the **Documentation**
  workflow, as described above.
