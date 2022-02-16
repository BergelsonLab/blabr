# blabr 0.2.1

* Suppress git message when checking out a tag.

# blabr 0.2.0

* `get_all_*` functions do not have `branch` and `commit` parameters anymore,
  instead they have a new `version` parameter that currently refers to a tag
  label in the corresponding dataset repository. Using `get_all_*` functions
  without supplying the version argument is discouraged, an appropriate warning
  is in place.
  
  Motivation for the change:
  - explicitly setting dataset version gives one a chance at reproducible
    analaysis,
  - using versions instead of commit hashes
* Added a `NEWS.md` file to track changes to the package.
