# blabr 0.3.0

* `get_*` functions produce similar results for ".csv" and ".feather" now. The
  attributes are not exactly the same and the orders of factor levels are
  different but now the outputs are both tibbles and have the same column types.
  The similarity is checked with `all.equal(..., check.attributes = FALSE))`

# blabr 0.2.1

* Suppress git message when checking out a tag.

# blabr 0.2.0

* `get_*` functions do not have `branch` and `commit` parameters anymore,
  instead they have a new `version` parameter that currently refers to a tag
  label in the corresponding dataset repository. Using `get_all_*` functions
  without supplying the version argument is discouraged, an appropriate warning
  is in place.
  
  Motivation for the change:
  - explicitly setting dataset version gives one a chance at reproducible
    analysis,
  - using versions instead of commit hashes lets us later choose a different
    non-git storage option. Or, even if we do go with git, the old and new
    hashes will not clash and/or confuse the users.
* Added a `NEWS.md` file to track changes to the package.
