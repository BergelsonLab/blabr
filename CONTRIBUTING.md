# Updating the package

-   Run `devtools::test()`.

-   Run `devtools::check()`.

-   Run `use_version()` then select the new version, do not commit.

-   Update `NEWS.md`.

-   If you are on a branch:

    -  Commit `NEWS.md` and `DESCRIPTION`.

    -  Merge branch into `main`. Create a merge commit, don't squash, don't fast-forward. Amend the merge commit's message if an automatic one was used.
    
-   If this is a small single-commit patch:

    - Commit everything directly on `main`.

-   Push.

-   Set and push a version tag:

        devtools::load_all()
        version <- packageVersion('blabr')
        cmd <- glue::glue('tag {version} && git push --tags')
        system2('git', glue::glue('tag {version}'))
        system2('git', 'push --tags')
