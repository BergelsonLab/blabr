# Updating the package

-   Run `devtools::check()`.

-   Run `devtools::test()`.

-   Run `devtools::document()`.

-   Run `use_version()` then select the new version.

-   Update `NEWS.md`.

-   Commit `NEWS.md` and `DESCRIPTION`.

-   Merge branch into `main`. Create a merge commit, don't squash, don't fast-forward. Amend the merge commit's message if an automatic one was used.

-   Push.

-   Set and push a version tag:

        devtools::load_all()
        version <- packageVersion('blabr')
        cmd <- glue::glue('tag {version} && git push --tags')
        system2('git', cmd)
