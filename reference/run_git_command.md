# Run a git command in a dataset repository

Run a git command in a dataset repository

## Usage

``` r
run_git_command(repo, command, return_output = FALSE)
```

## Arguments

- repo:

  dataset name: 'all_basiclevel', 'reliability', etc.

- command:

  what you would have after `git` on the command line except for the -C
  \\folder\\ part - the function will do that part for you.

- return_output:

  boolean, whether to return the printed output of the command as string

## Value

string with output if return_output is TRUE, else NULL

## Examples

``` r
if (FALSE) { # \dontrun{
run_git_command('all_basiclevel', 'status')
} # }
```
