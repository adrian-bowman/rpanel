## Test environments
* local R installation, R 4.5.2 (macos)
* win_devel
* win_release
* win_old_release
* mac_release

## R CMD check results

Local system:    0 errors ✔ | 0 warnings ✔ | 0 notes ✔
Other test environments:     Status: OK

## Checks on dependencies

check_packages_in_dir("..", reverse = list()) followed by
summarize_check_packages_in_dir_results("..")
produces these results:

```
Check status summary:
                  ERROR OK
  Source packages     0  1
  Reverse depends     1  8
```

For some reason, the `stpp` package could not be installed

## Comments

A small bug in the rp.plot4d function occurs for occasional configurations of random data and this has been circumvented at the testing stage.

One or two other very minor bugs have also been dealt with.

As requested, failure to download remote files in the rp.datalink package has been trapped and the function finishes smoothly.

I apologise that a version (1.1-6.2) submitted to CRAN last night had some last minute manual checking code which had inadvertently been left in the systematic tests and which caused a problem.
