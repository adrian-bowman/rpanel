## Test environments
* local R installation, R 4.5.2 (macos)
* win_devel
* win_release
* win_old_release

## R CMD check results

local system:    0 errors ✔ | 0 warnings ✔ | 0 notes ✔

## Checks on dependencies

check_packages_in_dir("..", reverse = list()) followed by summarize_check_packages_in_dir_results("..")
produces these results:

```
Check status summary:
                  ERROR OK
  Source packages     0  1
  Reverse depends     1  8
```

For some reason, the `stpp` package could not be installed

## Comments

A small bug in the function rp.sample came to light when the previous version of the package, submitted earlier this week, was tested on multiple platforms. This arose intermittently through particular configurations of randomly generated data. Apologies for missing this. The bug has now been corrected.
