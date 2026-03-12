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
  Source packages     1 10
  Reverse depends     0  1
```

For some reason, the `stpp` package could not be installed

## Comments

This release includes new functions rp.t_test, rp.lm, rp.coefficients and rp.drop1 to express the results of fitting (mostly linear) models graphically. It also includes some minor bug fixes.
