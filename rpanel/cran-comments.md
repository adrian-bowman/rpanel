## Test environments
* local R installation, R 4.0.2
* ubuntu 16.04 (on travis-ci), R 4.0.2
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 0 notes

* This is a new release.  The main change is that a check is now made on whether the BWidget Tcl/Tk module is available.  This allows the package to be installed under Macos, which previously failed.  If BWidget is not available then three functions (rp.combo, rp.notebook, rp.notebook.raise) are disabled and advisory messges printed if these functions are called.
