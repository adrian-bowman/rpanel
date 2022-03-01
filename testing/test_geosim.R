#     rp.geosim tests

library(rpanel)
if (reinstall) devtools::install("rpanel")

test_label("interactive use", test.prompt)
rp.geosim()

test_label("non-interactive use", test.prompt)
rp.geosim(panel = FALSE)
rp.geosim(panel = FALSE, smgrid = 100)

