# Save internal data for swt package

# In order to run this code and save the data as internal data please load
# the swt project. Then, open a project-specifc qmd pipeline from within the swt project.
# Internal data is stored in R/sysdata.rda.

library(fs)
library(testit)
PATH_PKG = file.path(fs::path_home(), "OneDrive - Swisstransplant", "Data", "swt")

# 1. Load existing data from various projects
load(file.path(PATH_PKG, "R", "sysdata.rda"))

# 2. Run project-specific pipeline to overwrite internal data
# KIDMO project:
idat.fit.kidmo = fit.mod1.csh

# 3. Quality checks: don't save sensitive data
assert(is.null(idat.fit.kidmo$x))
assert(is.null(idat.fit.kidmo$y))
assert(is.null(idat.fit.kidmo$model))

# 4. Save as internal data
usethis::use_data(
  # EXAM
  idat.md.temp.center,
  idat.md.temp.cov,
  idat.fn.D2.temp,
  # KIDMO
  idat.fit.kidmo,

  internal = TRUE, overwrite = TRUE)
