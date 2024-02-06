# Save internal data for swt package

# In order to run this code and save the data as internal data please load
# the swt project. Then, open a project-specific qmd file from within the swt
# project, run the pipeline until you generated the object for internal data.

# Internal data is stored in R/sysdata.rda.

library(fs)
library(testit)
PATH_PKG = file.path(fs::path_home(), "OneDrive - Swisstransplant", "Projects", "pkg_swt", "swt")

# 1. Load existing data from various projects
load(file.path(PATH_PKG, "R", "sysdata.rda"))

# 2. Run project-specific pipeline to overwrite internal data

# KIDMO project:

idat.kidmo.model.1 = fit.fg.del.1
idat.kidmo.model.1$x = NULL
idat.kidmo.model.1.scaling = fit.fg.del.1.scaling
idat.kidmo.model.1.hr2rank = fit.fg.del.1.scaling.hr2rank

# WAIT project:
idat.wait.model.he = fit.he
idat.wait.model.ki = fit.ki
idat.wait.model.li = fit.li
idat.wait.model.lu = fit.lu
idat.wait.model.pi = fit.pi

# 3. Quality checks: don't save sensitive data
assert(is.null(idat.kidmo.model.1$x))
assert(is.null(idat.kidmo.model.1$model))

# 4. Save as internal data
usethis::use_data(

  # EXAM
  idat.md.temp.center,
  idat.md.temp.cov,
  idat.fn.D2.temp,

  idat.md.perf.center,
  idat.md.perf.cov,
  idat.fn.D2.perf,

  # KIDMO
  idat.kidmo.model.1,
  idat.kidmo.model.1.scaling,
  idat.kidmo.model.1.hr2rank,

  # WAIT
  idat.wait.model.he,
  idat.wait.model.ki,
  idat.wait.model.li,
  idat.wait.model.lu,
  idat.wait.model.pi,

  internal = TRUE, overwrite = TRUE)
