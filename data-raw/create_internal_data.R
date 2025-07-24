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

idat.kidmo.model = fit.fg.del
idat.kidmo.model$x = NULL
idat.kidmo.model.hr2rank = fit.fg.del.hr2rank

# 3. Quality checks: don't save sensitive data
assert(is.null(idat.kidmo.model$x))
assert(is.null(idat.kidmo.model$model))

# 4. Save as internal data
usethis::use_data(

  # EXAM
  idat.md.perf.center,
  idat.md.perf.cov,
  idat.fn.D2.perf,

  idat.md.temp.center,
  idat.md.temp.cov,
  idat.fn.D2.temp,

  # KIDMO
  idat.kidmo.model,
  idat.kidmo.model.hr2rank,

  internal = TRUE, overwrite = TRUE
)
