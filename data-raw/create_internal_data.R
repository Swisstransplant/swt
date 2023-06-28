## Save internal data for swt package

## EXAM

# First run exam-Analysis.qmd to generate the data.
# The centers and covariances are fixed to calculate the Mahalanobis distance for
# new cases. The empirical cumulative distribution function of D2 is used to
# calculate percentile ranks.

# In order to run this code and save the data as internal data please load
# the swt project. Then, open a project-specifc qmd pipeline from within the swt project.
# Internal data is stored in R/sysdata.rda



library(fs)
library(testit)
PATH_PKG = file.path(fs::path_home(), "OneDrive - Swisstransplant", "Data", "swt")

# load existing data from various projects
load(file.path(PATH_PKG, "R", "sysdata.rda"))

# run project-specific pipeline to overwrite internal data

idat.fit.kidmo = fit.mod1.csh

# dont's save sensitive data
assert(is.null(idat.fit.kidmo$x))
assert(is.null(idat.fit.kidmo$y))
assert(is.null(idat.fit.kidmo$model))

# save internal data
usethis::use_data(
  # EXAM
  idat.md.temp.center,
  idat.md.temp.cov,
  idat.fn.D2.temp,
  # KIDMO
  idat.fit.kidmo,

  internal = TRUE, overwrite = TRUE)
