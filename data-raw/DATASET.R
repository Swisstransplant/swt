## Save internal data
# First run exam-analysis.qmd to generate the data.
# The centers and covariances are fixed to calculate the Mahalanobis distance for
# new cases. The empirical cumulative distribution funtion of D2 is used to
# calculate percentile ranks.

# In order to run this code and save the data as internal data please load
# the swt project, and open exam-analysis.qmd from within the swt project.
# Internal data is stored in R/sysdata.rda
usethis::use_data(idat.md.temp.center,
                  idat.md.temp.cov,
                  idat.fn.D2.temp,
                  internal = TRUE, overwrite = TRUE)

