#' EXAM device data
#'
#' Example data form the LifePort kidney transporter.
#'
#' @format ## `exam.device`
#' A data frame with self explaining variable names:
#' \describe{
#'   \item{SerialNumber}{serial number of the device}
#'   \item{UnitID}{name given to the device}
#'   ...
#' }
#' @source <https://swtdata.org/>
"exam.device"

#' EXAM organ data
#'
#' Example data form the LifePort kidney transporter.
#'
#' @format ## `exam.organ`
#' A data frame with self explaining variable names:
#' \describe{
#'   \item{OrganID}{identifier entered into the machine}
#'   \item{KidneySide}{left or right kidney}
#'   ...
#' }
#' @source <https://swtdata.org/>
"exam.organ"

#' EXAM summary statistics
#'
#' Example data form the LifePort kidney transporter.
#'
#' @format ## `exam.sumstats`
#' A data frame with self explaining variable names:
#' \describe{
#'   \item{perfusion.dur}{perfusion duration in minutes}
#'   \item{perfusion.dur.str}{perfusion duration in HH:MM:SS}
#'   ...
#' }
#' @source <https://swtdata.org/>
"exam.sumstats"

#' EXAM time series data
#'
#' Example data form the LifePort kidney transporter.
#'
#' @format ## `exam.timeseries`
#' A list of data frames with time series data:
#' \describe{
#'   \item{SerialNumber}{serial number of the device}
#'   \item{FlowRate }{flow rate}
#'   ...
#' }
#' @source <https://swtdata.org/>
"exam.timeseries"
