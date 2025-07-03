#' EXAM device data
#'
#' Example data form the LifePort kidney transporter.
#'
#' @format A data frame with self explaining variable names:
#' \describe{
#'   \item{SerialNumber}{serial number of the device}
#'   \item{Type}{type note}
#'   \item{SubType}{subtype note}
#'   \item{UnitID}{name given to the device}
#'   \item{FirmwareVersion}{firmware version}
#'   \item{FileID}{file id}
#'   \item{StartTime}{start date and time of machine}
#'   \item{DataState}{data state, if it is complete}
#'   \item{HasGaps}{whether data has haps}
#'   \item{Runtime}{run time of the machine}
#'   \item{StopTime}{time and date of machine stop}
#'   \item{Filename}{file name}#'
#' }
#' @source \url{https://data.swisstransplant.org/}
"exam.device"

#' EXAM organ data
#'
#' Example data form the LifePort kidney transporter.
#'
#' @format A data frame with self explaining variable names:
#' \describe{
#'   \item{OrganID}{identifier entered into the machine}
#'   \item{KidneySide}{left or right kidney}
#'   \item{BloodType}{self explaining}
#'   \item{CrossClampTime.Date}{self explaining}
#'   \item{CrossClampTimezone}{self explaining}
#'   \item{TotalIschemicTime}{self explaining}
#'   \item{PerfusateLot}{self explaining}
#'   \item{PerfusateExpirationDate}{self explaining}
#'   \item{PerfusateUsed}{self explaining}
#'   \item{Cannula}{self explaining}
#'   \item{CannulaExpirationDate}{self explaining}
#'   \item{CassetteLot.}{self explaining}
#'   \item{CasetteExpirationDate}{self explaining}
#'   \item{ID}{self explaining}
#'   \item{DonorID}{self explaining}
#' }
#' @source \url{https://data.swisstransplant.org/}
"exam.organ"

#' EXAM summary statistics
#'
#' Example data form the LifePort kidney transporter.
#'
#' @format A data frame with self explaining variable names:
#' \describe{
#'   \item{perfusion.dur}{perfusion duration in minutes}
#'   \item{perfusion.dur.str}{perfusion duration in HH:MM:SS}
#'   \item{systolicPressure.md}{self explaining}
#'   \item{diastolicPressure.mean}{self explaining}
#'   \item{flowRate.mean}{self explaining}
#'   \item{organResistance.mean}{self explaining}
#'   \item{organResistance.sd}{self explaining}
#'   \item{organResistance.x1}{self explaining}
#'   \item{organResistance.y1}{self explaining}
#'   \item{organResistance.x2}{self explaining}
#'   \item{organResistance.y2}{self explaining}
#'   \item{organResistance.delta}{self explaining}
#'   \item{organResistance.slope}{self explaining}
#'   \item{iceContainerTemperature.mean}{self explaining}
#'   \item{iceContainerTemperature.sd}{self explaining}
#'   \item{iceContainerTemperature.minAbove}{self explaining}
#'   \item{iceContainerTemperature.minAbove.str}{self explaining}
#'   \item{infuseTemperature.mean}{self explaining}
#'   \item{infuseTemperature.sd}{self explaining}
#'   \item{infuseTemperature.start}{self explaining}
#'   \item{infuseTemperature.minAbove}{self explaining}
#'   \item{infuseTemperature.minAbove.str}{self explaining}
#'   \item{D2temp}{self explaining}
#'   \item{Ptemp}{self explaining}
#'   \item{D2perf}{self explaining}
#'   \item{Pperf}{self explaining}
#' }
#' @source \url{https://data.swisstransplant.org/}
"exam.sumstats"

#' EXAM time series data
#'
#' Example data form the LifePort kidney transporter.
#'
#' @format A list of data frames with time series data:
#' \describe{
#'   \item{SerialNumber}{serial number of the device}
#'   \item{FlowRate }{flow rate}
#'   ...
#' }
#' @source \url{https://data.swisstransplant.org/}
"exam.timeseries"
