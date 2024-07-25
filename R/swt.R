swt_skeleton <- function(path) {

  # ensure path exists
  FILENAME = paste0(basename(path), ".qmd")
  PATH_R = file.path(path, "R")
  PATH_DATA = file.path(path, "data")

  dir.create(PATH_R, recursive = TRUE, showWarnings = FALSE)
  dir.create(PATH_DATA, recursive = TRUE, showWarnings = FALSE)

  # generate header for file
  content = c(
    "---",
    "title: 'Project Title'",
    "subtitle: 'Statistical report'",
    "author: Author Name",
    "date: last-modified",
    "abstract: 'Short description of the project'",
    "lang: en",
    "format:",
    "  html:",
    "    toc: true",
    "    css: swt.css",
    "    linkcolor: '#E5005C'",
    "    df-print: kable",
    "    embed-resources: true",
    "    code-fold: true",
    "    code-block-bg: '#E6EEF0'",
    "    code-block-border-left: '#3F698A'",
    "---",
    "",
    "# Objectives",
    "",
    "# Data import",
    "",
    "# Data processing",
    "",
    "# Quality control",
    "",
    "# Descriptive statistics",
    "",
    "# Primary analysis",
    "",
    "# Secondary analysis",
    "",
    "# Computing information",
    "",
    "```{r}",
    "sessionInfo()",
    "```",
    ""
  )

  if (!file.exists(file.path(path, FILENAME))) {
    writeLines(content, con = file.path(PATH_R, FILENAME))
  }

  # copy files
  SOURCEPATH = file.path(find.package("swt"), "rstudio", "templates", "project")
  myfiles = list.files(SOURCEPATH, pattern = "swt.css|SWT_2955_2021.png",
                       full.names = TRUE)
  file.copy(myfiles, PATH_R)
}

#' SWT theme for ggplot
#'
#' This function allows you to add the SWT theme to your ggplot graphics.
#'
#' @param title_size The font size of the title
#' @param subtitle_size The font size of the subtitle
#' @param font_size The font font size of the legend, axis text, and axis titles
#' @param grey_theme Whether to use the grey theme instead (TRUE or FALSE)
#' @param legend_position Position of the legend (top, bottom, left or right)
#'
#' @examples
#' \donttest{
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg)) +
#'    geom_point() +
#'    swt_style()
#' }
#'
#' @export
swt_style <- function(title_size=14, subtitle_size=14, font_size=10,
                      grey_theme = FALSE, legend_position="top") {
  # windowsFonts()
  font = "sans"

  bgColor   = "white"
  gridColor = "gray90"
  if (grey_theme) {
    bgColor   = "#F4F4F1"
    gridColor = "white"
  }

  ggplot2::theme(

    # Title
    plot.title = ggplot2::element_text(family=font,
                                       size=title_size,
                                       face="bold"),

    # Subtitle
    plot.subtitle = ggplot2::element_text(family=font,
                                          size=subtitle_size,
                                          #margin=ggplot2::margin(9,0,9,0)
    ),
    plot.caption = ggplot2::element_blank(),
    # This leaves the caption text element empty, because it is set elsewhere in
    # the finalise plot function

    # Legend
    legend.position = legend_position,
    legend.text.align = 0,
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(size=font_size),

    # Axis
    axis.text = ggplot2::element_text(family=font, size=font_size),
    axis.title = ggplot2::element_text(family=font, size=font_size),
    # axis.text.x = ggplot2::element_text(margin=ggplot2::margin(5, b = 10)),

    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),

    # Grid lines
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(color=gridColor),
    panel.grid.major.x = ggplot2::element_line(color=gridColor),

    # Background
    # This sets the panel background as blank, removing the standard grey ggplot
    # background colour from the plot
    panel.background = ggplot2::element_rect(fill = bgColor),
    plot.background = ggplot2::element_rect(fill = bgColor)

    # Strip background (This sets the panel background for facet-wrapped plots
    # to white, removing the standard grey ggplot background colour and sets the
    # title size of the facet-wrap title to font size 22)
    # strip.background = ggplot2::element_rect(fill="red"),
    # strip.text = ggplot2::element_text(size  = 22,  hjust = 0)
  )
}

#' SWT colors
#'
#' Easy access to official SWT color scheme.
#'
#' @return a SWT color object
#'
#' @examples
#' mycolors = swt_colors()
#' mycolors$red.liver
#'
#' @export
#'
swt_colors <- function() {
  colors = list(

    # primary colors
    blue.dark          = grDevices::rgb(  0, 55,100, maxColorValue = 255),
    blue.alt           = grDevices::rgb( 42, 84,138, maxColorValue = 255),
    turkis.cm          = grDevices::rgb(105,211,195, maxColorValue = 255),
    yellow.cndo        = grDevices::rgb(251,228, 70, maxColorValue = 255),
    strongred.akzent   = grDevices::rgb(229,  0, 92, maxColorValue = 255),

    # duplicate colors
    turkis.tpx         = grDevices::rgb(105,211,195, maxColorValue = 255),
    yellow.donation    = grDevices::rgb(251,228, 70, maxColorValue = 255),
    blue.swt           = grDevices::rgb(  0, 55,100, maxColorValue = 255),

    # secondary colors
    lightblue.lungs    = grDevices::rgb(155,189,197, maxColorValue = 255),
    green.pancreas     = grDevices::rgb(139,173,143, maxColorValue = 255),
    green.langerhans   = grDevices::rgb(139,173,143, maxColorValue = 255),
    darkyellow.kidney  = grDevices::rgb(242,175, 92, maxColorValue = 255),
    red.liver          = grDevices::rgb(217,143,143, maxColorValue = 255),
    beige.intestine    = grDevices::rgb(209,205,189, maxColorValue = 255),
    # convert 40% alpha to RGB: round(255 - 0.8*(255 - c(212,  0, 84)))
    # 50% pallette version will be 0.4
    pink.heart         = grDevices::rgb(221, 51,118, maxColorValue = 255),

    # background color
    grey.bg            = grDevices::rgb(244,244,241, maxColorValue = 255),
    white              = grDevices::rgb(255,255, 255, maxColorValue = 255)
  )

  # single 5 hue color scheme 100% 75% 50%  25% 0% (white)

  # primary colors
  colfun = grDevices::colorRampPalette(c(colors$blue.dark, colors$white))
  colors$pal.blue.dark = colfun(5)

  colfun = grDevices::colorRampPalette(c(colors$blue.alt, colors$white))
  colors$pal.blue.alt = colfun(5)

  colfun = grDevices::colorRampPalette(c(colors$turkis.cm, colors$white))
  colors$pal.turkis.cm = colfun(5)

  colfun = grDevices::colorRampPalette(c(colors$yellow.cndo, colors$white))
  colors$pal.yellow.cndo = colfun(5)

  colfun = grDevices::colorRampPalette(c(colors$strongred.akzent, colors$white))
  colors$pal.strongred.akzent = colfun(5)

  # duplicate colors
  colors$pal.blue.swt        = colors$pal.blue.dark
  colors$pal.turkis.tpx      = colors$pal.turkis.cm
  colors$pal.yellow.donation = colors$pal.yellow.cndo

  # secondary colors
  colfun = grDevices::colorRampPalette(c(colors$pink.heart, colors$white))
  colors$pal.pink.heart = colfun(5)

  colfun = grDevices::colorRampPalette(c(colors$lightblue.lungs, colors$white))
  colors$pal.lightblue.lungs = colfun(5)

  colfun = grDevices::colorRampPalette(c(colors$green.pancreas, colors$white))
  colors$pal.green.pancreas = colfun(5)

  colfun = grDevices::colorRampPalette(c(colors$green.langerhans, colors$white))
  colors$pal.green.langerhans = colfun(5)

  colfun = grDevices::colorRampPalette(c(colors$darkyellow.kidney, colors$white))
  colors$pal.darkyellow.kidney = colfun(5)

  colfun = grDevices::colorRampPalette(c(colors$red.liver, colors$white))
  colors$pal.red.liver = colfun(5)

  colfun = grDevices::colorRampPalette(c(colors$beige.intestine, colors$white))
  colors$pal.beige.intestine = colfun(5)

  return(colors)
}

#' Read LifePort raw data
#'
#' @param file data file with path
#' @param format guess (default), binary or plaintxt
#'
#' @return a list with LifePort data
#'
#' @export
#'
lifeport_read <- function(file, format="guess") {

  # guess ascii vs binary
  # this option will also implement error handling
  # we read the first line, for ascii it contains the full variable header
  # with 83 characters, for binary it only contains the UnitID which is
  # generally < 10 and sometimes an empty string , i.e. "".
  if (format == "guess") {

    # get serial number (binary)
    to.read = file(file, "rb")
    skip = readBin(to.read, raw(), n = 24, size = 1)
    SerialNumber = readBin(to.read, integer(), n = 1, size = 4)
    close(to.read)
    # catch problem of empty object
    SerialNumber = ifelse(length(SerialNumber) == 0, 0, SerialNumber)

    # get first line (header of txt file)
    con = file(file, "r")
    firstLine = readLines(con, n = 1, warn = F)
    close(con)
    firstLine = gsub('"', "", deparse(firstLine))
    firstLine = gsub('\\\\x[0-9a-fA-F]{2}', "?", firstLine)

    # valid binary file when serial number can be read and has 7 digits
    if ( nchar(as.character(SerialNumber)) == 7 ) {
      format = "binary"
    }   else if ( nchar(firstLine) == 83) { # valid txt file if header is 83 long
      format = "plaintxt"
    } else {
      stop(paste("Cannot read the file", basename(file)))
    }
  }

  # read from binary file
  if (format == "binary") {

    # reverse engineering
    # header is 1-64 bytes long
    # data starts from 65-end byte
    # numbers = rep(NA, 64) # keep at 64
    # for (i in 1:64) {
    #   to.read = file(file, "rb")
    #   skip = readBin(to.read, raw(), n =i, size = 1)
    #   print(paste0("byte ", i+1, ":", readBin(to.read, character(), n = 1, size = 1)))
    #   close(to.read)
    # }
    # which(numbers==515)

    # UnitID
    to.read = file(file, "rb")
    UnitID = readBin(to.read, character(), n = 1, size = 1)
    close(to.read)

    # SerialNumber
    to.read = file(file, "rb")
    skip = readBin(to.read, raw(), n = 24, size = 1)
    SerialNumber = readBin(to.read, integer(), n = 1, size = 4)
    close(to.read)

    # Data State?? 3 is complete
    to.read = file(file, "rb")
    skip = readBin(to.read, raw(), n = 28, size = 1)
    DataState = readBin(to.read, integer(), n = 1, size = 2)
    close(to.read)

    # FirmwareVersion
    to.read = file(file, "rb")
    skip = readBin(to.read, raw(), n = 30, size = 1)
    FirmwareVersion = readBin(to.read, integer(), n = 1, size = 2)
    close(to.read)

    # 33 ???
    # to.read = file(file, "rb")
    # skip = readBin(to.read, raw(), n =32, size = 1)
    # as.numeric(readBin(to.read, raw(), n = 1, size = 1))
    # close(to.read)

    # FileID
    to.read = file(file, "rb")
    skip = readBin(to.read, raw(), n = 33, size = 1)
    FileID = as.numeric(readBin(to.read, raw(), n = 1, size = 1))
    close(to.read)

    # Starttime incl. Date
    to.read = file(file, "rb")
    skip = readBin(to.read, raw(), n = 35, size = 1)
    # Date
    StartMonth = as.numeric(readBin(to.read, raw(), n = 1, size = 1))
    StartDay = as.numeric(readBin(to.read, raw(), n = 1, size = 1))
    StartYear = as.numeric(readBin(to.read, raw(), n = 1, size = 1)) + 2000
    # Time
    StartHour = as.numeric(readBin(to.read, raw(), n = 1, size = 1))
    StartMin = as.numeric(readBin(to.read, raw(), n = 1, size = 1))
    StartSec = as.numeric(readBin(to.read, raw(), n = 1, size = 1))
    close(to.read)
    # Create proper Date object "%Y-%m-%d %H:%M:%S"
    StartTime = sprintf("%02d-%02d-%02d %02d:%02d:%02d", StartYear, StartMonth,
                        StartDay, StartHour, StartMin, StartSec)

    # OrganID
    to.read = file(file, "rb")
    skip = readBin(to.read, raw(), n = 41, size = 1)
    OrganID = readBin(to.read, character(), n = 1, size = 1)
    OrganID = strsplit(OrganID, split = "\\\\|[^[:print:]]", fixed = FALSE)[[1]][1]
    close(to.read)

    # 54 KidneySide: 1 right, 2 left
    # 55 BloodType: 1 A, 2 B, 3 AB, 4 O
    to.read = file(file, "rb")
    skip = readBin(to.read, raw(), n = 53, size = 1)
    KidneySide.Nr = as.numeric(readBin(to.read, raw(), n = 1, size = 1))
    BloodTyp.Nr = as.numeric(readBin(to.read, raw(), n = 1, size = 1))
    close(to.read)

    # assign BloodType and KidneySide
    KidneySide = switch(KidneySide.Nr, "Right", "Left")
    BloodType = switch(BloodTyp.Nr, "A", "B", "AB", "0")
    KidneySide = ifelse(is.null(KidneySide), NA, KidneySide)
    BloodType = ifelse(is.null(BloodType), NA, BloodType)

    # ClampTime
    to.read = file(file, "rb")
    skip = readBin(to.read, raw(), n = 55, size = 1)
    # Date
    ClampMonth = as.numeric(readBin(to.read, raw(), n = 1, size = 1))
    ClampDay = as.numeric(readBin(to.read, raw(), n = 1, size = 1))
    ClampYear = as.numeric(readBin(to.read, raw(), n = 1, size = 1)) + 2000
    # Time
    ClampHour = as.numeric(readBin(to.read, raw(), n = 1, size = 1))
    ClampMin = as.numeric(readBin(to.read, raw(), n = 1, size = 1))
    ClampSec = as.numeric(readBin(to.read, raw(), n = 1, size = 1))
    close(to.read)
    # Create proper Date object "%Y-%m-%d %H:%M:%S"
    ClampTime = sprintf("%02d-%02d-%02d %02d:%02d:%02d", ClampYear, ClampMonth,
                        ClampDay, ClampHour, ClampMin, ClampSec)
    if (ClampTime == "2000-00-00 00:00:00") {ClampTime = NA}

    # data device
    data.device = data.frame(array(NA, dim=c(1,9)))
    colnames(data.device) = c("SerialNumber", "Type", "SubType", "UnitID",
                              "FirmwareVersion", "FileID", "StartTime",
                              "DataState", "HasGaps")
    data.device$SerialNumber = SerialNumber
    data.device$UnitID = UnitID
    data.device$FirmwareVersion = FirmwareVersion
    data.device$FileID = FileID
    data.device$StartTime = StartTime

    # data organ
    data.organ = data.frame(array(NA, dim=c(1,13)))
    colnames(data.organ) = c("OrganID", "KidneySide", "BloodType", "CrossClampTime.Date",
                             "CrossClampTimezone", "TotalIschemicTime", "PerfusateLot",
                             "PerfusateExpirationDate", "PerfusateUsed", "Cannula",
                             "CannulaExpirationDate", "CassetteLot.", "CasetteExpirationDate"
    )
    data.organ$OrganID = OrganID
    data.organ$KidneySide = KidneySide
    data.organ$BloodType = BloodType
    data.organ$CrossClampTime.Date = ClampTime

    # Data (timeseries)
    to.read = file(file, "rb")
    skip = readBin(to.read, raw(), n = 64, size = 1)
    data.raw = readBin(to.read, integer(), n =10^6, size = 2)
    close(to.read)
    no_rows = length(data.raw)/16
    data.raw = t(array(data.raw, dim=c(16,no_rows)))
    # remove last two rows filled with -1 (found in 2 examples, probably in all)
    if (nrow(data.raw) == 2) { # when file is empty, two rows of -1: fill NA
      data.raw = array(NA, dim = c(5,16))
    }

    data.raw = data.raw[1:(nrow(data.raw)-2),]

    data = data.frame(
      SequentialRecordNumber = data.raw[,1],
      SerialNumber           = NA,
      FileID                 = data.raw[,3],
      InfuseTime             = NA,
      FlowRate               = data.raw[,5],
      OrganResistance        = data.raw[,6],
      IceContainerTemperature= data.raw[,7],
      InfuseTemperature      = data.raw[,8],
      Error1                 = data.raw[,9],
      Error2                 = data.raw[,10],
      State                  = data.raw[,11],
      PressureSet            = data.raw[,12],
      AveragePressure        = data.raw[,13],
      DiastolicPressure      = data.raw[,14],
      SystolicPressure       = data.raw[,15],
      Checksum               = data.raw[,16]
    )

    # when file is ascii data (export from ORS Data Station)
  } else if (format == "plaintxt"){

    data.device = utils::read.csv(file = file, nrows = 1, head = TRUE)
    data.organ  = utils::read.csv(file = file, skip = 3, nrows = 1, head = TRUE)
    data = utils::read.csv(file = file, skip = 6, head = TRUE)

    # fix StartTime for consistency with binary data
    # bin file: 2022-05-06 11:18:07
    # txt file: 06.05.2022 11:18:07
    # as.POSIXct("1970-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "CET")
    data.device$StartTime =
      as.character(as.POSIXct(data.device$StartTime, format = "%d.%m.%Y %H:%M:%S",
                              tz = "CET"))
  }

  # Conversions since data is stored in integers
  data$InfuseTemperature = data$InfuseTemperature/10
  data$IceContainerTemperature = data$IceContainerTemperature/10
  data$OrganResistance = data$OrganResistance/100

  # fix for invalid UnitID with special characters (Genève)
  data.device$UnitID = iconv(data.device$UnitID, "ASCII", "UTF-8")

  data.list = list(
    data.device=data.device,
    data.organ = data.organ,
    data = data)

  return(data.list)
}

#' Process LifePort data. Adds runtime, clock time, and smoothed time series.
#'
#' @param lpdat a list with data from lifeport_read()
#' @param window_size rolling window size for filtering
#'
#' @return a list with LifePort data
#' @export
#'
lifeport_process <- function(lpdat, window_size = 15) {

  # Calculate runtime from StartTime and number of samples
  n = nrow(lpdat$data) # number of samples every 10 seconds
  start = as.POSIXct(lpdat$data.device$StartTime, format = "%Y-%m-%d %H:%M:%S",
                     tz = "CET")
  if (is.na(start))  {
    start = as.POSIXct("2000-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%S",
                       tz = "CET")
  }

  dur = (start + n*10) - start
  lpdat$data.device$Runtime = as.character(hms::round_hms(hms::as_hms(dur), 1))

  # We calculate own time vector ignoring the duplicated timestamps in InfuseTime
  # InfuseTime is only in the txt file so must be an bug in ORS software export

  # relative clock
  lpdat$data$time.clock = start + seq(0, n*10 - 1, 10)

  # absolute clock starting from 0
  start.zero = as.POSIXct("2000-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "CET")
  lpdat$data$time.zero = start.zero + seq(0, n*10 - 1, 10)

  # stop time
  lpdat$data.device$StopTime = as.character(lpdat$data$time.clock[nrow(lpdat$data)])

  # timeseries filtering
  lpdat$data$SystolicPressure.flt  =
    data.table::frollmean(lpdat$data$SystolicPressure, n = window_size, align = "center")
  lpdat$data$DiastolicPressure.flt =
    data.table::frollmean(lpdat$data$DiastolicPressure, n = window_size, align = "center")
  lpdat$data$AveragePressure.flt =
    data.table::frollmean(lpdat$data$AveragePressure, n = window_size, align = "center")
  lpdat$data$FlowRate.flt =
    data.table::frollmean(lpdat$data$FlowRate, n = window_size, align = "center")
  lpdat$data$OrganResistance.flt =
    data.table::frollmean(lpdat$data$OrganResistance, n = window_size, align = "center")

  # remove crazy temperatures
  lpdat$data$InfuseTemperature[lpdat$data$InfuseTemperature > 200] = NA
  lpdat$data$IceContainerTemperature[lpdat$data$IceContainerTemperature > 200] = NA

  return(lpdat)
}

#' Summary statistics for LifePort data.
#'
#' @param lpdat a list with data from lifeport_process()
#' @param ice_threshold threshold for ice temperature in degrees Celsius
#' @param infuse_threshold threshold for infuse temperature in degrees Celsius
#'
#' @return a list with LifePort data
#'
#' @importFrom stats median lm
#' @importFrom segmented segmented
#'
#' @export
#'
lifeport_sumstats <- function(lpdat, ice_threshold = 2.5,
                              infuse_threshold = 10) {

  # Thresholds that may be changed with good reasoning
  THR_ICE = ice_threshold
  THR_INF = infuse_threshold

  # Thresholds that are more kind of fixed
  THR_PRES = 0
  THR_FLOW = 5
  THR_FLOW_PERF = 25 # only 2.5% of cases is 25 or lower
  THR_RES  = 0

  IDX_2MIN = 12
  IDX_30MIN = 180
  IDX_60MIN = 360

  INF_START_WINDOW = 30 # 5 minutes
  INF_START_IDX = (IDX_2MIN + 1):(IDX_2MIN + INF_START_WINDOW)
  # average across 5 min but exclude first 2 min., thus 13:42 (from 3 to 7 min)

  NO_CHANGEPOINTS = 2 # number of change points for slope detection

  ## Perfusion time

  # The time in minutes duration the kidney was perfused
  perfusion.dur = (sum(lpdat$data$FlowRate.flt > THR_FLOW, na.rm = TRUE)*10)/60
  perfusion.dur.str = as.character(hms::round_hms(hms::as_hms(perfusion.dur*60), 1))

  ## Pressure

  # mean across positive values as machine may not be turned off after kidney removal
  # also perfusion needs to be at least 5 minutes
  idx = lpdat$data$SystolicPressure.flt > THR_PRES & perfusion.dur > 5
  systolicPressure.md = median(lpdat$data$SystolicPressure.flt[idx], na.rm = TRUE)

  idx = lpdat$data$DiastolicPressure.flt > THR_PRES & perfusion.dur > 5
  diastolicPressure.mean = mean(lpdat$data$DiastolicPressure.flt[idx], na.rm = TRUE)

  ## Flow rate and resistance

  # We split timeseries in first 30 min and the rest
  # only spitting when timeseries is > 30 min
  # for mean only flow > 0 is considered
  flowRate.mean  = NA
  organResistance.mean  = NA
  organResistance.sd  = NA

  organResistance.x1    = NA
  organResistance.y1    = NA
  organResistance.x2    = NA
  organResistance.y2    = NA
  organResistance.delta = NA
  organResistance.slope = NA

  # the indicators are only calculated when perfusion is larger than > 30 min.
  # previously it was when recording was > 30 min.
  #if (length(lpdat$data$SequentialRecordNumber) > IDX_30MIN) {
  if (perfusion.dur > 30) {

    # 181 samples is 30 min.
    idx = lpdat$data$SequentialRecordNumber > IDX_30MIN &
      lpdat$data$FlowRate.flt > THR_FLOW
    flowRate.mean = mean(lpdat$data$FlowRate.flt[idx], na.rm = TRUE)

    idx = lpdat$data$SequentialRecordNumber > IDX_30MIN &
      lpdat$data$OrganResistance.flt > THR_RES
    organResistance.mean = mean(lpdat$data$OrganResistance.flt[idx], na.rm = TRUE)
    organResistance.sd = stats::sd(lpdat$data$OrganResistance.flt[idx], na.rm = TRUE)

    # vascular indicators: delta and slope
    NO_NA = sum(is.na(lpdat$data$OrganResistance.flt[1:40])) # No of NA due to smoothing

    # data frame for change point detection in the first 60 min.
    l = min(IDX_60MIN, nrow(lpdat$data)) # calculate length
    d.vi = data.frame(
      x    = 1:l,
      y    = lpdat$data$OrganResistance.flt[1:l]
    )

    # calculate vascular indicators only if there is no flat line at the last 10 minutes
    # of the first 60 minutes period
    if ( !all(d.vi$y[(length(d.vi$y) - 10*6):length(d.vi$y)] <= THR_RES) ) {

      fit.lm = lm(y ~ 1 + x, data = d.vi)  # intercept-only model
      fit = segmented::segmented(fit.lm, seg.Z = ~x, npsi = NO_CHANGEPOINTS)
      y_ = c(rep(NA, NO_NA), fit$fitted.values) # adjust same length due to smoothing
      # debug
      # plot(fit); points(d.vi$y)

      # get segment with largest negative change (reduction in organ resistance)
      # calculate differences between yi at change point
      y_points = c(y_[NO_NA + 1], y_[fit$psi[,2]],  y_[length(y_)])
      x_points = c(NO_NA + 1, fit$psi[,2],  length(y_))
      k_seg = which.min(diff(y_points)) # pick segment with the strongest decrease

      # segment k has points P(x_k,y_k) and P(x_k+1, y_k+1)
      organResistance.x1    = x_points[k_seg]
      organResistance.y1    = y_points[k_seg]
      organResistance.x2    = x_points[k_seg + 1]
      organResistance.y2    = y_points[k_seg + 1]
      organResistance.delta = y_points[k_seg + 1] - y_points[k_seg]
      # scale as change in resistance per minute
      organResistance.slope = segmented::slope(fit)$x[,1][k_seg] * 6
    }

  }

  ## Temperature

  # ice
  iceContainerTemperature.mean = mean(lpdat$data$IceContainerTemperature, na.rm = TRUE)
  iceContainerTemperature.sd = stats::sd(lpdat$data$IceContainerTemperature, na.rm = TRUE)
  iceContainerTemperature.minAbove = (sum(lpdat$data$IceContainerTemperature > THR_ICE)*10)/60
  iceContainerTemperature.minAbove.str =
    as.character(hms::round_hms(hms::as_hms(iceContainerTemperature.minAbove*60), 1))

  # mean and SD of inf temperature are calculated excluding the first 2 min. and
  # excluding segments with no flow using idx
  # for example, infuse temp in first 2 min. affect sd and mean
  idx = lpdat$data$SequentialRecordNumber > IDX_2MIN &
    lpdat$data$FlowRate > THR_FLOW_PERF

  # infuse
  infuseTemperature.mean = NA
  infuseTemperature.sd = NA
  infuseTemperature.start = NA
  if (perfusion.dur > 5) { # only calculate mean and sd when > 5 min duration.
    # calculate the following indicators when flow is positive using idx
    # for start temp this works also well to exclude segments of no perfusion at start
    infuseTemperature.mean = mean(lpdat$data$InfuseTemperature[idx], na.rm = TRUE)
    infuseTemperature.sd = stats::sd(lpdat$data$InfuseTemperature[idx], na.rm = TRUE)

    infuseTemperature.start =
      mean(lpdat$data$InfuseTemperature[lpdat$data$FlowRate > THR_FLOW][INF_START_IDX], na.rm = TRUE)
  }
  infuseTemperature.minAbove = (sum(lpdat$data$InfuseTemperature[idx] > THR_INF, na.rm = TRUE)*10)/60
  infuseTemperature.minAbove.str = as.character(hms::round_hms(hms::as_hms(infuseTemperature.minAbove*60), 1))

  ## prepare data

  sumstats = data.frame(

    perfusion.dur     = perfusion.dur,
    perfusion.dur.str = perfusion.dur.str,

    systolicPressure.md    = systolicPressure.md,
    diastolicPressure.mean = diastolicPressure.mean,

    flowRate.mean         = flowRate.mean,
    organResistance.mean  = organResistance.mean,
    organResistance.sd    = organResistance.sd,

    organResistance.x1    = organResistance.x1,
    organResistance.y1    = organResistance.y1,
    organResistance.x2    = organResistance.x2,
    organResistance.y2    = organResistance.y2,
    organResistance.delta = organResistance.delta,
    organResistance.slope = organResistance.slope,

    iceContainerTemperature.mean     = iceContainerTemperature.mean,
    iceContainerTemperature.sd       = iceContainerTemperature.sd,
    iceContainerTemperature.minAbove = iceContainerTemperature.minAbove,
    iceContainerTemperature.minAbove.str = iceContainerTemperature.minAbove.str,

    infuseTemperature.mean = infuseTemperature.mean,
    infuseTemperature.sd = infuseTemperature.sd,
    infuseTemperature.start = infuseTemperature.start,
    infuseTemperature.minAbove = infuseTemperature.minAbove,
    infuseTemperature.minAbove.str = infuseTemperature.minAbove.str
  )

  lpdat$data.sumstats = sumstats

  return(lpdat)
}

#' Calculate Mahalanobis distance D-square for LifePort temperature and perfusion data.
#'
#' @param data data frame or matrix with temperature or perfusion data
#' @param type string, type of D-square either "temp" or "perf"
#'
#' @return vector with D-squares
#'
#' @importFrom stats mahalanobis
#'
#' @export
#'
lifeport_d2 <- function(data, type) {

  d2 = NA

  if (type == "temp") {
    d2 = stats::mahalanobis(x = data,
                            center = idat.md.temp.center,
                            cov = idat.md.temp.cov)
  } else if (type == "perf") {
    d2 = stats::mahalanobis(x = data,
                            center = idat.md.perf.center,
                            cov = idat.md.perf.cov)
  }

  return(d2)
}

#' Returns the percentile rank of the temperature or perfusion D-squared.
#'
#' @param d2 D-squared
#' @param type string, type of D-square either "temp" or "perf"
#'
#' @return percentile rank
#'
#' @export
#'
lifeport_d2toRank <- function(d2, type) {
  P = NA

  if (type == "temp") {
    P = idat.fn.D2.temp(d2)
  } else if (type == "perf") {
    P = idat.fn.D2.perf(d2)
  }

  return(P)
}

#' Returns mean and SD.
#'
#' @param x a numeric vector
#' @param d1 number of digits
#' @param d2 number of digits
#'
#' @return character object
#'
#' @importFrom stats sd
#'
#' @export
#'
mean_sd = function(x, d1 = 1, d2 = 1) {
  return(sprintf(paste0("%.", d1, "f (%.", d2, "f)"),
                 mean(x, na.rm = TRUE),
                 sd(x, na.rm = TRUE))
  )
}

#' Returns median and interquartile range IQR.
#'
#' @param x a numeric vector
#' @param d1 number of digits
#' @param d2 number of digits
#' @param d3 number of digits
#' @param compact use en dash instead of "from X to Y"
#'
#' @return character object
#'
#' @importFrom stats quantile
#'
#' @export
#'
median_iqr = function(x, d1 = 1, d2 = 1, d3 = 1, compact = FALSE) {

  if (compact) {

    format = paste0("%.", d1, "f (%.", d2, "f\U2012%.", d3, "f)")

  } else {

    format = paste0("%.", d1, "f (from %.", d2, "f to %.", d3, "f)")
  }

  return(sprintf(format, median(x, na.rm = TRUE),
                 quantile(x, probs = 0.25, na.rm = TRUE),
                 quantile(x, probs = 0.75, na.rm = TRUE))
  )
}

#' Returns frequency count and percentage.
#'
#' @param x a logical vector
#' @param count.na count NAs in denominator
#' @param d2 number of digits
#'
#' @return character object
#'
#' @export
#'
freq_perc = function(x, count.na = TRUE, d2 = 1) {
  if (!count.na) {x = x[!is.na(x)]}
  return(sprintf(paste0("%d (%.", d2, "f)"),
                 sum(x, na.rm = TRUE),
                 sum(x, na.rm = TRUE)/length(x)*100))
}

#' Returns frequency count and percentage of missing data.
#'
#' @param x a vector
#' @param d2 number of digits
#'
#' @return character object
#'
#' @export
#'
miss_perc = function(x, d2 = 1) {
  return(sprintf(paste0("%d (%.", d2, "f%%)"),
                 sum(is.na(x)),
                 sum(is.na(x))/length(x)*100))
}

#' Formats p-values.
#'
#' @param x numerical vector with p-values
#'
#' @return formatted p-values as character vector
#'
#' @export
#'
tidy_pvalues <- function(x) {

  f = function(p){

    if (is.na(p)) {
      p.fmt = NA_character_

    } else if (p >= 0 & p <= 0.001) {
      p.fmt = "< 0.001 ***"

    } else if (p > 0.001 & p <= 0.01) {
      p.fmt = sprintf("%.3f **", p)

    } else if (p > 0.01 & p <= 0.05) {
      p.fmt = sprintf("%.3f *", p)

    } else if (p > 0.05 & p <= 0.10) {
      p.fmt = sprintf("%.3f .", p)

    } else if (p > 0.10 & p <= 1) {
      p.fmt = sprintf("%.2f", p)

    } else {
      p.fmt = NA_character_
    }
  }
  return(vapply(X = x, FUN = f, FUN.VALUE = ""))
}

#' Tidy rms model fit results.
#'
#' @param fit model fit from rms
#' @param ... optional arguments to summary of the rms fit object.
#'
#' @return formatted data.frame
#'
#' @importFrom stats anova
#'
#' @export
#'
tidy_rmsfit <- function(fit, ...) {

  CHAR_DASH = "\U2013"

  tab.1 = as.data.frame(summary(fit, ...))
  tab.2 = as.data.frame(anova(fit)) # anova from rms package

  # OLS
  if (all(class(fit) == c("ols", "rms", "lm" ))) {

    tab.1$Diff.tidy = sprintf("%.2f (from %.2f to %.2f)",
                              tab.1$Diff., tab.1$Low, tab.1$High)
    tab.1$Effect.tidy = sprintf("%.2f (from %.2f to %.2f)",
                                tab.1$Effect, tab.1$`Lower 0.95`, tab.1$`Upper 0.95`)

    # improve nonlinear terms names so I can sort by rowname
    idx = grep("Nonlinear", rownames(tab.2), ignore.case = FALSE)
    mynames = paste(rownames(tab.2)[idx-1], "nonlinear") # get name one above nonlinear term
    rownames(tab.2)[idx] = mynames

    tab.2$F = prettyNum(signif(tab.2$F, digits = 3))
    tab.2$P = tidy_pvalues(tab.2$P)

    # merge both tables
    tab = merge(x = tab.2, y = tab.1, by = "row.names", all.x = TRUE, all.y = TRUE)
    rownames(tab) = tab$Row.names

    # put TOTAL NONLINEAR, TOTAL, and ERROR at the end
    k = grep("^ERROR$", rownames(tab)) # last row
    k_1 = grep("^TOTAL$", rownames(tab)) # last second last
    k_2 = grep("^TOTAL.NONLINEAR$", rownames(tab))
    tab = rbind(tab[c(-k, -k_1, -k_2),], tab[c(k_2, k_1, k),])

    # remove values when dichotomous or categorical and replace with endash
    tab$Diff.tidy[is.na(tab$Diff.)] = CHAR_DASH
    tab$Effect.tidy[is.na(tab$Effect.)] = CHAR_DASH
    tab$F[is.na(tab$F)] = CHAR_DASH
    tab$F[tab$F == "NA"] = CHAR_DASH
    tab$d.f.[is.na(tab$d.f.)] = CHAR_DASH
    tab$P[is.na(tab$P)] = CHAR_DASH

    tab = tab[, c("Diff.tidy", "Effect.tidy", "F", "d.f.", "P")]

    colnames(tab) = c("Interquartile difference",
                      "Effect estimate (95%-CI)",
                      "F-value", "d.f.", "p-value")

    # nice rownames
    rn = rownames(tab)
    rn = gsub("(.*)\\s-\\s(.*):(.*)", "\\1 \\2", rn) # remove baseline level
    rn = gsub("_|\\.", " ", rn) # remove underline and dots
    rownames(tab) = rn

    return(tab)

    # LRM or CPH
  } else if ( all(class(fit) == c("lrm", "rms", "glm" )) |
              all(class(fit) == c("cph", "rms", "coxph" )) ) {

    tab.1 = tab.1[!grepl("Hazard.Ratio|Odds.Ratio", rownames(tab.1)),] # remove HRs and ORs
    tab.1$Diff.tidy = sprintf("%.2f (%.2f\U2013%.2f)", tab.1$Diff., tab.1$Low, tab.1$High)
    tab.1$Effect.tidy = sprintf("%.2f (from %.2f to %.2f)",
                                exp(tab.1$Effect), exp(tab.1$`Lower 0.95`),
                                exp(tab.1$`Upper 0.95`))

    # improve nonlinear terms names so I can sort by rowname
    idx = grep("Nonlinear", rownames(tab.2), ignore.case = FALSE)
    mynames = paste(rownames(tab.2)[idx-1], "nonlinear") # get name one above nonlinear term
    rownames(tab.2)[idx] = mynames

    tab.2$`Chi-Square` = prettyNum(signif(tab.2$`Chi-Square`, digits = 3))
    tab.2$P = tidy_pvalues(tab.2$P)

    # merge both tables
    tab = merge(x = tab.2, y = tab.1, by = "row.names", all.x = TRUE, all.y = TRUE)
    rownames(tab) = tab$Row.names

    # put TOTAL NONLINEAR and TOTAL at the end
    k = grep("^TOTAL$", rownames(tab)) # last row
    k_1 = grep("^TOTAL.NONLINEAR$", rownames(tab)) # second last row
    tab = rbind(tab[c(-k, -k_1),], tab[c(k_1, k),])

    # remove values when dichotomous or categorical and replace with endash
    tab$Diff.tidy[is.na(tab$Diff.)] = CHAR_DASH
    tab$Effect.tidy[is.na(tab$Effect)] = CHAR_DASH
    tab$`Chi-Square`[is.na(tab$`Chi-Square`)] = CHAR_DASH
    tab$d.f.[is.na(tab$d.f.)] = CHAR_DASH
    tab$P[is.na(tab$P)] = CHAR_DASH

    # selection of colums to display
    tab = tab[,c("Diff.tidy", "Effect.tidy", "Chi-Square", "d.f.", "P")]

    if (class(fit)[1] == "lrm") {

      colnames(tab) = c("Interquartile difference",
                        "Odds ratio (95%-CI)",
                        "Chi-Square", "d.f.", "p-value")

    } else if ((class(fit)[1] == "cph")) {

      colnames(tab) = c("Interquartile difference",
                        "Hazard ratio (95%-CI)",
                        "Chi-Square", "d.f.", "p-value")

    }

    # nice rownames
    rn = rownames(tab)
    rn = gsub("(.*)\\.\\.\\.(.*)\\.(.*)", "\\1 \\2", rn) # remove baseline level
    rn = gsub("_|\\.", " ", rn) # remove underline and dots
    rownames(tab) = rn

    return(tab)

  } else {

    warning("Unknown model: Seriously, reconsider your life choices.")

  }

}

#' Tidy missing data summary from data frame.
#'
#' @param df data frame with raw data
#'
#' @return data frame with summary data
#'
#' @export
#'
tidy_missing = function(df) {

  tab = apply(df, 2, FUN = miss_perc)
  tab = as.data.frame((apply(df, 2, FUN = miss_perc)))
  colnames(tab)[1] = "Missing"
  return(tab)
}

#' Nearest element in vector for a given set of values.
#'
#' @param y vector to be searched
#' @param q vector of values of interest
#'
#' @return indices of the nearest elements in y for a set of values in q.
#'
#' @export
#'
nearest <- function(y, q) {
  ind = rep(NA, length(q))
  for (i in 1:length(q)) {
    ind[i] = which.min( abs(y - q[i]) )
  }
  return(ind)
}


#' Convert Excel days since origin to POSIXct data type (date/time)
#'
#' @param days days since origin as numeric or string
#' @param origin origin, default in excel is 1899-12-30
#' @param tz time zone to be forced upon
#' @param filter a fix for dates not recognized (default is TRUE)
#' @param pattern the pattern to find dates not recognized
#' @param format format to convert dates not recognized, e.g. \%d.\%m.\%Y \%H:\%M:\%OS
#' @param round recommended when format has no time, only date information
#'
#' @return date of the type POSIXct
#'
#' @importFrom lubridate force_tz
#'
#' @export
#'
num2date <- function(days, origin = "1899-12-30", tz = "CET", filter = TRUE,
                     pattern = "[0-9]{2}\\.[0-9]{2}\\.[0-9]{4}",
                     format = "%d.%m.%Y", round = TRUE) {
  # TODO: Add adjustment factor by +1 second (default)

  # when a data frame contains a date variable but it is all NA the data type is logical
  # and requires conversion to numeric
  if (is.logical(days) & all(is.na(days))) {
    days = as.numeric(days)
  }

  if ( !is.character(days) & !is.numeric(days) ) {
    stop("'days' must be of type numeric or character")
  }

  # sometimes dates are not recognized due to inconsistencies in excel
  # this filter fixes this issue for date of a specified pattern
  if (filter) {
    idx = grepl(pattern = pattern, days)
    #dates_fixed = as.Date(days[idx], tz = "CET", format = "%d.%m.%Y")
    dates_fixed = as.POSIXct(days[idx], tz = tz, format = format)
    days_fixed = as.numeric(difftime(dates_fixed, origin)) # convert back to numbers
    if (round) {days_fixed = round(days_fixed)}
    days[idx] = as.character(days_fixed) # round to fix 1 hour offset
  }

  if (is.character(days)) {
    days = as.numeric(days)
  }

  days[days == 0] = NA # sometimes empty dates are 0 in excel sheets
  dates = as.POSIXct(as.Date(days, origin = origin))

  dates = force_tz(dates, tzone = tz) # force timezone, as we have no tz info from Excel

  return(dates)
}

#' Convert POSIXct data type (date/time) to Excel days since origin
#'
#' @param dates character string in the form of YYYY-mm-dd
#'
#' @return number of days
#'
#' @export
#'
date2num <- function(dates) {

  if ( !is.character(dates) ) {
    stop("'dates' must be of type character")
  }

  days = as.numeric(as.POSIXct(dates, tz = "UTC") - as.POSIXct("1899-12-30", tz = "UTC"))

  return(days)
}

#' CKD-EPI Creatinine Equation for eGFR (2021)
#' see https://www.kidney.org/content/ckd-epi-creatinine-equation-2021
#' @param SCr Serum creatinine in mg/dL (US) or μmol/L (S)
#' @param age age in years
#' @param sex either "F" for female, or "M" for male
#' @param units unit for SCr, either "SI" (μmol/L; default) or "US" (mg/dL)
#'
#' @return eGFR mL/min/1.73m2
#'
#' @export
#'
egfr_ckd_epi <- function(SCr, age, sex, units = "SI") {

  n = length(SCr)

  if (units == "SI") {
    SCr = SCr/88.4
  } else if (!grepl("^SI$|^US$", units)) {
    stop("'units' must be 'SI' or 'US'")
  }

  K = rep(NA, n)
  K[sex == "F"] = 0.7
  K[sex == "M"] = 0.9

  alpha = rep(NA, n)
  alpha[sex == "F"] = -0.241
  alpha[sex == "M"] = -0.302

  egfr = 142 *
    pmin(SCr/K, 1)^alpha *
    pmax(SCr/K, 1)^-1.2 * 0.9938^age *
    ifelse(sex == "F", 1.012, 1)

  return(round(egfr))
}

#' Revised Schwartz Equation for eGFR (2009)
#' see https://www.mdcalc.com/calc/10008/revised-schwartz-equation-glomerular-filtration-rate-gfr-2009#evidence
#' @param SCr Serum creatinine in mg/dL (US) or μmol/L (S)
#' @param height in cm
#' @param units unit for SCr, either "SI" (μmol/L; default) or "US" (mg/dL)
#'
#' @return eGFR mL/min/1.73m2
#'
#' @export
#'
egfr_schwartz <- function(SCr, height, units = "SI") {

  if (units == "SI") {
    SCr = SCr/88.4
  } else if (!grepl("^SI$|^US$", units)) {
    stop("'units' must be 'SI' or 'US'")
  }

  k = 0.413
  egfr = k * height/SCr

  return(round(egfr))
}

# Format HLA
# Helper function to format HLA string for broads
# e.g. A(10) becomes A10
# "A" becomes NA

#' Helper function to format strings for broads, e.g. A(10) becomes A10 and A becomes NA
#'
#' @param v_char character vector
#'
#' @return formatted character vector
#' @export
#'
fmt_hla <- function(v_char) {
  # remove ()
  v_char = sub("\\((\\d*)\\)", "\\1", v_char)
  # replace empty with NA
  idx = grepl("^A$|^B$|^DR$", v_char)
  v_char[idx] = NA

  return(v_char)
}

#' Parser for the unstructured SOAS HLA information into structured data.
#'
#' @param D_HLA Donor HLA antigens. Character string from SOAS variable D HLA Ag.
#' @param R_HLA Recipient HLA antigens. Character string from SOAS variable R HLA Ag.
#'
#' @return a data frame with structured HLA information.
#' @export
#'
HLA_parse <- function(D_HLA, R_HLA) {

  A_pattern  = ".*A\\[(\\d*)(\\(\\d*?\\))*,(\\d*)(\\(\\d*?\\))*\\].*"
  B_pattern  = ".*B\\[(\\d*)(\\(\\d*?\\))*,(\\d*)(\\(\\d*?\\))*\\].*"
  DR_pattern =".*DR\\[(\\d*)(\\(\\d*?\\))*,(\\d*)(\\(\\d*?\\))*\\].*"
  # Note: In the molecular nomenclature, the name of this locus is DRB1.

  tab = data.frame(
    # 1, 2: split/broad and optional broad allele 1
    # 3, 4: split/broad and optional broad allele 2

    # Locus A
    D.A1   =         sub(A_pattern , "A\\1", D_HLA),
    D.A1.b = fmt_hla(sub(A_pattern , "A\\2", D_HLA)),
    D.A2   =         sub(A_pattern , "A\\3", D_HLA),
    D.A2.b = fmt_hla(sub(A_pattern , "A\\4", D_HLA)),

    R.A1   =         sub(A_pattern , "A\\1", R_HLA),
    R.A1.b = fmt_hla(sub(A_pattern , "A\\2", R_HLA)),
    R.A2   =         sub(A_pattern , "A\\3", R_HLA),
    R.A2.b = fmt_hla(sub(A_pattern , "A\\4", R_HLA)),

    # Locus B
    D.B1   =         sub(B_pattern , "B\\1", D_HLA),
    D.B1.b = fmt_hla(sub(B_pattern , "B\\2", D_HLA)),
    D.B2   =         sub(B_pattern , "B\\3", D_HLA),
    D.B2.b = fmt_hla(sub(B_pattern , "B\\4", D_HLA)),

    R.B1   =         sub(B_pattern , "B\\1", R_HLA),
    R.B1.b = fmt_hla(sub(B_pattern , "B\\2", R_HLA)),
    R.B2   =         sub(B_pattern , "B\\3", R_HLA),
    R.B2.b = fmt_hla(sub(B_pattern , "B\\4", R_HLA)),

    # Locus DR
    D.DR1   =         sub(DR_pattern , "DR\\1", D_HLA),
    D.DR1.b = fmt_hla(sub(DR_pattern , "DR\\2", D_HLA)),
    D.DR2   =         sub(DR_pattern , "DR\\3", D_HLA),
    D.DR2.b = fmt_hla(sub(DR_pattern , "DR\\4", D_HLA)),

    R.DR1   =         sub(DR_pattern , "DR\\1", R_HLA),
    R.DR1.b = fmt_hla(sub(DR_pattern , "DR\\2", R_HLA)),
    R.DR2   =         sub(DR_pattern , "DR\\3", R_HLA),
    R.DR2.b = fmt_hla(sub(DR_pattern , "DR\\4", R_HLA))
  )

  # add separately for A, B and DR for
  tab$D.HLA.A = sub(A_pattern, "A[\\1\\2,\\3\\4]", D_HLA)
  tab$R.HLA.A = sub(A_pattern, "A[\\1\\2,\\3\\4]", R_HLA)

  tab$D.HLA.B = sub(B_pattern, "B[\\1\\2,\\3\\4]", D_HLA)
  tab$R.HLA.B = sub(B_pattern, "B[\\1\\2,\\3\\4]", R_HLA)

  tab$D.HLA.DR = sub(DR_pattern, "DR[\\1\\2,\\3\\4]", D_HLA)
  tab$R.HLA.DR = sub(DR_pattern, "DR[\\1\\2,\\3\\4]", R_HLA)

  # add the original data from SOAS
  tab$D.HLA = D_HLA
  tab$R.HLA = R_HLA

  # quality checks
  i = 1:length(D_HLA)
  testit::assert(sapply(i, FUN = function(x) grepl(tab$D.HLA.A[x],  D_HLA[x], fixed = TRUE)))
  testit::assert(sapply(i, FUN = function(x) grepl(tab$D.HLA.B[x],  D_HLA[x], fixed = TRUE)))
  testit::assert(sapply(i, FUN = function(x) grepl(tab$D.HLA.DR[x], D_HLA[x], fixed = TRUE)))

  testit::assert(sapply(i, FUN = function(x) grepl(tab$R.HLA.A[x],  R_HLA[x], fixed = TRUE)))
  testit::assert(sapply(i, FUN = function(x) grepl(tab$R.HLA.B[x],  R_HLA[x], fixed = TRUE)))
  testit::assert(sapply(i, FUN = function(x) grepl(tab$R.HLA.DR[x], R_HLA[x], fixed = TRUE)))

  return(tab)
}

# The function calculates HLA mismatches. The serological nomenclature in SOAS
# is as follows:
#
# L[p, q]
#
# L is the locus A B or DR. p and q are the two alleles of the locus L and the
# convention is p <= q. The case p != q is known as heterozygote.
#
# A[2, 25]
#
# Homozygote if p = q. DR[11,11]
#
# The HLA-matching process has to handle broad and splits. Two alleles p and r
# on the same locus L match if they are equal or if one of the allele is the
# broad of the other allele . Two different splits of same broad do not match.
# calculate mismatch
# we look up donor antigens and match them in the recipient. In other words,
# how many unknown antigens are transferred to the donor?

#' The function calculates HLA mismatches.
#'
#' @param D.A1 Donor HLA Antigen on allele 1 locus A
#' @param D.A2 Donor HLA Antigen on allele 2 locus A
#' @param D.B1 Donor HLA Antigen on allele 1 locus B
#' @param D.B2 Donor HLA Antigen on allele 2 locus B
#' @param D.DR1 Donor HLA Antigen on allele 1 locus DR
#' @param D.DR2 Donor HLA Antigen on allele 2 locus DR
#' @param R.A1 Recipient HLA Antigen on allele 1 locus A
#' @param R.A2 Recipient HLA Antigen on allele 2 locus A
#' @param R.B1 Recipient HLA Antigen on allele 1 locus B
#' @param R.B2 Recipient HLA Antigen on allele 2 locus B
#' @param R.DR1 Recipient HLA Antigen on allele 1 locus DR
#' @param R.DR2 Recipient HLA Antigen on allele 2 locus DR
#'
#' @return data frame with mismatch information.
#' @export
#'
HLA_mismatch <- function(D.A1, D.A2, D.B1, D.B2, D.DR1, D.DR2,
                         R.A1, R.A2, R.B1, R.B2, R.DR1, R.DR2) {

  # Locus A
  A.Mm = rep(NA, length(D.A1))
  # subset homocygote allele
  idx = D.A1 == D.A2
  A.Mm[idx] = as.numeric( (D.A1[idx] != R.A1[idx]) & (D.A1[idx] != R.A2[idx]) )

  # subset heterocygote allele
  idx = D.A1 != D.A2
  # mismatch on the first donor allele, see HLA and priority score rules, page 12
  m1 = as.numeric( (D.A1[idx] != R.A1[idx]) & D.A1[idx] != R.A2[idx] )
  m2 = as.numeric( (D.A2[idx] != R.A1[idx]) & D.A2[idx] != R.A2[idx] )
  A.Mm[idx] = m1 + m2
  testit::assert(!all(is.na(A.Mm)))

  # Locus B
  B.Mm = rep(NA, length(D.B1))
  # subset homocygote allele
  idx = D.B1 == D.B2
  B.Mm[idx] = as.numeric( (D.B1[idx] != R.B1[idx]) & (D.B1[idx] != R.B2[idx]) )

  # subset heterocygote allele
  idx = D.B1 != D.B2
  # mismatch on the first donor allele, see HLA and priority score rules, page 12
  m1 = as.numeric( (D.B1[idx] != R.B1[idx]) & D.B1[idx] != R.B2[idx] )
  m2 = as.numeric( (D.B2[idx] != R.B1[idx]) & D.B2[idx] != R.B2[idx] )
  B.Mm[idx] = m1 + m2
  testit::assert(!all(is.na(B.Mm)))

  # Locus DR
  DR.Mm = rep(NA, length(D.DR1))
  # subset homocygote allele
  idx = D.DR1 == D.DR2
  DR.Mm[idx] = as.numeric( (D.DR1[idx] != R.DR1[idx]) & (D.DR1[idx] != R.DR2[idx]) )

  # subset heterocygote allele
  idx = D.DR1 != D.DR2
  # mismatch on the first donor allele, see HLA and priority score rules, page 12
  m1 = as.numeric( (D.DR1[idx] != R.DR1[idx]) & D.DR1[idx] != R.DR2[idx] )
  m2 = as.numeric( (D.DR2[idx] != R.DR1[idx]) & D.DR2[idx] != R.DR2[idx] )
  DR.Mm[idx] = m1 + m2
  testit::assert(!all(is.na(DR.Mm)))

  df = data.frame(A.Mm = A.Mm,
                  B.Mm = B.Mm,
                  DR.Mm = DR.Mm,
                  Total.Mm = A.Mm + B.Mm + DR.Mm)
  return(df)
}

#' Gets KIDMO prediction model fit.
#'
#' @return Model fit
#' @export
#'
kidmo_model <- function() {
  return(idat.kidmo.model.1)
}

#' Get KIDMO scaling factor.
#'
#' @return scaling factor
#' @export
#'
kidmo_scaling <- function() {
  return(idat.kidmo.model.1.scaling)
}

#' KIDMO conversion of hazard ratio to percentile rank.
#'
#' @param hr hazard ratio
#'
#' @return percentile
#' @export
#'
kidmo_hr2rank <- function(hr) {
  return(idat.kidmo.model.1.hr2rank(hr))
}

#' Gets WAIT multi-state model fit.
#'
#' @return Model fit
#' @export
#'
wait_model_he <- function() {
  return(idat.wait.model.he)
}

#' Gets WAIT multi-state model fit.
#'
#' @return Model fit
#' @export
#'
wait_model_ki <- function() {
  return(idat.wait.model.ki)
}

#' Gets WAIT multi-state model fit.
#'
#' @return Model fit
#' @export
#'
wait_model_li <- function() {
  return(idat.wait.model.li)
}

#' Gets WAIT multi-state model fit.
#'
#' @return Model fit
#' @export
#'
wait_model_lu <- function() {
  return(idat.wait.model.lu)
}

#' Gets WAIT multi-state model fit.
#'
#' @return Model fit
#' @export
#'
wait_model_pi <- function() {
  return(idat.wait.model.pi)
}
