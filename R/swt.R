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
#' Easy access to official SWT color scheme
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
  colors = list(# primary colors
    blue.swt           = grDevices::rgb( 42, 84,138, maxColorValue = 255),
    turkis.cm          = grDevices::rgb(105,211,195, maxColorValue = 255),
    yellow.cndo        = grDevices::rgb(251,228, 70, maxColorValue = 255),
    strongred.akzent   = grDevices::rgb(229,  0, 92, maxColorValue = 255),

    # duplicate colors
    turkis.tpx         = grDevices::rgb(105,211,195, maxColorValue = 255),
    yellow.donation    = grDevices::rgb(251,228, 70, maxColorValue = 255),

    # secondary colors
    lightblue.lungs    = grDevices::rgb(155,189,197, maxColorValue = 255),
    green.pancreas     = grDevices::rgb(139,173,143, maxColorValue = 255),
    green.langerhans   = grDevices::rgb(139,173,143, maxColorValue = 255),
    darkyellow.kidney  = grDevices::rgb(242,175, 92, maxColorValue = 255),
    red.liver          = grDevices::rgb(217,143,143, maxColorValue = 255),
    beige.intestine    = grDevices::rgb(209,205,189, maxColorValue = 255),
    # 40% alpha:
    pink.heart         = grDevices::rgb(212,  0, 84, 0.40*255, maxColorValue = 255),

    # background color
    grey.bg            = grDevices::rgb(244,244,241, maxColorValue = 255),
    white              = grDevices::rgb(255,255, 255, maxColorValue = 255)
  )

  # single 5 hue color scheme 100% 75% 50%  25% 0% (white)

  # primary colors
  colfun = grDevices::colorRampPalette(c(colors$blue.swt, colors$white))
  colors$pal.blue.swt = colfun(5)

  colfun = grDevices::colorRampPalette(c(colors$turkis.cm, colors$white))
  colors$pal.turkis.cm = colfun(5)

  colfun = grDevices::colorRampPalette(c(colors$yellow.cndo, colors$white))
  colors$pal.yellow.cndo = colfun(5)

  colfun = grDevices::colorRampPalette(c(colors$strongred.akzent, colors$white))
  colors$pal.strongred.akzent = colfun(5)

  # duplicate colors
  colfun = grDevices::colorRampPalette(c(colors$turkis.tpx, colors$white))
  colors$pal.turkis.tpx = colfun(5)

  colfun = grDevices::colorRampPalette(c(colors$blue.swt, colors$white))
  colors$pal.blue.swt = colfun(5)

  colfun = grDevices::colorRampPalette(c(colors$yellow.donation, colors$white))
  colors$pal.yellow.donation = colfun(5)

  # secondary colors
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

#' Read LifePort data
#'
#' @param file The data file
#' @param format guess, binary or plaintxt (default guess)
#'
#' @return a list with LifePort data
#' @export
#'
read_lifeport <- function(file, format="guess") {

  # guess ascii vs binary
  # we read the first line, for ascii it contains the full variable header
  # with 83 characters, for binary it only contains the UnitID which is
  # generally < 10 and sometimes an empty string , i.e. "".
  if (format == "guess") {
    con = file(file, "r")
    firstline = readLines(con, n = 1, warn = F)
    close(con)
    firstline = gsub('"', "", deparse(firstline))
    firstline = gsub('\\\\x[0-9a-fA-F]{2}', "?", firstline)
    format = ifelse(nchar(firstline) > 80, "plaintxt", "binary")
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
    as.POSIXct("1970-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "CET")
    data.device$StartTime =
      as.character(as.POSIXct(data.device$StartTime, format = "%d.%m.%Y %H:%M:%S", tz = "CET"))
  }

  # Conversions since data is stored in integers
  data$InfuseTemperature = data$InfuseTemperature/10
  data$IceContainerTemperature = data$IceContainerTemperature/10
  data$OrganResistance = data$OrganResistance/100

  # fix for invalid UnitID
  data.device$UnitID = gsub("\036|\x98f|\\xcc", NA,  data.device$UnitID)

  data.list = list(
    data.device=data.device,
    data.organ = data.organ,
    data = data)

  return(data.list)
}

#' Process LifePort data. Adds runtime, clock time vectors, and filtered time
#' series.
#'
#' @param lpdat A list with data from read.lifeport()
#' @param window_size rolling window size for filtering
#'
#' @return a list with additional processed data tables
#' @export
#'
process_lifeport <- function(lpdat, window_size = 30) {

  # Calculate runtime from StartTime and number of samples
  n = nrow(lpdat$data) # number of samples every 10 seconds
  start = as.POSIXct(lpdat$data.device$StartTime, format = "%Y-%m-%d %H:%M:%S", tz = "CET")
  dur = (start + n*10) - start
  lpdat$data.device$Runtime = as.character(hms::round_hms(hms::as_hms(dur), 1))

  # We calculate own time vector ignoring the duplicated timestamps in InfuseTime
  # InfuseTime is only in the txt file so must be an bug in ORS software export

  # relative clock
  lpdat$data$time.clock = start + seq(0, n*10 - 1, 10)

  # absolute clock starting from 0
  start.zero = as.POSIXct("1970-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "CET")
  lpdat$data$time.zero = start.zero + seq(0, n*10 - 1, 10)

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

  return(lpdat)
}


#' Create SWT LifePort Case Report in MS Word
#'
#' @param data.file Lifeport data file
#' @param output.file target file docx
#' @param template.file template file docx
#' @importFrom ggplot2 ggplot aes geom_line geom_hline labs theme ylim scale_color_manual element_blank element_text margin scale_y_continuous

#' @importFrom rlang .data
#'
#' @export
#'
swt_LifePortCaseReport <- function(data.file, output.file, template.file) {

  swtcol = swt_colors()

  d = read_lifeport(file = data.file)
  d = process_lifeport(lpdat = d, window_size = 15)

  # add OrganID from filename as well
  # OrganID: what was entered by the nurse (sometimes missing, then timestamp)
  # DonorID: Derived from filename and checked by SWT
  d$data.device$Filename = basename(data.file)
  d$data.organ$DonorID = gsub("^RD-|\\.txt$|\\.TXT$", "", basename(data.file))

  data.device = d$data.device
  data.organ  = d$data.organ
  data        = d$data

  # Table: Organ and Device Information
  tab = cbind(data.organ[, c("DonorID", "KidneySide"),],
              data.device[, c("SerialNumber", "UnitID", "StartTime", "Runtime")]
  )
  colnames(tab) = c("Donor ID", "Kidney side", "Serial number", "Unit ID",
                    "Start time", "Runtime")

  # Prepare data frames for ggplot
  tab.pressure = data.frame(
    time = rep(data$time.clock, 2),
    pressure = c(data$SystolicPressure.flt, data$DiastolicPressure.flt),
    group = rep(c("Systolic", "Diastolic"), each = nrow(data))
  )

  tab.temp = data.frame(
    time = rep(data$time.clock, 2),
    temperature = c(data$InfuseTemperature, data$IceContainerTemperature),
    group = rep(c("Infuse", "Ice"), each = nrow(data))
  )

  # Figure Pressure
  p1 = ggplot(tab.pressure, aes(x=.data$time, y=.data$pressure,
                                group=.data$group, col=.data$group)) +
    geom_line(size=0.5) +
    geom_hline(yintercept = 30, linetype="dashed") +
    scale_color_manual(values=c(swtcol$darkyellow.kidney, swtcol$blue.swt)) +
    ylim(c(0,40)) +
    labs(title = "Pressure", y = "mmHg") +
    swt_style(font_size = 8, title_size = 8) +
    theme(axis.title.x = element_blank(),
          plot.title = element_text(margin=margin(0,0,-15,0)),
          legend.box.margin=margin(0,0,-15,0)
    )

  # Figure Flow
  p2 = ggplot(data, aes(x=.data$time.clock, y=.data$FlowRate.flt, col="Flow rate")) +
    geom_line(size=0.5) +
    geom_hline(yintercept = 85, linetype="dashed") +
    scale_color_manual(values=swtcol$blue.swt) +
    ylim(c(0,200)) +
    labs(title = "Flow", y = "ml/min") +
    swt_style(font_size = 8, title_size = 8) +
    theme(axis.title.x = element_blank(),
          plot.title = element_text(margin=margin(0,0,-15,0)),
          legend.box.margin=margin(0,0,-15,0))

  # Figure Resistance
  p3 = ggplot(data, aes(x=.data$time.clock, y=.data$OrganResistance.flt,
                        col="Organ resistance")) +
    geom_line(size=0.5) +
    ylim(c(0,0.5)) +
    geom_hline(yintercept = 0.28, linetype="dashed") +
    geom_hline(yintercept = 0.10, linetype="dashed") +
    scale_color_manual(values=swtcol$blue.swt) +
    labs(title = "Resistance", y = "mmHg/ml/min") +
    swt_style(font_size = 8, title_size = 8) +
    theme(axis.title.x = element_blank(),
          plot.title = element_text(margin=margin(0,0,-15,0)),
          legend.box.margin=margin(0,0,-15,0))

  # Figure Temperature
  p4 = ggplot(tab.temp, aes(x=.data$time, y=.data$temperature,
                            group=.data$group, col=.data$group)) +
    geom_line(size=0.5) +
    geom_hline(yintercept = c(4,8), linetype="dashed") +
    scale_color_manual(values=c(swtcol$blue.swt,
                                swtcol$darkyellow.kidney)) +
    #ylim(c(0,16)) +
    scale_y_continuous(breaks = seq(0,20,4), limits = c(0,20)) +
    labs(title = "Temperature", y = "\u2103") +
    swt_style(font_size = 8, title_size = 8) +
    theme(axis.title.x = element_blank(),
          plot.title = element_text(margin=margin(0,0,-15,0)),
          legend.box.margin=margin(0,0,-15,0))

  myplot = cowplot::plot_grid(p1, p2, p3, p4, nrow = 2, ncol = 2)

  myDoc = officer::read_docx(template.file)

  myDoc = officer::body_add_par(myDoc, value = "Case Report", style = "Title")
  myDoc = officer::body_add_par(myDoc, value = paste("LifePort Kidney Allograft Case Report from",
                                                     format(Sys.Date(), "%d %b %Y")), style = "Subtitle")

  # Table
  myDoc = officer::body_add_par(myDoc, value = "Organ and Device Information", style = "heading 3")
  myDoc = officer::body_add_par(myDoc, value = "", style = "Normal")
  myDoc = officer::body_add_table(myDoc, value = tab, style = "SWT", align_table	= "left")

  # Data Charts
  myDoc = officer::body_add_par(myDoc, value = "Data Charts", style = "heading 3")
  myDoc = officer::body_add_par(myDoc, value = "", style = "Normal")
  myDoc = officer::body_add_gg(myDoc, value = myplot, width = 6, height = 3.5)

  # General information
  myDoc = officer::body_add_par(myDoc, value = "General Information", style = "heading 3")
  myDoc = officer::body_add_par(myDoc, value = "The flow rate is automatically adjusted so that the target pressure is never exceeded (30 mmHg). In normal kidney behavior the flow is slightly increasing and renal resistance decreasing over time due to vasodilation. In abnormal behavior flow is not increasing, and resistance is not decreasing. Excellent perfusion parameters may be reassuring if considering a marginal kidney for transplantation, i.e. 81% of initial function if renal resistance is < 0.28 (Jochmans et al.; 2011).", style = "Normal")

  # Comments
  myDoc = officer::body_add_par(myDoc, value = "Comments", style = "heading 3")
  myDoc = officer::body_add_par(myDoc, value = "None", style = "Normal")

  print(myDoc, target = output.file)

}
