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
swt_style <- function(title_size=18, subtitle_size=14, font_size=14,
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
#' mycolors$pink.liver
#'
#' @export
#'
swt_colors <- function() {
  list(blue.swt           = grDevices::rgb( 42, 84,138, maxColorValue = 255),
       turkis.tx          = grDevices::rgb(105,211,195, maxColorValue = 255),
       yellow.organ       = grDevices::rgb(251,228, 70, maxColorValue = 255),
       red.heart          = grDevices::rgb(229,  0, 92, maxColorValue = 255),

       pink.liver         = grDevices::rgb(217,143,143, maxColorValue = 255),
       orange.kidney      = grDevices::rgb(242,175, 92, maxColorValue = 255),
       green.pancreas     = grDevices::rgb(139,173,143, maxColorValue = 255),
       sky.lungs          = grDevices::rgb(155,189,197, maxColorValue = 255),
       beige.intestine    = grDevices::rgb(209,205,189, maxColorValue = 255),

       grey.bg            = grDevices::rgb(244,244,241, maxColorValue = 255)
  )

}

#' Read LifePort data
#'
#' @param file The data file
#' @param binary Whether the data file is binary (default FALSE)
#'
#' @return a list with LifePort data
#' @export
#'
read.lifeport <- function(file, binary=FALSE) {

  # read from binary file
  if (binary) {

    to.read = file(file, "rb")

    UnitID = readBin(to.read, character(), n = 1, size = 1)
    skip = readBin(to.read, raw(), n =34, size = 1)

    OrganID = readBin(to.read, character(), n =1, size = 1, signed = FALSE)
    skip = readBin(to.read, integer(), n =6, size = 2)

    data.raw = readBin(to.read, integer(), n =10^6, size = 2)
    close(to.read)

    # data device
    data.device = data.frame(array(NA, dim=c(1,9)))
    colnames(data.device) = c("SerialNumber", "Type", "SubType", "UnitID",
                              "FirmwareVersion", "FileID", "StartTime",
                              "DataState", "HasGaps")
    data.device$UnitID = UnitID

    # data organ
    data.organ = data.frame(array(NA, dim=c(1,13)))
    colnames(data.organ) = c("OrganID", "KidneySide", "BloodType", "CrossClampTime.Date",
                              "CrossClampTimezone", "TotalIschemicTime", "PerfusateLot",
                              "PerfusateExpirationDate", "PerfusateUsed", "Cannula",
                              "CannulaExpirationDate", "CassetteLot.", "CasetteExpirationDate"
    )
    data.organ$OrganID = OrganID

    # data
    no_rows = length(data.raw)/16
    data.raw = t(array(data.raw, dim=c(16,no_rows)))

    # remove last two rows filled with -1 (found in 2 examples, probably in all)
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
  } else {

    data.device = utils::read.csv(file = file, nrows = 1, head = TRUE)
    data.organ  = utils::read.csv(file = file, skip = 3, nrows = 1, head = TRUE)
    data = utils::read.csv(file = file, skip = 6, head = TRUE)

  }

  # Conversions since data is stored in integers
  data$InfuseTemperature = data$InfuseTemperature/10
  data$IceContainerTemperature = data$IceContainerTemperature/10
  data$OrganResistance = data$OrganResistance/100

  data.list = list(
    data.device=data.device,
    data.organ = data.organ,
    data = data)

  return(data.list)
}
