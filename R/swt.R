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
#' @export
swt_style <- function(title_size=18, subtitle_size=14, font_size=14,
                      grey_theme = FALSE, legend_position="top") {
  # windowsFonts()
  font = "sans" # SWT uses font Segement Light

  bgColor = "white"
  gridColor = "gray90"
  if (grey_theme) {
    bgColor = "#F4F4F1"
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

