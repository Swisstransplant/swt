#' Add SWT theme to ggplot chart
#'
#' This function allows you to add the SWT theme to your ggplot graphics.
#' @keywords swt_style
#'
#' @export
swt_style <- function() {
  # windowsFonts()
  font = "sans" # SWT uses font Segement Light
  bgColor = "#F4F4F1"

  ggplot2::theme(

    # Title
    plot.title = ggplot2::element_text(family=font,
                                       size=18,
                                       face="bold"),

    # Subtitle
    plot.subtitle = ggplot2::element_text(family=font,
                                          size=14,
                                          #margin=ggplot2::margin(9,0,9,0)
    ),
    plot.caption = ggplot2::element_blank(),
    # This leaves the caption text element empty, because it is set elsewhere in
    # the finalise plot function

    # Legend
    # legend.position = "top",
    legend.text.align = 0,
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(size=14),

    # Axis
    axis.text = ggplot2::element_text(family=font, size=14),
    axis.title = ggplot2::element_text(family=font, size=14),
    # axis.text.x = ggplot2::element_text(margin=ggplot2::margin(5, b = 10)),
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),

    # Grid lines
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(color="white"),
    panel.grid.major.x = ggplot2::element_blank(),

    # Background
    # This sets the panel background as blank, removing the standard grey ggplot
    # background colour from the plot
    panel.background = element_rect(fill = bgColor),
    plot.background = element_rect(fill = bgColor)

    # Strip background (This sets the panel background for facet-wrapped plots
    # to white, removing the standard grey ggplot background colour and sets the
    # title size of the facet-wrap title to font size 22)
    # strip.background = ggplot2::element_rect(fill="red"),
    # strip.text = ggplot2::element_text(size  = 22,  hjust = 0)
  )
}

save_plot <- function (plot_grid, width, height, save_filepath) {
  grid::grid.draw(plot_grid)
  #save it
  ggplot2::ggsave(filename = save_filepath,
                  plot=plot_grid, width=(width/72), height=(height/72),
                  bg="white", type="cairo-png")
}

#Left align text
left_align <- function(plot_name, pieces){
  grob = ggplot2::ggplotGrob(plot_name)
  n = length(pieces)
  grob$layout$l[grob$layout$name %in% pieces] = 2
  return(grob)
}

create_footer <- function (source_name, logo_image_path, color = "#F4F4F1") {
  #Make the footer
  footer = grid::grobTree(# grid::linesGrob(x = grid::unit(c(0, 1), "npc"),
    #                 y = grid::unit(1.1, "npc")),
    grid::rectGrob(gp=grid::gpar(fill=color, col=color)),
    grid::textGrob(source_name, x = 0.01, hjust = 0,
                   gp = grid::gpar(fontsize=10)),
    grid::rasterGrob(png::readPNG(logo_image_path),
                     x = 0.98))
  return(footer)

}

#' Arrange alignment and save SWT plot
#'
#' Running this function will save your plot with the correct guidelines for
#' publication for a SWT graphic. It will left align the title and subtitle,
#' add the SWT logo at the bottom right and save it to your specified location.
#' @param plot_name The ggplot object of the plot
#' @param source_name The text in the footer
#' @param save_filepath Exact filepath that you want the plot to be saved to
#' @param width_pixels Width in pixels - defaults to 640
#' @param height_pixels Height in pixels - defaults to 450
#' @param logo_image_path File path for the logo image
#' @return (Invisibly) an updated ggplot object.

#' @keywords finalise_plot
#'
#' @export
finalise_plot <- function(plot_name,
                          source_name,
                          save_filepath,
                          width_pixels=640,
                          height_pixels=450,
                          logo_image_path = file.path(system.file("data",
                                                                  package = 'swt'),
                                                      "swtlogo.png"),
                          #logo_image_path = "swtlogo.png",
                          footer = TRUE) {

  #Draw your left-aligned grid
  plot_left_aligned = left_align(plot_name, c("subtitle", "title", "caption"))

  myFooter=NULL
  if (footer) {
    myFooter = create_footer(source_name, logo_image_path, color="#F4F4F1")
    plot_grid = ggpubr::ggarrange(plot_left_aligned, myFooter,
                                  ncol = 1, nrow = 2,
                                  heights = c(1, 0.045/(height_pixels/450))
    )
  } else {
    plot_grid = ggpubr::ggarrange(plot_left_aligned,
                                  ncol = 1, nrow = 1)
  }
  ## print(paste("Saving to", save_filepath))
  save_plot(plot_grid, width_pixels, height_pixels, save_filepath)
  ## Return (invisibly) a copy of the graph. Can be assigned to a
  ## variable or silently ignored.
  invisible(plot_grid)
}


swt_colors <- function() {
  list(blue.swt                = rgb( 42, 84,138, maxColorValue = 255),
       turkis.transplantation  = rgb(105,211,195, maxColorValue = 255),
       yellow.organdonation    = rgb(251,228, 70, maxColorValue = 255),
       strongred.heart         = rgb(229,  0, 92, maxColorValue = 255),

       red.liver               = rgb(217,143,143, maxColorValue = 255),
       darkyellow.kidney       = rgb(242,175, 92, maxColorValue = 255),
       green.pancreas          = rgb(139,173,143, maxColorValue = 255),
       lightblue.lungs         = rgb(155,189,197, maxColorValue = 255),
       beige.smallintestine    = rgb(209,205,189, maxColorValue = 255)
       )

}

