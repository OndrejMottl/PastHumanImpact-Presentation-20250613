plot_line <- function(data_source, sel_col = "black", show_axis = FALSE) {
  if (
    isFALSE(show_axis)
  ) {
    p0_line <-
      p0_line +
      ggplot2::theme(
        axis.title.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank()
      )
  }

  p0_line +
    ggplot2::geom_line(
      data = data_source,
      linewidth = line_size * 10, # [config criteria]
      color = sel_col # [config criteria]
    )
}