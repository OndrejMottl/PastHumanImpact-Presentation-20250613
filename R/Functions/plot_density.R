plot_density <- function(data_source, sel_col = "black", show_axis = FALSE) {
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
    ggplot2::geom_ribbon(
      data = data_source,
      mapping = ggplot2::aes(
        x = age,
        ymax = value,
        ymin = 0
      ),
      orientation = "x",
      color = sel_col,
      fill = colorspace::lighten(sel_col, amount = 0.5), # [config criteria]
      linewidth = line_size * 10 # [config criteria]
    )
}