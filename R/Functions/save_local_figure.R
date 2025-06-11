save_local_figure <- function(
    plot, filename,
    sel_width = fig_width, # [config]
    sel_height = fig_height # [config]
    ) {
  ggplot2::ggsave(
    filename = here::here("Materials/R_generated/", filename),
    plot = plot,
    width = sel_width,
    height = sel_height,
    dpi = 300,
    units = "in",
    bg = col_white # [config]
  )
}
