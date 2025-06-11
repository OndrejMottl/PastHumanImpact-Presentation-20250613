include_local_figure <- function(data_source) {
  knitr::include_graphics(
    path = here::here(
      "Materials",
      data_source
    ),
    error = TRUE
  )
}