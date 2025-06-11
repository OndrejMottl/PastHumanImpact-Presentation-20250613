#----------------------------------------------------------#
#
#
#                  Past Human Impact
#             --------------------------------
#                      Config file
#
#
#                    Ondřej Mottl
#                        2024
#
#----------------------------------------------------------#

# Set the current environment
current_env <- environment()

# set seed
set.seed(1234)

library(here)


#----------------------------------------------------------#
# Function -----
#----------------------------------------------------------#

# get vector of general functions
fun_list <-
  list.files(
    path = "R/Functions/",
    pattern = "*.R",
    recursive = TRUE
  )

# source them
if (
  length(fun_list) > 0
) {
  sapply(
    paste0("R/Functions/", fun_list, sep = ""),
    source
  )
}

#----------------------------------------------------------#
# Load packages -----
#----------------------------------------------------------#

if (
  isFALSE(
    exists("already_synch", envir = current_env)
  )
) {
  already_synch <- FALSE
}

if (
  isFALSE(already_synch)
) {
  library(here)
  # Synchronise the package versions
  renv::restore(
    lockfile = here::here("renv.lock")
  )
  already_synch <- TRUE

  # Save snapshot of package versions
  # renv::snapshot(lockfile =  "renv.lock")  # do only for update
}

lapply(
  c(
    "tidyverse",
    "knitr",
    "colorspace",
    "cowplot"
  ),
  install_if_missing
)

if (!require("RUtilpol")) remotes::install_github("HOPE-UIB-BIO/R-Utilpol-package")

library(RUtilpol)

#----------------------------------------------------------#
# Quarto -----
#----------------------------------------------------------#

fig_width <- 7
fig_height <- 5

knitr::opts_chunk$set(
  fig.width = fig_width,
  fig.height = fig_height,
  fig.align = "center",
  out.width = "100%"
)

options(htmltools.dir.version = FALSE)

#----------------------------------------------------------#
# Graphical option -----
#----------------------------------------------------------#

text_size <- 15
line_size <- 0.1
point_size <- 1

region_label_wrap <- 10

#--------------------------------------------------#
# Colours -----
#--------------------------------------------------#

col_land <- "grey90"

col_common_gray <- "#636363"

col_ecosystem <- "#64b264"

col_crema <- "#f2f7f2"

col_white <- "#ffffff"

palette_ecozones <-
  c(
    "Polar" = "#907A8E",
    "Cold - Cold Summer" = "#8C4418",
    "Cold - Warm Summer" = "#DC702E",
    "Cold - Hot Summer" = "#AA6133",
    "Cold - Dry Winter" = "#CB8152",
    "Cold - Dry Summer" = "#E59463",
    "Temperate" = "#371E71",
    "Temperate - Dry Winter" = "#562FB1",
    "Temperate - Dry Summer" = "#9A7EDD",
    "Arid" = "#DDDF78",
    "Tropical" = "#D68FD6"
  )

palette_predictors <- c(
  human = "#c99b38",
  climate = "#1f6f6f"
)

palette_ecozones_labels <-
  palette_ecozones %>%
  rlang::set_names(
    nm = get_climatezone_label(names(palette_ecozones))
  )

data_climate_zones <-
  tibble::tibble(
    climatezone = factor(
      c(
        "Polar",
        "Cold_Cold_Summer",
        "Cold_Warm_Summer",
        "Cold_Hot_Summer",
        "Cold_Dry_Winter",
        "Cold_Dry_Summer",
        "Temperate",
        "Temperate_Dry_Winter",
        "Temperate_Dry_Summer",
        "Tropical",
        "Arid"
      ),
      levels = c(
        "Polar",
        "Cold_Cold_Summer",
        "Cold_Warm_Summer",
        "Cold_Hot_Summer",
        "Cold_Dry_Winter",
        "Cold_Dry_Summer",
        "Temperate",
        "Temperate_Dry_Winter",
        "Temperate_Dry_Summer",
        "Tropical",
        "Arid"
      )
    ),
    climatezone_label = c(
      "Polar",
      "Cold - Cold Summer",
      "Cold - Warm Summer",
      "Cold - Hot Summer",
      "Cold - Dry Winter",
      "Cold - Dry Summer",
      "Temperate",
      "Temperate - Dry Winter",
      "Temperate - Dry Summer",
      "Tropical",
      "Arid"
    )
  )
