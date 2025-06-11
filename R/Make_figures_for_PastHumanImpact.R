#----------------------------------------------------------#
#
#
#                  Past Human Impact
#             --------------------------------
#                      Make Figures
#
#
#                    Ondřej Mottl
#                        2024
#
#----------------------------------------------------------#

#----------------------------------------------------------#
# Setup -----
#----------------------------------------------------------#

source(
  here::here("R", "Config_file_PastHumanImpact.R")
)


#----------------------------------------------------------#
# Data -----
#----------------------------------------------------------#

data_records_meta <-
  RUtilpol::get_latest_file(
    file_name = "data_records_meta",
    dir = here::here("Data")
  )

data_records_meta_eu <-
  data_records_meta %>%
  dplyr::filter(
    region == "Europe"
  )

data_records_meta_eu_temperate <-
  data_records_meta_eu %>%
  dplyr::filter(
    climatezone == "Temperate"
  )

data_example_records <-
  RUtilpol::get_latest_file(
    file_name = "data_example_record",
    dir = here::here("Data")
  )

data_example_record_raw <-
  data_example_records %>%
  purrr::chuck("data_merge", 1)

data_paps_line <-
  data_example_record_raw %>%
  dplyr::select(
    "age",
    "N0", "N1", "N2",
    "N2 divided by N1", "N1 divided by N0",
    "DCCA axis 1", "ROC"
  ) %>%
  tidyr::pivot_longer(
    cols = -age,
    names_to = "predictor",
    values_to = "value"
  ) %>%
  add_predictor_names_as_factors()

data_paps_density <-
  data_example_record_raw %>%
  dplyr::select(
    "age",
    "Density diversity", "Density turnover"
  ) %>%
  tidyr::pivot_longer(
    cols = -age,
    names_to = "predictor",
    values_to = "value"
  ) %>%
  add_predictor_names_as_factors()

data_climate_line <-
  data_example_record_raw %>%
  dplyr::select(
    "age",
    "MAT", "MTCO",
    "MAPS", "MAPW"
  ) %>%
  tidyr::pivot_longer(
    cols = -age,
    names_to = "predictor",
    values_to = "value"
  ) %>%
  add_predictor_names_as_factors()

data_c14_prepared <-
  RUtilpol::get_latest_file(
    file_name = "data_c14_prepared",
    dir = here::here("Data")
  ) %>%
  dplyr::filter(
    dplyr::between(
      long,
      min(data_records_meta_eu$long),
      max(data_records_meta_eu$long)
    ),
    dplyr::between(
      lat,
      min(data_records_meta_eu$lat),
      max(data_records_meta_eu$lat)
    )
  )

data_human_density <-
  data_example_record_raw %>%
  dplyr::select(
    "age",
    "SPD"
  ) %>%
  tidyr::pivot_longer(
    cols = -age,
    names_to = "predictor",
    values_to = "value"
  ) %>%
  add_predictor_names_as_factors()

data_impact_by_records <-
  RUtilpol::get_latest_file(
    file_name = "data_impact_by_records",
    dir = here::here("Data")
  ) %>%
  add_region_as_factor() %>%
  add_climatezone_as_factor() %>%
  add_predictors_as_factor()

data_impact_by_record_quantiles <-
  RUtilpol::get_latest_file(
    file_name = "data_impact_by_record_quantiles",
    dir = here::here("Data")
  ) %>%
  add_region_as_factor() %>%
  add_climatezone_as_factor() %>%
  add_predictors_as_factor()

data_impact_by_climatezone <-
  RUtilpol::get_latest_file(
    file_name = "data_impact_by_climatezone",
    dir = here::here("Data")
  ) %>%
  add_region_as_factor() %>%
  add_climatezone_as_factor() %>%
  add_predictors_as_factor()

data_impact_by_climatezone_eu <-
  data_impact_by_climatezone %>%
  dplyr::filter(
    region == "Europe"
  )

data_impact_by_region <-
  RUtilpol::get_latest_file(
    file_name = "data_impact_by_region",
    dir = here::here("Data")
  ) %>%
  add_region_as_factor() %>%
  add_predictors_as_factor()

#----------------------------------------------------------#
# Figures -----
#----------------------------------------------------------#

#--------------------------------------------------#
## Pollen map -----
#--------------------------------------------------#

p0_map <-
  ggplot2::ggplot(
    mapping = ggplot2::aes(
      x = long,
      y = lat
    ),
  ) +
  ggplot2::borders(
    fill = col_land,
    colour = NA
  ) +
  ggplot2::scale_fill_manual(
    values = palette_ecozones,
    drop = FALSE,
    guide = "none"
  ) +
  ggplot2::scale_colour_manual(
    values = palette_ecozones,
    drop = FALSE,
    guide = "none"
  ) +
  ggplot2::coord_quickmap() +
  ggplot2::theme_void() +
  ggplot2::theme(
    legend.position = "none",
    legend.text = ggplot2::element_text(
      size = text_size, # [config criteria]
      colour = col_common_gray # [config criteria]
    ),
    legend.title = ggplot2::element_text(
      size = text_size, # [config criteria]
      colour = col_common_gray # [config criteria]
    ),
    panel.background = ggplot2::element_rect(
      fill = col_white,
      colour = col_white
    ),
    plot.background = ggplot2::element_rect(
      fill = col_white,
      colour = col_white
    ),
    panel.spacing = ggplot2::unit(c(0, 0, 0, 0), "null"),
    plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm"),
  ) +
  ggplot2::labs(
    x  = "Longitude",
    y  = "Latitude"
  )

fig_pollen_map_full <-
  p0_map +
  ggplot2::geom_point(
    data = data_records_meta,
    mapping = ggplot2::aes(
      colour = climatezone
    ),
    size = 1
  )

save_local_figure(
  plot = fig_pollen_map_full,
  filename = "pollen_map_full.png"
)

fig_pollen_map_europe <-
  p0_map +
  ggplot2::geom_point(
    data = data_records_meta_eu,
    mapping = ggplot2::aes(
      colour = climatezone
    ),
    size = 2
  ) +
  ggplot2::coord_quickmap(
    xlim = range(data_records_meta_eu$long),
    ylim = range(data_records_meta_eu$lat)
  )

save_local_figure(
  plot = fig_pollen_map_europe,
  filename = "pollen_map_europe.png",
  sel_width = fig_height, # [config criteria]
  sel_height = fig_height # [config criteria]
)

fig_pollen_map_europe_temperate <-
  p0_map +
  ggplot2::geom_point(
    data = data_records_meta_eu_temperate,
    mapping = ggplot2::aes(
      colour = climatezone
    ),
    size = 3
  ) +
  ggplot2::coord_quickmap(
    xlim = range(data_records_meta_eu$long),
    ylim = range(data_records_meta_eu$lat)
  )

save_local_figure(
  plot = fig_pollen_map_europe_temperate,
  filename = "pollen_map_europe_temperate.png",
  sel_width = fig_height, # [config criteria]
  sel_height = fig_height # [config criteria]
)

fig_pollen_map_example_record <-
  p0_map +
  ggplot2::geom_point(
    data = data_records_meta_eu_temperate,
    mapping = ggplot2::aes(
      colour = col_common_gray # [config criteria]
    ),
    size = 1
  ) +
  ggplot2::geom_point(
    data = data_records_meta_eu_temperate %>%
      dplyr::filter(dataset_id == 215),
    mapping = ggplot2::aes(
      colour = climatezone
    ),
    size = 5
  ) +
  ggplot2::coord_quickmap(
    xlim = range(data_records_meta_eu$long),
    ylim = range(data_records_meta_eu$lat)
  )

save_local_figure(
  plot = fig_pollen_map_example_record,
  filename = "pollen_map_example_record.png",
  sel_width = fig_height, # [config criteria]
  sel_height = fig_height # [config criteria]
)

fig_map_example_record_bare <-
  p0_map +
  ggplot2::geom_point(
    data = data_records_meta_eu_temperate %>%
      dplyr::filter(dataset_id == 215),
    mapping = ggplot2::aes(
      colour = climatezone
    ),
    size = 5
  ) +
  ggplot2::coord_quickmap(
    xlim = range(data_records_meta_eu$long),
    ylim = range(data_records_meta_eu$lat)
  )

save_local_figure(
  plot = fig_map_example_record_bare,
  filename = "map_example_record_bare.png",
  sel_width = fig_height, # [config criteria]
  sel_height = fig_height # [config criteria]
)

fig_map_example_record_rc <-
  p0_map +
  ggplot2::geom_point(
    data = data_c14_prepared,
    mapping = ggplot2::aes(
      x = long,
      y = lat,
    ),
    size = 1,
    col = palette_predictors["human"]
  ) +
  ggplot2::geom_point(
    data = data_records_meta_eu_temperate %>%
      dplyr::filter(dataset_id == 215),
    mapping = ggplot2::aes(
      colour = climatezone
    ),
    size = 5
  ) +
  ggplot2::coord_quickmap(
    xlim = range(data_records_meta_eu$long),
    ylim = range(data_records_meta_eu$lat)
  )

save_local_figure(
  plot = fig_map_example_record_rc,
  filename = "map_example_record_rc.png",
  sel_width = fig_height, # [config criteria]
  sel_height = fig_height # [config criteria]
)

#--------------------------------------------------#
## Line plot -----
#--------------------------------------------------#

time_step <- 1

p0_line <-
  ggplot2::ggplot(
    mapping = ggplot2::aes(
      x = age,
      y = value
    )
  ) +
  ggplot2::facet_wrap(
    ~predictor,
    nrow = 1,
    scales = "free_x",
    labeller = ggplot2::labeller(
      predictor = ggplot2::label_wrap_gen(5)
    )
  ) +
  ggplot2::scale_y_continuous(
    breaks = scales::pretty_breaks(n = 3)
  ) +
  ggplot2::scale_x_continuous(
    trans = "reverse",
    breaks = seq(2e3, 8.5e3, time_step * 1e3),
    labels = seq(2, 8.5, time_step)
  ) +
  ggplot2::coord_flip(
    xlim = c(7e3, 2e3)
  ) +
  ggplot2::labs(
    x = "Age (ka cal yr BP)",
    y = "Value"
  ) +
  ggplot2::theme_classic() +
  ggplot2::theme(
    plot.margin = grid::unit(c(0, 1, 0, 1), "mm"),
    plot.background = ggplot2::element_rect(
      fill = col_white, # [config criteria]
      color = NA
    ),
    panel.background = ggplot2::element_rect(
      fill = col_land, # [config criteria]
      color = NA
    ),
    panel.spacing.y = grid::unit(3, "mm"),
    legend.position = "none",
    legend.text = ggplot2::element_text(
      size = text_size, # [config criteria]
      color = col_common_gray # [config criteria]
    ),
    legend.title = ggplot2::element_text(
      size = text_size, # [config criteria]
      color = col_common_gray # [config criteria]
    ),
    text = ggplot2::element_text(
      size = text_size, # [config criteria]
      color = col_common_gray # [config criteria]
    ),
    axis.text.y = ggplot2::element_text(
      size = text_size, # [config criteria]
      color = col_common_gray # [config criteria]
    ),
    axis.text.x = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_text(
      size = text_size, # [config criteria]
      color = col_common_gray # [config criteria]
    ),
    axis.title.x = ggplot2::element_blank(),
    axis.line.x = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    line = ggplot2::element_line(
      linewidth = line_size, # [config criteria]
      color = col_common_gray # [config criteria]
    ),
    strip.background = ggplot2::element_rect(
      fill = col_white, # [config criteria]
      color = NA
    ),
    strip.text = ggplot2::element_text(
      size = text_size * 0.65, # [config criteria]
      color = col_common_gray # [config criteria]
    )
  ) +
  ggplot2::geom_vline(
    xintercept = seq(0, 10e3, time_step * 1e3),
    col = colorspace::lighten(
      col_common_gray, # [config criteria]
      amount = 0.5
    ),
    linetype = 1,
    alpha = 0.5,
    linewidth = line_size # [config criteria]
  )

fig_paps_line <-
  plot_line(
    data_source = data_paps_line,
    show_axis = TRUE,
    sel_col = col_ecosystem
  )

fig_paps_density <-
  plot_density(
    data_source = data_paps_density,
    sel_col = col_ecosystem
  )

fig_paps_all <-
  cowplot::plot_grid(
    fig_paps_line,
    fig_paps_density,
    nrow = 1,
    align = "h",
    rel_widths = c(8, 2)
  )

save_local_figure(
  plot = fig_paps_all,
  filename = "pollen_paps_all.png"
)

fig_climate_line <-
  plot_line(
    data_source = data_climate_line,
    show_axis = TRUE,
    sel_col = palette_predictors["climate"]
  )

save_local_figure(
  plot = fig_climate_line,
  filename = "climate_line.png"
)

fig_human_density <-
  plot_density(
    data_source = data_human_density,
    show_axis = TRUE,
    sel_col = palette_predictors["human"]
  )

save_local_figure(
  plot = fig_human_density,
  filename = "human_density.png"
)

fig_examle_combined <-
  cowplot::plot_grid(
    fig_paps_line,
    # The figure cannot be re-used, because of the axis
    plot_density(
      data_source = data_paps_density,
      show_axis = FALSE,
      sel_col = col_ecosystem # [config criteria]
    ),
    # The figure cannot be re-used, because of the axis
    plot_line(
      data_source = data_climate_line,
      show_axis = FALSE,
      sel_col = palette_predictors["climate"]
    ),
    # The figure cannot be re-used, because of the axis
    plot_density(
      data_source = data_human_density,
      show_axis = FALSE,
      sel_col = palette_predictors["human"]
    ),
    nrow = 1,
    align = "h",
    rel_widths = c(7, 2, 4, 1)
  )

save_local_figure(
  plot = fig_examle_combined,
  filename = "example_combined.png"
)

#----------------------------------------#
### Importance -----
#----------------------------------------#

fig_importance_example <-
  data_example_records %>%
  purrr::chuck("varpar_summary_table", 1) %>%
  dplyr::select(
    predictor,
    ratio = i_perc_percent
  ) %>%
  ggplot2::ggplot() +
  ggplot2::geom_bar(
    ggplot2::aes(
      x = 1,
      y = ratio / 100,
      fill = predictor,
      color = predictor,
    ),
    stat = "identity",
    col = NA,
    position = "stack",
    show.legend = TRUE
  ) +
  ggplot2::scale_y_continuous(
    position = "left",
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2),
    oob = scales::squish
  ) +
  ggplot2::scale_fill_manual(
    values = palette_predictors, # [config criteria]
    drop = FALSE
  ) +
  ggplot2::scale_colour_manual(
    values = palette_predictors, # [config criteria]
    drop = FALSE
  ) +
  ggplot2::guides(
    colour = "none",
    alpha = "none",
    fill = ggplot2::guide_legend(
      title = "Predictors",
      title.position = "top",
      title.theme = ggplot2::element_text(
        size = text_size, # [config criteria]
        colour = col_common_gray # [config criteria]
      ),
      label.theme = ggplot2::element_text(
        size = text_size, # [config criteria]
        colour = col_common_gray # [config criteria]
      ),
      nrow = 2,
      ncol = 1,
      byrow = TRUE
    )
  ) +
  ggplot2::theme(
    legend.position = "right",
    legend.title = element_text(
      size = text_size, # [config criteria]
      colour = col_common_gray, # [config criteria]
    ),
    legend.text = ggplot2::element_text(
      size = text_size, # [config criteria]
      colour = col_common_gray # [config criteria]
    ),
    legend.margin = ggplot2::margin(
      t = 0,
      r = 0,
      b = 0,
      l = 0,
      unit = "pt"
    ),
    legend.background = ggplot2::element_rect(
      fill = col_white, # [config criteria]
      colour = NA
    ),
    panel.background = ggplot2::element_rect(
      fill = col_white, # [config criteria]
      colour = NA
    ),
    panel.grid.major = ggplot2::element_blank(),
    plot.background = ggplot2::element_rect(
      fill = col_white, # [config criteria]
      colour = NA
    ),
    axis.title = ggplot2::element_text(
      size = text_size, # [config criteria]
      colour = col_common_gray # [config criteria]
    ),
    axis.text.x = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    axis.line.x = ggplot2::element_blank(),
    text = ggplot2::element_text(
      size = text_size, # [config criteria]
      colour = col_common_gray # [config criteria]
    ),
    line = ggplot2::element_line(
      linewidth = line_size, # [config criteria]
      colour = col_common_gray # [config criteria]
    )
  ) +
  ggplot2::labs(
    y = "Ratio of importance",
    x = NULL
  )

fig_examle_combined_stripped <-
  cowplot::plot_grid(
    # The figure cannot be re-used, because of the axis
    plot_line(
      data_source = data_climate_line,
      show_axis = FALSE,
      sel_col = palette_predictors["climate"]
    ) %>%
      remove_axis(),
    # The figure cannot be re-used, because of the axis
    plot_density(
      data_source = data_human_density,
      show_axis = FALSE,
      sel_col = palette_predictors["human"]
    ) %>%
      remove_axis(),
    nrow = 1,
    align = "h",
    rel_widths = c(4, 1)
  )

fig_example_with_importance <-
  cowplot::plot_grid(
    fig_examle_combined_stripped,
    NULL,
    fig_importance_example,
    nrow = 1,
    rel_widths = c(6, 1, 4)
  )

save_local_figure(
  plot = fig_example_with_importance,
  filename = "example_with_importance.png"
)

#--------------------------------------------------#
## Summary plot -----
#--------------------------------------------------#

sel_range <- c(0, 1)

p_summary_0 <-
  tibble::tibble() %>%
  ggplot2::ggplot() +
  ggplot2::coord_cartesian(
    ylim = sel_range
  ) +
  ggplot2::theme(
    plot.margin = grid::unit(c(0, 0, 0, 0), "mm"),
    panel.spacing.y = grid::unit(5, "mm"),
    legend.position = "none",
    plot.background = ggplot2::element_rect(
      fill = col_white, # [config criteria]
      colour = NA
    ),
    panel.background = ggplot2::element_rect(
      fill = col_land, # [config criteria]
      colour = NA
    ),
    strip.background = ggplot2::element_rect(
      fill = col_white, # [config criteria]
      colour = NA
    ),
    legend.text = ggplot2::element_text(
      size = text_size, # [config criteria]
      color = col_common_gray # [config criteria]
    ),
    legend.title = ggplot2::element_text(
      size = text_size, # [config criteria]
      color = col_common_gray # [config criteria]
    ),
    text = ggplot2::element_text(
      size = text_size, # [config criteria]
      color = col_common_gray # [config criteria]
    ),
    axis.text.y = ggplot2::element_text(
      size = text_size, # [config criteria]
      color = col_common_gray # [config criteria]
    ),
    axis.title.y = ggplot2::element_text(
      size = text_size, # [config criteria]
      color = col_common_gray # [config criteria]
    ),
    line = ggplot2::element_line(
      linewidth = line_size, # [config criteria]
      color = col_common_gray # [config criteria]
    ),
    strip.text = ggplot2::element_text(
      size = text_size, # [config criteria]
      color = col_common_gray # [config criteria]
    )
  ) +
  ggplot2::scale_y_continuous(
    position = "right",
    expand = c(0.05, 0.05),
    breaks = seq(
      min(sel_range),
      max(sel_range),
      by = max(sel_range) / 4
    )
  ) +
  ggplot2::labs(
    x = "",
    y = "Ratio of importance"
  ) +
  ggplot2::geom_hline(
    yintercept = seq(0, 1, 0.25),
    col = colorspace::lighten(
      col_common_gray, # [config criteria]
      amount = 0.5
    ),
    linetype = 1,
    alpha = 0.5,
    size = line_size # [config criteria]
  )

fig_summary_example_record <-
  data_impact_by_records %>%
  dplyr::filter(dataset_id == 215) %>%
  plot_summary_regions_points()

save_local_figure(
  plot = fig_summary_example_record,
  filename = "summary_example_record.png",
  sel_width = fig_height / 2, # [config criteria]
  sel_height = fig_height # [config criteria]
)

fig_summary_eu_temperate <-
  data_impact_by_records %>%
  dplyr::filter(region == "Europe") %>%
  dplyr::filter(climatezone == "Temperate") %>%
  plot_summary_regions_points()

save_local_figure(
  plot = fig_summary_eu_temperate,
  filename = "summary_eu_temperate.png",
  sel_width = fig_height / 2, # [config criteria]
  sel_height = fig_height # [config criteria]
)

fig_summary_eu_temperate_quantile <-
  data_impact_by_record_quantiles %>%
  dplyr::filter(region == "Europe") %>%
  dplyr::filter(climatezone == "Temperate") %>%
  plot_summary_regions_quatiles(
    data_source_climatezone = data_impact_by_climatezone %>%
      dplyr::filter(region == "Europe") %>%
      dplyr::filter(climatezone == "Temperate"),
  )

save_local_figure(
  plot = fig_summary_eu_temperate_quantile,
  filename = "summary_eu_temperate_quantile.png",
  sel_width = fig_height / 2, # [config criteria]
  sel_height = fig_height # [config criteria]
)

fig_summary_eu_quantile <-
  data_impact_by_record_quantiles %>%
  dplyr::filter(region == "Europe") %>%
  plot_summary_regions_quatiles(
    data_source_climatezone = data_impact_by_climatezone %>%
      dplyr::filter(region == "Europe"),
  )

save_local_figure(
  plot = fig_summary_eu_quantile,
  filename = "summary_eu_quantile.png",
  sel_width = fig_height, # [config criteria]
  sel_height = fig_height # [config criteria]
)

fig_summary_density_eu <-
  data_impact_by_records %>%
  dplyr::filter(region == "Europe") %>%
  plot_density_summary() +
  ggplot2::theme(
    axis.title.y = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank()
  )


fig_summary_eu_quantile_no_axis <-
  fig_summary_eu_quantile +
  ggplot2::theme(
    strip.text.y = ggplot2::element_blank()
  )

fig_summary_eu_with_density <-
  cowplot::plot_grid(
    add_vertical_line(
      plot = fig_summary_density_eu,
      data_source = data_impact_by_region %>%
        dplyr::filter(region == "Europe"),
    ),
    add_vertical_line(
      plot = fig_summary_eu_quantile_no_axis,
      data_source = data_impact_by_region %>%
        dplyr::filter(region == "Europe"),
    ),
    nrow = 1,
    align = "h",
    rel_widths = c(2, 7)
  )

save_local_figure(
  plot = fig_summary_eu_with_density,
  filename = "summary_eu_with_density.png",
  sel_width = fig_height, # [config criteria]
  sel_height = fig_height # [config criteria]
)

fig_summary_eu_quantile_climate <-
  data_impact_by_record_quantiles %>%
  dplyr::filter(region == "Europe") %>%
  plot_summary_regions_quatiles(
    data_source_climatezone = data_impact_by_climatezone %>%
      dplyr::filter(region == "Europe"),
    sel_var = "climate",
    sel_var_label = "Climate importance"
  ) +
  ggplot2::theme(
    strip.text.y = ggplot2::element_blank()
  )

fig_summary_density_eu_climate <-
  data_impact_by_records %>%
  dplyr::filter(region == "Europe") %>%
  plot_density_summary(
    sel_var = "climate"
  ) +
  ggplot2::theme(
    axis.title.y = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank()
  )

fig_summary_eu_with_density_climate <-
  cowplot::plot_grid(
    add_vertical_line(
      plot = fig_summary_density_eu_climate,
      data_source = data_impact_by_region %>%
        dplyr::filter(region == "Europe"),
      sel_var = "climate"
    ),
    add_vertical_line(
      plot = fig_summary_eu_quantile_climate,
      data_source = data_impact_by_region %>%
        dplyr::filter(region == "Europe"),
      sel_var = "climate"
    ),
    nrow = 1,
    align = "h",
    rel_widths = c(2, 7)
  )

save_local_figure(
  plot = fig_summary_eu_with_density_climate,
  filename = "summary_eu_with_density_climate.png",
  sel_width = fig_height, # [config criteria]
  sel_height = fig_height # [config criteria]
)
