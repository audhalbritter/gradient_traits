#!/usr/bin/env Rscript

source("load_libraries.R")
library(targets)

source("R/Functions/make_figure_helpers.r")
source("R/Functions/make_climate_figures.r")

tar_load(
  c(
    trait_pca_full_long,
    trait_pca_full_climate_models_region_output,
    trait_pca_full_climate_models_output,
    trait_pca_full_variance,
    leps_variance_partition,
    trait_mean_long,
    trait_models_region_output,
    trait_models_output
  )
)

presentation_itv_regions <- c(
  "Svalbard",
  "Southern Scandes",
  "Central Andes",
  "Drakensberg"
)

presentation_region_scale_colour <- function() {
  ggplot2::scale_colour_manual(
    values = create_region_color_mapping(),
    name = "Region",
    breaks = climate_region_levels(),
    guide = ggplot2::guide_legend(nrow = 2, byrow = TRUE)
  )
}

presentation_climate_theme <- function() {
  ggplot2::theme_bw(base_size = 18) +
    ggplot2::theme(
      legend.position = "top",
      legend.title = ggplot2::element_text(size = 18),
      legend.text = ggplot2::element_text(size = 16),
      axis.title = ggplot2::element_text(size = 20),
      axis.text = ggplot2::element_text(size = 16)
    )
}

make_pca_panel <- function(pc_axis, climate_variable, x_label) {
  point_data <- trait_pca_full_long |>
    dplyr::filter(pc_axis == !!pc_axis, climate_variable == !!climate_variable)

  region_pred <- trait_pca_full_climate_models_region_output |>
    dplyr::filter(pc_axis == !!pc_axis, climate_variable == !!climate_variable) |>
    dplyr::select(is_significant, predictions) |>
    tidyr::unnest(predictions)

  global_pred <- trait_pca_full_climate_models_output |>
    dplyr::filter(pc_axis == !!pc_axis, climate_variable == !!climate_variable) |>
    dplyr::select(is_significant, predictions) |>
    tidyr::unnest(predictions)

  axis_label <- paste0(
    pc_axis,
    " (",
    round(as.numeric(trait_pca_full_variance[[pc_axis]]), 1),
    "% variance)"
  )

  ggplot(point_data, aes(x = climate_value_raw, y = trait_value)) +
    geom_point(aes(colour = region), alpha = 0.5, size = 2.4) +
    geom_ribbon(
      data = region_pred,
      aes(x = climate_value, y = .fitted, ymin = plo, ymax = phi, fill = region),
      alpha = 0.16,
      colour = NA
    ) +
    geom_line(
      data = region_pred,
      aes(x = climate_value, y = .fitted, colour = region, linetype = is_significant),
      linewidth = 1.1
    ) +
    geom_ribbon(
      data = global_pred,
      aes(x = climate_value, y = .fitted, ymin = plo, ymax = phi),
      fill = "grey55",
      alpha = 0.13,
      colour = NA
    ) +
    geom_line(
      data = global_pred,
      aes(x = climate_value, y = .fitted, linetype = is_significant),
      colour = "grey35",
      linewidth = 1.3
    ) +
    presentation_region_scale_colour() +
    scale_fill_manual(values = create_region_color_mapping(), guide = "none") +
    scale_linetype_manual(values = c("FALSE" = "22", "TRUE" = "solid"), guide = "none") +
    presentation_climate_theme() +
    labs(
      x = x_label,
      y = axis_label
    )
}

make_trait_climate_panel <- function(
    point_data,
    region_output,
    global_output,
    trait_trans,
    climate_variable,
    x_label,
    y_label
) {
  point_data <- point_data |>
    dplyr::filter(
      trait_trans == !!trait_trans,
      climate_variable == !!climate_variable
    )

  region_pred <- region_output |>
    dplyr::filter(
      trait_trans == !!trait_trans,
      climate_variable == !!climate_variable
    ) |>
    dplyr::select(is_significant, predictions) |>
    tidyr::unnest(predictions)

  global_pred <- global_output |>
    dplyr::filter(
      trait_trans == !!trait_trans,
      climate_variable == !!climate_variable
    ) |>
    dplyr::select(is_significant, predictions) |>
    tidyr::unnest(predictions)

  ggplot2::ggplot(point_data, ggplot2::aes(x = climate_value_raw, y = trait_value)) +
    ggplot2::geom_point(ggplot2::aes(colour = region), alpha = 0.5, size = 2.4) +
    ggplot2::geom_ribbon(
      data = region_pred,
      ggplot2::aes(x = climate_value, y = .fitted, ymin = plo, ymax = phi, fill = region),
      alpha = 0.16,
      colour = NA
    ) +
    ggplot2::geom_line(
      data = region_pred,
      ggplot2::aes(x = climate_value, y = .fitted, colour = region, linetype = is_significant),
      linewidth = 1.1
    ) +
    ggplot2::geom_ribbon(
      data = global_pred,
      ggplot2::aes(x = climate_value, y = .fitted, ymin = plo, ymax = phi),
      fill = "grey55",
      alpha = 0.13,
      colour = NA
    ) +
    ggplot2::geom_line(
      data = global_pred,
      ggplot2::aes(x = climate_value, y = .fitted, linetype = is_significant),
      colour = "grey35",
      linewidth = 1.3
    ) +
    presentation_region_scale_colour() +
    ggplot2::scale_fill_manual(values = create_region_color_mapping(), guide = "none") +
    ggplot2::scale_linetype_manual(values = c("FALSE" = "22", "TRUE" = "solid"), guide = "none") +
    presentation_climate_theme() +
    ggplot2::labs(
      x = x_label,
      y = y_label
    )
}

p_pc1_gs_length <- make_pca_panel(
  pc_axis = "PC1",
  climate_variable = "gs_length",
  x_label = "Growing season length (days)"
)

p_pc2_gs_temperature <- make_pca_panel(
  pc_axis = "PC2",
  climate_variable = "gs_temperature",
  x_label = "Growing season temperature (°C)"
)

presentation_fig <- p_pc1_gs_length + p_pc2_gs_temperature +
  patchwork::plot_layout(guides = "collect") &
  theme(legend.position = "top")

ggplot2::ggsave(
  filename = "figures/presentation_pca_axes_climate.png",
  plot = presentation_fig,
  width = 18,
  height = 8,
  dpi = 300
)

message("Saved: figures/presentation_pca_axes_climate.png")

make_turnover_itv_plot <- function(partition_data, regions) {
  partition_data |>
    dplyr::filter(
      region %in% regions,
      process %in% c("turnover", "intraspecific")
    ) |>
    dplyr::group_by(region, trait_trans) |>
    dplyr::mutate(
      sum_val = sum(proportion),
      proportion_standardized = proportion / sum_val
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      region = factor(region, levels = regions),
      process = dplyr::recode(process, intraspecific = "ITV"),
      process = factor(process, levels = c("ITV", "turnover")),
      proportion_standardized = dplyr::if_else(
        process == "turnover",
        -1 * proportion_standardized,
        proportion_standardized
      )
    ) |>
    ggplot2::ggplot(ggplot2::aes(x = figure_names, y = proportion_standardized, fill = process)) +
    ggplot2::geom_col() +
    ggplot2::geom_hline(yintercept = 0, colour = "grey", linetype = "dashed") +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(
      name = "Process",
      values = c("ITV" = "#005BBB", "turnover" = "#FFD500")
    ) +
    ggplot2::scale_x_discrete(limits = rev, labels = scales::label_parse()) +
    ggplot2::lims(y = c(-1, 1)) +
    ggplot2::labs(x = "", y = "Relative contribution to variance") +
    ggplot2::facet_grid(class ~ region, scales = "free", space = "free_y") +
    ggplot2::theme_bw(base_size = 18) +
    ggplot2::theme(
      legend.position = "top",
      legend.title = ggplot2::element_text(size = 18),
      legend.text = ggplot2::element_text(size = 16),
      axis.title = ggplot2::element_text(size = 20),
      axis.text = ggplot2::element_text(size = 16),
      strip.text = ggplot2::element_text(size = 16),
      strip.text.y = ggplot2::element_blank(),
      panel.spacing = ggplot2::unit(0.3, "cm")
    )
}

turnover_itv_fig <- make_turnover_itv_plot(leps_variance_partition, presentation_itv_regions)

ggplot2::ggsave(
  filename = "figures/presentation_turnover_itv.png",
  plot = turnover_itv_fig,
  width = 14,
  height = 10,
  dpi = 300
)

message("Saved: figures/presentation_turnover_itv.png")

p_sla_vpd <- make_trait_climate_panel(
  point_data = trait_mean_long,
  region_output = trait_models_region_output,
  global_output = trait_models_output,
  trait_trans = "sla_cm2_g",
  climate_variable = "gs_vpd",
  x_label = "Growing season VPD",
  y_label = "SLA (cm²/g)"
)

ggplot2::ggsave(
  filename = "figures/presentation_sla_vpd.png",
  plot = p_sla_vpd,
  width = 10,
  height = 8,
  dpi = 300
)

message("Saved: figures/presentation_sla_vpd.png")
