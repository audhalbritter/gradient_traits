## Trait figures

make_trait_region_climate_plot <- function(data, prediction_region, prediction_global, x_label, y_label = "Trait value") {
  data <- data |>
    mutate(region = factor(region, levels = c(
      "Svalbard", "Southern Scandes", "Rocky Mountains",
      "Eastern Himalaya", "Central Andes", "Drakensberg"
    )))

  prediction_region <- prediction_region |>
    mutate(region = factor(region, levels = c(
      "Svalbard", "Southern Scandes", "Rocky Mountains",
      "Eastern Himalaya", "Central Andes", "Drakensberg"
    )))

  data <- data %>% fancy_trait_name_dictionary()
  prediction_region <- prediction_region %>% fancy_trait_name_dictionary()
  prediction_global <- prediction_global %>% fancy_trait_name_dictionary()

  ggplot(data, aes(x = climate_value_raw, y = trait_value)) +
    geom_point(aes(colour = region), alpha = 0.4, size = 1.5) +
    geom_ribbon(
      data = prediction_region,
      aes(x = climate_value, y = .fitted, ymin = plo, ymax = phi, fill = region),
      alpha = 0.15, colour = NA
    ) +
    geom_line(
      data = prediction_region,
      aes(x = climate_value, y = .fitted, colour = region, linetype = is_significant),
      linewidth = 0.8
    ) +
    geom_ribbon(
      data = prediction_global,
      aes(x = climate_value, y = .fitted, ymin = plo, ymax = phi),
      fill = "grey60", alpha = 0.1, colour = NA
    ) +
    geom_line(
      data = prediction_global,
      aes(x = climate_value, y = .fitted, linetype = is_significant),
      colour = "grey60", linewidth = 1
    ) +
    scale_colour_manual(values = create_region_color_mapping(), name = "Region") +
    scale_fill_manual(values = create_region_color_mapping(), name = "Region") +
    scale_linetype_manual(values = c("FALSE" = "22", "TRUE" = "solid"), guide = "none") +
    facet_wrap(~trait_fancy, scales = "free_y") +
    theme_bw() +
    theme(
      legend.position = "top",
      strip.text = element_text(size = 10, face = "bold")
    ) +
    labs(x = x_label, y = y_label)
}

make_pca_climate_plot <- function(data, prediction_region, prediction_global, x_label, variance_explained = NULL) {
  pc_labels <- if (!is.null(variance_explained)) {
    stats::setNames(
      paste0(names(variance_explained)[names(variance_explained) %in% unique(data$pc_axis)], " (",
             round(variance_explained[names(variance_explained) %in% unique(data$pc_axis)], 1), "%)"),
      names(variance_explained)[names(variance_explained) %in% unique(data$pc_axis)]
    )
  } else {
    stats::setNames(unique(data$pc_axis), unique(data$pc_axis))
  }

  data <- data |>
    mutate(
      region = factor(region, levels = climate_region_levels()),
      pc_fancy = pc_labels[as.character(pc_axis)]
    )

  prediction_region <- prediction_region |>
    mutate(
      region = factor(region, levels = climate_region_levels()),
      pc_fancy = pc_labels[as.character(pc_axis)]
    )

  prediction_global <- prediction_global |>
    mutate(pc_fancy = pc_labels[as.character(pc_axis)])

  ggplot(data, aes(x = climate_value_raw, y = trait_value)) +
    geom_point(aes(colour = region), alpha = 0.4, size = 1.5) +
    geom_ribbon(
      data = prediction_region,
      aes(x = climate_value, y = .fitted, ymin = plo, ymax = phi, fill = region),
      alpha = 0.15, colour = NA
    ) +
    geom_line(
      data = prediction_region,
      aes(x = climate_value, y = .fitted, colour = region, linetype = is_significant),
      linewidth = 0.8
    ) +
    geom_ribbon(
      data = prediction_global,
      aes(x = climate_value, y = .fitted, ymin = plo, ymax = phi),
      fill = "grey60", alpha = 0.1, colour = NA
    ) +
    geom_line(
      data = prediction_global,
      aes(x = climate_value, y = .fitted, linetype = is_significant),
      colour = "grey60", linewidth = 1
    ) +
    scale_colour_manual(values = create_region_color_mapping(), name = "Region") +
    scale_fill_manual(values = create_region_color_mapping(), name = "Region") +
    scale_linetype_manual(values = c("FALSE" = "22", "TRUE" = "solid"), guide = "none") +
    facet_wrap(~pc_fancy, scales = "free_y") +
    theme_bw() +
    theme(
      legend.position = "top",
      strip.text = element_text(size = 10, face = "bold")
    ) +
    labs(x = x_label, y = "PCA score")
}

make_pca_climate_comparison_plot <- function(region_output, global_output, raw_data, climate_var, x_label, variance_explained = NULL) {
  reg_data <- region_output |>
    filter(climate_variable == climate_var) |>
    select(pc_axis, is_significant, predictions) |>
    unnest(predictions)

  glob_data <- global_output |>
    filter(climate_variable == climate_var) |>
    select(pc_axis, is_significant, predictions) |>
    unnest(predictions)

  points_data <- raw_data |>
    filter(climate_variable == climate_var) |>
    filter(pc_axis %in% unique(reg_data$pc_axis))

  make_pca_climate_plot(
    data = points_data,
    prediction_region = reg_data,
    prediction_global = glob_data,
    x_label = x_label,
    variance_explained = variance_explained
  )
}

make_trait_comparison_plot <- function(
    region_output,
    global_output,
    raw_data,
    climate_var,
    x_label,
    y_label = "Trait value"
) {
  reg_data <- region_output %>%
    filter(climate_variable == climate_var) %>%
    select(trait_trans, is_significant, predictions) %>%
    unnest(predictions)

  glob_data <- global_output %>%
    filter(climate_variable == climate_var) %>%
    select(trait_trans, is_significant, predictions) %>%
    unnest(predictions)

  points_data <- raw_data %>%
    filter(climate_variable == climate_var) %>%
    filter(trait_trans %in% unique(reg_data$trait_trans))

  make_trait_region_climate_plot(
    data = points_data,
    prediction_region = reg_data,
    prediction_global = glob_data,
    x_label = x_label,
    y_label = y_label
  )
}

make_trait_ridgeline_plot <- function(data) {
  data |>
    fancy_trait_name_dictionary() |>
    mutate(elevation_bin = cut(elevation_m,
      breaks = seq(0, 6000, by = 500),
      labels = paste0(seq(0, 5500, by = 500), "-", seq(500, 6000, by = 500), " m")
    )) |>
    filter(!is.na(elevation_bin)) |>
    ggplot(aes(x = trait_value, y = elevation_bin, fill = region, colour = region)) +
    geom_density_ridges(alpha = 0.7, scale = 1.2, rel_min_height = 0.01) +
    facet_wrap(~trait_fancy, scales = "free_x", ncol = 3) +
    scale_fill_manual(values = create_region_color_mapping(), name = "Region") +
    scale_colour_manual(values = create_region_color_mapping(), name = "Region") +
    labs(
      x = "Trait Value",
      y = ""
    ) +
    theme_bw() +
    theme(
      legend.position = "top",
      axis.title = element_text(size = 8)
    )
}

trait_plot_key <- function(dat, scale_hierarchy) {
  cols <- as.character(scale_hierarchy)
  cols <- cols[cols != "global"]
  apply(dat |> dplyr::select(dplyr::any_of(cols)), 1, paste, collapse = "_")
}

# Unique plot key for raw trait observations (South Africa reuses plot_id across aspects)
trait_observation_plot_key <- function(dat) {
  dplyr::if_else(
    dat$country == "sa",
    paste(dat$gradient, dat$plot_id, sep = "/"),
    dat$plot_id
  )
}

# traitstrap::fortify_filled_trait() fails with dplyr >= 1.2 when cover is duplicated
# per plot × taxon; average abundance within taxon before summing cover by level
fortify_trait_coverage <- function(filled_traits) {
  attrib <- attr(filled_traits, "attrib")
  abun_col <- attrib$abundance_col
  trait_col <- attrib$trait_col
  taxon_col <- attrib$taxon_col

  scale_hierarchy <- as.character(attrib$scale_hierarchy)
  scale_hierarchy <- scale_hierarchy[scale_hierarchy != "global"]

  dat <- filled_traits |> dplyr::ungroup()
  plot_key <- trait_plot_key(dat, scale_hierarchy)

  dat |>
    dplyr::mutate(.id = plot_key) |>
    dplyr::group_by(.data$.id, .data$level, .data[[trait_col]], .data[[taxon_col]]) |>
    dplyr::summarise(
      cover = mean(.data[[abun_col]], na.rm = TRUE),
      sum_abun = dplyr::first(.data$sum_abun),
      .groups = "drop"
    ) |>
    dplyr::group_by(.data$.id, .data$level) |>
    dplyr::summarise(
      s = sum(cover) / dplyr::first(sum_abun),
      .groups = "drop"
    )
}

make_trait_coverage_plot <- function(filled_traits) {
  attrib <- attr(filled_traits, "attrib")
  scale_hierarchy <- as.character(attrib$scale_hierarchy)
  scale_hierarchy <- scale_hierarchy[scale_hierarchy != "global"]

  dat <- filled_traits |> dplyr::ungroup()
  plot_key <- trait_plot_key(dat, scale_hierarchy)

  plot_meta <- dat |>
    dplyr::mutate(.id = plot_key) |>
    dplyr::distinct(.data$.id, .data$country, .data$region, .data$site, .data$gradient, .data$plot_id)

  plot_data <- fortify_trait_coverage(filled_traits) |>
    dplyr::left_join(plot_meta, by = ".id") |>
    dplyr::group_by(.data$.id) |>
    dplyr::mutate(s_prop = .data$s / sum(.data$s, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      plot_axis = dplyr::if_else(
        .data$country == "sa",
        paste(.data$gradient, .data$plot_id, sep = "/"),
        .data$plot_id
      ),
      country = factor(
        .data$country,
        levels = c("sv", "no", "co", "ch", "pe", "sa")
      )
    )

  ggplot(plot_data, aes(x = plot_axis, y = s_prop, fill = level)) +
    geom_col(width = 0.85) +
    facet_wrap(~country, scales = "free_x") +
    scale_x_discrete(guide = guide_axis(angle = 90, check.overlap = TRUE)) +
    scale_y_continuous(limits = c(0, 1), expand = expansion(mult = c(0, 0.02))) +
    theme_bw() +
    theme(
      axis.text.x = element_text(size = 6, vjust = 0.5, hjust = 1),
      strip.text = element_text(face = "bold"),
      legend.position = "top"
    ) +
    labs(
      x = "Plot",
      y = "Proportion of cover",
      fill = "Sampling level"
    )
}
