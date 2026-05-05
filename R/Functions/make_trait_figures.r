## Trait figures

make_trait_region_climate_plot <- function(data, prediction_region, prediction_global, x_label) {
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

  ggplot(data, aes(x = climate_value, y = trait_value)) +
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
    labs(x = x_label, y = "Trait Value")
}

make_trait_comparison_plot <- function(region_output, global_output, raw_data, climate_var, x_label) {
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
    x_label = x_label
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
