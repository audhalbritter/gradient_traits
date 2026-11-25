## Diversity and general figures

make_region_world_map <- function(coords) {
  # Sites span ~79°N (Svalbard) to ~29°S (Drakensberg); omit Antarctica.
  lat_lim <- c(-50, 85)
  lon_lim <- c(-180, 180)

  cache_path <- file.path("WorldClimData")
  if (!dir.exists(cache_path)) {
    dir.create(cache_path, recursive = TRUE)
  }
  elev_raster <- geodata::worldclim_global(var = "elev", res = 10, path = cache_path)
  elev_raster <- terra::crop(
    elev_raster,
    terra::ext(lon_lim[1], lon_lim[2], lat_lim[1], lat_lim[2])
  )
  elev_df <- as.data.frame(elev_raster, xy = TRUE, na.rm = TRUE)
  names(elev_df) <- c("lon", "lat", "elev")
  elev_df <- elev_df |>
    dplyr::filter(elev > 0)

  world <- ggplot2::map_data("world") |>
    dplyr::filter(!region %in% "Antarctica")

  coords <- coords |>
    dplyr::mutate(
      region_label = dplyr::case_when(
        region %in% c("sv", "Svalbard") ~ "Svalbard",
        region %in% c("no", "Southern Scandes") ~ "Southern Scandes",
        region %in% c("co", "Rocky Mountains") ~ "Rocky Mountains",
        region %in% c("ch", "Eastern Himalaya") ~ "Eastern Himalaya",
        region %in% c("pe", "Central Andes") ~ "Central Andes",
        region %in% c("sa", "Drakensberg") ~ "Drakensberg",
        TRUE ~ as.character(region)
      ),
      region_label = factor(
        region_label,
        levels = c(
          "Svalbard", "Southern Scandes", "Rocky Mountains",
          "Eastern Himalaya", "Central Andes", "Drakensberg"
        )
      )
    )

  site_pts <- coords |>
    dplyr::distinct(region_label, site, longitude_e, latitude_n)

  ggplot2::ggplot() +
    ggplot2::geom_polygon(
      data = world,
      ggplot2::aes(long, lat, group = group),
      fill = "grey88",
      colour = "grey55",
      linewidth = 0.15
    ) +
    ggplot2::geom_raster(
      data = elev_df,
      ggplot2::aes(lon, lat, fill = elev),
      alpha = 0.92
    ) +
    ggplot2::scale_fill_gradientn(
      colours = c("grey40", "grey50", "grey60", "grey70", "white"),
      name = "Elevation (m)"
    ) +
    ggplot2::geom_point(
      data = site_pts,
      ggplot2::aes(x = longitude_e, y = latitude_n, colour = region_label),
      alpha = 0.9,
      size = 3
    ) +
    ggplot2::scale_color_manual(
      values = create_region_color_mapping(),
      drop = FALSE,
      name = "Region"
    ) +
    ggplot2::coord_fixed(
      xlim = lon_lim,
      ylim = lat_lim,
      expand = FALSE
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "grey92"),
      legend.position = "top",
      legend.box = "vertical",
      legend.box.just = "left",
      legend.spacing.y = ggplot2::unit(0.4, "cm"),
      plot.margin = ggplot2::margin(12, 6, 6, 6, unit = "pt")
    ) +
    ggplot2::labs(x = "Longitude", y = "Latitude")
}

make_diversity_plot <- function(data, compact = FALSE) {
  pt <- if (compact) 1.8 else 2.4
  lw <- if (compact) 0.95 else 1.2
  title_txt <- if (compact) 12 else 14
  axis_txt <- if (compact) 10 else 11

  plot_data <- data |>
    unnest(data_with_predictions) |>
    mutate(region = factor(region, levels = c(
      "Svalbard", "Southern Scandes", "Rocky Mountains",
      "Eastern Himalaya", "Central Andes", "Drakensberg"
    )))

  n_idx <- length(unique(plot_data$diversity_index))

  gg <- ggplot(plot_data, aes(x = latitude_n, y = value, color = region)) +
    geom_point(alpha = 0.6, size = pt) +
    geom_line(aes(x = latitude_n, y = .fitted, linetype = is_significant),
      linewidth = lw, color = "grey40", show.legend = FALSE
    ) +
    geom_ribbon(aes(x = latitude_n, ymin = plo, ymax = phi),
      alpha = 0.2, color = NA, fill = "grey40"
    ) +
    scale_color_manual(values = create_region_color_mapping()) +
    scale_linetype_manual(
      values = c("FALSE" = "dashed", "TRUE" = "solid"),
      guide = "none"
    ) +
    theme_bw() +
    theme(
      legend.position = "top",
      legend.box = "horizontal",
      strip.text = element_text(size = title_txt, face = "bold"),
      axis.title = element_text(size = title_txt),
      axis.text = element_text(size = axis_txt)
    ) +
    labs(
      x = "Latitude (°N)",
      y = "Shannon diversity",
      color = "Region"
    )

  if (n_idx > 1L) {
    gg <- gg + facet_wrap(~diversity_index, scales = "free_y", labeller = label_value)
  }

  gg
}

make_diversity_climate_plot <- function(region_predictions, global_predictions, climate_variable, xlab = "Climate", compact = FALSE) {
  pt <- if (compact) 1.8 else 2.4
  lw <- if (compact) 0.95 else 1.2
  title_txt <- if (compact) 12 else 14
  axis_txt <- if (compact) 10 else 11

  region_data <- region_predictions |>
    dplyr::filter(climate_variable == !!climate_variable) |>
    unnest(data_with_predictions) |>
    mutate(region = factor(region, levels = c(
      "Svalbard", "Southern Scandes", "Rocky Mountains",
      "Eastern Himalaya", "Central Andes", "Drakensberg"
    )))

  global_data <- global_predictions |>
    dplyr::filter(climate_variable == !!climate_variable) |>
    unnest(data_with_predictions) |>
    mutate(region = factor(region, levels = c(
      "Svalbard", "Southern Scandes", "Rocky Mountains",
      "Eastern Himalaya", "Central Andes", "Drakensberg"
    )))

  n_idx <- length(unique(region_data$diversity_index))

  gg <- ggplot(region_data, aes(x = climate_value, y = value)) +
    geom_point(aes(color = region), alpha = 0.6, size = pt) +
    geom_ribbon(
      data = region_data,
      aes(y = .fitted, ymin = plo, ymax = phi, fill = region),
      alpha = 0.15, color = NA
    ) +
    geom_line(
      data = region_data,
      aes(y = .fitted, color = region, linetype = is_significant),
      linewidth = lw, show.legend = FALSE
    ) +
    geom_ribbon(
      data = global_data,
      aes(y = .fitted, ymin = plo, ymax = phi),
      fill = "grey60", alpha = 0.1, color = NA
    ) +
    geom_line(
      data = global_data,
      aes(y = .fitted, linetype = is_significant),
      color = "grey60", linewidth = lw + 0.2, show.legend = FALSE
    ) +
    scale_color_manual(values = create_region_color_mapping(), name = "Region") +
    scale_fill_manual(values = create_region_color_mapping(), name = "Region", guide = "none") +
    scale_linetype_manual(values = c("FALSE" = "dashed", "TRUE" = "solid"), guide = "none") +
    theme_bw() +
    theme(
      legend.position = "top",
      legend.box = "horizontal",
      strip.text = element_text(size = title_txt, face = "bold"),
      axis.title = element_text(size = title_txt),
      axis.text = element_text(size = axis_txt)
    ) +
    labs(
      x = xlab,
      y = "Shannon diversity",
      color = "Region"
    )

  if (n_idx > 1L) {
    gg <- gg + facet_wrap(~diversity_index, scales = "free_y", labeller = label_value)
  }

  gg
}

# Shannon diversity vs latitude and all five growing-season climate variables
make_diversity_climate_five_panel_plot <- function(climate_predictions, lat_predictions) {
  labels <- climate_variable_labels()

  p_lat <- make_diversity_plot(lat_predictions, compact = TRUE) +
    ggplot2::labs(y = "Shannon diversity")

  climate_panels <- purrr::imap(
    labels,
    function(xlab, climate_variable) {
      make_diversity_climate_plot(
        region_predictions = climate_predictions$region,
        global_predictions = climate_predictions$global,
        climate_variable = climate_variable,
        xlab = xlab,
        compact = TRUE
      ) +
        ggplot2::labs(y = NULL)
    }
  )

  patchwork::wrap_plots(c(list(latitude = p_lat), climate_panels), ncol = 3, guides = "collect") &
    ggplot2::theme(
      legend.position = "top",
      legend.box = "horizontal",
      legend.title = ggplot2::element_text(size = 11),
      legend.text = ggplot2::element_text(size = 10),
      axis.title = ggplot2::element_text(size = 12),
      axis.text = ggplot2::element_text(size = 10),
      plot.margin = ggplot2::margin(4, 4, 4, 4)
    )
}

make_beta_turnover_nestedness_plot <- function(beta_adjacent_pairs, beta_region_summary) {
  pair_long <- beta_adjacent_pairs |>
    dplyr::select(region, gradient, elev_mid, turnover_fraction, nestedness_fraction) |>
    tidyr::pivot_longer(
      cols = c(turnover_fraction, nestedness_fraction),
      names_to = "component",
      values_to = "fraction"
    ) |>
    dplyr::mutate(
      component = dplyr::recode(
        component,
        turnover_fraction = "Turnover",
        nestedness_fraction = "Nestedness"
      ),
      region = factor(region, levels = climate_region_levels())
    )

  p_pairs <- pair_long |>
    ggplot(aes(x = elev_mid, y = fraction, colour = component)) +
    geom_point(alpha = 0.55, size = 1.5) +
    geom_smooth(method = "loess", se = FALSE, linewidth = 0.7) +
    facet_wrap(~region, ncol = 3) +
    scale_color_manual(values = c("Turnover" = "#1b9e77", "Nestedness" = "#d95f02")) +
    scale_y_continuous(limits = c(0, 1)) +
    theme_bw() +
    theme(
      legend.position = "top",
      strip.text = element_text(size = 10, face = "bold"),
      axis.title = element_text(size = 11),
      axis.text = element_text(size = 9)
    ) +
    labs(
      x = "Elevation midpoint between adjacent plots (m)",
      y = "Fraction of total beta diversity",
      colour = "Component"
    )

  region_long <- beta_region_summary |>
    dplyr::select(region, mean_turnover_fraction, mean_nestedness_fraction) |>
    tidyr::pivot_longer(
      cols = c(mean_turnover_fraction, mean_nestedness_fraction),
      names_to = "component",
      values_to = "fraction"
    ) |>
    dplyr::mutate(
      component = dplyr::recode(
        component,
        mean_turnover_fraction = "Turnover",
        mean_nestedness_fraction = "Nestedness"
      ),
      region = factor(region, levels = climate_region_levels())
    )

  p_region <- region_long |>
    ggplot(aes(x = region, y = fraction, fill = component)) +
    geom_col(position = "stack") +
    coord_flip() +
    scale_fill_manual(values = c("Turnover" = "#1b9e77", "Nestedness" = "#d95f02")) +
    scale_y_continuous(limits = c(0, 1)) +
    theme_bw() +
    theme(
      legend.position = "top",
      axis.title = element_text(size = 11),
      axis.text = element_text(size = 9)
    ) +
    labs(
      x = "",
      y = "Mean fraction across adjacent pairs",
      fill = "Component"
    )

  patchwork::wrap_plots(p_pairs, p_region, ncol = 1, heights = c(2.6, 1.2), guides = "collect")
}
