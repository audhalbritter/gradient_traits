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

climate_variable_labels <- function() {
  c(
    gs_length = "Growing season length (days)",
    gs_temperature = "Growing season temperature (°C)",
    gs_vpd = "Growing season VPD",
    gdd = "Growing degree days (>2°C)",
    gs_diurnal_range = "Diurnal range (°C)"
  )
}

# All five growing-season climate variables versus site latitude (faceted)
make_climate_latitude_plot <- function(dat) {
  labels <- climate_variable_labels()

  plot_data <- dat |>
    group_by(region) |>
    mutate(elevation_percentile = percent_rank(elevation_m) * 100) |>
    ungroup() |>
    mutate(region = factor(region, levels = c(
      "Svalbard", "Southern Scandes", "Rocky Mountains",
      "Eastern Himalaya", "Central Andes", "Drakensberg"
    ))) |>
    pivot_longer(
      cols = all_of(names(labels)),
      names_to = "climate_variable",
      values_to = "climate_value"
    ) |>
    filter(!is.na(climate_value)) |>
    mutate(climate_variable = factor(
      climate_variable,
      levels = names(labels),
      labels = unname(labels)
    ))

  ggplot(plot_data, aes(x = latitude_n, y = climate_value, color = region)) +
    geom_point(aes(size = elevation_percentile), alpha = 0.6) +
    facet_wrap(~climate_variable, scales = "free_y") +
    scale_color_manual(values = create_region_color_mapping()) +
    scale_size_continuous(name = "Elevation percentile", range = c(1.5, 5)) +
    theme_bw() +
    theme(
      legend.position = "top",
      legend.box = "horizontal",
      strip.text = element_text(size = 11, face = "bold"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10)
    ) +
    labs(
      x = "Latitude (°N)",
      y = NULL,
      color = "Region",
      size = "Elevation percentile"
    )
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

make_diversity_three_panel_plot <- function(lat_predictions, climate_predictions) {
  p_lat <- make_diversity_plot(lat_predictions, compact = TRUE) +
    ggplot2::labs(y = "Shannon diversity")

  p_t2m <- make_diversity_climate_plot(
    region_predictions = climate_predictions$region,
    global_predictions = climate_predictions$global,
    climate_variable = "gs_temperature",
    xlab = "Growing season temperature (°C)",
    compact = TRUE
  ) +
    ggplot2::labs(y = NULL)

  p_vpd <- make_diversity_climate_plot(
    region_predictions = climate_predictions$region,
    global_predictions = climate_predictions$global,
    climate_variable = "gs_vpd",
    xlab = "Growing season VPD",
    compact = TRUE
  ) +
    ggplot2::labs(y = NULL)

  patchwork::wrap_plots(p_lat, p_t2m, p_vpd, ncol = 3, guides = "collect") &
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
