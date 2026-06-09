## Climate figures: growing-season climate vs latitude, and daily temperature
## seasonality with the detected growing-season window per country.

# Country code -> region label (north to south), matching the rest of the figures.
climate_region_lookup <- function() {
  tibble::tibble(
    country = c("sv", "no", "co", "ch", "pe", "sa"),
    region = c(
      "Svalbard", "Southern Scandes", "Rocky Mountains",
      "Eastern Himalaya", "Central Andes", "Drakensberg"
    )
  )
}

climate_region_levels <- function() {
  c(
    "Svalbard", "Southern Scandes", "Rocky Mountains",
    "Eastern Himalaya", "Central Andes", "Drakensberg"
  )
}

climate_variable_labels <- function() {
  c(
    gs_length = "Growing season length (days)",
    gs_temperature = "Growing season temperature (°C)",
    gs_vpd = "Growing season VPD",
    gdd = "Growing degree days (>5°C)",
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
    mutate(region = factor(region, levels = climate_region_levels())) |>
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

# Mean daily temperature through the year for each country, with the detected
# growing-season window drawn as a bar and the 5 degC threshold marked. The
# series is ordered with the hemisphere-aware key so the (austral) summer is
# contiguous; the x axis is the day within the growing-season year.
make_climate_seasonality_plot <- function(daily, gs) {
  region_lookup <- climate_region_lookup()
  region_levels <- climate_region_levels()

  # Day within the (hemisphere-ordered) season year, 1 = first day of the series.
  origin <- daily |>
    group_by(country) |>
    summarise(origin = min(gs_order), .groups = "drop")

  daily_country <- daily |>
    left_join(origin, by = "country") |>
    mutate(season_day = as.integer(gs_order - origin) + 1L) |>
    group_by(country, season_day) |>
    summarise(t_mean = mean(t_mean, na.rm = TRUE), .groups = "drop") |>
    left_join(region_lookup, by = "country") |>
    mutate(region = factor(region, levels = region_levels))

  # Per-site windows, then country mean + min–max span across sites (elevational gradient).
  gs_plot <- gs |>
    filter(gs_found) |>
    left_join(origin, by = "country") |>
    mutate(
      start_day = as.integer(gs_order_start - origin) + 1L,
      end_day = as.integer(gs_order_end - origin) + 1L
    )

  gs_site <- gs_plot |>
    group_by(country, site) |>
    summarise(
      start_day = round(mean(start_day)),
      end_day = round(mean(end_day)),
      gs_length = round(mean(gs_length)),
      .groups = "drop"
    )

  gs_country <- gs_site |>
    group_by(country) |>
    summarise(
      start_min = min(start_day),
      end_max = max(end_day),
      start_mean = round(mean(start_day)),
      end_mean = round(mean(end_day)),
      gs_length = round(mean(gs_length)),
      .groups = "drop"
    ) |>
    left_join(region_lookup, by = "country") |>
    mutate(region = factor(region, levels = region_levels))

  # Shared y-axis: bars sit at a fixed position below the coldest daily mean.
  y_bar <- floor(min(daily_country$t_mean, na.rm = TRUE)) - 3
  y_label <- y_bar - 2.5
  gs_country <- gs_country |>
    mutate(y_bar = y_bar, y_label = y_label)

  ggplot(daily_country, aes(x = season_day, y = t_mean)) +
    geom_hline(yintercept = 5, linetype = "dashed", colour = "grey55") +
    geom_line(aes(colour = region), linewidth = 0.6) +
    geom_segment(
      data = gs_country,
      aes(x = start_min, xend = end_max, y = y_bar, yend = y_bar, colour = region),
      alpha = 0.35,
      linewidth = 4,
      lineend = "butt"
    ) +
    geom_segment(
      data = gs_country,
      aes(x = start_mean, xend = end_mean, y = y_bar, yend = y_bar, colour = region),
      linewidth = 3,
      lineend = "butt"
    ) +
    geom_text(
      data = gs_country,
      aes(x = (start_mean + end_mean) / 2, y = y_label, label = paste0(gs_length, " days")),
      size = 3
    ) +
    facet_wrap(~region) +
    scale_colour_manual(values = create_region_color_mapping(), drop = FALSE) +
    theme_bw() +
    theme(
      legend.position = "none",
      strip.text = element_text(size = 11, face = "bold"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10)
    ) +
    labs(
      x = "Day of growing-season year (1 = Jan 1; Southern Hemisphere: 1 = Jul 1)",
      y = "Mean daily temperature (°C)"
    )
}
