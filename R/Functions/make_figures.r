### Figures

## REGION COLOR MAPPING
# Function to create consistent color mapping for regions sorted by latitude (north to south)
create_region_color_mapping <- function() {
  # Define regions in order from north to south by latitude
  regions_ordered <- c(
    "Svalbard", "Southern Scandes", "Rocky Mountains",
    "Eastern Himalaya", "Central Andes", "Drakensberg"
  )

  # Create MetBrewer Archambault palette and reverse it so Svalbard gets yellow, Drakensberg gets blue
  colors <- rev(met.brewer("Archambault", n = length(regions_ordered)))

  # Create named vector for consistent mapping
  names(colors) <- regions_ordered

  return(colors)
}

## WORLD MAP OF REGIONS
make_region_world_map <- function(coords) {
  # Elevation raster background from WorldClim (10 arc-min)
  cache_path <- file.path("WorldClimData")
  if (!dir.exists(cache_path)) dir.create(cache_path, recursive = TRUE)
  elev_raster <- geodata::worldclim_global(var = "elev", res = 10, path = cache_path)
  elev_df <- as.data.frame(elev_raster, xy = TRUE, na.rm = TRUE)
  names(elev_df) <- c("lon", "lat", "elev")

  # World polygons
  world <- ggplot2::map_data("world")

  # Harmonize region labels to match palette (ensure Svalbard appears)
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
      region_label = factor(region_label,
        levels = c(
          "Svalbard", "Southern Scandes", "Rocky Mountains",
          "Eastern Himalaya", "Central Andes", "Drakensberg"
        )
      )
    )

  ggplot2::ggplot() +
    # Elevation background (as raster)
    ggplot2::geom_raster(data = elev_df, ggplot2::aes(lon, lat, fill = elev)) +
    ggplot2::scale_fill_gradientn(colors = c("grey40", "grey50", "grey60", "grey70", "white"), name = "Elevation (m)") +
    # Land outlines
    ggplot2::geom_polygon(
      data = world, ggplot2::aes(long, lat, group = group),
      fill = NA, color = "grey70", linewidth = 0.2
    ) +
    # Region points
    ggplot2::geom_point(
      data = dplyr::distinct(coords, region_label, site, longitude_e, latitude_n),
      ggplot2::aes(x = longitude_e, y = latitude_n, color = region_label),
      alpha = 0.9, size = 3
    ) +
    ggplot2::scale_color_manual(values = create_region_color_mapping(), drop = FALSE, name = "Region") +
    ggplot2::coord_quickmap() +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(), legend.position = "top", legend.box = "horizontal") +
    ggplot2::labs(x = "Longitude", y = "Latitude")
}

## DIVERSITY VS PREDICTOR PLOT
make_diversity_plot <- function(data) {
  # Unnest the data_with_predictions to get the combined data
  plot_data <- data |>
    unnest(data_with_predictions) |>
    # Ensure region is ordered consistently (north to south)
    mutate(region = factor(region, levels = c(
      "Svalbard", "Southern Scandes", "Rocky Mountains",
      "Eastern Himalaya", "Central Andes", "Drakensberg"
    )))

  # Check if diversity_index has values
  if (length(unique(plot_data$diversity_index)) == 0) {
    stop("No diversity_index values found in data")
  }

  n_idx <- length(unique(plot_data$diversity_index))

  gg <- ggplot(plot_data, aes(x = latitude_n, y = value, color = region)) +
    geom_point(alpha = 0.6, size = 2) +
    geom_line(aes(x = latitude_n, y = .fitted, linetype = is_significant),
      linewidth = 1, color = "grey40", show.legend = FALSE
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
      strip.text = element_text(size = 12, face = "bold"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10)
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

## DIVERSITY VS ANNUAL TEMPERATURE (BIOCLIM) PLOT
make_diversity_temp_annual_plot <- function(data) {
  plot_data <- data |>
    unnest(data_with_predictions) |>
    mutate(region = factor(region, levels = c(
      "Svalbard", "Southern Scandes", "Rocky Mountains",
      "Eastern Himalaya", "Central Andes", "Drakensberg"
    )))

  if (length(unique(plot_data$diversity_index)) == 0) {
    stop("No diversity_index values found in data")
  }

  n_idx <- length(unique(plot_data$diversity_index))

  gg <- ggplot(plot_data, aes(x = annual_temperature_bioclim, y = value, color = region)) +
    geom_point(alpha = 0.6, size = 2) +
    geom_line(aes(y = .fitted, linetype = is_significant),
      linewidth = 1, color = "grey40", show.legend = FALSE
    ) +
    geom_ribbon(aes(ymin = plo, ymax = phi),
      alpha = 0.2, color = NA, fill = "grey40"
    ) +
    scale_color_manual(values = create_region_color_mapping()) +
    scale_linetype_manual(values = c("FALSE" = "dashed", "TRUE" = "solid"), guide = "none") +
    theme_bw() +
    theme(
      legend.position = "top",
      legend.box = "horizontal",
      strip.text = element_text(size = 12, face = "bold"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10)
    ) +
    labs(
      x = "Annual Mean Temperature (°C)",
      y = "Shannon diversity",
      color = "Region"
    )

  if (n_idx > 1L) {
    gg <- gg + facet_wrap(~diversity_index, scales = "free_y", labeller = label_value)
  }

  gg
}

## TRAIT VS CLIMATE PREDICTOR PLOT (for long-format data)
make_trait_climate_plot <- function(data, climate_variable, data_source = NULL, x_label) {
  # Filter data for the specific climate variable and optionally by data source
  filtered_data <- data |>
    filter(climate_variable == !!climate_variable)

  # Add data source filter if specified
  if (!is.null(data_source)) {
    filtered_data <- filtered_data |>
      filter(data_source == !!data_source)
  }

  # Ensure region is ordered consistently (north to south)
  filtered_data <- filtered_data |>
    mutate(region = factor(region, levels = c(
      "Svalbard", "Southern Scandes", "Rocky Mountains",
      "Eastern Himalaya", "Central Andes", "Drakensberg"
    )))

  # Basic checks
  if (nrow(filtered_data) == 0) {
    stop("No data found for climate variable: ", climate_variable)
  }

  # Add trait names to data using the fancy_traits function
  plot_data <- filtered_data |>
    fancy_trait_name_dictionary() |>
    mutate(trait_name = factor(trait_fancy, levels = unique(trait_fancy)))

  # The significance info should already be in the filtered_data since it comes from the unnested predictions
  # Let's check if it's there, and if not, add it
  if (!"is_significant" %in% names(plot_data)) {
    # Get significance info from the parent data structure
    significance_info <- data |>
      filter(climate_variable == !!climate_variable)

    if (!is.null(data_source)) {
      significance_info <- significance_info |>
        filter(data_source == !!data_source)
    }

    significance_info <- significance_info |>
      select(trait_trans, is_significant) |>
      distinct()

    # Join significance info to plot data
    plot_data <- plot_data |>
      left_join(significance_info, by = "trait_trans")
  }

  # Plot with raw data points, prediction line, and confidence intervals
  ggplot(plot_data, aes(x = climate_value, y = trait_value, color = region)) +
    geom_point(alpha = 0.6, size = 2) +
    # Add prediction line with different line types based on significance
    geom_line(aes(y = .fitted, linetype = is_significant),
      linewidth = 1, color = "grey40", show.legend = FALSE
    ) +
    # Add confidence intervals
    geom_ribbon(aes(ymin = plo, ymax = phi),
      alpha = 0.2, color = NA, fill = "grey40"
    ) +
    scale_color_manual(values = create_region_color_mapping()) +
    # Set line types: solid for significant, dashed for non-significant (no legend)
    scale_linetype_manual(
      values = c("FALSE" = "dashed", "TRUE" = "solid"),
      guide = "none"
    ) +
    facet_wrap(~trait_name, scales = "free_y", labeller = label_value) +
    theme_bw() +
    theme(
      legend.position = "top",
      legend.box = "horizontal",
      strip.text = element_text(size = 10, face = "bold"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10)
    ) +
    labs(
      x = x_label,
      y = "Trait Value",
      color = "Region"
    )
}

# Regional vs Global plotting function
make_trait_region_climate_plot <- function(data, prediction_region, prediction_global, x_label) {
  # Ensure region is ordered consistently
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

  # Add trait names
  data <- data %>% fancy_trait_name_dictionary()
  prediction_region <- prediction_region %>% fancy_trait_name_dictionary()
  prediction_global <- prediction_global %>% fancy_trait_name_dictionary()

  # Plot
  ggplot(data, aes(x = climate_value, y = trait_value)) +
    # Points
    geom_point(aes(colour = region), alpha = 0.4, size = 1.5) +
    # Regional Ribbons
    geom_ribbon(
      data = prediction_region,
      aes(x = climate_value, y = .fitted, ymin = plo, ymax = phi, fill = region),
      alpha = 0.15, colour = NA
    ) +
    # Regional Lines
    geom_line(
      data = prediction_region,
      aes(x = climate_value, y = .fitted, colour = region, linetype = is_significant),
      linewidth = 0.8
    ) +
    # Global Ribbon
    geom_ribbon(
      data = prediction_global,
      aes(x = climate_value, y = .fitted, ymin = plo, ymax = phi),
      fill = "grey60", alpha = 0.1, colour = NA
    ) +
    # Global Line
    geom_line(
      data = prediction_global,
      aes(x = climate_value, y = .fitted, linetype = is_significant),
      colour = "grey60", linewidth = 1
    ) +
    # Formatting
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

# Wrapper to handle data filtering and plotting for Global vs Regional comparisons
make_trait_comparison_plot <- function(region_output, global_output, raw_data, climate_var, x_label) {
  # Prepare regional data
  reg_data <- region_output %>%
    filter(climate_variable == climate_var) %>%
    select(trait_trans, is_significant, predictions) %>%
    unnest(predictions)

  # Prepare global data
  glob_data <- global_output %>%
    filter(climate_variable == climate_var) %>%
    select(trait_trans, is_significant, predictions) %>%
    unnest(predictions)

  # Prepare raw points
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

# Ridgeline plot to show trait distributions by region and elevation
make_trait_ridgeline_plot <- function(data) {
  data |>
    # Add fancy names
    fancy_trait_name_dictionary() |>
    # Bin elevation into 500m increments for cleaner ridgelines
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
      x = "Log Transformed Trait Value",
      y = ""
    ) +
    theme_bw() +
    theme(
      legend.position = "top",
      axis.title = element_text(size = 8)
    )
}
