### Figures

## REGION COLOR MAPPING
# Function to create consistent color mapping for regions sorted by latitude (north to south)
create_region_color_mapping <- function() {
  # Define regions in order from north to south by latitude
  regions_ordered <- c("Svalbard", "Southern Scandes", "Rocky Mountains", 
                       "Eastern Himalaya", "Central Andes", "Drakensberg")
  
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
        levels = c("Svalbard","Southern Scandes","Rocky Mountains",
                   "Eastern Himalaya","Central Andes","Drakensberg")
      )
    )

  ggplot2::ggplot() +
    # Elevation background (as raster)
    ggplot2::geom_raster(data = elev_df, ggplot2::aes(lon, lat, fill = elev)) +
    ggplot2::scale_fill_gradientn(colors = c("grey40","grey50","grey60","grey70","white"), name = "Elevation (m)") +
    # Land outlines
    ggplot2::geom_polygon(data = world, ggplot2::aes(long, lat, group = group), 
                          fill = NA, color = "grey70", linewidth = 0.2) +
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
    mutate(region = factor(region, levels = c("Svalbard", "Southern Scandes", "Rocky Mountains", 
                                             "Eastern Himalaya", "Central Andes", "Drakensberg")))
  
  # Check if diversity_index has values
  if (length(unique(plot_data$diversity_index)) == 0) {
    stop("No diversity_index values found in data")
  }
  
  # Create the plot using modern ggplot2 syntax
  ggplot(plot_data, aes(x = elevation_km, y = value, color = region)) +
    # Add points for each plot
    geom_point(alpha = 0.6, size = 2) +
    # Add prediction line from lmer model with different line types based on significance
    geom_line(aes(x = elevation_km, y = .fitted, linetype = is_significant), 
              linewidth = 1, color = "grey40", show.legend = FALSE) +
    # Add confidence intervals
    geom_ribbon(aes(x = elevation_km, ymin = plo, ymax = phi), 
                alpha = 0.2, color = NA, fill = "grey40") +
    # Use consistent color palette based on latitude
    scale_color_manual(values = create_region_color_mapping()) +
    # Set line types: solid for significant, dashed for non-significant (no legend)
    scale_linetype_manual(values = c("FALSE" = "dashed", "TRUE" = "solid"),
                         guide = "none") +
    # Facet by diversity index
    facet_wrap(~diversity_index, scales = "free_y", labeller = label_value) +
    # Theme
    theme_bw() +
    theme(
      legend.position = "top",  # Move legend to top
      legend.box = "horizontal",  # Split legend across multiple rows
      strip.text = element_text(size = 12, face = "bold"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10)
    ) +
    labs(
      x = "Elevation (km)",
      y = "Diversity Index Value",
      color = "Region"  # Update legend title
    )
}

## TRAIT VS PREDICTOR PLOT
make_trait_predictor_plot <- function(data, predictor, x_label) {
  
  # Map bioclim identifier to actual column name
  predictor_col <- case_when(
    predictor == "lat" ~ "latitude_n",
    predictor == "elev" ~ "elevation_m",
    predictor == "gsl" ~ "growing_season_length",
    predictor == "gst" ~ "growing_season_temperature",
    predictor == "pet" ~ "potential_evapotranspiration",
    predictor == "diurnal" ~ "mean_diurnal_range_chelsa",
    TRUE ~ predictor
  )
  
  # Unnest combined data and filter for this predictor
  filtered_data <- data |>
    unnest(data_with_predictions) |>
    # Prefer filtering by mapped predictor column name to avoid label mismatches
    filter(predictor == predictor_col) |>
    # Ensure region is ordered consistently (north to south)
    mutate(region = factor(region, levels = c("Svalbard", "Southern Scandes", "Rocky Mountains", 
                                             "Eastern Himalaya", "Central Andes", "Drakensberg")))

  # Basic checks
  if (!predictor_col %in% names(filtered_data)) {
    stop("Predictor column '", predictor_col, "' not found in combined data")
  }
  if (nrow(filtered_data) == 0) {
    stop("No data found after filtering for bioclim = '", predictor, "'")
  }

  # Plot with prediction line and ribbon from precomputed columns
  ggplot(filtered_data, aes(x = .data[[predictor_col]], y = mean, color = region)) +
    geom_point(alpha = 0.6, size = 2) +
    # Add prediction line with different line types based on significance
    geom_line(aes(y = .fitted, linetype = is_significant), linewidth = 1, color = "grey40", show.legend = FALSE) +
    geom_ribbon(aes(ymin = plo, ymax = phi), alpha = 0.2, color = NA, fill = "grey40") +
    scale_color_manual(values = create_region_color_mapping()) +
    # Set line types: solid for significant, dashed for non-significant (no legend)
    scale_linetype_manual(values = c("FALSE" = "dashed", "TRUE" = "solid"),
                         guide = "none") +
    facet_wrap(~figure_names, scales = "free_y", labeller = label_parsed) +
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
