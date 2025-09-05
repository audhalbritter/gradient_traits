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
  # Download/cache WorldClim elevation data (10 arc-minutes resolution) to project cache
  cache_path <- file.path("WorldClimData")
  if (!dir.exists(cache_path)) dir.create(cache_path, recursive = TRUE)
  elev_raster <- geodata::worldclim_global(var = "elev", res = 10, path = cache_path)
  
  # Convert raster to data frame for ggplot
  elev_df <- as.data.frame(elev_raster, xy = TRUE, na.rm = TRUE)
  names(elev_df) <- c("lon", "lat", "elev")
  
  # World polygons
  world <- ggplot2::map_data("world")

  # Region order/colors
  coords <- coords |>
    dplyr::mutate(region = factor(region,
      levels = c("Svalbard","Southern Scandes","Rocky Mountains",
                 "Eastern Himalaya","Central Andes","Drakensberg")
    ))

  ggplot2::ggplot() +
    # Elevation background (as raster)
    ggplot2::geom_raster(data = elev_df, ggplot2::aes(lon, lat, fill = elev)) +
    ggplot2::scale_fill_gradientn(
      colors = c("grey20", "grey40", "grey60", "grey80", "white"),
      name = "Elevation (m)"
    ) +
    # Land polygons
    ggplot2::geom_polygon(
      data = world, ggplot2::aes(long, lat, group = group),
      fill = NA, color = "grey70", linewidth = 0.2
    ) +
    # Region points
    ggplot2::geom_point(
      data = dplyr::distinct(coords, region, site, longitude_e, latitude_n),
      ggplot2::aes(x = longitude_e, y = latitude_n, color = region),
      alpha = 0.9, size = 3
    ) +
    ggplot2::scale_color_manual(values = create_region_color_mapping(), drop = FALSE) +
    ggplot2::coord_quickmap() +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid = ggplot2::element_blank()) +
    ggplot2::labs(x = "Longitude", y = "Latitude", color = "Region")
}

## BIOCLIM CORRELATION PLOT
make_bioclim_correlation_plot <- function(bioclim){

  # prepare bioclim data for correlation analysis
  bioclim_corr_data <- bioclim |>
    select(-(country:ID))

  # calculate correlation matrix
  cor_matrix <- cor(bioclim_corr_data, use = "pairwise.complete.obs")

  # convert to long format for plotting
  cor_long <- as.data.frame(cor_matrix) |>
    rownames_to_column("var1") |>
    pivot_longer(cols = -var1, names_to = "var2", values_to = "correlation") |>
    # create variable categories for coloring
    mutate(
      var1_category = case_when(
        str_detect(var1, "temperature|temp") ~ "Temperature",
        str_detect(var1, "precipitation|precip") ~ "Precipitation",
        TRUE ~ "Other"
      ),
      var2_category = case_when(
        str_detect(var2, "temperature|temp") ~ "Precipitation",
        TRUE ~ "Other"
      ),
      # create combined category for coloring
      combined_category = case_when(
        var1_category == var2_category ~ var1_category,
        TRUE ~ "Mixed"
      ),
      combined_category = factor(combined_category, 
                               levels = c("Temperature", "Precipitation", "Mixed", "Other"))
    )

  # create correlation plot
  output <- ggplot(cor_long, aes(x = var1, y = var2, fill = correlation)) +
    geom_tile() +
    # add correlation values as text
    geom_text(aes(label = round(correlation, 2)), 
              size = 2.5, 
              color = ifelse(abs(cor_long$correlation) > 0.7, "white", "black")) +
    # color scale
    scale_fill_gradient2(
      low = "#d73027", 
      mid = "white", 
      high = "#1f78b4",
      midpoint = 0,
      limits = c(-1, 1),
      name = "Correlation"
    ) +
    # theme
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      axis.text.y = element_text(size = 8),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      legend.position = "right"
    ) +
    labs(title = "Bioclimatic Variables Correlation Matrix")

}

## DIVERSITY VS PREDICTOR PLOT
make_diversity_predictor_plot <- function(data, predictor, x_label) {
  
  # Map bioclim identifier to actual column name
  predictor_col <- case_when(
    predictor == "elev" ~ "elevation_m",
    predictor == "anntemp" ~ "annual_temperature",
    predictor == "temprange" ~ "temperature_annual_range",
    TRUE ~ predictor
  )
  
  # Check if predictor column exists
  if (!predictor_col %in% names(data)) {
    stop("Predictor column '", predictor_col, "' not found in data")
  }
  
  # Filter data for the specified predictor (bioclim identifier)
  filtered_data <- data |>
    filter(bioclim == predictor) |>
    # Ensure region is ordered consistently (north to south)
    mutate(region = factor(region, levels = c("Svalbard", "Southern Scandes", "Rocky Mountains", 
                                             "Eastern Himalaya", "Central Andes", "Drakensberg")))
  
  
  # Check if we have any data after filtering
  if (nrow(filtered_data) == 0) {
    stop("No data found after filtering for bioclim = '", predictor, "'")
  }
  
  # Check if diversity_index has values
  if (length(unique(filtered_data$diversity_index)) == 0) {
    stop("No diversity_index values found in filtered data")
  }
  
  # Create the plot using modern ggplot2 syntax
  ggplot(filtered_data, aes(x = .data[[predictor_col]], y = value, color = region)) +
    # Add points for each plot
    geom_point(alpha = 0.6, size = 2) +
    # Add prediction lines - different approach for interaction models
    {if (predictor == "elev") {
      # For elevation model (with interaction), show separate lines per region
      geom_line(aes(y = .fixed, group = region, color = region), size = 1, alpha = 0.8)
    } else {
      # For other models (no interaction), show single overall line
      geom_line(aes(y = .fixed, group = 1), size = 1, color = "grey40")
    }} +
    # Add confidence intervals (if they exist)
    {if (all(c(".conf.low", ".conf.high") %in% names(filtered_data))) {
      geom_ribbon(aes(ymin = .conf.low, ymax = .conf.high, fill = region), 
                  alpha = 0.2, color = NA)
    } else {
      NULL
    }} +
    # Use consistent color palette based on latitude
    scale_color_manual(values = create_region_color_mapping()) +
    scale_fill_manual(values = create_region_color_mapping()) +
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
      x = x_label,
      y = "Diversity Index Value",
      color = "Region"  # Update legend title
    )
}

## TRAIT VS PREDICTOR PLOT
make_trait_predictor_plot <- function(data, predictor, x_label) {
  
  # Map bioclim identifier to actual column name
  predictor_col <- case_when(
    predictor == "elev" ~ "elevation_m",
    predictor == "anntemp" ~ "annual_temperature",
    predictor == "temprange" ~ "temperature_annual_range",
    TRUE ~ predictor
  )
  
  # Check if predictor column exists
  if (!predictor_col %in% names(data)) {
    stop("Predictor column '", predictor_col, "' not found in data")
  }
  
  # Filter data for the specified predictor (bioclim identifier)
  filtered_data <- data |>
    filter(bioclim == predictor) |>
    # Ensure region is ordered consistently (north to south)
    mutate(region = factor(region, levels = c("Svalbard", "Southern Scandes", "Rocky Mountains", 
                                             "Eastern Himalaya", "Central Andes", "Drakensberg")))
  
  
  # Check if we have any data after filtering
  if (nrow(filtered_data) == 0) {
    stop("No data found after filtering for bioclim = '", predictor, "'")
  }
  
  # Check if trait_trans has values
  if (length(unique(filtered_data$trait_trans)) == 0) {
    stop("No trait_trans values found in filtered data")
  }
  
  # Create the plot using modern ggplot2 syntax
  ggplot(filtered_data, aes(x = .data[[predictor_col]], y = mean, color = region)) +
    # Add points for each plot
    geom_point(alpha = 0.6, size = 2) +
    # Add prediction lines - different approach for interaction models
    {if (predictor == "elev") {
      # For elevation model (with interaction), show separate lines per region
      geom_line(aes(y = .fixed, group = region, color = region), size = 1, alpha = 0.8)
    } else {
      # For other models (no interaction), show single overall line
      geom_line(aes(y = .fixed, group = 1), size = 1, color = "grey40")
    }} +
    # Add confidence intervals (if they exist)
    {if (all(c(".conf.low", ".conf.high") %in% names(filtered_data))) {
      geom_ribbon(aes(ymin = .conf.low, ymax = .conf.high, fill = region), 
                  alpha = 0.2, color = NA)
    } else {
      NULL
    }} +
    # Use consistent color palette based on latitude
    scale_color_manual(values = create_region_color_mapping()) +
    scale_fill_manual(values = create_region_color_mapping()) +
    # Facet by trait using figure_names for labels
    facet_wrap(~figure_names, scales = "free_y", labeller = label_parsed) +
    # Theme
    theme_bw() +
    theme(
      legend.position = "top",  # Move legend to top
      legend.box = "horizontal",  # Split legend across multiple rows
      strip.text = element_text(size = 10, face = "bold"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10)
    ) +
    labs(
      x = x_label,
      y = "Trait Value",
      color = "Region"  # Update legend title
    )
}
