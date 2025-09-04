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
    predictor == "lat" ~ "latitude_n",
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
    # Add prediction lines (single line across all ecosystems)
    geom_line(aes(y = .fixed, group = 1), size = 1, color = "grey40") +
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
      legend.position = "right",  # Show legend since region is now the color
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
