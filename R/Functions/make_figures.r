### Figures

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
        str_detect(var2, "temperature|temp") ~ "Temperature",
        str_detect(var2, "precipitation|precip") ~ "Precipitation",
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