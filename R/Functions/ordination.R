# ORDINATIONS

## TRAIT (PCA)
make_trait_pca <- function(trait_mean){

  set.seed(32)

  # Filter trait data and pivot to wide format
  cwm_fat <- trait_mean %>%
    filter(!trait_trans %in% c("cn_ratio", "np_ratio")) %>%
    select(country:mean) %>%
    pivot_wider(names_from = "trait_trans", values_from = "mean") %>%
    ungroup()
  
  # Get trait columns (exclude metadata)
  trait_cols <- cwm_fat %>%
    select(-c(country, region, gradient, site, plot_id, elevation_m, latitude_n, longitude_e, ecosystem)) %>%
    names()
  
  # Check for missing values in trait columns only
  trait_data <- cwm_fat %>%
    select(all_of(trait_cols))
  
  # Remove rows with any missing trait values
  complete_rows <- complete.cases(trait_data)
  
  if (sum(complete_rows) < 3) {
    stop("Not enough complete cases for PCA (need at least 3, have ", sum(complete_rows), ")")
  }
  
  pca_output <- trait_data[complete_rows, ] %>%
    rda(scale = TRUE, center = TRUE)

  pca_sites <- bind_cols(
    cwm_fat[complete_rows, ] %>%
      select(country:ecosystem),
    fortify(pca_output, display = "sites")
  )

  # arrows
  pca_traits <- fortify(pca_output, display = "species") %>%
    mutate(trait_trans = label) %>%
    fancy_trait_name_dictionary() |>
    mutate(class = as.character(class),
           class = factor(class, levels = c("Size", "Leaf economics", "Isotopes", "Environment")))

  # # permutation test
  # # traits
  # raw <- cwm_fat %>% select(-(Gradient:SoilTemperature))
  # # meta data
  # meta <- cwm_fat %>% select(Gradient:SoilTemperature) %>%
  #   mutate(Site = factor(Site))
  # 
  # # adonis test
  #   adonis_result <- adonis2(raw ~ annual_temperature , data = meta, permutations = 999, method = "euclidean")

  outputList <- list(pca_sites, pca_traits, pca_output)

  return(outputList)
}



make_pca_plot <- function(trait_pca){

  # eigenvalues
  e_B <- eigenvals(trait_pca[[3]])/sum(eigenvals(trait_pca[[3]]))

  # Add elevation categories within each gradient
  # First, get unique sites with their elevation rankings
  site_elevation_categories <- trait_pca[[1]] %>%
    select(country, gradient, site, elevation_m) %>%
    distinct() %>%
    group_by(country, gradient) %>%
    mutate(
      elevation_rank = rank(elevation_m),
      n_sites = n(),
      elevation_category = case_when(
        elevation_rank == 1 ~ "Low",
        elevation_rank == n_sites ~ "High", 
        TRUE ~ "Middle"
      ),
      elevation_category = factor(elevation_category, levels = c("Low", "Middle", "High"))
    ) %>%
    ungroup() %>%
    select(country, gradient, site, elevation_category)
  
  # Join back to the full PCA data
  pca_sites_with_elevation <- trait_pca[[1]] %>%
    left_join(site_elevation_categories, by = c("country", "gradient", "site"))

  pca_sites_with_elevation %>% 
    ggplot(aes(x = PC1, y = PC2, colour = region, shape = elevation_category)) +
    geom_point(size = 2.5) +
    coord_equal() +
    stat_ellipse(aes(group = region)) +
    geom_segment(data = trait_pca[[2]],
                 aes(x = 0, y = 0, xend = PC1, yend = PC2, linetype = class),
                 colour = "grey40",
                 arrow = arrow(length = unit(0.2, "cm")),
                 inherit.aes = FALSE) +
    geom_text(data = trait_pca[[2]] |> 
                mutate(figure_names = str_remove(figure_names, "Size~-~|LES~-~|I~-~")) #|>
                # mutate(PC1 = case_when(label == "thickness_mm_log" ~ -1.2,
                #                        TRUE ~ PC1),
                #        PC2 = case_when(label == "thickness_mm_log" ~ -0.6,
                #                        label == "c_percent" ~ -1.2,
                                       #TRUE ~ PC2))
                                       ,
              aes(x = PC1 + 0.1, y = PC2 + 0.1, label = figure_names),
              size = 2.5,
              inherit.aes = FALSE,
              show.legend = FALSE, parse = TRUE) +
    scale_colour_manual(values = create_region_color_mapping(), drop = FALSE) +
    scale_shape_manual(name = "Elevation", 
                       values = c("Low" = 25, "Middle" = 21, "High" = 24), # down triangle, circle, up triangle
                       labels = c("Low" = "Lowest", "Middle" = "Middle", "High" = "Highest")) +
    scale_linetype_manual(name = "", values = c("solid", "dashed", "dotted")) +
    labs(x = glue("PCA1 ({round(e_B[1] * 100, 1)}%)"),
         y = glue("PCA2 ({round(e_B[2] * 100, 1)}%)"),
         colour = "Region") +
    theme_bw() +
    theme(legend.position = "right")

}