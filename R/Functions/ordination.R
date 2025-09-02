# ORDINATIONS

## TRAIT (PCA)
make_trait_pca <- function(trait_mean){

  set.seed(32)

  # make wide trait table
  cwm_fat <- trait_mean %>%
    # remove nutrient ratio traits, because correlated with c, n, and p content
    filter(!trait_trans %in% c("cn_ratio", "np_ratio")) |>
    group_by(country, ecosystem) %>%
    mutate(annual_temperature = mean(annual_temperature)) %>%
    select(country:mean, annual_temperature) %>%
    pivot_wider(names_from = "trait_trans", values_from = "mean") %>%
    filter(!is.na(plant_height_cm_log)) |>
    ungroup()

  pca_output <- cwm_fat %>%
    select(-(country:annual_temperature)) %>%
    rda(scale = TRUE, center = TRUE)

  pca_sites <- bind_cols(
    cwm_fat %>%
      select(country:annual_temperature),
    fortify(pca_output, display = "sites")
  )

  # arrows
  pca_traits <- fortify(pca_output, display = "species") %>%
    mutate(trait_trans = label) %>%
    fancy_trait_name_dictionary() |>
    # mutate(figure_names = str_remove(figure_names, "Size~-~|LES~-~|I~-~"),
    #        figure_names = str_extract(figure_names, "[^~]+"),
    #        figure_names = recode(figure_names,
    #                                  "δ^{13}" = "δ^{13}~C",
    #                                  "δ^{15}" = "δ^{15}~N")) |>
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

  trait_pca[[1]] %>% 
    ggplot(aes(x = PC1, y = PC2, colour = country)) +
    geom_point(size = 2) +
    coord_equal() +
    stat_ellipse(aes(group = ecosystem)) +
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
    scale_colour_viridis_d(end = 0.8, option = "plasma") +
    scale_linetype_manual(name = "", values = c("solid", "dashed", "dotted")) +
    labs(x = glue("PCA1 ({round(e_B[1] * 100, 1)}%)"),
         y = glue("PCA2 ({round(e_B[2] * 100, 1)}%)")) +
    theme_bw()

}

## BIOCLIM (PCA)
make_bioclim_pca <- function(bioclim){

  set.seed(32)

  # run PCA on bioclim variables only
  bioclim_vars <- bioclim |>
    select(-(country:latitude_n))

  pca_output <- bioclim_vars %>%
    rda(scale = TRUE, center = TRUE)

  # extract site scores
  pca_sites <- bind_cols(
    bioclim %>%
      select(country:latitude_n),
    fortify(pca_output, display = "sites")
  )

  # extract variable scores (arrows)
  pca_vars <- fortify(pca_output, display = "species") %>%
    mutate(variable = label) %>%
    # create variable categories for plotting
    mutate(
      var_category = case_when(
        str_detect(variable, "temperature|temp") ~ "Temperature",
        str_detect(variable, "precipitation|precip") ~ "Precipitation",
        TRUE ~ "Other"
      ),
      var_category = factor(var_category, levels = c("Temperature", "Precipitation", "Other"))
    )

  outputList <- list(pca_sites, pca_vars, pca_output)

  return(outputList)
}

make_bioclim_pca_plot <- function(bioclim_pca){

  # eigenvalues
  e_B <- eigenvals(bioclim_pca[[3]])/sum(eigenvals(bioclim_pca[[3]]))

  bioclim_pca[[1]] %>% 
    ggplot(aes(x = PC1, y = PC2, colour = country)) +
    geom_point(size = 2) +
    coord_equal() +
    stat_ellipse(aes(group = country)) +
    geom_segment(data = bioclim_pca[[2]],
                 aes(x = 0, y = 0, xend = PC1, yend = PC2, linetype = var_category),
                 colour = "grey40",
                 arrow = arrow(length = unit(0.2, "cm")),
                 inherit.aes = FALSE) +
    geom_text(data = bioclim_pca[[2]],
              aes(x = PC1 + 0.1, y = PC2 + 0.1, label = variable),
              size = 2.5,
              inherit.aes = FALSE,
              show.legend = FALSE) +
    scale_colour_viridis_d(end = 0.8, option = "plasma") +
    scale_linetype_manual(name = "", values = c("solid", "dashed", "dotted")) +
    labs(x = glue("PCA1 ({round(e_B[1] * 100, 1)}%)"),
         y = glue("PCA2 ({round(e_B[2] * 100, 1)}%)")) +
    theme_bw()

}

## BIOCLIM CORRELATION PLOT
make_bioclim_correlation_plot <- function(bioclim){

  # prepare bioclim data for correlation analysis
  bioclim_corr_data <- bioclim |>
    select(-(country:latitude_n))

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
  ggplot(cor_long, aes(x = var1, y = var2, fill = correlation)) +
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
