# ORDINATIONS

## TRAIT (PCA)
make_trait_pca <- function(trait_mean){

  set.seed(32)

  # make wide trait table
  cwm_fat <- trait_mean %>%
    # remove nutrient ratio traits, also height, because china has no height
    filter(!trait_trans %in% c("cn_ratio", "np_ratio", "plant_height_cm_log")) |>
    # norway needs to be removed for now because no chem traits
    filter(country != "no") |> 
    group_by(country, ecosystem) %>%
    mutate(annual_temperature = mean(annual_temperature)) %>%
    select(country:mean, annual_temperature) %>%
    pivot_wider(names_from = "trait_trans", values_from = "mean") %>%
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
    ggplot(aes(x = PC1, y = PC2, colour = annual_temperature)) +
    geom_point(size = 2) +
    coord_equal() +
    stat_ellipse(aes(group = ecosystem)) +
    geom_segment(data = trait_pca[[2]],
                 aes(x = 0, y = 0, xend = PC1, yend = PC2, linetype = class),
                 colour = "grey40",
                 arrow = arrow(length = unit(0.2, "cm")),
                 inherit.aes = FALSE) +
    geom_text(data = trait_pca[[2]] |> 
                mutate(figure_names = str_remove(figure_names, "Size~-~|LES~-~|I~-~")) |>
                mutate(PC1 = case_when(label == "thickness_mm_log" ~ -1.2,
                                       TRUE ~ PC1),
                       PC2 = case_when(label == "thickness_mm_log" ~ -0.6,
                                       label == "c_percent" ~ -1.2,
                                       TRUE ~ PC2)),
              aes(x = PC1 + 0.1, y = PC2 + 0.1, label = figure_names),
              size = 2.5,
              inherit.aes = FALSE,
              show.legend = FALSE, parse = TRUE) +
    scale_colour_viridis_c(end = 0.8, option = "plasma", name = "Mean annual temperature") +
    scale_linetype_manual(name = "", values = c("solid", "dashed", "dotted")) +
    labs(x = glue("PCA1 ({round(e_B[1] * 100, 1)}%)"),
         y = glue("PCA2 ({round(e_B[2] * 100, 1)}%)")) +
    theme_bw()

}
