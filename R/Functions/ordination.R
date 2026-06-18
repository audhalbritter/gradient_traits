# ORDINATIONS

# ggplot2 4.x exports fortify(); ggvegan::fortify() then breaks on display = ...
fortify_rda <- function(x, display = c("sites", "species")) {
  display <- match.arg(display)
  out <- as.data.frame(vegan::scores(x, display = display))
  if (display == "species") {
    out$label <- rownames(out)
    rownames(out) <- NULL
  }
  tibble::as_tibble(out)
}

## TRAIT (PCA)
make_trait_pca <- function(trait_mean){

  set.seed(32)

  # Pivot to wide format (trait subsetting is done upstream in trait_plan)
  cwm_fat <- trait_mean %>%
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
    fortify_rda(pca_output, display = "sites")
  )

  # arrows
  pca_traits <- fortify_rda(pca_output, display = "species") %>%
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

pca_variance_explained <- function(trait_pca) {
  eig <- vegan::eigenvals(trait_pca[[3]])
  stats::setNames(eig / sum(eig) * 100, paste0("PC", seq_along(eig)))
}

prepare_pca_climate_long <- function(trait_pca_output, trait_mean, pc_axes = c("PC1", "PC2")) {
  climate_cols <- c("gs_length", "gs_temperature", "gs_vpd", "gdd", "gs_diurnal_range")
  climate_labels <- climate_variable_labels()

  climate_plot <- trait_mean |>
    dplyr::select(
      country, region, gradient, site, plot_id, elevation_m, latitude_n, longitude_e, ecosystem,
      dplyr::any_of(climate_cols)
    ) |>
    dplyr::distinct()

  trait_pca_output[[1]] |>
    dplyr::select(country:ecosystem, dplyr::any_of(pc_axes)) |>
    tidyr::pivot_longer(dplyr::any_of(pc_axes), names_to = "pc_axis", values_to = "trait_value") |>
    dplyr::left_join(
      climate_plot,
      by = dplyr::join_by(country, region, gradient, site, plot_id, elevation_m, latitude_n, longitude_e, ecosystem)
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::any_of(climate_cols),
      names_to = "climate_variable",
      values_to = "climate_value"
    ) |>
    dplyr::mutate(
      data_source = "Growing season",
      climate_variable_clean = dplyr::recode(climate_variable, !!!climate_labels)
    ) |>
    dplyr::filter(!is.na(climate_value)) |>
    center_climate_long(group_vars = "climate_variable") |>
    dplyr::mutate(pc_axis = factor(pc_axis, levels = pc_axes))
}

make_pca_scree_plot <- function(pca_named_list) {
  scree_dat <- purrr::imap_dfr(pca_named_list, function(pca, label) {
    eig <- vegan::eigenvals(pca[[3]])
    tibble::tibble(
      pca_label = label,
      axis_num = seq_along(eig),
      axis = paste0("PC", axis_num),
      variance = eig / sum(eig) * 100
    )
  })

  scree_dat |>
    ggplot2::ggplot(ggplot2::aes(x = axis_num, y = variance)) +
    ggplot2::geom_col(fill = "grey70", width = 0.7) +
    ggplot2::geom_line(ggplot2::aes(group = pca_label), linewidth = 0.4) +
    ggplot2::geom_point(size = 1.5) +
    ggplot2::facet_wrap(~pca_label, scales = "free_x") +
    ggplot2::scale_x_continuous(breaks = function(x) seq(min(x), max(x), by = 1)) +
    ggplot2::labs(
      x = "Principal component",
      y = "Variance explained (%)",
      title = "PCA scree plots"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(strip.text = ggplot2::element_text(face = "bold"))
}

make_pca_plot <- function(trait_pca){

  # eigenvalues
  e_B <- eigenvals(trait_pca[[3]])/sum(eigenvals(trait_pca[[3]]))

  # Add relative elevation within each region
  # First, average elevation by site, then calculate percentiles within region
  site_elevation_relative <- trait_pca[[1]] %>%
    select(country, region, gradient, site, elevation_m) %>%
    # Average elevation by site (in case there are multiple plots per site)
    group_by(country, region, gradient, site) %>%
    summarise(elevation_m = mean(elevation_m, na.rm = TRUE), .groups = "drop") %>%
    # Now calculate percentiles within region
    group_by(region) %>%
    mutate(
      # Calculate percentile rank within region (0-100)
      elevation_percentile = percent_rank(elevation_m) * 100,
      # Calculate standardized elevation within region (mean=0, sd=1)
      elevation_std = (elevation_m - mean(elevation_m)) / sd(elevation_m),
      # Calculate position within elevation range (0-1)
      elevation_range_pos = (elevation_m - min(elevation_m)) / (max(elevation_m) - min(elevation_m))
    ) %>%
    ungroup() %>%
    select(country, region, gradient, site, elevation_percentile, elevation_std, elevation_range_pos)
  
  # Join back to the full PCA data
  pca_sites_with_elevation <- trait_pca[[1]] %>%
    left_join(site_elevation_relative, by = c("country", "region", "gradient", "site"))

  pca_sites_with_elevation %>% 
    ggplot(aes(x = PC1, y = PC2, colour = region)) +
    geom_point(aes(size = elevation_percentile), alpha = 0.8) +
    coord_equal() +
    stat_ellipse(aes(group = region), alpha = 0.3) +
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
    scale_size_continuous(name = "Elevation\nPercentile", 
                          range = c(1, 4),
                          breaks = c(0, 25, 50, 75, 100),
                          labels = c("0%", "25%", "50%", "75%", "100%")) +
    scale_linetype_manual(name = "", values = c("solid", "dashed", "dotted")) +
    labs(x = glue("PCA1 ({round(e_B[1] * 100, 1)}%)"),
         y = glue("PCA2 ({round(e_B[2] * 100, 1)}%)"),
         colour = "Region") +
    theme_bw() +
    theme(legend.position = "right")

}