# Díaz Global Spectrum species means vs PFTC measured species means

# Height excluded so China (Eastern Himalaya) can enter the complete-case set
diaz_comparison_traits <- function() {
  c(
    "leaf_area_cm2_log",
    "sla_cm2_g",
    "ldmc",
    "n_percent"
  )
}

#' Read and harmonize Díaz Species_mean_traits.xlsx to PFTC trait_trans scales.
#'
#' Units in the file (TRY.81 / Scientific Data 2022):
#' leaf area mm2, Nmass mg/g, LMA g/m2, plant height m, LDMC g/g.
harmonize_diaz_traits <- function(diaz_file) {
  raw <- readxl::read_excel(diaz_file) |>
    janitor::clean_names()

  # Expected cleaned names from Species_mean_traits.xlsx
  name_map <- c(
    taxon = "species_name_standardized_against_tpl",
    leaf_area_mm2 = "leaf_area_mm2",
    nmass_mg_g = "nmass_mg_g",
    lma_g_m2 = "lma_g_m2",
    ldmc = "ldmc_g_g"
  )

  missing <- setdiff(unname(name_map), names(raw))
  if (length(missing) > 0) {
    stop(
      "Unexpected Díaz column names after clean_names(). Missing: ",
      paste(missing, collapse = ", "),
      "\nAvailable: ", paste(names(raw), collapse = ", ")
    )
  }

  raw |>
    dplyr::transmute(
      source = "diaz",
      taxon = tolower(trimws(.data[[name_map[["taxon"]]]])),
      leaf_area_cm2 = .data[[name_map[["leaf_area_mm2"]]]] / 100,
      # LMA g m^-2 -> SLA cm^2 g^-1
      sla_cm2_g = 10000 / .data[[name_map[["lma_g_m2"]]]],
      ldmc = .data[[name_map[["ldmc"]]]],
      # mg/g -> %
      n_percent = .data[[name_map[["nmass_mg_g"]]]] / 10
    ) |>
    dplyr::filter(!is.na(taxon), taxon != "") |>
    tidyr::pivot_longer(
      cols = c(leaf_area_cm2, sla_cm2_g, ldmc, n_percent),
      names_to = "trait",
      values_to = "value"
    ) |>
    dplyr::filter(!is.na(value), value > 0) |>
    dplyr::mutate(
      value_trans = dplyr::if_else(
        trait == "leaf_area_cm2",
        log(value),
        value
      ),
      trait_trans = dplyr::recode(
        trait,
        leaf_area_cm2 = "leaf_area_cm2_log"
      ),
      trait_trans = factor(trait_trans, levels = diaz_comparison_traits())
    ) |>
    dplyr::select(source, taxon, trait_trans, value_trans)
}

#' Species x region means from PFTC individual measurements (not CWMs).
pftc_species_means_for_diaz <- function(traits) {
  traits |>
    dplyr::filter(trait_trans %in% diaz_comparison_traits()) |>
    dplyr::filter(!is.na(value_trans), !is.na(taxon), taxon != "") |>
    dplyr::mutate(taxon = tolower(trimws(as.character(taxon)))) |>
    dplyr::group_by(country, region, taxon, trait_trans) |>
    dplyr::summarise(
      value_trans = mean(value_trans, na.rm = TRUE),
      .groups = "drop"
    )
}

#' Wide complete-case matrix for the Díaz comparison traits.
diaz_trait_wide <- function(long_dat, id_cols) {
  trait_levels <- diaz_comparison_traits()

  wide <- long_dat |>
    dplyr::mutate(trait_trans = as.character(trait_trans)) |>
    tidyr::pivot_wider(
      id_cols = tidyselect::all_of(id_cols),
      names_from = trait_trans,
      values_from = value_trans
    )

  trait_cols <- intersect(trait_levels, names(wide))
  complete <- stats::complete.cases(wide[, trait_cols, drop = FALSE])

  list(
    wide = wide[complete, , drop = FALSE],
    trait_cols = trait_cols,
    n_complete = sum(complete),
    n_total = nrow(wide)
  )
}

#' PCA on Díaz species means; project PFTC species means into that space.
#'
#' Uses prcomp so newdata projection matches the fitted global axes exactly.
make_diaz_pftc_pca <- function(diaz_traits_long, pftc_species_means) {
  diaz_w <- diaz_trait_wide(diaz_traits_long, id_cols = c("source", "taxon"))
  if (diaz_w$n_complete < 3) {
    stop(
      "Not enough Díaz complete cases for PCA (need >= 3, have ",
      diaz_w$n_complete, " of ", diaz_w$n_total, ")"
    )
  }

  trait_cols <- diaz_w$trait_cols
  diaz_mat <- as.data.frame(diaz_w$wide[, trait_cols, drop = FALSE])

  pca <- stats::prcomp(diaz_mat, center = TRUE, scale. = TRUE)

  # Flip PC1 so positive = faster leaf economics (SLA, N), matching trait_pca_full
  diaz_scores <- dplyr::bind_cols(
    diaz_w$wide |> dplyr::select(source, taxon),
    tibble::tibble(PC1 = -pca$x[, 1], PC2 = pca$x[, 2])
  )

  arrows <- tibble::tibble(
    PC1 = -pca$rotation[, 1],
    PC2 = pca$rotation[, 2],
    label = trait_cols,
    trait_trans = trait_cols
  ) |>
    fancy_trait_name_dictionary() |>
    dplyr::mutate(
      class = as.character(class),
      class = factor(class, levels = c("Size", "Leaf economics", "Isotopes"))
    )

  # Scale arrow lengths for display (same idea as a biplot)
  arrow_scale <- 1
  if (nrow(diaz_scores) > 0) {
    site_range <- max(abs(c(diaz_scores$PC1, diaz_scores$PC2)), na.rm = TRUE)
    rot_range <- max(abs(c(arrows$PC1, arrows$PC2)), na.rm = TRUE)
    if (is.finite(site_range) && is.finite(rot_range) && rot_range > 0) {
      arrow_scale <- 0.7 * site_range / rot_range
    }
  }
  arrows <- arrows |>
    dplyr::mutate(
      PC1 = PC1 * arrow_scale,
      PC2 = PC2 * arrow_scale
    )

  pftc_w <- diaz_trait_wide(
    pftc_species_means,
    id_cols = c("country", "region", "taxon")
  )

  pftc_proj <- NULL
  if (pftc_w$n_complete > 0) {
    pftc_mat <- as.data.frame(pftc_w$wide[, trait_cols, drop = FALSE])
    proj <- stats::predict(pca, newdata = pftc_mat)

    pftc_proj <- dplyr::bind_cols(
      pftc_w$wide |> dplyr::select(country, region, taxon),
      tibble::tibble(PC1 = -proj[, 1], PC2 = proj[, 2])
    ) |>
      dplyr::mutate(
        region = factor(
          region,
          levels = c(
            "Svalbard", "Southern Scandes", "Rocky Mountains",
            "Eastern Himalaya", "Central Andes", "Drakensberg"
          )
        )
      )
  }

  var_exp <- as.numeric(pca$sdev^2 / sum(pca$sdev^2) * 100)

  list(
    diaz_scores = diaz_scores,
    pftc_scores = pftc_proj,
    arrows = arrows,
    pca = pca,
    variance_pct = var_exp,
    n_diaz = diaz_w$n_complete,
    n_pftc = pftc_w$n_complete,
    trait_cols = trait_cols
  )
}

#' Overlay PFTC species means on Díaz global trait PCA.
#'
#' @param presentation If TRUE, enlarge axis labels, trait arrows, and
#'   interpretation arrows for slides.
make_diaz_pftc_pca_plot <- function(diaz_pca, presentation = FALSE) {
  var_exp <- diaz_pca$variance_pct
  region_levels <- c(
    "Svalbard", "Southern Scandes", "Rocky Mountains",
    "Eastern Himalaya", "Central Andes", "Drakensberg"
  )
  region_colors <- create_region_color_mapping()
  diaz_label <- "Díaz et al. 2016"
  colour_values <- c(stats::setNames("grey70", diaz_label), region_colors)
  colour_breaks <- c(diaz_label, region_levels)

  # Sizes: manuscript vs presentation
  if (presentation) {
    sz <- list(
      diaz_pt = 1.4,
      pftc_pt = 2.0,
      trait_arrow = 0.9,
      trait_arrow_head = 0.35,
      trait_label = 5,
      axis_arrow = 0.8,
      axis_arrow_head = 0.3,
      axis_label = 5.5,
      axis_title = 18,
      axis_text = 14,
      legend_text = 14,
      legend_key = 0.9
    )
  } else {
    sz <- list(
      diaz_pt = 1.1,
      pftc_pt = 1.5,
      trait_arrow = 0.5,
      trait_arrow_head = 0.2,
      trait_label = 2.8,
      axis_arrow = 0.45,
      axis_arrow_head = 0.18,
      axis_label = 3,
      axis_title = 11,
      axis_text = 10,
      legend_text = 10,
      legend_key = 0.5
    )
  }

  p <- ggplot2::ggplot() +
    ggplot2::geom_point(
      data = diaz_pca$diaz_scores,
      ggplot2::aes(x = PC1, y = PC2, colour = diaz_label),
      alpha = 0.3,
      size = sz$diaz_pt
    )

  if (!is.null(diaz_pca$pftc_scores) && nrow(diaz_pca$pftc_scores) > 0) {
    p <- p +
      ggplot2::geom_point(
        data = diaz_pca$pftc_scores,
        ggplot2::aes(x = PC1, y = PC2, colour = region),
        size = sz$pftc_pt,
        alpha = 0.85
      )
  }

  # Axis interpretation arrows (after PC1 flip: +PC1 = faster, +PC2 = larger leaves)
  score_x <- diaz_pca$diaz_scores$PC1
  score_y <- diaz_pca$diaz_scores$PC2
  if (!is.null(diaz_pca$pftc_scores) && nrow(diaz_pca$pftc_scores) > 0) {
    score_x <- c(score_x, diaz_pca$pftc_scores$PC1)
    score_y <- c(score_y, diaz_pca$pftc_scores$PC2)
  }
  x_max <- max(abs(score_x), na.rm = TRUE)
  y_max <- max(abs(score_y), na.rm = TRUE)
  y_min <- min(score_y, na.rm = TRUE)
  x_max_raw <- max(score_x, na.rm = TRUE)

  axis_arrows <- tibble::tibble(
    x = c(0.15 * x_max, x_max_raw * 0.92),
    y = c(y_min * 0.92, 0.15 * y_max),
    xend = c(0.85 * x_max, x_max_raw * 0.92),
    yend = c(y_min * 0.92, 0.85 * y_max),
    label = c("faster traits", "larger leaves"),
    lx = c(0.5 * x_max, x_max_raw * 0.92),
    ly = c(y_min * 0.92, 0.5 * y_max),
    angle = c(0, 90)
  )

  p +
    ggplot2::scale_colour_manual(
      name = NULL,
      values = colour_values,
      breaks = colour_breaks,
      drop = FALSE,
      guide = ggplot2::guide_legend(
        override.aes = list(
          size = c(sz$diaz_pt, rep(sz$pftc_pt, length(region_levels))),
          alpha = c(0.6, rep(0.85, length(region_levels)))
        )
      )
    ) +
    ggplot2::geom_segment(
      data = diaz_pca$arrows,
      ggplot2::aes(
        x = 0, y = 0, xend = PC1, yend = PC2, linetype = class
      ),
      colour = "grey25",
      arrow = ggplot2::arrow(length = grid::unit(sz$trait_arrow_head, "cm")),
      linewidth = sz$trait_arrow
    ) +
    ggplot2::geom_text(
      data = diaz_pca$arrows |>
        dplyr::mutate(
          figure_names = stringr::str_remove(figure_names, "Size~-~|LES~-~|I~-~")
        ),
      ggplot2::aes(x = PC1 * 1.12, y = PC2 * 1.12, label = figure_names),
      size = sz$trait_label,
      parse = TRUE,
      colour = "grey20"
    ) +
    ggplot2::geom_segment(
      data = axis_arrows,
      ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
      colour = "grey30",
      linewidth = sz$axis_arrow,
      arrow = ggplot2::arrow(length = grid::unit(sz$axis_arrow_head, "cm")),
      inherit.aes = FALSE
    ) +
    ggplot2::geom_text(
      data = axis_arrows,
      ggplot2::aes(x = lx, y = ly, label = label, angle = angle),
      colour = "grey30",
      size = sz$axis_label,
      vjust = -0.6,
      inherit.aes = FALSE
    ) +
    ggplot2::scale_linetype_manual(
      name = "",
      values = c("Size" = "solid", "Leaf economics" = "dashed", "Isotopes" = "dotted")
    ) +
    ggplot2::coord_equal() +
    ggplot2::labs(
      x = glue::glue("PC1 ({round(var_exp[1], 1)}%)"),
      y = glue::glue("PC2 ({round(var_exp[2], 1)}%)")
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "right",
      axis.title = ggplot2::element_text(size = sz$axis_title),
      axis.text = ggplot2::element_text(size = sz$axis_text),
      legend.text = ggplot2::element_text(size = sz$legend_text),
      legend.key.size = grid::unit(sz$legend_key, "cm")
    )
}
