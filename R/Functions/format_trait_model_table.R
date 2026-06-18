## Format trait–climate mixed-model output for gt tables in results.qmd.

#' Build a tidy table of climate fixed effects from trait model output.
#'
#' Keeps only climate terms (linear and squared), labels model scope and type,
#' and drops regional dummy coefficients from regional models.
#'
#' @param model_output `trait_models_output` or `trait_models_region_output`.
#' @param model_scope Label shown in the table (`"Global"` or `"Regional"`).
format_trait_climate_model_table <- function(model_output, model_scope) {
  model_output |>
    dplyr::select(trait_trans, climate_variable, model_type, tidy_results) |>
    tidyr::unnest(tidy_results) |>
    dplyr::filter(
      effect == "fixed",
      term != "(Intercept)",
      grepl("climate_value", term)
    ) |>
    dplyr::mutate(
      model_scope = model_scope,
      model_type_clean = dplyr::if_else(model_type == "poly", "Polynomial", "Linear"),
      term_clean = dplyr::case_when(
        term == "climate_value" ~ "Climate (linear)",
        grepl("\\^2", term) ~ "Climate²",
        TRUE ~ term
      ),
      trait_clean = dplyr::case_when(
        trait_trans == "plant_height_cm_log" ~ "Height cm",
        trait_trans == "dry_mass_g_log" ~ "Dry mass g",
        trait_trans == "leaf_area_cm2_log" ~ "Area cm²",
        trait_trans == "thickness_mm_log" ~ "Thickness mm",
        trait_trans == "ldmc" ~ "LDMC",
        trait_trans == "sla_cm2_g" ~ "SLA cm²/g",
        trait_trans == "c_percent" ~ "C %",
        trait_trans == "n_percent" ~ "N %",
        TRUE ~ stringr::str_replace_all(trait_trans, "_", " ") |>
          stringr::str_to_title()
      ),
      predictor_clean = dplyr::case_when(
        climate_variable == "gs_length" ~ "Growing season length (days)",
        climate_variable == "gs_temperature" ~ "Growing season temperature (°C)",
        climate_variable == "gs_vpd" ~ "Growing season VPD",
        climate_variable == "gdd" ~ "Growing degree days (>5°C)",
        climate_variable == "gs_diurnal_range" ~ "Diurnal range (°C)",
        TRUE ~ climate_variable
      ),
      estimate = round(estimate, 3),
      std.error = round(std.error, 3),
      statistic = round(statistic, 2),
      p.value = round(p.value, 3),
      is_significant = p.value < 0.05
    ) |>
    dplyr::select(
      predictor_clean, model_scope, model_type_clean, trait_clean, term_clean,
      estimate, std.error, statistic, df, p.value, is_significant
    )
}

#' Build a tidy table of climate fixed effects from PCA axis model output.
#'
#' @param model_output `trait_pca_full_climate_models_output` or regional equivalent.
#' @param model_scope Label shown in the table (`"Global"` or `"Regional"`).
#' @param variance_explained Named numeric vector from `pca_variance_explained()`.
format_pca_climate_model_table <- function(model_output, model_scope, variance_explained) {
  model_output |>
    dplyr::select(pc_axis, climate_variable, model_type, tidy_results) |>
    tidyr::unnest(tidy_results) |>
    dplyr::filter(
      effect == "fixed",
      term != "(Intercept)",
      grepl("climate_value", term)
    ) |>
    dplyr::mutate(
      model_scope = model_scope,
      model_type_clean = dplyr::if_else(model_type == "poly", "Polynomial", "Linear"),
      term_clean = dplyr::case_when(
        term == "climate_value" ~ "Climate (linear)",
        grepl("\\^2", term) ~ "Climate²",
        TRUE ~ term
      ),
      trait_clean = paste0(
        pc_axis,
        " (",
        round(variance_explained[as.character(pc_axis)], 1),
        "% variance)"
      ),
      predictor_clean = dplyr::case_when(
        climate_variable == "gs_length" ~ "Growing season length (days)",
        climate_variable == "gs_temperature" ~ "Growing season temperature (°C)",
        climate_variable == "gs_vpd" ~ "Growing season VPD",
        climate_variable == "gdd" ~ "Growing degree days (>5°C)",
        climate_variable == "gs_diurnal_range" ~ "Diurnal range (°C)",
        TRUE ~ climate_variable
      ),
      estimate = round(estimate, 3),
      std.error = round(std.error, 3),
      statistic = round(statistic, 2),
      p.value = round(p.value, 3),
      is_significant = p.value < 0.05
    ) |>
    dplyr::select(
      predictor_clean, model_scope, model_type_clean, trait_clean, term_clean,
      estimate, std.error, statistic, df, p.value, is_significant
    )
}
