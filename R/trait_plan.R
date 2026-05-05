# Trait analysis

trait_plan <- list(
  # ordination reduced, all traits fewer countries
  tar_target(
    name = trait_pca,
    command = make_trait_pca(trait_mean |>
      filter(!country %in% c("no", "sa")) |>
      filter(!trait_trans %in% c("plant_height_cm_log")))
  ),

  # ordination with all countries, fewer traits
  tar_target(
    name = trait_pca_full,
    command = make_trait_pca(trait_mean |>
      filter(trait_trans %in% c("dry_mass_g_log", "leaf_area_cm2_log", "thickness_mm_log", "ldmc", "sla_cm2_g")))
  ),

  # trait data in long format with climate variables stacked
  tar_target(
    name = trait_mean_long,
    command = {
      trait_mean |>
        pivot_longer(
          cols = c(ds_t2m, ds_vpd),
          names_to = "climate_variable",
          values_to = "climate_value"
        ) |>
        mutate(
          data_source = "Downscaled",
          climate_variable_clean = case_when(
            climate_variable == "ds_t2m" ~ "Mean annual temperature",
            climate_variable == "ds_vpd" ~ "Vapour pressure deficit",
            TRUE ~ climate_variable
          )
        ) |>
        filter(!is.na(climate_value)) |>
        rename(trait_value = mean) |>
        select(
          country:ecosystem, elevation_m, latitude_n, longitude_e, trait_trans, trait_value,
          climate_variable, climate_variable_clean, climate_value, data_source
        )
    }
  ),

  # Regional trait models
  tar_target(
    name = trait_models_region_all,
    command = {
      trait_mean_long |>
        filter(trait_trans %in% trait_trans_mean_for_climate) |>
        group_by(trait_trans, climate_variable, data_source) |>
        nest() |>
        mutate(
          model_linear = purrr::map(data, ~ {
            safelmer <- purrr::safely(lmerTest::lmer)
            result <- safelmer(trait_value ~ climate_value + region + (1 | site), data = .x)
            result$result
          }),
          model_poly = purrr::map(data, ~ {
            safelmer <- purrr::safely(lmerTest::lmer)
            result <- safelmer(trait_value ~ (climate_value + I(climate_value^2)) + region + (1 | site), data = .x)
            result$result
          }),
          glance_linear = purrr::map(model_linear, ~ {
            safe_glance <- purrr::safely(broom.mixed::glance)
            result <- safe_glance(.x)
            result$result
          }),
          glance_poly = purrr::map(model_poly, ~ {
            safe_glance <- purrr::safely(broom.mixed::glance)
            result <- safe_glance(.x)
            result$result
          })
        ) |>
        tidyr::pivot_longer(
          cols = c(model_linear, model_poly, glance_linear, glance_poly),
          names_sep = "_",
          names_to = c(".value", "model_type")
        )
    }
  ),

  tar_target(
    name = trait_models_region_best,
    command = {
      trait_models_region_all |>
        unnest(glance) |>
        group_by(trait_trans, climate_variable, data_source) |>
        filter(AIC == min(AIC, na.rm = TRUE)) |>
        slice(1) |>
        select(-AIC) |>
        ungroup()
    }
  ),

  tar_target(
    name = trait_models_region_output,
    command = {
      trait_models_region_best |>
        mutate(
          tidy_results = purrr::map(model, ~ {
            safe_tidy <- purrr::safely(broom.mixed::tidy)
            result <- safe_tidy(.x)
            result$result
          }),
          is_significant = purrr::map_lgl(tidy_results, ~ {
            if (is.null(.x)) return(FALSE)
            any(.x$p.value[grepl("climate_value", .x$term) & .x$effect == "fixed"] < 0.05, na.rm = TRUE)
          }),
          predictions = purrr::map2(model, data, ~ lmer_prediction_smooth(fit = .x, dat = .y))
        )
    }
  ),

  # Global trait models
  tar_target(
    name = trait_models_all,
    command = {
      trait_mean_long |>
        filter(trait_trans %in% trait_trans_mean_for_climate) |>
        group_by(trait_trans, climate_variable, data_source) |>
        nest() |>
        mutate(
          model_linear = purrr::map(data, ~ {
            safelmer <- purrr::safely(lmerTest::lmer)
            result <- safelmer(trait_value ~ climate_value + (1 | site), data = .x)
            result$result
          }),
          model_poly = purrr::map(data, ~ {
            safelmer <- purrr::safely(lmerTest::lmer)
            result <- safelmer(trait_value ~ climate_value + I(climate_value^2) + (1 | site), data = .x)
            result$result
          }),
          glance_linear = purrr::map(model_linear, ~ {
            safe_glance <- purrr::safely(broom.mixed::glance)
            result <- safe_glance(.x)
            result$result
          }),
          glance_poly = purrr::map(model_poly, ~ {
            safe_glance <- purrr::safely(broom.mixed::glance)
            result <- safe_glance(.x)
            result$result
          })
        ) |>
        tidyr::pivot_longer(
          cols = c(model_linear, model_poly, glance_linear, glance_poly),
          names_sep = "_",
          names_to = c(".value", "model_type")
        )
    }
  ),

  tar_target(
    name = trait_models_best,
    command = {
      trait_models_all |>
        unnest(glance) |>
        group_by(trait_trans, climate_variable, data_source) |>
        filter(AIC == min(AIC, na.rm = TRUE)) |>
        slice(1) |>
        select(-AIC) |>
        ungroup()
    }
  ),

  tar_target(
    name = trait_models_output,
    command = {
      trait_models_best |>
        mutate(
          tidy_results = purrr::map(model, ~ {
            safe_tidy <- purrr::safely(broom.mixed::tidy)
            result <- safe_tidy(.x)
            result$result
          }),
          climate_pvalue = purrr::map_dbl(tidy_results, ~ {
            climate_row <- .x |> filter(term == "climate_value" & effect == "fixed")
            if (nrow(climate_row) > 0) climate_row$p.value else NA_real_
          }),
          is_significant = climate_pvalue < 0.05,
          predictions = purrr::map2(model, data, ~ {
            lmer_prediction_global_smooth(fit = .x, dat = .y)
          })
        )
    }
  ),

  tar_target(
    name = trait_model_checks,
    command = {
      trait_models_output |>
        rowwise() |>
        mutate(model_check = list(performance::check_model(model))) |>
        ungroup() |>
        filter(!is.null(model_check))
    }
  ),

  tar_target(
    name = trait_model_checks_region,
    command = {
      trait_models_region_output |>
        rowwise() |>
        mutate(model_check = list(performance::check_model(model))) |>
        ungroup() |>
        filter(!is.null(model_check))
    }
  ),

  # Trait variance vs climate
  tar_target(
    name = trait_variance_data,
    command = {
      trait_mean |>
        filter(!is.na(ds_t2m)) |>
        select(country:ecosystem, trait_trans, var, ds_t2m) |>
        mutate(trait_value = var) |>
        mutate(climate_value = ds_t2m) |>
        filter(trait_trans %in% trait_trans_mean_for_climate)
    }
  ),

  tar_target(
    name = trait_variance_model,
    command = {
      trait_variance_data |>
        group_by(trait_trans) |>
        nest() |>
        mutate(
          model = purrr::map(data, ~ {
            safelmer <- purrr::safely(lmerTest::lmer)
            result <- safelmer(trait_value ~ climate_value + (1 | site), data = .x)
            result$result
          }),
          tidy_results = purrr::map(model, ~ {
            safe_tidy <- purrr::safely(broom.mixed::tidy)
            result <- safe_tidy(.x)
            result$result
          }),
          climate_pvalue = purrr::map_dbl(tidy_results, ~ {
            if (!is.null(.x)) {
              climate_row <- .x |> filter(term == "climate_value" & effect == "fixed")
              if (nrow(climate_row) > 0) climate_row$p.value else NA_real_
            } else {
              NA_real_
            }
          }),
          is_significant = climate_pvalue < 0.05
        )
    }
  ),

  tar_target(
    name = trait_variance_model_checks,
    command = {
      trait_variance_model |>
        rowwise() |>
        mutate(model_check = list(performance::check_model(model))) |>
        ungroup() |>
        filter(!is.null(model_check))
    }
  ),

  tar_target(
    name = trait_variance_all,
    command = {
      trait_variance_data |>
        group_by(trait_trans) |>
        nest() |>
        mutate(
          model_linear = purrr::map(data, ~ {
            safelmer <- purrr::safely(lmerTest::lmer)
            result <- safelmer(trait_value ~ climate_value + (1 | site), data = .x)
            result$result
          }),
          model_poly = purrr::map(data, ~ {
            safelmer <- purrr::safely(lmerTest::lmer)
            result <- safelmer(trait_value ~ climate_value + I(climate_value^2) + (1 | site), data = .x)
            result$result
          }),
          glance_linear = purrr::map(model_linear, ~ {
            safe_glance <- purrr::safely(broom.mixed::glance)
            result <- safe_glance(.x)
            result$result
          }),
          glance_poly = purrr::map(model_poly, ~ {
            safe_glance <- purrr::safely(broom.mixed::glance)
            result <- safe_glance(.x)
            result$result
          })
        ) |>
        tidyr::pivot_longer(
          cols = c(model_linear, model_poly, glance_linear, glance_poly),
          names_sep = "_",
          names_to = c(".value", "model_type")
        )
    }
  ),

  tar_target(
    name = trait_variance_best,
    command = {
      trait_variance_all |>
        unnest(glance) |>
        dplyr::select(trait_trans:model, AIC) |>
        filter(AIC == min(AIC)) |>
        select(-AIC)
    }
  ),

  tar_target(
    name = trait_variance_output,
    command = {
      trait_variance_best |>
        mutate(
          tidy_results = purrr::map(model, ~ {
            safe_tidy <- purrr::safely(broom.mixed::tidy)
            result <- safe_tidy(.x)
            result$result
          }),
          climate_pvalue = purrr::map_dbl(tidy_results, ~ {
            climate_row <- .x |> filter(term == "climate_value" & effect == "fixed")
            if (nrow(climate_row) > 0) climate_row$p.value else NA_real_
          }),
          is_significant = climate_pvalue < 0.05,
          predictions = purrr::map2(data, model, ~ {
            safe_pred <- purrr::safely(lmer_prediction_trait)
            pred_result <- safe_pred(dat = .x, fit = .y, predictor = "climate_value")
            bind_cols(.x |> select(-climate_value), pred_result$result)
          })
        )
    }
  )
)
