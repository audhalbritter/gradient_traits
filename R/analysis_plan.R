# Data analysis

analysis_plan <- list(

  # ordination reduced, all traits fewer countries
  tar_target(
    name = trait_pca,
    command = make_trait_pca(trait_mean |>
      # norway and sa needs to be removed for now because no chem traits, ch needs removed because no plant height
      filter(!country %in% c("no", "sa")) |>
      filter(!trait_trans %in% c("plant_height_cm_log")))
  ),

  # ordination with all countries, fewer traits
  tar_target(
    name = trait_pca_full,
    command = make_trait_pca(trait_mean |>
      # remove incomplete traits
      filter(trait_trans %in% c("dry_mass_g_log", "leaf_area_cm2_log", "thickness_mm_log", "ldmc", "sla_cm2_g")))
  ),

  # Run models
  # diversity model
  tar_target(
    name = diversity_model,
    command = {
      safelmer <- purrr::safely(lmerTest::lmer)

      diversity |>
        filter(diversity_index == "diversity") |>
        mutate(elevation_km = elevation_m / 1000) |>
        # Standardize latitude for better model convergence
        group_by(diversity_index) |>
        mutate(
          # Store original values for back-transformation
          latitude_original = latitude_n,
          # Calculate scaling parameters
          latitude_mean = mean(latitude_n, na.rm = TRUE),
          latitude_sd = sd(latitude_n, na.rm = TRUE),
          # Scale the latitude values (center and scale)
          latitude_n = (latitude_n - latitude_mean) / latitude_sd
        ) |>
        ungroup() |>
        group_by(diversity_index) |>
        nest() |>
        mutate(
          model_linear = purrr::map(.x = data, .f = ~ safelmer(value ~ latitude_n + (1 | site), data = .)$result),
          model_poly = purrr::map(.x = data, .f = ~ safelmer(value ~ latitude_n + I(latitude_n^2) + (1 | site), data = .)$result),
          glance_linear = purrr::map(.x = model_linear, .f = ~ broom.mixed::glance(.x)),
          glance_poly = purrr::map(.x = model_poly, .f = ~ broom.mixed::glance(.x)),
          result_linear = purrr::map(model_linear, broom.mixed::tidy),
          result_poly = purrr::map(model_poly, broom.mixed::tidy)
        ) |>
        # Pivot to long format to stack linear and polynomial models
        tidyr::pivot_longer(
          cols = c(model_linear, model_poly, glance_linear, glance_poly, result_linear, result_poly),
          names_sep = "_",
          names_to = c(".value", "model_type")
        ) |>
        # Unnest glance data to access AIC values
        unnest(glance) |>
        # select the best model based on AIC
        filter(AIC == min(AIC, na.rm = TRUE))
    }
  ),

  # diversity predictions
  tar_target(
    name = diversity_predictions,
    command = {
      diversity_model |>
        mutate(
          prediction = map2(.x = model, .y = data, .f = ~ lmer_prediction(dat = .y, fit = .x)),
          # Extract p-value for latitude_n term to determine line type
          latitude_pvalue = map_dbl(result, ~ {
            lat_row <- .x |> filter(term == "latitude_n" & effect == "fixed")
            if (nrow(lat_row) > 0) {
              lat_row$p.value
            } else {
              NA_real_
            }
          }),
          # Determine if relationship is significant (p < 0.05)
          is_significant = latitude_pvalue < 0.05
        ) |>
        mutate(
          data_with_predictions = map2(.x = data, .y = prediction, .f = ~ bind_cols(.x |> select(-elevation_km, -latitude_n, -latitude_original, -latitude_mean, -latitude_sd), .y))
        )
    }
  ),

  # Shannon diversity vs downscaled climate: same pattern as trait_mean_long (pivot + nest by climate_variable)
  tar_target(
    name = diversity_climate_long,
    command = {
      diversity |>
        filter(diversity_index == "diversity") |>
        tidyr::pivot_longer(
          cols = c(ds_t2m, ds_vpd),
          names_to = "climate_variable",
          values_to = "climate_value"
        ) |>
        filter(!is.na(climate_value)) |>
        group_by(climate_variable) |>
        mutate(
          climate_value_original = climate_value,
          climate_mean = mean(climate_value, na.rm = TRUE),
          climate_sd = sd(climate_value, na.rm = TRUE),
          climate_value = (climate_value - climate_mean) / climate_sd
        ) |>
        ungroup()
    }
  ),

  tar_target(
    name = diversity_model_ds_climate,
    command = {
      safelmer <- purrr::safely(lmerTest::lmer)

      diversity_climate_long |>
        group_by(diversity_index, climate_variable) |>
        tidyr::nest() |>
        mutate(
          model_linear = purrr::map(.x = data, .f = ~ safelmer(value ~ climate_value + (1 | site), data = .)$result),
          model_poly = purrr::map(.x = data, .f = ~ safelmer(value ~ climate_value + I(climate_value^2) + (1 | site), data = .)$result),
          glance_linear = purrr::map(.x = model_linear, .f = ~ broom.mixed::glance(.x)),
          glance_poly = purrr::map(.x = model_poly, .f = ~ broom.mixed::glance(.x)),
          result_linear = purrr::map(model_linear, broom.mixed::tidy),
          result_poly = purrr::map(model_poly, broom.mixed::tidy)
        ) |>
        tidyr::pivot_longer(
          cols = c(model_linear, model_poly, glance_linear, glance_poly, result_linear, result_poly),
          names_sep = "_",
          names_to = c(".value", "model_type")
        ) |>
        unnest(glance) |>
        group_by(diversity_index, climate_variable) |>
        filter(AIC == min(AIC, na.rm = TRUE)) |>
        ungroup()
    }
  ),

  tar_target(
    name = diversity_predictions_ds_climate,
    command = {
      diversity_model_ds_climate |>
        mutate(
          prediction = purrr::map2(
            .x = model,
            .y = data,
            .f = ~ lmer_prediction_diversity_climate(dat = .y, fit = .x, predictor = "climate_value")
          ),
          climate_pvalue = purrr::map_dbl(result, ~ {
            term_row <- .x |>
              dplyr::filter(term == "climate_value" & effect == "fixed")
            if (nrow(term_row) > 0) term_row$p.value else NA_real_
          }),
          is_significant = climate_pvalue < 0.05,
          data_with_predictions = purrr::map2(
            .x = data,
            .y = prediction,
            .f = ~ dplyr::bind_cols(
              .x |>
                dplyr::select(-climate_value, -climate_value_original, -climate_mean, -climate_sd),
              .y
            )
          )
        )
    }
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
            climate_variable == "ds_t2m" ~ "Mean annual temperature at 2 m (downscaled)",
            climate_variable == "ds_vpd" ~ "Vapour pressure deficit (downscaled)",
            TRUE ~ climate_variable
          )
        ) |>
        # Filter out rows with NA climate values
        filter(!is.na(climate_value)) |>
        # Scale climate variables by climate_variable group to enable back-transformation
        group_by(climate_variable) |>
        mutate(
          # Store original values for back-transformation
          climate_value_original = climate_value,
          # Calculate scaling parameters
          climate_mean = mean(climate_value, na.rm = TRUE),
          climate_sd = sd(climate_value, na.rm = TRUE),
          # Scale the climate values (center and scale)
          climate_value = (climate_value - climate_mean) / climate_sd
        ) |>
        ungroup() |>
        # Rename mean to trait_value for consistency with prediction function
        rename(trait_value = mean) |>
        # Keep elevation and latitude as separate columns
        select(
          country:ecosystem, elevation_m, latitude_n, longitude_e, trait_trans, trait_value,
          climate_variable, climate_variable_clean, climate_value, climate_value_original,
          climate_mean, climate_sd, data_source
        )
    }
  ),

  # Step 1: Fit both Linear and Polynomial regional models for all trait/climate pairs
  tar_target(
    name = trait_models_region_all,
    command = {
      trait_mean_long |>
        filter(trait_trans %in% trait_trans_mean_for_climate) |>
        group_by(trait_trans, climate_variable, data_source) |>
        nest() |>
        mutate(
          # Linear model (additive, parallel slopes)
          model_linear = purrr::map(data, ~ {
            safelmer <- purrr::safely(lmerTest::lmer)
            result <- safelmer(trait_value ~ climate_value + region + (1 | site), data = .x)
            result$result
          }),
          # Polynomial model (additive, parallel curvature)
          model_poly = purrr::map(data, ~ {
            safelmer <- purrr::safely(lmerTest::lmer)
            result <- safelmer(trait_value ~ (climate_value + I(climate_value^2)) + region + (1 | site), data = .x)
            result$result
          }),
          # Glance data for AIC comparison
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
        # Stack linear and poly models
        tidyr::pivot_longer(
          cols = c(model_linear, model_poly, glance_linear, glance_poly),
          names_sep = "_",
          names_to = c(".value", "model_type")
        )
    }
  ),

  # Step 2: Select the best regional model based on AIC
  tar_target(
    name = trait_models_region_best,
    command = {
      trait_models_region_all |>
        unnest(glance) |>
        group_by(trait_trans, climate_variable, data_source) |>
        filter(AIC == min(AIC, na.rm = TRUE)) |>
        slice(1) |> # Tie-breaker
        select(-AIC) |>
        ungroup()
    }
  ),

  # Step 3: Generate smooth predictions and tidy summaries for the best regional models
  tar_target(
    name = trait_models_region_output,
    command = {
      trait_models_region_best |>
        mutate(
          # Extract tidy Results
          tidy_results = purrr::map(model, ~ {
            safe_tidy <- purrr::safely(broom.mixed::tidy)
            result <- safe_tidy(.x)
            result$result
          }),
          # Check for overall significance (p < 0.05 for any term involving climate_value)
          is_significant = purrr::map_lgl(tidy_results, ~ {
            if (is.null(.x)) {
              return(FALSE)
            }
            any(.x$p.value[grepl("climate_value", .x$term) & .x$effect == "fixed"] < 0.05, na.rm = TRUE)
          }),
          # Generate smooth predictions for plotting
          predictions = purrr::map2(model, data, ~ lmer_prediction_smooth(fit = .x, dat = .y))
        )
    }
  ),

  # trait models with long format climate data
  tar_target(
    name = trait_models_all,
    command = {
      trait_mean_long |>
        filter(trait_trans %in% trait_trans_mean_for_climate) |>
        # Group by trait and climate variable
        group_by(trait_trans, climate_variable, data_source) |>
        nest() |>
        # Run models for each combination
        mutate(
          # Linear model
          model_linear = purrr::map(data, ~ {
            safelmer <- purrr::safely(lmerTest::lmer)
            result <- safelmer(trait_value ~ climate_value + (1 | site), data = .x)
            result$result
          }),
          # Polynomial model (second order)
          model_poly = purrr::map(data, ~ {
            safelmer <- purrr::safely(lmerTest::lmer)
            result <- safelmer(trait_value ~ climate_value + I(climate_value^2) + (1 | site), data = .x)
            result$result
          }),
          # Glance data for linear model
          glance_linear = purrr::map(model_linear, ~ {
            safe_glance <- purrr::safely(broom.mixed::glance)
            result <- safe_glance(.x)
            result$result
          }),
          # Glance data for polynomial model
          glance_poly = purrr::map(model_poly, ~ {
            safe_glance <- purrr::safely(broom.mixed::glance)
            result <- safe_glance(.x)
            result$result
          })
        ) |>
        # Pivot to long format to stack linear and polynomial models
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
        # Get tidy results from the best models
        mutate(
          tidy_results = purrr::map(model, ~ {
            safe_tidy <- purrr::safely(broom.mixed::tidy)
            result <- safe_tidy(.x)
            result$result
          }),
          # Extract p-value for climate_value term to determine significance
          climate_pvalue = purrr::map_dbl(tidy_results, ~ {
            climate_row <- .x |> filter(term == "climate_value" & effect == "fixed")
            if (nrow(climate_row) > 0) {
              climate_row$p.value
            } else {
              NA_real_
            }
          }),
          # Determine if relationship is significant (p < 0.05)
          is_significant = climate_pvalue < 0.05,
          # Add predictions for the best models (using smooth prediction for plotting)
          predictions = purrr::map2(model, data, ~ {
            lmer_prediction_global_smooth(fit = .x, dat = .y)
          })
        )
    }
  ),

  # diversity model checks
  tar_target(
    name = diversity_model_checks,
    command = {
      diversity_model |>
        rowwise() |>
        mutate(
          model_check = list(performance::check_model(model))
        ) |>
        ungroup()
    }
  ),

  # trait model checks
  tar_target(
    name = trait_model_checks,
    command = {
      trait_models_output |>
        rowwise() |>
        mutate(
          model_check = list(performance::check_model(model))
        ) |>
        ungroup() |>
        filter(!is.null(model_check)) # Remove rows with NULL model_check
    }
  ),

  # Trait variance vs climate (uses downscaled T2m only; variance modelling optional downstream)
  tar_target(
    name = trait_variance_data,
    command = {
      trait_mean |>
        filter(!is.na(ds_t2m)) |>
        select(country:ecosystem, trait_trans, var, ds_t2m) |>
        mutate(trait_value = var) |>
        group_by(trait_trans) |>
        mutate(
          climate_value_original = ds_t2m,
          climate_mean = mean(ds_t2m, na.rm = TRUE),
          climate_sd = sd(ds_t2m, na.rm = TRUE),
          climate_value = (ds_t2m - climate_mean) / climate_sd
        ) |>
        ungroup() |>
        filter(trait_trans %in% trait_trans_mean_for_climate)
    }
  ),

  # Trait variance model
  tar_target(
    name = trait_variance_model,
    command = {
      trait_variance_data |>
        group_by(trait_trans) |>
        nest() |>
        mutate(
          # Linear model for variance vs growing season temperature
          model = purrr::map(data, ~ {
            safelmer <- purrr::safely(lmerTest::lmer)
            result <- safelmer(trait_value ~ climate_value + (1 | site), data = .x)
            result$result
          }),
          # Get tidy results
          tidy_results = purrr::map(model, ~ {
            safe_tidy <- purrr::safely(broom.mixed::tidy)
            result <- safe_tidy(.x)
            result$result
          }),
          # Extract p-value for climate_value term
          climate_pvalue = purrr::map_dbl(tidy_results, ~ {
            if (!is.null(.x)) {
              climate_row <- .x |> filter(term == "climate_value" & effect == "fixed")
              if (nrow(climate_row) > 0) {
                climate_row$p.value
              } else {
                NA_real_
              }
            } else {
              NA_real_
            }
          }),
          # Determine if relationship is significant
          is_significant = climate_pvalue < 0.05
        )
    }
  ),

  # Trait variance model checks
  tar_target(
    name = trait_variance_model_checks,
    command = {
      trait_variance_model |>
        rowwise() |>
        mutate(
          model_check = list(performance::check_model(model))
        ) |>
        ungroup() |>
        filter(!is.null(model_check))
    }
  ),


  # Trait variance models (linear and polynomial), best model selection, and output
  tar_target(
    name = trait_variance_all,
    command = {
      trait_variance_data |>
        group_by(trait_trans) |>
        nest() |>
        mutate(
          # Linear model
          model_linear = purrr::map(data, ~ {
            safelmer <- purrr::safely(lmerTest::lmer)
            result <- safelmer(trait_value ~ climate_value + (1 | site), data = .x)
            result$result
          }),
          # Polynomial model (second order)
          model_poly = purrr::map(data, ~ {
            safelmer <- purrr::safely(lmerTest::lmer)
            result <- safelmer(trait_value ~ climate_value + I(climate_value^2) + (1 | site), data = .x)
            result$result
          }),
          # Glance data for linear model
          glance_linear = purrr::map(model_linear, ~ {
            safe_glance <- purrr::safely(broom.mixed::glance)
            result <- safe_glance(.x)
            result$result
          }),
          # Glance data for polynomial model
          glance_poly = purrr::map(model_poly, ~ {
            safe_glance <- purrr::safely(broom.mixed::glance)
            result <- safe_glance(.x)
            result$result
          })
        ) |>
        # Pivot to long format to stack linear and polynomial models
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
            if (nrow(climate_row) > 0) {
              climate_row$p.value
            } else {
              NA_real_
            }
          }),
          is_significant = climate_pvalue < 0.05,
          predictions = purrr::map2(data, model, ~ {
            safe_pred <- purrr::safely(lmer_prediction_trait)
            pred_result <- safe_pred(dat = .x, fit = .y, predictor = "climate_value")
            # Remove climate_value from original data to avoid duplicates when binding
            bind_cols(.x |> select(-climate_value), pred_result$result)
          })
        )
    }
  )
)
