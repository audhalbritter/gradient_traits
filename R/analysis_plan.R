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
        filter(diversity_index != "sum_abundance") |>
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
        mutate(model_linear = purrr::map(.x = data, .f = ~ safelmer(value ~ latitude_n + (1|site), data = .)$result),
            model_poly = purrr::map(.x = data, .f = ~ safelmer(value ~ latitude_n + I(latitude_n^2) + (1|site), data = .)$result),
            glance_linear = purrr::map(.x = model_linear, .f = ~ broom.mixed::glance(.x)),
            glance_poly = purrr::map(.x = model_poly, .f = ~ broom.mixed::glance(.x)),
            result_linear = purrr::map(model_linear, broom.mixed::tidy),
            result_poly = purrr::map(model_poly, broom.mixed::tidy)) |>
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
          prediction = map2(.x = model, .y = data, .f = ~ lmer_prediction(dat = .y, fit = .x, predictor = "latitude_n")),
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

  # diversity model using WorldClim annual temperature (bioclim)
  tar_target(
    name = diversity_model_temp_annual,
    command = {
      safelmer <- purrr::safely(lmerTest::lmer)

      diversity |>
        filter(diversity_index != "sum_abundance") |>
        # Standardize annual temperature for better model convergence
        group_by(diversity_index) |>
        mutate(
          # Store original values for back-transformation
          annual_temperature_original = annual_temperature_bioclim,
          # Calculate scaling parameters
          annual_temperature_mean = mean(annual_temperature_bioclim, na.rm = TRUE),
          annual_temperature_sd = sd(annual_temperature_bioclim, na.rm = TRUE),
          # Scale the temperature values (center and scale)
          annual_temperature_bioclim = (annual_temperature_bioclim - annual_temperature_mean) / annual_temperature_sd
        ) |>
        ungroup() |>
        group_by(diversity_index) |>
        nest() |>
        mutate(
          # Linear model
          model_linear = purrr::map(.x = data, .f = ~ safelmer(value ~ annual_temperature_bioclim + (1|site), data = .)$result),
          # Polynomial model (second order)
          model_poly = purrr::map(.x = data, .f = ~ safelmer(value ~ annual_temperature_bioclim + I(annual_temperature_bioclim^2) + (1|site), data = .)$result),
          # Glance data for linear model
          glance_linear = purrr::map(.x = model_linear, .f = ~ broom.mixed::glance(.x)),
          # Glance data for polynomial model
          glance_poly = purrr::map(.x = model_poly, .f = ~ broom.mixed::glance(.x)),
          # Tidy results for linear model
          result_linear = purrr::map(model_linear, broom.mixed::tidy),
          # Tidy results for polynomial model
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


  # predictions for the annual temperature diversity model
  tar_target(
    name = diversity_predictions_temp_annual,
    command = {
      diversity_model_temp_annual |>
        mutate(
          prediction = map2(
            .x = model,
            .y = data,
            .f = ~ lmer_prediction(
              dat = .y,
              fit = .x,
              predictor = "annual_temperature_bioclim"
            )
          ),
          # Extract p-value for the temperature term to determine line type
          temp_pvalue = map_dbl(result, ~ {
            term_row <- .x |>
              filter(term == "annual_temperature_bioclim" & effect == "fixed")
            if (nrow(term_row) > 0) term_row$p.value else NA_real_
          }),
          is_significant = temp_pvalue < 0.05
        ) |>
        mutate(
          data_with_predictions = map2(
            .x = data,
            .y = prediction,
            .f = ~ bind_cols(
              .x |>
                select(-annual_temperature_bioclim, -annual_temperature_original, -annual_temperature_mean, -annual_temperature_sd),
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
        # Pivot climate variables to long format
        pivot_longer(
          cols = c(
            # GEE variables
            growing_season_length,
            # CHELSA variables
            `gsl_1981-2010_chelsa`, `gst_1981-2010_chelsa`, `gsp_1981-2010_chelsa`, `pet_penman_mean_1981-2010_chelsa`, `vpd_mean_1981-2010_chelsa`,
            # WorldClim bioclim variables
            mean_temperture_warmest_quarter_bioclim, precipitation_warmest_quarter_bioclim, diurnal_range_bioclim, annual_temperature_bioclim
          ),
          names_to = "climate_variable",
          values_to = "climate_value"
        ) |>
        # Add data source column
        mutate(
          data_source = case_when(
            climate_variable == "growing_season_length" ~ "GEE",
            climate_variable %in% c("gsl_1981-2010_chelsa", "gst_1981-2010_chelsa", "gsp_1981-2010_chelsa", "pet_penman_mean_1981-2010_chelsa", "vpd_mean_1981-2010_chelsa") ~ "CHELSA",
            climate_variable %in% c("mean_temperture_warmest_quarter_bioclim", "precipitation_warmest_quarter_bioclim", "diurnal_range_bioclim", "annual_temperature_bioclim") ~ "WorldClim",
            TRUE ~ "Other"
          ),
          # Clean up climate variable names for display
          climate_variable_clean = case_when(
            climate_variable == "growing_season_length" ~ "Growing Season Length",
            climate_variable == "gsl_1981-2010_chelsa" ~ "Growing Season Length",
            climate_variable == "gst_1981-2010_chelsa" ~ "Growing Season Temperature",
            climate_variable == "gsp_1981-2010_chelsa" ~ "Growing Season Precipitation",
            climate_variable == "pet_penman_mean_1981-2010_chelsa" ~ "Potential Evapotranspiration",
            climate_variable == "vpd_mean_1981-2010_chelsa" ~ "Vapour Pressure Deficit",
            climate_variable == "mean_temperture_warmest_quarter_bioclim" ~ "Mean Temperature Warmest Quarter",
            climate_variable == "precipitation_warmest_quarter_bioclim" ~ "Precipitation Warmest Quarter",
            climate_variable == "diurnal_range_bioclim" ~ "Mean Diurnal Range",
            climate_variable == "annual_temperature_bioclim" ~ "Annual Temperature",
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
        select(country:ecosystem, elevation_m, latitude_n, longitude_e, trait_trans, trait_value, 
               climate_variable, climate_variable_clean, climate_value, climate_value_original, 
               climate_mean, climate_sd, data_source)
    }
  ),

  # trait models with long format climate data
  tar_target(
    name = trait_models_all,
    command = {
      trait_mean_long |>
        # Filter for the same traits as trait_models
        filter(trait_trans %in% c("plant_height_cm_log", "dry_mass_g_log", "leaf_area_cm2_log", "thickness_mm_log", "ldmc", "sla_cm2_g")) |>
        # Group by trait and climate variable
        group_by(trait_trans, climate_variable, data_source) |>
        nest() |>
        # Run models for each combination
        mutate(
          # Linear model
          model_linear = purrr::map(data, ~ {
            safelmer <- purrr::safely(lmerTest::lmer)
            result <- safelmer(trait_value ~ climate_value + (1|site), data = .x)
            result$result
          }),
          # Polynomial model (second order)
          model_poly = purrr::map(data, ~ {
            safelmer <- purrr::safely(lmerTest::lmer)
            result <- safelmer(trait_value ~ climate_value + I(climate_value^2) + (1|site), data = .x)
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
        dplyr::select(trait_trans:model, AIC) |>
        filter(AIC == min(AIC)) |>
        select(-AIC)
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
          # Add predictions for the best models
          predictions = purrr::map2(data, model, ~ {
            safe_pred <- purrr::safely(lmer_prediction_trait)
            pred_result <- safe_pred(dat = .x, fit = .y, predictor = "climate_value")
            # Remove climate_value from original data to avoid duplicates when binding
            bind_cols(.x |> select(-climate_value), pred_result$result)
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
        filter(!is.null(model_check))  # Remove rows with NULL model_check
    }
  ),

  # R² summary for trait models
  tar_target(
    name = trait_model_r2_summary,
    command = {
      trait_models_output |>
        rowwise() |>
        mutate(
          r2_nakagawa = tryCatch({
            if (!is.null(model)) {
              r2_result <- performance::r2_nakagawa(model)
              r2_result$R2_marginal
            } else {
              NA_real_
            }
          }, error = function(e) NA_real_),
          r2_conditional = tryCatch({
            if (!is.null(model)) {
              r2_result <- performance::r2_nakagawa(model)
              r2_result$R2_conditional
            } else {
              NA_real_
            }
          }, error = function(e) NA_real_),
          aic = tryCatch({
            if (!is.null(model)) {
              AIC(model)
            } else {
              NA_real_
            }
          }, error = function(e) NA_real_)
        ) |>
        ungroup() |>
        select(trait_trans, climate_variable, data_source, model_type, 
               r2_nakagawa, r2_conditional, aic, is_significant) |>
        arrange(desc(r2_nakagawa))
    }
  ),

  # Trait variance analysis - variance vs annual temperature (WorldClim)
  tar_target(
    name = trait_variance_data,
    command = {
      trait_mean |>
        # Filter for annual temperature (WorldClim) data
        filter(!is.na(annual_temperature_bioclim)) |>
        # Select relevant columns including variance
        select(country:ecosystem, trait_trans, var, annual_temperature_bioclim) |>
        # Provide a duplicate 'mean' column equal to variance for downstream compatibility
        mutate(trait_value = var) |>
        # Scale the climate variable
        group_by(trait_trans) |>
        mutate(
          climate_value_original = annual_temperature_bioclim,
          climate_mean = mean(annual_temperature_bioclim, na.rm = TRUE),
          climate_sd = sd(annual_temperature_bioclim, na.rm = TRUE),
          climate_value = (annual_temperature_bioclim - climate_mean) / climate_sd
        ) |>
        ungroup() |>
        # Filter for the same traits as trait_models
        filter(trait_trans %in% c("plant_height_cm_log", "dry_mass_g_log", "leaf_area_cm2_log", "thickness_mm_log", "ldmc", "sla_cm2_g"))
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
            result <- safelmer(trait_value ~ climate_value + (1|site), data = .x)
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
            result <- safelmer(trait_value ~ climate_value + (1|site), data = .x)
            result$result
          }),
          # Polynomial model (second order)
          model_poly = purrr::map(data, ~ {
            safelmer <- purrr::safely(lmerTest::lmer)
            result <- safelmer(trait_value ~ climate_value + I(climate_value^2) + (1|site), data = .x)
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
