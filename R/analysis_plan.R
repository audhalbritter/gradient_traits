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
        group_by(diversity_index) |>
        nest() |>
        mutate(model = purrr::map(.x = data, .f = ~ safelmer(value ~ latitude_n + (1|country), data = .)$result),
        result = purrr::map(model, broom.mixed::tidy))
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
          data_with_predictions = map2(.x = data, .y = prediction, .f = ~ bind_cols(.x |> select(-elevation_km, -value, -latitude_n), .y))
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
        group_by(diversity_index) |>
        nest() |>
        mutate(
          model = purrr::map(
            .x = data,
            .f = ~ safelmer(value ~ annual_temperature_bioclim + (1|country), data = .)$result
          ),
          result = purrr::map(model, broom.mixed::tidy)
        )
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
                select(-annual_temperature_bioclim, -value),
              .y
            )
          )
        )
    }
  ),

  # trait models
  tar_target(
    name = trait_models,
    command = {
      trait_mean |>
        filter(trait_trans %in% c("dry_mass_g_log", "leaf_area_cm2_log", "thickness_mm_log", "ldmc", "sla_cm2_g")) |>
        fit_lmer_set(response = "mean", group_var = "trait_trans") |>
        # make long table
        tidyr::pivot_longer(cols = -c(trait_trans, data),
                     names_pattern = "model_(.+)",
                     names_to = "bioclim",
                     values_to = "model") |>
        # Add glance data back
        left_join(
          trait_mean |>
            filter(trait_trans %in% c("dry_mass_g_log", "leaf_area_cm2_log", "thickness_mm_log", "ldmc", "sla_cm2_g")) |>
            fit_lmer_set(response = "mean", group_var = "trait_trans") |>
            select(trait_trans, starts_with("glance")) |>
            tidyr::pivot_longer(cols = -trait_trans,
                         names_pattern = "glance_(.+)",
                         names_to = "bioclim",
                         values_to = "glance"),
          by = c("trait_trans", "bioclim")
        )
    }
  ),

  # Tidy results from trait models
  tar_target(
    name = trait_results,
    command = {
      trait_models |>
        filter(bioclim != "null", !is.na(bioclim)) |>
        rowwise() |>
        mutate(
          result = list({
            if(!is.null(model)) {
              broom.mixed::tidy(model)
            } else {
              NULL
            }
          })
        ) |>
        select(trait_trans, bioclim, result) |>
        unnest(result)
    }
  ),

  # trait predictions
  tar_target(
    name = trait_predictions,
    command = {
      trait_models |>
        filter(bioclim != "null") |>  # Remove null models - only needed for comparison
        dplyr::mutate(
          predictor = dplyr::case_when(
            bioclim == "lat" ~ "latitude_n",
            bioclim == "elev" ~ "elevation_m",
            bioclim == "gsl_gee" ~ "growing_season_length",  # GEE growing season length
            bioclim == "gsl_chelsa" ~ "gsl_1981-2010_chelsa",  # CHELSA growing season length
            bioclim == "gst_chelsa" ~ "gst_1981-2010_chelsa",  # CHELSA growing season temperature
            bioclim == "gsp_chelsa" ~ "gsp_1981-2010_chelsa",  # CHELSA growing season precipitation
            bioclim == "pet_chelsa" ~ "pet_penman_mean_1981-2010_chelsa",  # CHELSA potential evapotranspiration
            bioclim == "temp_warm_bioclim" ~ "mean_temperture_warmest_quarter_bioclim",  # WorldClim mean temperature warmest quarter
            bioclim == "precip_warm_bioclim" ~ "precipitation_warmest_quarter_bioclim",  # WorldClim precipitation warmest quarter
            bioclim == "diurnal_bioclim" ~ "diurnal_range_bioclim",  # WorldClim diurnal range
            TRUE ~ bioclim
          ),
          # Extract p-value for the predictor term to determine line type
          predictor_pvalue = pmap_dbl(
            list(model, predictor),
            function(fit, pred_col){
              if (is.null(fit)) return(NA_real_)
              # Get the model results
              model_results <- broom.mixed::tidy(fit)
              # Find the row for this predictor
              pred_row <- model_results |> 
                filter(term == pred_col & effect == "fixed")
              if (nrow(pred_row) > 0) {
                pred_row$p.value
              } else {
                NA_real_
              }
            }
          ),
          # Determine if relationship is significant (p < 0.05)
          is_significant = predictor_pvalue < 0.05,
          prediction = pmap(
            list(data, model, predictor),
            function(dat, fit, pred_col){
              safe_pred <- purrr::safely(lmer_prediction_trait)
              result <- safe_pred(dat = dat, fit = fit, predictor = pred_col)
              if (!is.null(result$error)) {
                cat("Error in prediction for", pred_col, ":", result$error$message, "\n")
                return(NULL)
              }
              return(result$result)
            }
          ),
          data_with_predictions = pmap(
            list(data, prediction, predictor, model),
            function(dat, pred, pred_col, fit){
              if (is.null(pred)) return(NULL)
              # Align rows with model's training data (handle na.action like in prediction)
              if (!is.null(attr(fit@frame, "na.action"))) {
                na_action <- attr(fit@frame, "na.action")
                if (length(na_action) > 0) {
                  dat <- dat[-na_action, ]
                }
              }
              cols_to_drop <- intersect(names(dat), c(pred_col, "mean"))
              dplyr::bind_cols(dplyr::select(dat, -dplyr::all_of(cols_to_drop)), pred)
            }
          )
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
      trait_models |>
        filter(bioclim != "null") |>  # Remove null models
        rowwise() |>
        mutate(
          model_check = list({
            if (!is.null(model)) {
              performance::check_model(model)
            } else {
              NULL
            }
          })
        ) |>
        ungroup() |>
        filter(!is.null(model_check))  # Remove rows with NULL model_check
    }
  )

)
