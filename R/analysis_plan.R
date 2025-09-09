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
        mutate(model = purrr::map(.x = data, .f = ~ safelmer(value ~ elevation_km + (1|country), data = .)$result),
        result = purrr::map(model, broom.mixed::tidy))
    }
  ),

  # diversity predictions
  tar_target(
    name = diversity_predictions,
    command = {
      diversity_model |>
        mutate(
          prediction = map2(.x = model, .y = data, .f = ~ lmer_prediction(dat = .y, fit = .x, predictor = "elevation_km")),
          # Extract p-value for elevation_km term to determine line type
          elevation_pvalue = map_dbl(result, ~ {
            elev_row <- .x |> filter(term == "elevation_km" & effect == "fixed")
            if (nrow(elev_row) > 0) {
              elev_row$p.value
            } else {
              NA_real_
            }
          }),
          # Determine if relationship is significant (p < 0.05)
          is_significant = elevation_pvalue < 0.05
        ) |>
        mutate(
          data_with_predictions = map2(.x = data, .y = prediction, .f = ~ bind_cols(.x |> select(-elevation_km, -value), .y))
        )
    }
  ),

  # trait models
  tar_target(
    name = trait_models,
    command = {
      trait_mean |>
        fit_lmer_set(response = "mean", group_var = "trait_trans") |>
        # make long table
        tidyr::pivot_longer(cols = -c(trait_trans, data),
                     names_sep = "_",
                     names_to = c(".value", "bioclim"))
    }
  ),

  # Tidy results from trait models
  tar_target(
    name = trait_results,
    command = {
      trait_models |>
        filter(bioclim %in% c("lat", "elev", "gsl", "gst", "pet", "diurnal")) |>
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
            bioclim == "gsl" ~ "growing_season_length",
            bioclim == "gst" ~ "growing_season_temperature",
            bioclim == "pet" ~ "potential_evapotranspiration",
            bioclim == "diurnal" ~ "mean_diurnal_range_chelsa",
            TRUE ~ bioclim
          ),
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
          model_check = list(performance::check_model(model))
        ) |>
        ungroup()
    }
  )

)
