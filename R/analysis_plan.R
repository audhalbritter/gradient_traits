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
  # diversity models
  tar_target(
    name = diversity_models,
    command = {
      diversity |>
        dplyr::filter(diversity_index != "sum_abundance") |>
        fit_lmer_set(response = "value", group_var = "diversity_index") |>
        # make long table
        tidyr::pivot_longer(cols = -c(diversity_index, data),
                     names_sep = "_",
                     names_to = c(".value", "bioclim"))
    }
  ),

    # Tidy results from existing models (no refit)
  tar_target(
    name = diversity_results,
    command = {
      diversity_models |>
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
        select(diversity_index, bioclim, result) |>
        unnest(result)
    }
  ),

  # diversity predictions
  tar_target(
    name = diversity_predictions,
    command = {
      diversity_models |>
        filter(bioclim != "null") |>  # Remove null models - only needed for comparison
        mutate(
          prediction = map2(.x = model, .y = data, .f = ~ broom.mixed::augment(.x, newdata = .y))
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
        mutate(
          prediction = map2(.x = model, .y = data, .f = ~ broom.mixed::augment(.x, newdata = .y))
        )
    }
  ),

  # diversity model checks
  tar_target(
    name = diversity_model_checks,
    command = {
      diversity_models |>
        filter(bioclim != "null") |>  # Remove null models
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
