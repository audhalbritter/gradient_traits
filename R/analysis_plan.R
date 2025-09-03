# Data analysis

analysis_plan <- list(

  # ordination reduced, all traits fewer countries
  tar_target(
    name = trait_pca,
    command = make_trait_pca(trait_mean |>
    # norway and sa needs to be removed for now because no chem traits, ch needs removed because no plant height
    filter(!country %in% c("no", "sa", "ch")))
  ),

    # ordination with all countries, fewer traits
    tar_target(
    name = trait_pca_full,
    command = make_trait_pca(trait_mean |> 
    # remove incomplete traits
    filter(trait_trans %in% c("dry_mass_g_log", "leaf_area_cm2_log", "thickness_mm_log", "ldmc", "sla_cm2_g", "plant_height_cm_log")))
  ),

  # bioclim PCA using all bioclimatic variables
  tar_target(
    name = bioclim_pca,
    command = make_bioclim_pca(bioclim |>
    select(country:ID, annual_temperature,
    annual_precipitation, precipitation_seasonality, precipitation_driest_quarter, precipitation_warmest_quarter, 
    diurnal_range, temperature_seasonality,
    max_temperture_warmest_month, temperature_annual_range, mean_temperture_driest_quarter))
  ),

# Run models
  # diversity models
  tar_target(
    name = diversity_models,
    command = {
      safelmer <- safely(.f = lme4::lmer)

      diversity |>
        filter(diversity_index != "sum_abundance") |>
        group_by(diversity_index) |>
        nest() |>
        mutate(
          model_null = map(.x = data, .f = ~ safelmer(value ~ 1 + (1|country), data = .)$result),
          model_lat = map(.x = data, .f = ~ safelmer(value ~ latitude_n + (1|country), data = .)$result),
          model_anntemp = map(.x = data, .f = ~ safelmer(value ~ annual_temperature + (1|country), data = .)$result),
          model_temprange = map(.x = data, .f = ~ safelmer(value ~ temperature_annual_range + (1|country), data = .)$result),
          glance_null = map(model_null, broom.mixed::glance),
          glance_lat = map(model_lat, broom.mixed::glance),
          glance_anntemp = map(model_anntemp, broom.mixed::glance),
          glance_temprange = map(model_temprange, broom.mixed::glance)
        ) |>
        # make long table
        pivot_longer(cols = -c(diversity_index, data),
                     names_sep = "_",
                     names_to = c(".value", "bioclim"))
    }
  ),

    # Tidy results from existing models (no refit)
  tar_target(
    name = diversity_results,
    command = {
      diversity_models |>
        filter(bioclim %in% c("lat", "anntemp", "temprange")) |>
        mutate(
          result = map(model, broom.mixed::tidy)
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

  # traits models
  tar_target(
    name = latitude_model,
    command = {
      safelm <- safely(.f = lm)

      trait_mean |>
        group_by(trait_trans, trait_fancy) |>
        nest() |>
        mutate(
          model = map(.x = data, .f = ~ safelm(mean ~ annual_temperature, data = .)$result),
          result = map(model, tidy),
          # make new data for prediction
          # newdata = map(.x = data, .f = ~. |>
          #                        mutate(min)
          prediction = map2(.x = model, .y = data, .f = predict, interval = "confidence"),
          output = map2(.x = data, .y = prediction, ~ bind_cols(.x, .y))
        )
    }
  )

)
