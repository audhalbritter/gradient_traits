# Data analysis

analysis_plan <- list(

  # diversity across latitude
  tar_target(
    name = diversity_lat_model,
    command = {
      safelm <- safely(.f = lm)

      diversity |>
        group_by(diversity_index) |>
        nest() |>
        mutate(
          model = map(.x = data, .f = ~ safelm(value ~ temperature_annual_range * country, data = .)$result),
          result = map(model, tidy),
          anova = map(model, car::Anova),
          anova_tidy = map(anova, tidy),
          prediction = map2(.x = model, .y = data, .f = predict, interval = "confidence"),
          output = map2(.x = data, .y = prediction, ~ bind_cols(.x, .y))
        )

    }
  ),


  # diversity within country model
  tar_target(
    name = diversity_model,
    command = {

      safelm = safely(.f = lm)

      diversity |>
        group_by(country, ecosystem, diversity_index) |> 
        nest() |>
        mutate(model = map(.x = data, .f= ~ safelm(value ~ annual_temperature, data = .)$result),
               result = map(model, tidy),
               prediction = map2(.x = model, .y = data, .f = predict, interval = "confidence"),
               output = map2(.x = data, .y = prediction, ~ bind_cols(.x, .y)))
    }
  ),

  # testing across latitude
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
  ),

  # testing within country
  tar_target(
    name = trait_mean_model,
    command = {
      safelm <- safely(.f = lm)

      trait_mean |>
        group_by(country, ecosystem, gradient, trait_trans, trait_fancy) |>
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
  ),


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
    select(country:diurnal_range, max_temperture_warmest_month, min_temperture_coldest_month, 
    annual_precipitation, precipitation_driest_month, precipitation_driest_quarter))
  ),

  # bioclim correlation plot
  tar_target(
    name = bioclim_correlation_plot,
    command = make_bioclim_correlation_plot(bioclim)
  )

)
