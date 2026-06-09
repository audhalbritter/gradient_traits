# Diversity analysis

diversity_plan <- list(
  # diversity model vs. latitude
  tar_target(
    name = diversity_model,
    command = {
      safelmer <- purrr::safely(lmerTest::lmer)

      diversity |>
        filter(diversity_index == "diversity") |>
        mutate(elevation_km = elevation_m / 1000) |>
        group_by(diversity_index) |>
        mutate(
          latitude_original = latitude_n,
          latitude_mean = mean(latitude_n, na.rm = TRUE),
          latitude_sd = sd(latitude_n, na.rm = TRUE),
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
        tidyr::pivot_longer(
          cols = c(model_linear, model_poly, glance_linear, glance_poly, result_linear, result_poly),
          names_sep = "_",
          names_to = c(".value", "model_type")
        ) |>
        unnest(glance) |>
        mutate(
          delta_aic = AIC - min(AIC, na.rm = TRUE),
          selection_rank = case_when(
            model_type == "linear" & delta_aic <= 2 ~ 0L,
            TRUE ~ 1L
          )
        ) |>
        arrange(selection_rank, AIC) |>
        slice(1)
    }
  ),

  tar_target(
    name = diversity_predictions,
    command = {
      diversity_model |>
        mutate(
          prediction = map2(.x = model, .y = data, .f = ~ lmer_prediction(dat = .y, fit = .x)),
          latitude_pvalue = map_dbl(result, ~ {
            lat_row <- .x |> filter(term == "latitude_n" & effect == "fixed")
            if (nrow(lat_row) > 0) lat_row$p.value else NA_real_
          }),
          is_significant = latitude_pvalue < 0.05
        ) |>
        mutate(
          data_with_predictions = map2(.x = data, .y = prediction, .f = ~ bind_cols(.x |> select(-elevation_km, -latitude_n, -latitude_original, -latitude_mean, -latitude_sd), .y))
        )
    }
  ),

  tar_target(
    name = diversity_climate_long,
    command = {
      diversity |>
        filter(diversity_index == "diversity") |>
        tidyr::pivot_longer(
          cols = c(gs_length, gs_temperature, gs_vpd, gdd, gs_diurnal_range),
          names_to = "climate_variable",
          values_to = "climate_value"
        ) |>
        filter(!is.na(climate_value))
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
        mutate(
          delta_aic = AIC - min(AIC, na.rm = TRUE),
          selection_rank = case_when(
            model_type == "linear" & delta_aic <= 2 ~ 0L,
            TRUE ~ 1L
          )
        ) |>
        arrange(selection_rank, AIC, .by_group = TRUE) |>
        slice(1) |>
        ungroup()
    }
  ),

  tar_target(
    name = diversity_model_region_ds_climate,
    command = {
      safelmer <- purrr::safely(lmerTest::lmer)

      diversity_climate_long |>
        group_by(diversity_index, climate_variable) |>
        tidyr::nest() |>
        mutate(
          model_linear = purrr::map(.x = data, .f = ~ safelmer(value ~ climate_value + region + (1 | site), data = .)$result),
          model_poly = purrr::map(.x = data, .f = ~ safelmer(value ~ climate_value + I(climate_value^2) + region + (1 | site), data = .)$result),
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
        mutate(
          delta_aic = AIC - min(AIC, na.rm = TRUE),
          selection_rank = case_when(
            model_type == "linear" & delta_aic <= 2 ~ 0L,
            TRUE ~ 1L
          )
        ) |>
        arrange(selection_rank, AIC, .by_group = TRUE) |>
        slice(1) |>
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
              .x |> dplyr::select(-climate_value),
              .y
            )
          )
        )
    }
  ),

  tar_target(
    name = diversity_predictions_region_ds_climate,
    command = {
      diversity_model_region_ds_climate |>
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
              .x |> dplyr::select(-climate_value),
              .y
            )
          )
        )
    }
  )#,

  #tar_target(
  #  name = diversity_model_checks,
  #  command = {
  #    diversity_model |>
  #      rowwise() |>
  #      mutate(model_check = list(performance::check_model(model))) |>
  #      ungroup()
  #  }
  #),

  #tar_target(
  #  name = diversity_model_checks_ds_climate,
  #  command = {
  #    diversity_model_ds_climate |>
  #      rowwise() |>
  #      mutate(model_check = list(performance::check_model(model))) |>
  #      ungroup()
  #  }
  #),

  #tar_target(
  #  name = diversity_model_checks_region_ds_climate,
  #  command = {
  #    diversity_model_region_ds_climate |>
  #      rowwise() |>
  #      mutate(model_check = list(performance::check_model(model))) |>
  #      ungroup()
  #  }
  #),
)
