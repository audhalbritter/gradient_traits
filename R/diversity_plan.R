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
        filter(!is.na(climate_value)) |>
        center_climate_long(group_vars = c("diversity_index", "climate_variable"))
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
              .x |> dplyr::select(-climate_value, -climate_mean),
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
              .x |> dplyr::select(-climate_value, -climate_mean),
              .y
            )
          )
        )
    }
  ),

  tar_target(
    name = beta_adjacent_pairs,
    command = {
      make_adjacent_beta_pairs <- function(dat) {
        if (nrow(dat) < 2) {
          return(tibble())
        }

        species_matrix <- dat |>
          dplyr::select(plot_key, taxon, presence) |>
          tidyr::pivot_wider(
            names_from = taxon,
            values_from = presence,
            values_fill = 0,
            values_fn = max
          ) |>
          dplyr::arrange(plot_key)

        plot_meta <- dat |>
          dplyr::distinct(plot_key, elevation_m) |>
          dplyr::arrange(elevation_m, plot_key) |>
          dplyr::mutate(step_index = dplyr::row_number())

        if (nrow(plot_meta) < 2) {
          return(tibble())
        }

        mat <- species_matrix |>
          tibble::column_to_rownames("plot_key") |>
          as.matrix()

        core <- betapart::betapart.core(mat)
        pair <- betapart::beta.pair(core, index.family = "sorensen")

        sim_mat <- as.matrix(pair$beta.sim)
        sne_mat <- as.matrix(pair$beta.sne)
        sor_mat <- as.matrix(pair$beta.sor)

        adjacent_pairs <- plot_meta |>
          dplyr::mutate(
            to_plot = dplyr::lead(plot_key),
            elev_to = dplyr::lead(elevation_m)
          ) |>
          dplyr::filter(!is.na(to_plot)) |>
          dplyr::transmute(
            from_plot = plot_key,
            to_plot = to_plot,
            elev_from = elevation_m,
            elev_to = elev_to
          ) |>
          dplyr::rowwise() |>
          dplyr::mutate(
            beta_sim = sim_mat[from_plot, to_plot],
            beta_sne = sne_mat[from_plot, to_plot],
            beta_sor = sor_mat[from_plot, to_plot],
            elev_mid = (elev_from + elev_to) / 2,
            elev_diff_m = abs(elev_to - elev_from),
            turnover_fraction = dplyr::if_else(beta_sor > 0, beta_sim / beta_sor, NA_real_),
            nestedness_fraction = dplyr::if_else(beta_sor > 0, beta_sne / beta_sor, NA_real_)
          ) |>
          dplyr::ungroup()

        adjacent_pairs
      }

      community |>
        dplyr::group_by(country, region, gradient, site, plot_id, elevation_m, taxon) |>
        dplyr::summarise(cover = sum(cover, na.rm = TRUE), .groups = "drop") |>
        dplyr::mutate(
          plot_key = paste(country, region, gradient, site, plot_id, sep = "__"),
          presence = as.integer(cover > 0)
        ) |>
        dplyr::group_by(country, region, gradient) |>
        tidyr::nest() |>
        dplyr::mutate(adjacent_pairs = purrr::map(data, make_adjacent_beta_pairs)) |>
        dplyr::select(-data) |>
        tidyr::unnest(adjacent_pairs)
    }
  ),

  tar_target(
    name = beta_adjacent_validation,
    command = {
      beta_adjacent_pairs |>
        dplyr::mutate(
          beta_sum_error = abs(beta_sor - (beta_sim + beta_sne)),
          decomposition_ok = beta_sum_error < 1e-8
        ) |>
        dplyr::summarise(
          n_pairs = dplyr::n(),
          n_regions = dplyr::n_distinct(region),
          n_gradients = dplyr::n_distinct(paste(country, gradient, sep = "__")),
          max_beta_sum_error = max(beta_sum_error, na.rm = TRUE),
          all_decomposition_ok = all(decomposition_ok, na.rm = TRUE)
        )
    }
  ),

  tar_target(
    name = beta_gradient_summary,
    command = {
      beta_adjacent_pairs |>
        dplyr::group_by(country, region, gradient) |>
        dplyr::summarise(
          n_pairs = dplyr::n(),
          mean_beta_sim = mean(beta_sim, na.rm = TRUE),
          median_beta_sim = median(beta_sim, na.rm = TRUE),
          mean_beta_sne = mean(beta_sne, na.rm = TRUE),
          median_beta_sne = median(beta_sne, na.rm = TRUE),
          mean_turnover_fraction = mean(turnover_fraction, na.rm = TRUE),
          mean_nestedness_fraction = mean(nestedness_fraction, na.rm = TRUE),
          mean_elev_diff_m = mean(elev_diff_m, na.rm = TRUE),
          .groups = "drop"
        )
    }
  ),

  tar_target(
    name = beta_region_summary,
    command = {
      beta_adjacent_pairs |>
        dplyr::group_by(region) |>
        dplyr::summarise(
          n_pairs = dplyr::n(),
          n_gradients = dplyr::n_distinct(paste(country, gradient, sep = "__")),
          mean_beta_sim = mean(beta_sim, na.rm = TRUE),
          mean_beta_sne = mean(beta_sne, na.rm = TRUE),
          mean_beta_sor = mean(beta_sor, na.rm = TRUE),
          mean_turnover_fraction = mean(turnover_fraction, na.rm = TRUE),
          se_turnover_fraction = sd(turnover_fraction, na.rm = TRUE) / sqrt(n_pairs),
          mean_nestedness_fraction = mean(nestedness_fraction, na.rm = TRUE),
          se_nestedness_fraction = sd(nestedness_fraction, na.rm = TRUE) / sqrt(n_pairs),
          .groups = "drop"
        ) |>
        dplyr::mutate(
          region = factor(region, levels = c(
            "Svalbard", "Southern Scandes", "Rocky Mountains",
            "Eastern Himalaya", "Central Andes", "Drakensberg"
          ))
        ) |>
        dplyr::arrange(region)
    }
  ),

  tar_target(
    name = diversity_model_checks,
    command = {
      diversity_model |>
        rowwise() |>
        mutate(model_check = list(performance::check_model(model))) |>
        ungroup()
    }
  ),

  tar_target(
    name = diversity_model_checks_ds_climate,
    command = {
      diversity_model_ds_climate |>
        rowwise() |>
        mutate(model_check = list(performance::check_model(model))) |>
        ungroup()
    }
  ),

  tar_target(
    name = diversity_model_checks_region_ds_climate,
    command = {
      diversity_model_region_ds_climate |>
        rowwise() |>
        mutate(model_check = list(performance::check_model(model))) |>
        ungroup()
    }
  )
)
