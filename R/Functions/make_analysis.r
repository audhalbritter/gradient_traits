# Make analysis

#' Center climate predictors within groups before mixed modelling.
#'
#' Keeps the original scale in `climate_value_raw`, replaces `climate_value`
#' with the mean-centered values used in models, and stores `climate_mean` for
#' back-transforming smooth prediction curves to the original scale in figures.
#'
#' @param dat Long data with `climate_value`.
#' @param group_vars Columns defining separate centring groups (typically
#'   `climate_variable`, or `diversity_index` + `climate_variable`).
center_climate_long <- function(dat, group_vars = "climate_variable") {
  dat |>
    mutate(climate_value_raw = climate_value) |>
    group_by(across(all_of(group_vars))) |>
    mutate(
      climate_mean = mean(climate_value_raw, na.rm = TRUE),
      climate_value = climate_value_raw - climate_mean
    ) |>
    ungroup()
}

# Diversity vs latitude: scaled predictor with back-transform for plotting
lmer_prediction <- function(dat, fit, predictor = "latitude_n") {
  stopifnot(identical(predictor, "latitude_n"))
  newdat <- dat %>%
    dplyr::select(latitude_n, value, latitude_original, latitude_mean, latitude_sd)

  newdat$.fitted <- predict(fit, newdat, re.form = NA)

  prediction <- tryCatch(
    {
      mm <- model.matrix(terms(fit), newdat)
      vc <- vcov(fit)
      re_var <- as.numeric(VarCorr(fit)$site[1])
      newdat %>%
        mutate(
          pvar1 = diag(mm %*% tcrossprod(vc, mm)),
          tvar1 = pvar1 + re_var,
          cmult = 1.96
        ) %>%
        mutate(
          plo = .fitted - cmult * sqrt(pvar1),
          phi = .fitted + cmult * sqrt(pvar1),
          tlo = .fitted - cmult * sqrt(tvar1),
          thi = .fitted + cmult * sqrt(tvar1)
        )
    },
    error = function(e) {
      newdat %>%
        mutate(
          pvar1 = NA_real_,
          tvar1 = NA_real_,
          cmult = 1.96,
          plo = NA_real_,
          phi = NA_real_,
          tlo = NA_real_,
          thi = NA_real_
        )
    }
  ) %>%
    dplyr::mutate(latitude_n = latitude_original) %>%
    dplyr::select(.fitted, pvar1, tvar1, cmult, plo, phi, tlo, thi, latitude_n)

  prediction
}

# Prediction function for trait models with long-format climate data
lmer_prediction_trait <- function(dat, fit, predictor) {
  newdat <- dat |>
    select(any_of(c("trait_value", predictor, "region", "climate_value_raw", "climate_mean")))

  newdat$.fitted <- predict(fit, newdat, re.form = NA)

  prediction <- tryCatch(
    {
      mm <- model.matrix(terms(fit), newdat)
      vc <- vcov(fit)
      re_var <- as.numeric(VarCorr(fit)$site[1])
      newdat |>
        mutate(
          pvar1 = diag(mm %*% tcrossprod(vc, mm)),
          tvar1 = pvar1 + re_var,
          cmult = 1.96
        ) |>
        mutate(
          plo = .fitted - cmult * sqrt(pvar1),
          phi = .fitted + cmult * sqrt(pvar1),
          tlo = .fitted - cmult * sqrt(tvar1),
          thi = .fitted + cmult * sqrt(tvar1)
        )
    },
    error = function(e) {
      newdat |>
        mutate(
          pvar1 = NA_real_,
          tvar1 = NA_real_,
          cmult = 1.96,
          plo = NA_real_,
          phi = NA_real_,
          tlo = NA_real_,
          thi = NA_real_
        )
    }
  ) |>
    mutate(
      climate_value = if ("climate_value_raw" %in% names(newdat)) {
        climate_value_raw
      } else {
        .data[[predictor]]
      }
    ) |>
    select(.fitted, pvar1, tvar1, cmult, plo, phi, tlo, thi, climate_value)

  prediction
}

# Diversity vs long-format climate (same scaling/back-transform as trait climate models)
lmer_prediction_diversity_climate <- function(dat, fit, predictor = "climate_value") {
  lmer_prediction_trait(
    dat |> dplyr::rename(trait_value = value),
    fit,
    predictor
  )
}


# Prediction function for regional models with smooth lines across a range
lmer_prediction_smooth <- function(fit, dat) {
  value_col <- if ("climate_value_raw" %in% names(dat)) "climate_value_raw" else "climate_value"
  climate_mean <- stats::median(dat$climate_mean, na.rm = TRUE)

  newdat <- dat |>
    group_by(region) |>
    summarise(
      min_val = min(.data[[value_col]], na.rm = TRUE),
      max_val = max(.data[[value_col]], na.rm = TRUE),
      .groups = "drop"
    ) |>
    rowwise() |>
    mutate(climate_value_raw = list(seq(min_val, max_val, length.out = 100))) |>
    tidyr::unnest(climate_value_raw) |>
    mutate(climate_value = climate_value_raw - climate_mean)

  newdat$.fitted <- predict(fit, newdat, re.form = NA)

  mm <- model.matrix(delete.response(terms(fit)), newdat)
  vc <- vcov(fit)
  mm <- mm[, colnames(vc), drop = FALSE]
  pvar1 <- diag(mm %*% tcrossprod(vc, mm))

  newdat |>
    mutate(
      plo = .fitted - 1.96 * sqrt(pvar1),
      phi = .fitted + 1.96 * sqrt(pvar1)
    ) |>
    transmute(
      region,
      climate_value = climate_value_raw,
      .fitted,
      plo,
      phi
    )
}

# Prediction function for global models with smooth lines
lmer_prediction_global_smooth <- function(fit, dat) {
  value_col <- if ("climate_value_raw" %in% names(dat)) "climate_value_raw" else "climate_value"
  climate_mean <- stats::median(dat$climate_mean, na.rm = TRUE)
  raw_seq <- seq(min(dat[[value_col]], na.rm = TRUE), max(dat[[value_col]], na.rm = TRUE), length.out = 100)

  newdat <- tibble(
    climate_value_raw = raw_seq,
    climate_value = raw_seq - climate_mean
  )

  newdat$.fitted <- predict(fit, newdat, re.form = NA)

  mm <- model.matrix(delete.response(terms(fit)), newdat)
  vc <- vcov(fit)
  mm <- mm[, colnames(vc), drop = FALSE]
  pvar1 <- diag(mm %*% tcrossprod(vc, mm))

  newdat |>
    mutate(
      plo = .fitted - 1.96 * sqrt(pvar1),
      phi = .fitted + 1.96 * sqrt(pvar1)
    ) |>
    transmute(
      climate_value = climate_value_raw,
      .fitted,
      plo,
      phi
    )
}
