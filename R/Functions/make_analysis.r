# Make analysis

# make prediction for lmer
lmer_prediction <- function(dat, fit, predictor){

  # Create new data with predictor, response variables, and scaling parameters
  # Handle both latitude and annual temperature scaling
  if (predictor == "latitude_n") {
    newdat <- dat %>%
      select(all_of(predictor), value, latitude_original, latitude_mean, latitude_sd)
  } else if (predictor == "annual_temperature_bioclim") {
    newdat <- dat %>%
      select(all_of(predictor), value, annual_temperature_original, annual_temperature_mean, annual_temperature_sd)
  } else {
    newdat <- dat %>%
      select(all_of(predictor), value)
  }

  # Make predictions
  newdat$.fitted <- predict(fit, newdat, re.form = NA)

  # Calculate confidence intervals
  prediction <- tryCatch({
    mm <- model.matrix(terms(fit), newdat)
    vc <- vcov(fit)
    re_var <- as.numeric(VarCorr(fit)$site[1])
    tmp <- newdat %>%
      mutate(pvar1 = diag(mm %*% tcrossprod(vc, mm)),
             tvar1 = pvar1 + re_var,
             cmult = 1.96) %>%
      mutate(plo = .fitted - cmult*sqrt(pvar1),
             phi = .fitted + cmult*sqrt(pvar1),
             tlo = .fitted - cmult*sqrt(tvar1),
             thi = .fitted + cmult*sqrt(tvar1))
    tmp
  }, error = function(e) {
    n <- nrow(newdat)
    newdat %>%
      mutate(pvar1 = NA_real_,
             tvar1 = NA_real_,
             cmult = 1.96,
             plo = NA_real_,
             phi = NA_real_,
             tlo = NA_real_,
             thi = NA_real_)
  }) %>%
    # Back-transform predictor for plotting
    mutate(
      latitude_n = if (predictor == "latitude_n") latitude_original else NA_real_,
      annual_temperature_bioclim = if (predictor == "annual_temperature_bioclim") annual_temperature_original else NA_real_
    ) %>%
    # Return only prediction-related columns
    select(.fitted, pvar1, tvar1, cmult, plo, phi, tlo, thi, latitude_n, annual_temperature_bioclim)

  return(prediction)
}

# Prediction function for trait models with long-format climate data
lmer_prediction_trait <- function(dat, fit, predictor) {
  # Create new data with predictors and response variable needed for model matrix calculation
  newdat <- dat %>%
    select(trait_value, all_of(predictor), climate_value_original, climate_mean, climate_sd)

  # Make predictions
  newdat$.fitted <- predict(fit, newdat, re.form = NA)

  # Calculate confidence intervals
  prediction <- tryCatch({
    mm <- model.matrix(terms(fit), newdat)
    vc <- vcov(fit)
    re_var <- as.numeric(VarCorr(fit)$site[1])
    tmp <- newdat %>%
      mutate(pvar1 = diag(mm %*% tcrossprod(vc, mm)),
             tvar1 = pvar1 + re_var,
             cmult = 1.96) %>%
      mutate(plo = .fitted - cmult*sqrt(pvar1),
             phi = .fitted + cmult*sqrt(pvar1),
             tlo = .fitted - cmult*sqrt(tvar1),
             thi = .fitted + cmult*sqrt(tvar1))
    tmp
  }, error = function(e) {
    n <- nrow(newdat)
    newdat %>%
      mutate(pvar1 = NA_real_,
             tvar1 = NA_real_,
             cmult = 1.96,
             plo = NA_real_,
             phi = NA_real_,
             tlo = NA_real_,
             thi = NA_real_)
  }) %>%
    # Back-transform climate values to original scale for plotting
    mutate(climate_value = climate_value_original) %>%
    # Only return the prediction-related columns, not the original data
    select(.fitted, pvar1, tvar1, cmult, plo, phi, tlo, thi, climate_value)

  return(prediction)
}
