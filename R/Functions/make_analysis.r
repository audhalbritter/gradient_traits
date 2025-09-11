# Make analysis

# make prediction for lmer
lmer_prediction <- function(dat, fit, predictor){

  # Create new data with predictor and response variables
  newdat <- dat %>%
    select(all_of(predictor), value)

  # Make predictions
  newdat$.fitted <- predict(fit, newdat, re.form = NA)

  # Calculate confidence intervals
  mm <- model.matrix(terms(fit), newdat)
  
  prediction <- newdat %>%
    mutate(pvar1 = diag(mm %*% tcrossprod(vcov(fit), mm)),
           tvar1 = pvar1 + VarCorr(fit)$country[1],
           cmult = 1.96) %>%
    mutate(plo = .fitted - cmult*sqrt(pvar1),
           phi = .fitted + cmult*sqrt(pvar1),
           tlo = .fitted - cmult*sqrt(tvar1),
           thi = .fitted + cmult*sqrt(tvar1))

  return(prediction)
}

# Prediction function for trait models with long-format climate data
lmer_prediction_trait <- function(dat, fit, predictor) {
  # Create new data with predictor and response variables
  newdat <- dat %>%
    select(all_of(predictor), mean, climate_value_original, climate_mean, climate_sd)

  # Make predictions
  newdat$.fitted <- predict(fit, newdat, re.form = NA)

  # Calculate confidence intervals
  mm <- model.matrix(terms(fit), newdat)
  
  prediction <- newdat %>%
    mutate(pvar1 = diag(mm %*% tcrossprod(vcov(fit), mm)),
           tvar1 = pvar1 + VarCorr(fit)$country[1],
           cmult = 1.96) %>%
    mutate(plo = .fitted - cmult*sqrt(pvar1),
           phi = .fitted + cmult*sqrt(pvar1),
           tlo = .fitted - cmult*sqrt(tvar1),
           thi = .fitted + cmult*sqrt(tvar1)) %>%
    # Back-transform climate values to original scale for plotting
    mutate(climate_value = climate_value_original) %>%
    # Only return the prediction-related columns, not the original data
    select(.fitted, pvar1, tvar1, cmult, plo, phi, tlo, thi, climate_value)

  return(prediction)
}
