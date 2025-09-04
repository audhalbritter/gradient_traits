# Make analysis

# make prediction for lmer
lmer_prediction <- function(dat, fit, predictor){
  
  # Map bioclim identifiers to actual column names
  predictor_col <- case_when(
    predictor == "lat" ~ "latitude_n",
    predictor == "anntemp" ~ "annual_temperature", 
    predictor == "temprange" ~ "temperature_annual_range",
    TRUE ~ predictor
  )
  
  # Check if the predictor column exists
  if (!predictor_col %in% names(dat)) {
    stop("Predictor column '", predictor_col, "' not found in data")
  }

  # Create new data with predictor and response variables
  newdat <- dat %>%
    select(all_of(predictor_col), value)

  # Make predictions
  newdat$value <- predict(fit, newdat, re.form = NA)

  # Calculate confidence intervals
  mm <- model.matrix(terms(fit), newdat)
  
  prediction <- newdat %>%
    mutate(pvar1 = diag(mm %*% tcrossprod(vcov(fit), mm)),
           tvar1 = pvar1 + VarCorr(fit)$country[1],
           cmult = 1.96) %>%
    mutate(plo = value - cmult*sqrt(pvar1),
           phi = value + cmult*sqrt(pvar1),
           tlo = value - cmult*sqrt(tvar1),
           thi = value + cmult*sqrt(tvar1))

  return(prediction)
}
