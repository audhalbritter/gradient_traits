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

# make prediction for lmer - trait version (handles 'mean' column instead of 'value')
lmer_prediction_trait <- function(dat, fit, predictor){
  
  # Create new data with predictor and response variables
  newdat <- dat %>%
    select(all_of(predictor), mean)
  
  # Handle missing data by using the same na.action as the model
  # This ensures the prediction data matches the model's training data
  if (!is.null(attr(fit@frame, "na.action"))) {
    # Remove the same rows that were removed during model fitting
    na_action <- attr(fit@frame, "na.action")
    if (length(na_action) > 0) {
      newdat <- newdat[-na_action, ]
    }
  }
  
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

# Fit a standard set of lmer models for a response across groups
# Inputs:
# - data: a data.frame with columns for predictors and grouping
# - response: name of the response column (string), e.g., "value" or "mean"
# - group_var: name of the grouping variable to facet by (string), e.g., "diversity_index" or "trait_trans"
# - random_effect: random effect grouping column (default: "country")
# Output: long tibble with columns: {{group_var}}, data, model, bioclim, glance
fit_lmer_set <- function(data, response, group_var, random_effect = "country") {
  safelmer <- purrr::safely(lmerTest::lmer)

  # Build formulas as strings and then convert to formulas
  f_null       <- stats::as.formula(paste(response, "~ 1 + (1|", random_effect, ")"))
  f_lat        <- stats::as.formula(paste(response, "~ latitude_n + (1|", random_effect, ")"))
  f_elev       <- stats::as.formula(paste(response, "~ elevation_m + (1|", random_effect, ")"))
  f_gsl_gee    <- stats::as.formula(paste(response, "~ growing_season_length + (1|", random_effect, ")"))
  f_gsl_chelsa <- stats::as.formula(paste(response, "~ `gsl_1981-2010_chelsa` + (1|", random_effect, ")"))
  f_gst_chelsa <- stats::as.formula(paste(response, "~ `gst_1981-2010_chelsa` + (1|", random_effect, ")"))
  f_gsp_chelsa <- stats::as.formula(paste(response, "~ `gsp_1981-2010_chelsa` + (1|", random_effect, ")"))
  f_pet_chelsa <- stats::as.formula(paste(response, "~ `pet_penman_mean_1981-2010_chelsa` + (1|", random_effect, ")"))
  f_temp_warm_bioclim <- stats::as.formula(paste(response, "~ mean_temperture_warmest_quarter_bioclim + (1|", random_effect, ")"))
  f_precip_warm_bioclim <- stats::as.formula(paste(response, "~ precipitation_warmest_quarter_bioclim + (1|", random_effect, ")"))
  f_diurnal_bioclim <- stats::as.formula(paste(response, "~ diurnal_range_bioclim + (1|", random_effect, ")"))

  data |>
    dplyr::group_by(.data[[group_var]]) |>
    tidyr::nest() |>
    dplyr::mutate(
      model_null        = purrr::map(.x = data, .f = ~ safelmer(f_null,         data = .)$result),
      model_lat         = purrr::map(.x = data, .f = ~ safelmer(f_lat,          data = .)$result),
      model_elev        = purrr::map(.x = data, .f = ~ safelmer(f_elev,         data = .)$result),
      model_gsl_gee     = purrr::map(.x = data, .f = ~ safelmer(f_gsl_gee,      data = .)$result),
      model_gsl_chelsa  = purrr::map(.x = data, .f = ~ safelmer(f_gsl_chelsa,   data = .)$result),
      model_gst_chelsa  = purrr::map(.x = data, .f = ~ safelmer(f_gst_chelsa,   data = .)$result),
      model_gsp_chelsa  = purrr::map(.x = data, .f = ~ safelmer(f_gsp_chelsa,   data = .)$result),
      model_pet_chelsa  = purrr::map(.x = data, .f = ~ safelmer(f_pet_chelsa,   data = .)$result),
      model_temp_warm_bioclim = purrr::map(.x = data, .f = ~ safelmer(f_temp_warm_bioclim, data = .)$result),
      model_precip_warm_bioclim = purrr::map(.x = data, .f = ~ safelmer(f_precip_warm_bioclim, data = .)$result),
      model_diurnal_bioclim = purrr::map(.x = data, .f = ~ safelmer(f_diurnal_bioclim, data = .)$result),
      glance_null       = purrr::map(model_null,        ~ if(!is.null(.)) broom.mixed::glance(.) else NULL),
      glance_lat        = purrr::map(model_lat,         ~ if(!is.null(.)) broom.mixed::glance(.) else NULL),
      glance_elev       = purrr::map(model_elev,        ~ if(!is.null(.)) broom.mixed::glance(.) else NULL),
      glance_gsl_gee    = purrr::map(model_gsl_gee,     ~ if(!is.null(.)) broom.mixed::glance(.) else NULL),
      glance_gsl_chelsa = purrr::map(model_gsl_chelsa,  ~ if(!is.null(.)) broom.mixed::glance(.) else NULL),
      glance_gst_chelsa = purrr::map(model_gst_chelsa,  ~ if(!is.null(.)) broom.mixed::glance(.) else NULL),
      glance_gsp_chelsa = purrr::map(model_gsp_chelsa,  ~ if(!is.null(.)) broom.mixed::glance(.) else NULL),
      glance_pet_chelsa = purrr::map(model_pet_chelsa,  ~ if(!is.null(.)) broom.mixed::glance(.) else NULL),
      glance_temp_warm_bioclim = purrr::map(model_temp_warm_bioclim, ~ if(!is.null(.)) broom.mixed::glance(.) else NULL),
      glance_precip_warm_bioclim = purrr::map(model_precip_warm_bioclim, ~ if(!is.null(.)) broom.mixed::glance(.) else NULL),
      glance_diurnal_bioclim = purrr::map(model_diurnal_bioclim, ~ if(!is.null(.)) broom.mixed::glance(.) else NULL)
    )
}
