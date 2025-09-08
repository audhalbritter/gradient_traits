# Make analysis

# make prediction for lmer
lmer_prediction <- function(dat, fit, predictor){
  
  # Map bioclim identifiers to actual column names
  predictor_col <- case_when(
    predictor == "lat" ~ "latitude_n",
    predictor == "elev" ~ "elevation_m",
    predictor == "gsl" ~ "growing_season_length",
    predictor == "gst" ~ "growing_season_temperature", 
    predictor == "pet" ~ "potential_evapotranspiration",
    predictor == "diurnal" ~ "mean_diurnal_range_chelsa",
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

# Fit a standard set of lmer models for a response across groups
# Inputs:
# - data: a data.frame with columns for predictors and grouping
# - response: name of the response column (string), e.g., "value" or "mean"
# - group_var: name of the grouping variable to facet by (string), e.g., "diversity_index" or "trait_trans"
# - random_effect: random effect grouping column (default: "country")
# Output: long tibble with columns: {{group_var}}, data, model, bioclim, glance
fit_lmer_set <- function(data, response, group_var, random_effect = "country") {
  safelmer <- purrr::safely(lme4::lmer)

  # Build formulas as strings and then convert to formulas
  f_null       <- stats::as.formula(paste(response, "~ 1 + (1|", random_effect, ")"))
  f_lat        <- stats::as.formula(paste(response, "~ latitude_n + (1|", random_effect, ")"))
  f_elev       <- stats::as.formula(paste(response, "~ elevation_m + (1|", random_effect, ")"))
  f_gsl        <- stats::as.formula(paste(response, "~ growing_season_length + (1|", random_effect, ")"))
  f_gst        <- stats::as.formula(paste(response, "~ growing_season_temperature + (1|", random_effect, ")"))
  f_pet        <- stats::as.formula(paste(response, "~ potential_evapotranspiration + (1|", random_effect, ")"))
  f_diurnal    <- stats::as.formula(paste(response, "~ mean_diurnal_range_chelsa + (1|", random_effect, ")"))

  data |>
    dplyr::group_by(.data[[group_var]]) |>
    tidyr::nest() |>
    dplyr::mutate(
      model_null      = purrr::map(.x = data, .f = ~ safelmer(f_null,       data = .)$result),
      model_lat       = purrr::map(.x = data, .f = ~ safelmer(f_lat,        data = .)$result),
      model_elev      = purrr::map(.x = data, .f = ~ safelmer(f_elev,       data = .)$result),
      model_gsl       = purrr::map(.x = data, .f = ~ safelmer(f_gsl,        data = .)$result),
      model_gst       = purrr::map(.x = data, .f = ~ safelmer(f_gst,        data = .)$result),
      model_pet       = purrr::map(.x = data, .f = ~ safelmer(f_pet,        data = .)$result),
      model_diurnal   = purrr::map(.x = data, .f = ~ safelmer(f_diurnal,    data = .)$result),
      glance_null     = purrr::map(model_null,      ~ if(!is.null(.)) broom.mixed::glance(.) else NULL),
      glance_lat      = purrr::map(model_lat,       ~ if(!is.null(.)) broom.mixed::glance(.) else NULL),
      glance_elev     = purrr::map(model_elev,      ~ if(!is.null(.)) broom.mixed::glance(.) else NULL),
      glance_gsl      = purrr::map(model_gsl,       ~ if(!is.null(.)) broom.mixed::glance(.) else NULL),
      glance_gst      = purrr::map(model_gst,       ~ if(!is.null(.)) broom.mixed::glance(.) else NULL),
      glance_pet      = purrr::map(model_pet,       ~ if(!is.null(.)) broom.mixed::glance(.) else NULL),
      glance_diurnal  = purrr::map(model_diurnal,   ~ if(!is.null(.)) broom.mixed::glance(.) else NULL)
    )
}
