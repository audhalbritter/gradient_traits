# Growing-season climate derived from the hourly PFTC extract.
# See R/Functions/growing_season_climate.R for the underlying functions.

climate_data_plan <- list(

  # Daily aggregates per plot (+ hemisphere-aware ordering key)
  tar_target(
    name = daily_climate,
    command = aggregate_climate_to_daily(hourly_climate) |>
      add_growing_season_order()
  ),

  # Growing-season window per plot (5 consecutive days with daily mean > 2 degC)
  tar_target(
    name = growing_season,
    command = detect_growing_season(daily_climate, threshold = 2, run_length = 5)
  ),

  # Derived growing-season climate variables per plot
  tar_target(
    name = growing_season_climate,
    command = summarise_growing_season_climate(daily_climate, growing_season, threshold = 2)
  ),

  # Site-level means used as a fallback for plots without plot-level climate
  tar_target(
    name = growing_season_climate_site,
    command = summarise_growing_season_climate_site(growing_season_climate)
  )

)
