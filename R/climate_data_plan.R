# Growing-season climate derived from the hourly PFTC extract.
# See R/Functions/growing_season_climate.R for the underlying functions.

climate_data_plan <- list(

  # Daily aggregates per plot (+ hemisphere-aware ordering key)
  tar_target(
    name = daily_climate,
    command = aggregate_climate_to_daily(hourly_climate) |>
      add_growing_season_order()
  ),

  # Growing-season window per plot: longest warm run (daily mean > 5 degC),
  # bridging short cold snaps (< 5 days) between warm spells
  tar_target(
    name = growing_season,
    command = detect_growing_season(daily_climate, threshold = 5, run_length = 5, bridge = 5)
  ),

  # Derived growing-season climate variables per plot (GDD base 5 degC)
  tar_target(
    name = growing_season_climate,
    command = summarise_growing_season_climate(daily_climate, growing_season, threshold = 5)
  ),

  # Site-level means used as a fallback for plots without plot-level climate
  tar_target(
    name = growing_season_climate_site,
    command = summarise_growing_season_climate_site(growing_season_climate)
  )

)
