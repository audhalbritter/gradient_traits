# Díaz Global Spectrum vs PFTC species-mean trait space

diaz_plan <- list(

  tar_target(
    name = download_diaz_traits,
    command = "data/diaz/Species_mean_traits.xlsx",
    format = "file"
  ),

  tar_target(
    name = diaz_traits,
    command = harmonize_diaz_traits(download_diaz_traits)
  ),

  # Species means from measured PFTC traits (not community-weighted means)
  tar_target(
    name = pftc_species_means_diaz,
    command = pftc_species_means_for_diaz(traits)
  ),

  tar_target(
    name = diaz_pftc_pca,
    command = make_diaz_pftc_pca(diaz_traits, pftc_species_means_diaz)
  ),

  tar_target(
    name = diaz_pftc_pca_fig,
    command = make_diaz_pftc_pca_plot(diaz_pftc_pca)
  )
)
