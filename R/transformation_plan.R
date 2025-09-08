# try to make as few targets as possible as each target is cached.
# With many intermediate steps, it uses a lot of disk space.

transformation_plan <- list(

  # COMMUNITY
  # merge all communities
  tar_target(
    name = community,
    command = bind_rows(community_sv, community_pe, community_ch, community_no, community_co, community_sa)
  ),

  # calculate diversity indices
  tar_target(
    name = diversity,
    command = {
      # First aggregate community data to plot level
      community_agg <- community |> 
        group_by(country, region, season, gradient, site, plot_id, ecosystem, elevation_m, longitude_e, latitude_n) |>
        summarise(
          richness = n(),
          diversity = diversity(cover),
          evenness = diversity / log(richness),
          sum_abundance = sum(cover),
          .groups = "drop"
        )
      
      # Then join with CHELSA bioclim+ data
      community_agg |>
        pivot_longer(cols = richness:sum_abundance, names_to = "diversity_index", values_to = "value") |>
        tidylog::left_join(chelsa_extracted, by = join_by(country, region,gradient, site, plot_id, elevation_m, longitude_e, latitude_n, ecosystem)) |>
        # Ensure region is ordered consistently (north to south)
        mutate(region = factor(region, levels = c("Svalbard", "Southern Scandes", "Rocky Mountains", 
                                                 "Eastern Himalaya", "Central Andes", "Drakensberg"))) |>
        # Ensure diversity_index is ordered consistently (richness, diversity, evenness, sum_abundance)
        mutate(diversity_index = factor(diversity_index, levels = c("richness", "diversity", "evenness", "sum_abundance")))

    }
  ),

  ## TRAITS
  # merge traits
  tar_target(
    name = traits,
    command = bind_rows(traits_sv, traits_pe, traits_ch, traits_no, traits_co, traits_sa) |>
      # fix trait names
      mutate(
        trait = tolower(trait),
        trait = case_match(trait,
          "d_c13_permil" ~ "dc13_permil",
          "d_n15_permil" ~ "dn15_permil",
          "leaf_thickness_ave_mm" ~ "leaf_thickness_mm",
          .default = trait
        )
      ) |>
      # remove wet mass, correlated with dry mass
      tidylog::filter(trait != "wet_mass_g") |>
      # log transform size and area traits
      mutate(
        value_trans = if_else(
          trait %in% c(
            "plant_height_cm",
            "dry_mass_g",
            "leaf_area_cm2",
            "leaf_thickness_mm"
          ),
          true = suppressWarnings(log(value)), # suppress warnings from log(-value) in isotopes (these are calculated but not kept)
          false = value
        ),
        trait_trans = recode(
          trait,
          "plant_height_cm" = "plant_height_cm_log",
          "dry_mass_g" = "dry_mass_g_log",
          "leaf_area_cm2" = "leaf_area_cm2_log",
          "leaf_thickness_mm" = "thickness_mm_log"
        )
      ) |>
      # order traits
      mutate(trait_trans = factor(trait_trans, levels = c("plant_height_cm_log", "dry_mass_g_log", "leaf_area_cm2_log", "thickness_mm_log", "ldmc", "sla_cm2_g", "c_percent", "n_percent", "cn_ratio", "p_percent", "np_ratio", "dc13_permil", "dn15_permil")))
  ),


  # bootstrapping
  # trait impute
  tar_target(
    name = trait_imputed,
    command = {
      # prepare trait data
      trait <- fancy_trait_name_dictionary(traits) %>%
        select(country, region,gradient, site, plot_id, taxon, trait_trans, value_trans, trait_fancy)

      # set seed for bootstrapping repeatability
      set.seed(2023)
      trait_imp <- trait_fill(
        comm = community,
        traits = trait,
        scale_hierarchy = c("country", "region", "gradient", "site", "plot_id"),
        global = F,
        taxon_col = "taxon",
        trait_col = "trait_trans",
        value_col = "value_trans",
        abundance_col = "cover",
        other_col = c("elevation_m", "latitude_n", "longitude_e", "ecosystem"),
        min_n_in_sample = 2
      )
    }
  ),

  # bootstrapping
  tar_target(
    name = trait_mean,
    command = {
      # bootstrapping
      CWM <- trait_np_bootstrap(trait_imputed, nrep = 100, sample_size = 200)

      # summarise
      CWM_mean <- trait_summarise_boot_moments(CWM) |>
        select(country:mean, var, skew, kurt, -n)

      # prepare bootstrapped trait data for analyses
      trait_mean <- fancy_trait_name_dictionary(CWM_mean) |>
        ungroup() |>
        mutate(trait_trans = factor(trait_trans,
          levels = c(
            "plant_height_cm_log",
            "dry_mass_g_log",
            "leaf_area_cm2_log",
            "thickness_mm_log",
            "ldmc", "sla_cm2_g",
            "c_percent", "n_percent",
            "cn_ratio", "p_percent",
            "np_ratio",
            "dc13_permil",
            "dn15_permil"
          )
        )) |>
        tidylog::left_join(chelsa_extracted, by = join_by(country, region, gradient, site, plot_id, elevation_m, longitude_e, latitude_n, ecosystem)) |>
        # Ensure region is ordered consistently (north to south)
        mutate(region = factor(region, levels = c("Svalbard", "Southern Scandes", "Rocky Mountains", 
                                                 "Eastern Himalaya", "Central Andes", "Drakensberg")))
    }
  ),

  # add community data coordinates to all_coordinates
  tar_target(
    name = all_coordinates,
    command = {
      # Extract coordinates from community data for countries that don't have separate meta targets
      coords_from_community <- community |>
        distinct(country, region, gradient, site, plot_id, elevation_m, longitude_e, latitude_n, ecosystem) |>
        filter(!is.na(longitude_e), !is.na(latitude_n))
      coords_from_community
    }
  )
  

  # # trait coverage
  # tar_target(
  #   name = trait_coverage,
  #   command = fortify(trait_impute) |>
  #     ungroup() |>
  #     #filter(Trait == "CN_ratio") |>
  #     complete(.id, level, trait_trans, fill = list(s = 0)) |>
  #     filter(level == "PlotID") |>
  #     group_by(Gradient, trait_trans) |>
  #     # prob = 0.25 gives 75% of the plots
  #     # also run prob = 0.5 for 50% of the plots
  #     summarise(q = quantile(s, prob = 0.25))
  #
  # )
)
