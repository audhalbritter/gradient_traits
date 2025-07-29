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
        group_by(country, season, gradient, site, plot_id, ecosystem, elevation_m, longitude_e, latitude_n) |>
        summarise(
          richness = n(),
          diversity = diversity(cover),
          evenness = diversity / log(richness),
          sum_abundance = sum(cover),
          .groups = "drop"
        )
      
      # Then join with bioclim data
      community_agg |>
        pivot_longer(cols = richness:sum_abundance, names_to = "diversity_index", values_to = "value") |>
        tidylog::left_join(bioclim, by = join_by(country, gradient, site, plot_id, elevation_m)) |>
        rename(longitude_e = longitude_e.x, latitude_n = latitude_n.x)
    }
  ),

  # diagnostic: rows only in bioclim
  tar_target(
    name = bioclim_only,
    command = {
      community_keys <- community |>
        group_by(country, gradient, site, plot_id, elevation_m) |>
        summarise(.groups = "drop") |>
        mutate(in_community = TRUE)
      
      bioclim_keys <- bioclim |>
        select(country, gradient, site, plot_id, elevation_m) |>
        mutate(in_bioclim = TRUE)
      
      bioclim_keys |>
        left_join(community_keys, by = c("country", "gradient", "site", "plot_id", "elevation_m")) |>
        filter(is.na(in_community)) |>
        select(-in_community, -in_bioclim)
    }
  ),

  # diagnostic: duplicates in bioclim
  tar_target(
    name = bioclim_duplicates,
    command = {
      bioclim |>
        group_by(country, gradient, site, plot_id, elevation_m) |>
        filter(n() > 1) |>
        arrange(country, gradient, site, plot_id, elevation_m)
    }
  ),

  # diagnostic: duplicates in community
  tar_target(
    name = community_duplicates,
    command = {
      community |>
        group_by(country, gradient, site, plot_id, elevation_m) |>
        filter(n() > 1) |>
        arrange(country, gradient, site, plot_id, elevation_m)
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
        select(country, gradient, site, plot_id, taxon, trait_trans, value_trans, trait_fancy)

      # set seed for bootstrapping repeatability
      set.seed(2023)
      trait_imp <- trait_fill(
        comm = community,
        traits = trait,
        scale_hierarchy = c("country", "gradient", "site", "plot_id"),
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
        left_join(bioclim, by = join_by(country, gradient, site, plot_id, elevation_m)) |>
        # because coords for sv_B_1 were changes (was in the water), needs to be fixed now
        rename(latitude_n = latitude_n.x, longitude_e = longitude_e.x) |>
        select(-latitude_n.y, -longitude_e.y)
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
