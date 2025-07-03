## cleaning functions

# cleaning Svalbard data
clean_sv_communit <- function(raw_community_sv){
  
  raw_community_sv |> 
    clean_names() |> 
    tidylog::distinct() |> 
    mutate(country = "sv",
           ecosystem = "arctic",
           gradient = if_else(gradient == "B", "N", gradient),
           site = as.character(site),
           site = paste0(country, "_", gradient, "_", site),
           plot_id = paste0(gradient, "_", site, "_", plot_id)) |> 
    tidylog::select(country, year, date, gradient, site, plot_id, taxon, cover, elevation_m, latitude_n, longitude_e, ecosystem)
}


clean_sv_traits <- function(raw_traits_sv){
  
  raw_traits_sv |> 
    clean_names() |> 
      # remove bryo
      filter(project == "Gradient") |> 
    mutate(country = "sv",
           ecosystem = "arctic",
           gradient = if_else(gradient == "B", "N", gradient),
           site = as.character(site),
           site = paste0(country, "_", gradient, "_", site),
           plot_id = paste0(gradient, "_", site, "_", plot_id)) |> 
    tidylog::select(country, year, date, gradient, site:individual_nr, leaf_id = id, taxon, trait:longitude_e, ecosystem)
  
}


# Cleaning Peru data
# community
clean_pe_community <- function(raw_community_pe){
 
  raw_community_pe |> 
    filter(!treatment %in% c("NB", "BB"),
           site != "OCC") |> 
    mutate(country = "pe",
           ecosystem = "tropic",
           site = paste0(country, "_", treatment, "_", site),
           plot_id = paste0(treatment, "_", site, "_", plot_id),
           taxon = tolower(taxon)) |> 
    tidylog::select(country, year, season, month, gradient = treatment, site, plot_id, functional_group, family, taxon, cover, elevation_m = elevation, latitude_n = latitude, longitude_e = longitude, ecosystem)
    
}

# cleaning Peru Trait
clean_pe_traits <- function(raw_traits_pe){
 
  raw_traits_pe |> 
    filter(!treatment %in% c("NB", "BB"),
           site != "OCC") |> 
    mutate(country = "pe",
           gradient = treatment,
           ecosystem = "tropic",
           site = paste0(country, "_", treatment, "_", site),
           plot_id = paste0(treatment, "_", site, "_", plot_id),
           taxon = tolower(taxon)) |> 
    tidylog::select(country, year, season, month, gradient = treatment, site, plot_id, individual_nr, leaf_id = id, functional_group, family, taxon, trait, value, elevation_m = elevation, latitude_n = latitude, longitude_e = longitude, ecosystem)

}  


# Import and clean China community data
import_clean_ch_community <- function(raw_meta_ch){
  
  con <- dbConnect(dbConnect(SQLite(), dbname = "data/transplant.sqlite"))
  
  taxon <- tbl(con, "taxon")
  
  # assemble dataset
  community <- tbl(con, "turfCommunity") |> 
    left_join(tbl(con, "turfs"), by = "turfID") |> 
    # only control plots
    filter(TTtreat %in% c("C", "0")) |>  
    left_join(tbl(con, "plots") |> 
                select(-slope, -aspect), by = c("originPlotID" = "plotID")) |>  
    left_join(tbl(con, "blocks") %>% 
                select(-slope, -aspect), by = c("blockID")) |>  
    left_join(tbl(con, "sites")%>% 
                select(-slope, -aspect), by = c("siteID")) |> 
    left_join(taxon, by = c("species")) |> 
    collect()
  
  community |> 
    left_join(raw_meta_ch, by = c("siteID" = "site")) |>
    mutate(country = "ch",
           gradient = "C",
           ecosystem = "sub-tropics",
           site = paste0(country, "_", siteID),
           plot_id = paste0(country, "_", originPlotID),
           speciesName = tolower(speciesName)) |>
    filter(year == 2016) |> 
    select(country, year, gradient, site, plot_id, taxon = speciesName, cover, functional_group = functionalGroup, family, elevation_m = elevation, latitude_n = latitude.y, longitude_e = longitude.y, ecosystem) |> 
    mutate(taxon = recode(taxon, "Potentilla stenophylla var. emergens" = "Potentilla stenophylla"))
    
}


# Clean China trait data
clean_ch_traits <- function(raw_traits_leaf_ch, raw_traits_chem_ch, raw_meta_ch){
  
  # leaves
  leaf <- raw_traits_leaf_ch |> 
    clean_names() |> 
    select(date:dry_mass_g, leaf_thickness_ave_mm:ldmc, stoich_label) |> 
    pivot_longer(cols = c(wet_mass_g:ldmc), names_to = "trait", values_to = "value")
    
  # chemical data
  chem <- raw_traits_chem_ch |> 
    clean_names() |> 
    select(-n) |> 
    mutate(np_ratio = n_percent / p_percent) |> 
    pivot_longer(cols = c(p_percent:d_c13_permil, np_ratio), names_to = "trait", values_to = "value")
  
  # merge
  leaf |>  
    bind_rows(chem) |>  
    filter(!is.na(value)) |> 
    filter(treatment %in% c("LOCAL", "0", "C")) |>
    left_join(raw_meta_ch, by = c("site", "elevation")) |> 
    mutate(taxon = tolower(trimws(taxon))) |>  
    mutate(year = year(date),
           country = "ch",
           gradient = "C",
           ecosystem = "sub-tropics",
           site = paste0(country, "_", site),
           plot_id = paste0(country, "_", dest_block_id),
           leaf_id = paste(site, treatment, taxon, individual_number, leaf_number, sep = "_"),
           taxon = tolower(taxon)) |> 
    select(country, year, date, gradient, site, plot_id, individual_number, leaf_id, taxon, trait, value, elevation_m = elevation, latitude_n = latitude, longitude_e = longitude, ecosystem)
    
}



# clean norway community
clean_no_comm <- function(raw_community_no, sp_list_no){
  
  threeD_community <- raw_community_no |>
    # filter for 2022 and trait data treatments
    filter(year == 2022,
           warming == "A",
           grazing %in% c("C", "N"),
           Nlevel %in% c(1, 2, 3)) |> 
    # group species that are uncertain
    # e.g. Antennaria dioica, Ant alpina and Ant sp
    mutate(species = case_when(str_detect(species, "Antennaria") ~ "Antennaria sp",
                               str_detect(species, "Luzula") ~ "Luzula sp",
                               str_detect(species, "Pyrola") ~ "Pyrola sp",
                               TRUE ~ species)) |>
    # Remove Carex rupestris and norvegica cf, Carex sp, because they are very uncertain
    filter(!str_detect(species, "Unknown"),
           !species %in% c("Carex rupestris", "Carex rupestris cf",
                           "Carex norvegica cf", "Carex sp")) |>
    
    # add variables
    mutate(country = "no",
           gradient = "C",
           ecosystem = "boreal",
           elevation_m = if_else(destSiteID == "Joa", 920, 1290),
           latitude_n = if_else(destSiteID == "Joa", 60.86183, 60.85994),
           longitude_e = if_else(destSiteID == "Joa", 7.16800, 7.19504),
           site = paste0(country, "_", destSiteID),
           plot_id = paste0(site, "_", turfID)) |>
    
    # add taxon information
    left_join(sp_list_no |>
                mutate(species = paste(genus, species, sep = " ")),
              by = "species") |>
    # fix NA's in functional group
    mutate(functional_group = case_when(species == "Carex nigra" ~ "graminoid",
                                        species %in% c("Oxytropa laponica", "Galium verum", "Veronica officinalis", "Erigeron uniflorus", "Epilobium anagallidifolium") ~ "forb",
                                        TRUE ~ functional_group)) |>
    ungroup() |>
    select(country, year, date, gradient, site, plot_id, taxon = species, cover, family, functional_group, elevation_m, latitude_n, longitude_e, ecosystem)
  
  # vcg plant community data
  con <- dbConnect(SQLite(), dbname = "data/seedclim.sqlite")
  
  #dbListTables(con)
  
  vcg_community <- tbl(con, "turf_community")  |>
    select(-cf, -flag) |>
    left_join(tbl(con, "turfs"), by = "turfID") |>
    
    # only control plots
    filter(TTtreat %in% c("TTC")) |>
    select(-RTtreat, -GRtreat, -destinationPlotID) |>
    
    # join plot, block and site IDs
    left_join(tbl(con, "plots"), by = c("originPlotID" = "plotID")) |>
    rename("plotID" = originPlotID) |>
    select(-aspect, -slope) |>
    left_join(tbl(con, "blocks"), by = c("blockID")) |>
    select(-aspect, -slope) |>
    left_join(tbl(con, "sites"), by = c("siteID")) |>
    select(-comment, -norwegian_name, -site_code, -c(biogeographic_zone:precipitation_level)) |>
    
    # filter 2 sites, and last year
    filter(siteID %in% c("Hogsete", "Vikesland"),
           year == 2019) |>
    left_join(tbl(con, "taxon"), by = "species") |>
    group_by(year, siteID, turfID, species_name, family, elevation, latitude, longitude) |>
    summarise(cover = mean(cover)) |>
    rename(taxon = species_name) |>
    collect() |> 
    
    mutate(country = "no",
           gradient = "C",
           ecosystem = "boreal",
           site = paste0(country, "_", siteID),
           plot_id = paste0(site, "_", turfID)) |> 
    ungroup() |> 
    select(country, year, gradient, site, plot_id, taxon, cover, family, elevation_m = elevation, latitude_n = latitude, longitude_e = longitude, ecosystem)
  
  bind_rows(threeD_community, vcg_community)
  
}


# clean norway traits
clean_no_traits <- function(raw_traits_no){
  
  raw_traits_no |> 
    filter(warming == "A",
           grazing %in% c("C", "N"),
           Namount_kg_ha_y == 0) |> 
    # add variables
    mutate(year = year(date),
           country = "no",
           gradient = "C",
           ecosystem = "boreal",
           site = paste0(country, "_", siteID),
           plot_id = paste0(site, "_", turfID)) |>
    select(country, year, date, gradient, site, plot_id, taxon = species, trait, value, elevation_m = elevation_m_asl, ecosystem)
  
}




# Colorado data
# clean Colorado meta Community
# clean_colorado_meta_community <- function(metaCommunityCO){
#   metaCommunityCO |> 
#     clean_names() |> 
#     filter(!is.na(site)) |>  
#     mutate(plot_id = recode(plot_id, "plot_3_pct" = "plot3_pct"),
#            gradient = "c") |> 
#     mutate(plot_id = recode(plot_id, "plot1_pct" = "1", "plot2_pct" = "2", "plot3_pct" = "3", "plot4_pct" = "4", "plot5_pct" = "5"),
#            plot_id = paste(site, plot_id, sep = "_")) |> 
#     pivot_wider(names_from = group, values_from = cover) |> 
#     select(-date_y, -mean_height_cm_y) |>  
#     rename(date = date_x, mean_height_cm = mean_height_cm_x, graminoid = 'Total Graminoid', herb = 'Total Herb', shrub = 'Total Shrub', bare_soil_litter_dead = 'Bare (Bare soil + Litter + Dead)', bare_soil = 'Bare soil')
# 
# }


# clean Colorado community
clean_colorado_community <- function(raw_community_co, coords_co){
  raw_community_co |> 
    clean_names() |> 
    filter(site != "PBM") |> 
    select(-common_name_morpho, -total, -photo_taken, -specimen_collected) |>  
    pivot_longer(cols = c(plot_1:plot_5), names_to = "plot_id", values_to = "cover") |>  
    rename(taxon = species) |> 
    mutate(date = dmy(date),
           country = "co",
           ecosystem = "temperate",
           gradient = "C",
           year = year(date)) |> 
    mutate(plot_id = str_replace(plot_id, "plot_", ""),
           site = paste0(country, "_", site),
           plot_id = paste0(site, "_", plot_id)) |> 
    filter(!is.na(cover), !cover == 0) |> 
    tidylog::left_join(coords_co) |> 
    select(country, year, date, gradient, site, plot_id, taxon, cover, elevation_m, latitude_n, longitude_e, ecosystem)

}


# clean Colorado trait
clean_colorado_trait <- function(raw_sp_co, raw_trait_co, coords_co){
  
  Row5 <- tribble(
    ~Site, ~Species_from_abundance_data, ~Likely_same_species_from_trait_data,
    "Almont",   "Elymus elymoides", "Hordeum jubatum",
    "Almont",   "Elymus elymoides", "Hesperostipa comata"
  )
  
  raw_sp_co <- raw_sp_co |> 
    filter(!c(Site == "Almont" & Species_from_abundance_data == "Elymus elymoides")) |> 
    bind_rows(Row5)
  
  raw_trait_co |>  
    filter(site %in% c("Almont", "CBT", "Road", "Pfeiler", "Cinnamon")) |> 
    select(year, site, block, taxon_std, leaf_area, wet_mass, dry_mass, SLA, height_flower, height_leaf, height, height_2, thickness, pc_C, pc_N, pc_P, d13C, d15N,  C_N, N_P) |> 
    rename(plot_id = block, taxon = taxon_std, Leaf_Area_cm2 = leaf_area, Wet_Mass_g = wet_mass, Dry_Mass_g = dry_mass, SLA_cm2_g = SLA, Plant_Height_cm = height_flower, Leaf_Thickness_Ave_mm = thickness, C_percent = pc_C, N_percent = pc_N, dC13_permil = d13C, dN15_permil = d15N, CN_ratio = C_N, NP_ratio = N_P, P_percent = pc_P) |> 
    mutate(country = "co",
           ecosystem = "temperate",
           gradient = "C",
           LDMC = Dry_Mass_g/Wet_Mass_g) |> 
    mutate(plot_id = str_replace(plot_id, "Block", ""),
           site = paste0(country, "_", site),
           plot_id = paste0(site, "_", plot_id)) %>%
    mutate(P_percent = as.numeric(P_percent),
           C_percent = as.numeric(C_percent),
           N_percent = as.numeric(N_percent),
           dC13_permil = as.numeric(dC13_permil),
           dN15_permil = as.numeric(dN15_permil),
           CN_ratio = as.numeric(CN_ratio),
           NP_ratio = as.numeric(NP_ratio)) |> 
    select(-height_leaf, -height, -height_2) |> 
    pivot_longer(cols = c(Leaf_Area_cm2:NP_ratio, LDMC), names_to = "trait", values_to = "value") |> 
    filter(!is.na(value)) |> 
    filter(!is.na(taxon)) |> 
    filter(!(trait == "SLA_cm2_g" & value > 500),
           !(trait == "SLA_cm2_g" & value < 5)) |> 
    # Replace species to match the community dataset
    left_join(raw_sp_co, by = c("site" = "Site", "taxon" = "Likely_same_species_from_trait_data")) |> 
    mutate(taxon = if_else(!is.na(Species_from_abundance_data), Species_from_abundance_data, taxon)) |>
    tidylog::left_join(coords_co |> 
                         distinct(site, elevation_m, latitude_n, longitude_e,     country, gradient)) |> 
    select(country, year, gradient, site, plot_id, taxon, trait, value, elevation_m, latitude_n, longitude_e, ecosystem)
   
  
}  
