#

download_plan <- list(

  # Svalbard Data
  # trait data
  tar_target(
    name = download_traits_sv,
    command = get_file(node = "smbqh",
                       file = "PFTC4_Svalbard_2018_Gradient_Traits.csv",
                       path = "data/",
                       remote_path = "Traits"),
    format = "file"
    ),

  # community data
  tar_target(
    name = download_community_sv,
    command = get_file(node = "smbqh",
                       file = "PFTC4_Svalbard_2018_Community_Gradient.csv",
                       path = "data",
                       remote_path = "Community"),
    format = "file"
  ),
  
  # Peru Data
  # trait data
  tar_target(
    name = download_traits_pe,
    command = get_file(node = "gs8u6",
                       file = "PFTC3-Puna-PFTC5_Peru_2018-2020_LeafTraits_clean.csv",
                       path = "data/",
                       remote_path = "traits"),
    format = "file"
  ),
  
  # community data
  tar_target(
    name = download_community_pe,
    command = get_file(node = "gs8u6",
                       file = "PFTC3-Puna-PFTC5_Peru_2018-2020_CommunityCover_clean.csv",
                       path = "data",
                       remote_path = "community"),
    format = "file"
  ),
  
  # China Data
  # trait data
  tar_target(
    name = download_traits_leaf_ch,
    command = get_file(node = "f3knq",
                       file = "PFTC1.2_China_2015_2016_LeafTraits.csv",
                       path = "data/",
                       remote_path = "Traits"),
    format = "file"
  ),
  
  tar_target(
    name = download_traits_chem_ch,
    command = get_file(node = "f3knq",
                       file = "PFTC1.2_China_2015_2016_ChemicalTraits.csv",
                       path = "data/",
                       remote_path = "Traits"),
    format = "file"
  ),
  
  # community data
  tar_target(
    name = download_community_ch,
    command = get_file(node = "f3knq",
                       file = "transplant.sqlite",
                       path = "data",
                       remote_path = "Community"),
    format = "file"
  ),
  
  # meta data
  tar_target(
    name = download_meta_ch,
    command = "data/metaCH.csv",
    format = "file"
  ),
  
  
  # Norway Data
  # vegetation
  tar_target(
    name = download_community_no,
    command =  get_file(node = "pk4bg",
                        file = "Three-D_clean_cover_2019-2022.csv",
                        path = "data",
                        remote_path = "Vegetation"),
    format = "file"
  ),
  
  # Seedclim comm data for Høgsete and Vikesland
  # file needs to be unziped
  tar_target(
    name = download_community_no2,
    command =  get_file(node = "npfa9",
                        file = "seedclim.2020.4.15.zip",
                        path = "data",
                        remote_path = "3_Community_data"),
    format = "file"
  ),
  
  tar_target(
    name = download_sp_no,
    command =  get_file(node = "pk4bg",
                        file = "Three-D_clean_taxonomy.csv",
                        path = "data",
                        remote_path = "Vegetation"),
    format = "file"
  ),
  
  # traits
  tar_target(
    name = download_traits_no,
    command =  get_file(node = "fcbw4",
                        file = "PFTC6_ThreeD_clean_leaf_traits_2022.csv",
                        path = "data",
                        remote_path = "trait_data"),
    format = "file"
  ),
  
  # meta data
  tar_target(
    name = metaTurfID_no,
    command = create_threed_meta_data()
  )
  
)
