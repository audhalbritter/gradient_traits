# get bioclim data
library(geodata)
library(terra)


meta_scand <- community |> 
  filter(country %in% c("sv", "no")) |> 
  mutate(longitude_e = if_else(site == "sv_N_1", 15.343, longitude_e)) |> 
  distinct(country, gradient, site, plot_id, elevation_m, longitude_e, latitude_n)


coords_scand <- meta_scand |> 
  select(longitude_e, latitude_n) |> 
  as.matrix()

biomclim <- worldclim_tile("bio", 
                           lon = coords_scand[1, 1],
                           lat = coords_scand[1, 2], 
                           path = "WorldClimData")

scand <- terra::extract(biomclim, coords_scand)

scandinavia <- bind_cols(meta_scand, scand)


meta_ch <- community |> 
  filter(country %in% c("ch")) |> 
  distinct(country, gradient, site, plot_id, elevation_m, longitude_e, latitude_n)


coords_ch <- meta_ch |> 
  select(longitude_e, latitude_n) |> 
  as.matrix()

biomclim <- worldclim_tile("bio", 
                           lon = coords_ch[1, 1],
                           lat = coords_ch[1, 2], 
                           path = "WorldClimData")

china <- terra::extract(biomclim, coords_ch) |> 
  as_tibble()

china_data <- bind_cols(meta_ch, china)


meta_pe <- community |> 
  filter(country %in% c("pe")) |> 
  distinct(country, gradient, site, plot_id, elevation_m, longitude_e, latitude_n)

coords_pe <- meta_pe |> 
  select(longitude_e, latitude_n) |> 
  as.matrix()

biomclim <- worldclim_tile("bio", 
                           lon = coords_pe[1, 1],
                           lat = coords_pe[1, 2], 
                           path = "WorldClimData")

peru <- terra::extract(biomclim, coords_pe) |> 
  as_tibble()

peru_data <- bind_cols(meta_pe, peru)


# co
tar_load(coords_co)
coordinates_co <- coords_co |> 
  select(longitude_e, latitude_n) |> 
  as.matrix()

biomclim <- worldclim_tile("bio", 
                           lon = coordinates_co[1, 1],
                           lat = coordinates_co[1, 2], 
                           path = "WorldClimData")

colorado <- terra::extract(biomclim, coordinates_co) |> 
  as_tibble()

colorado_data <- bind_cols(coords_co, colorado)



bind_rows(scandinavia |> 
            rename_with(~gsub("tile_7_wc2.1_30s_", "", .x)),
          china_data |> 
            rename_with(~gsub("tile_34_wc2.1_30s_", "", .x)),
          peru_data |> 
            rename_with(~gsub("tile_40_wc2.1_30s_", "", .x)),
          colorado_data |> 
            rename_with(~gsub("tile_15_wc2.1_30s_", "", .x))) |> 
  write_csv("data/bioclim_new.csv")
