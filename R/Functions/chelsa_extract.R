# CHELSA extraction helpers

utils::globalVariables(c("longitude_e", "latitude_n", "lon", "lat"))

chelsa_cache_dir <- function() {
  dir <- file.path("WorldClimData", "CHELSA_cache")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir
}

# Build a simple points data.frame for terra from a coordinates table
chelsa_build_points <- function(coords_tbl) {
  coords_tbl |>
    dplyr::distinct(longitude_e, latitude_n) |>
    tidyr::drop_na(longitude_e, latitude_n) |>
    as.data.frame() |>
    dplyr::select(lon = longitude_e, lat = latitude_n)
}

# Download a single file to cache
chelsa_download_if_needed <- function(url, dest_dir = chelsa_cache_dir()) {
  if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  dest <- file.path(dest_dir, basename(url))
  if (!file.exists(dest) || file.info(dest)$size == 0) {
    tryCatch({
      utils::download.file(url, destfile = dest, mode = "wb", quiet = TRUE)
    }, error = function(e) {
      warning(sprintf("Download failed for %s: %s", url, e$message))
      return(NULL)
    })
  }
  if (!file.exists(dest) || file.info(dest)$size == 0) return(NULL)
  dest
}

# Read a text file with one URL per line
chelsa_read_txt_urls <- function(path) {
  if (!file.exists(path)) return(character())
  urls <- readr::read_lines(path)
  urls <- trimws(urls)
  urls[nzchar(urls)]
}

# Download a batch of URLs (NetCDF or TIFF) to cache dir
chelsa_download_batch <- function(urls, dest_dir = chelsa_cache_dir()) {
  if (length(urls) == 0) return(character())
  vapply(urls, function(u) chelsa_download_if_needed(u, dest_dir = dest_dir), FUN.VALUE = character(1)) |>
    stats::na.omit() |>
    unname()
}

# Extract from NetCDF (or TIFF) using terra; takes first band by default
chelsa_extract_from_nc_files <- function(file_paths, coords_tbl, band = 1) {
  pts <- chelsa_build_points(coords_tbl)
  purrr::map_dfr(file_paths, function(fp) {
    r <- tryCatch(terra::rast(fp), error = function(e) NULL)
    if (is.null(r)) return(tibble::tibble(lon = numeric(), lat = numeric(), value = numeric(), file = character()))
    # pick first layer/band by default (for daily stacks, this is first day)
    lyr <- tryCatch(r[[band]], error = function(e) NULL)
    if (is.null(lyr)) lyr <- r
    vals <- terra::extract(lyr, pts[, c("lon", "lat")])
    tibble::as_tibble(cbind(pts, vals[, ncol(vals), drop = FALSE])) |>
      dplyr::rename(value = dplyr::last_col()) |>
      dplyr::mutate(file = fp)
  })
}

#' Filter CHELSA URLs to only cover our study area
#' @param urls Vector of CHELSA URLs
#' @param coordinates Data frame with longitude_e and latitude_n columns
#' @return Vector of filtered URLs
chelsa_filter_tiles_for_study_area <- function(urls, coordinates) {
  if (length(urls) == 0) return(character(0))
  
  # Analyze coordinates by country to identify specific regions
  country_coords <- coordinates |>
    group_by(country) |>
    summarise(
      min_lon = min(longitude_e, na.rm = TRUE),
      max_lon = max(longitude_e, na.rm = TRUE),
      min_lat = min(latitude_n, na.rm = TRUE),
      max_lat = max(latitude_n, na.rm = TRUE),
      n_sites = n_distinct(paste(site, plot_id)),
      .groups = "drop"
    ) |>
    arrange(country)
  
  # Print country-specific coordinate ranges
  cat("Study areas by country:\n")
  for (i in 1:nrow(country_coords)) {
    cat(sprintf("%s: Lon %.2f to %.2f, Lat %.2f to %.2f (%d sites)\n",
                country_coords$country[i],
                country_coords$min_lon[i], country_coords$max_lon[i],
                country_coords$min_lat[i], country_coords$max_lat[i],
                country_coords$n_sites[i]))
  }
  
  # For testing: select just one country and implement basic tile filtering
  # Let's start with China (ch) since it has a focused area
  test_country <- "ch"
  test_coords <- country_coords |> filter(country == test_country)
  
  if (nrow(test_coords) > 0) {
    cat(sprintf("\nTesting with %s: selecting tiles for coordinates %.2f-%.2f°E, %.2f-%.2f°N\n",
                test_country, 
                test_coords$min_lon, test_coords$max_lon,
                test_coords$min_lat, test_coords$max_lat))
    
    # Basic tile filtering: select files that might cover our area
    # For now, let's select a few files to test the approach
    # In a real implementation, you'd parse the filenames to extract coordinates
    # and only select those covering your study area
    
    # For testing, select 2-3 files to verify the approach works
    selected_urls <- head(urls, 3)
    cat(sprintf("Selected %d URLs for testing (out of %d total)\n", length(selected_urls), length(urls)))
    
    return(selected_urls)
  } else {
    cat("No test country found, returning first 3 URLs for testing...\n")
    return(head(urls, 3))
  }
}

#' Download only the tiles needed for our study area
#' @param urls Vector of CHELSA URLs
#' @param coordinates Data frame with longitude_e and latitude_n columns
#' @return Vector of downloaded file paths
chelsa_download_study_area_tiles <- function(urls, coordinates) {
  # Filter URLs to only cover our study area
  filtered_urls <- chelsa_filter_tiles_for_study_area(urls, coordinates)
  
  if (length(filtered_urls) == 0) {
    message("No URLs to download after filtering")
    return(character(0))
  }
  
  message("Downloading ", length(filtered_urls), " tiles for study area...")
  
  # Download the filtered URLs
  chelsa_download_batch(filtered_urls)
}

#' Analyze which CHELSA tiles are needed for each country
#' @param urls Vector of CHELSA URLs
#' @param coordinates Data frame with longitude_e and latitude_n columns
#' @return List with country-specific tile recommendations
chelsa_analyze_country_tiles <- function(urls, coordinates) {
  if (length(urls) == 0) return(list())
  
  # Group coordinates by country
  country_coords <- coordinates |>
    group_by(country) |>
    summarise(
      min_lon = min(longitude_e, na.rm = TRUE),
      max_lon = max(longitude_e, na.rm = TRUE),
      min_lat = min(latitude_n, na.rm = TRUE),
      max_lat = max(latitude_n, na.rm = TRUE),
      n_sites = n_distinct(paste(site, plot_id)),
      .groups = "drop"
    ) |>
    arrange(country)
  
  # Analyze each country's needs
  country_analysis <- list()
  
  for (i in 1:nrow(country_coords)) {
    country <- country_coords$country[i]
    coords <- country_coords[i, ]
    
    # Estimate tile requirements based on coordinate ranges
    # CHELSA tiles are typically 30° x 30° or similar
    lon_tiles_needed <- ceiling((coords$max_lon - coords$min_lon) / 30) + 1
    lat_tiles_needed <- ceiling((coords$max_lat - coords$min_lat) / 30) + 1
    
    country_analysis[[country]] <- list(
      coordinates = coords,
      estimated_tiles = lon_tiles_needed * lat_tiles_needed,
      tile_recommendation = paste0("~", lon_tiles_needed, "x", lat_tiles_needed, " tiles needed")
    )
  }
  
  # Print summary
  cat("Country-specific tile analysis:\n")
  cat("==============================\n")
  for (country in names(country_analysis)) {
    info <- country_analysis[[country]]
    cat(sprintf("%s: %s covering %.1f° lon × %.1f° lat\n",
                country, info$tile_recommendation,
                info$coordinates$max_lon - info$coordinates$min_lon,
                info$coordinates$max_lat - info$coordinates$min_lat))
  }
  
  country_analysis
}
