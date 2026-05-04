#' Downscaled climate aligned to trait/community **sites**.
#'
#' The CSV repeats similar values across plots within a site; the extract also
#' omits some plot IDs (e.g. Norway turf IDs that appear only for certain grazing
#' levels). Aggregating to `country`, `gradient`, and `site` matches how traits
#' are keyed and avoids losing rows when joining climate to analyses.

#' Add `country`, `gradient`, and `site` from raw `area` / `plot_id`.
#'
#' `site` strings match those built in [cleaning_functions.R] (e.g. `no_Liahovden`,
#' `co_CBT`, `pe_B_ACJ`, `sv_C_2`).
#'
#' @return Input plus `area_raw`, `plot_id_raw`, `country`, `gradient`, `site`.
downscaled_climate_add_site_keys <- function(dat) {
  dat |>
    rename(area_raw = area, plot_id_raw = plot_id) |>
    mutate(
      country = case_match(area_raw,
        "China" ~ "ch",
        "Colorado" ~ "co",
        "Norway" ~ "no",
        "Peru" ~ "pe",
        "SouthAfrica" ~ "sa",
        "Svalbard" ~ "sv",
        .default = NA_character_
      )
    ) |>
    mutate(
      site = pmap_chr(list(country, plot_id_raw), dc_trait_site_chr),
      gradient = pmap_chr(list(country, plot_id_raw), dc_trait_gradient_chr)
    )
}


dc_trait_site_chr <- function(cnt, raw) {
  if (is.na(cnt) || is.na(raw)) {
    return(NA_character_)
  }
  dc_trait_site_gradient(cnt, raw)$site
}


dc_trait_gradient_chr <- function(cnt, raw) {
  if (is.na(cnt) || is.na(raw)) {
    return(NA_character_)
  }
  dc_trait_site_gradient(cnt, raw)$gradient
}


dc_trait_site_gradient <- function(cnt, raw) {
  out <- list(site = NA_character_, gradient = NA_character_)
  if (is.na(cnt) || is.na(raw)) {
    return(out)
  }

  if (cnt == "ch") {
    m <- str_match(raw, "^CH_([AMLH])")
    if (!is.na(m[1, 2])) {
      out$site <- paste0("ch_", m[1, 2])
      out$gradient <- "C"
    }
    return(out)
  }

  if (cnt == "co") {
    out$gradient <- "C"
    out$site <- case_when(
      str_detect(raw, "^US_cbt") ~ "co_CBT",
      str_detect(raw, "^US_almont") ~ "co_Almont",
      str_detect(raw, "^US_cinnamon") ~ "co_Cinnamon",
      str_detect(raw, "^US_pfeiler") ~ "co_Pfeiler",
      str_detect(raw, "^US_road") ~ "co_Road",
      str_detect(raw, "^US_pbm") ~ "co_PBM",
      TRUE ~ NA_character_
    )
    return(out)
  }

  if (cnt == "no") {
    out$gradient <- "C"
    out$site <- case_when(
      str_detect(raw, "^NO_Lia_") ~ "no_Liahovden",
      str_detect(raw, "^NO_Joa_") ~ "no_Joasete",
      str_detect(raw, "^NO_Hog_") ~ "no_Hogsete",
      str_detect(raw, "^NO_Vik_") ~ "no_Vikesland",
      TRUE ~ NA_character_
    )
    return(out)
  }

  if (cnt == "pe") {
    return(pe_trait_site_gradient_from_raw(raw))
  }

  if (cnt == "sa") {
    return(sa_trait_site_gradient_from_raw(raw))
  }

  if (cnt == "sv") {
    return(sv_trait_site_gradient_from_raw(raw))
  }

  out
}


pe_trait_site_gradient_from_raw <- function(raw) {
  out <- list(site = NA_character_, gradient = NA_character_)
  if (!str_detect(raw, "^PE_")) {
    return(out)
  }
  core <- str_sub(raw, 4L)
  parts <- str_split(core, "_")[[1]]
  n <- length(parts)
  if (n < 3L) {
    return(out)
  }
  grad <- parts[n - 1L]
  sitecode <- str_flatten(parts[seq_len(n - 2L)], collapse = "_")
  out$site <- paste0("pe_", grad, "_", sitecode)
  out$gradient <- grad
  out
}


sa_trait_site_gradient_from_raw <- function(raw) {
  out <- list(site = NA_character_, gradient = NA_character_)
  m <- str_match(raw, "^SA_(\\d+)(east|west)")
  if (any(is.na(m[1, ]))) {
    return(out)
  }
  elev <- m[1, 2]
  asp <- m[1, 3]
  lu <- c(`2000` = 1L, `2200` = 2L, `2400` = 3L, `2600` = 4L, `2800` = 5L)
  sid <- lu[[elev]]
  if (is.na(sid)) {
    return(out)
  }
  out$site <- paste0("sa_", sid)
  out$gradient <- if (asp == "east") "E" else "W"
  out
}


sv_trait_site_gradient_from_raw <- function(raw) {
  out <- list(site = NA_character_, gradient = NA_character_)
  core <- str_remove(raw, "^SV_")
  if (str_detect(core, "^C\\d+")) {
    m <- str_match(core, "^C(\\d+)")
    if (!is.na(m[1, 2])) {
      out$site <- paste0("sv_C_", m[1, 2])
      out$gradient <- "C"
    }
    return(out)
  }
  if (str_detect(core, "^B\\d+")) {
    m <- str_match(core, "^B(\\d+)")
    if (!is.na(m[1, 2])) {
      out$site <- paste0("sv_N_B", m[1, 2])
      out$gradient <- "N"
    }
    return(out)
  }
  out
}


#' Average numeric climate columns to one row per `country` × `gradient` × `site`.
summarise_downscaled_climate_by_site <- function(dat) {
  numeric_cols <- dat |>
    select(where(is.numeric)) |>
    names()

  if (length(numeric_cols) == 0L) {
    return(dat |> filter(FALSE))
  }

  dat |>
    filter(!is.na(site)) |>
    summarise(
      area_raw = dplyr::first(area_raw),
      n_raw_plot_cells = dplyr::n(),
      plot_id_raw = str_flatten(unique(plot_id_raw), collapse = "; "),
      across(all_of(numeric_cols), \(x) mean(x, na.rm = TRUE)),
      .by = c(country, gradient, site)
    )
}


#' Fill trait sites still missing after aggregation (e.g. `sv_C_1` absent from CSV).
#'
#' @param site_tbl Output from [summarise_downscaled_climate_by_site()].
#' @param traits Trait table; uses `country`, `gradient`, `site`.
complete_downscaled_climate_sites <- function(site_tbl, traits) {
  numeric_cols <- site_tbl |>
    select(where(is.numeric)) |>
    names()

  need <- traits |>
    distinct(country, gradient, site) |>
    filter(!is.na(site))

  out <- site_tbl

  missing <- need |>
    anti_join(out, by = join_by(country, gradient, site))

  fill_sv <- complete_dc_sv_site_fallback(out, missing, numeric_cols)
  out <- bind_rows(out, fill_sv)

  missing <- need |>
    anti_join(out, by = join_by(country, gradient, site))

  fill_sa_c <- complete_dc_sa_c_gradient_fallback(out, missing, numeric_cols)
  out <- bind_rows(out, fill_sa_c)

  missing <- need |>
    anti_join(out, by = join_by(country, gradient, site))

  fill_sa_rest <- complete_dc_sa_residual_fallback(out, missing, numeric_cols)
  bind_rows(out, fill_sa_rest)
}


#' Any remaining SA keys (e.g. `site` `sa_NA`) — mean climate across all SA rows in the extract.
complete_dc_sa_residual_fallback <- function(site_tbl, missing_trait_sites, numeric_cols) {
  if (length(numeric_cols) == 0L) {
    return(tibble())
  }

  ms <- missing_trait_sites |>
    filter(country == "sa")

  if (nrow(ms) == 0L) {
    return(tibble())
  }

  ref <- site_tbl |>
    filter(country == "sa")

  if (nrow(ref) == 0L) {
    return(tibble())
  }

  fb <- ref |>
    summarise(across(all_of(numeric_cols), \(x) mean(x, na.rm = TRUE)))

  out <- ms
  for (nm in numeric_cols) {
    out[[nm]] <- fb[[nm]][[1]]
  }

  out |>
    mutate(
      area_raw = "SouthAfrica",
      n_raw_plot_cells = NA_integer_,
      plot_id_raw = paste0("filled_mean_all_sa_for_", site, "_grad_", gradient)
    )
}


#' Traits sometimes use gradient `C` for SA while the extract only has east/west.
#' Use the mean of `E` and `W` climate at the same `site`.
complete_dc_sa_c_gradient_fallback <- function(site_tbl, missing_trait_sites, numeric_cols) {
  if (length(numeric_cols) == 0L) {
    return(tibble())
  }

  ms <- missing_trait_sites |>
    filter(country == "sa", gradient == "C")

  if (nrow(ms) == 0L) {
    return(tibble())
  }

  ew <- site_tbl |>
    filter(country == "sa", gradient %in% c("E", "W"))

  if (nrow(ew) == 0L) {
    return(tibble())
  }

  site_ew <- ew |>
    summarise(across(all_of(numeric_cols), \(x) mean(x, na.rm = TRUE)), .by = site)

  joined <- ms |>
    inner_join(site_ew, by = "site")

  if (nrow(joined) == 0L) {
    return(tibble())
  }

  joined |>
    mutate(
      country = "sa",
      gradient = "C",
      area_raw = "SouthAfrica",
      n_raw_plot_cells = NA_integer_,
      plot_id_raw = paste0("filled_mean_E_plus_W_for_", site)
    )
}


complete_dc_sv_site_fallback <- function(site_tbl, missing_trait_sites, numeric_cols) {
  if (length(numeric_cols) == 0L) {
    return(tibble())
  }

  ms <- missing_trait_sites |>
    filter(country == "sv", str_detect(site, "^sv_C_"))

  if (nrow(ms) == 0L) {
    return(tibble())
  }

  sv_ref <- site_tbl |>
    filter(country == "sv", str_detect(site, "^sv_C_"))

  if (nrow(sv_ref) == 0L) {
    return(tibble())
  }

  fb <- sv_ref |>
    summarise(across(all_of(numeric_cols), \(x) mean(x, na.rm = TRUE)))

  out <- ms |>
    mutate(
      area_raw = "Svalbard",
      n_raw_plot_cells = NA_integer_,
      plot_id_raw = paste0("filled_mean_all_sv_C_sites_for_", site)
    )

  for (nm in numeric_cols) {
    out[[nm]] <- fb[[nm]][[1]]
  }

  out
}
