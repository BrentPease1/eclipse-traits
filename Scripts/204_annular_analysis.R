# Analyses for annular eclipse
## conduct Analysis 1 (logistic regression comparing eclipse day versus preceding day)
## conduct Analysis 2 (logistic GAM quantifying vocalization progression)
library(tidyverse)
library(mgcv)
library(glmmTMB)
library(brms)
library(hms)
library(here)

birdnet <- readr::read_csv( here::here("Data/merged_es_bw_apr2025_viirs.csv"))

bw_loc <- dplyr::filter(birdnet, data_source == "BW") |> 
  dplyr::filter( date == ymd("2023-10-13") | date == ymd("2023-10-14")) |>
  dplyr::select(Station,
                date,
                Latitude, 
                Longitude) |> 
  dplyr::distinct() |> 
  dplyr::group_by(date, Station) |> 
  dplyr::slice(1) |> 
  dplyr::ungroup()

bw <- filter(birdnet, data_source == "BW") |> 
  dplyr::filter( date == ymd("2023-10-13") | date == ymd("2023-10-14")) |>
  dplyr::select(-Latitude, -Longitude) |> 
  dplyr::left_join(bw_loc)

df <- dplyr::filter(birdnet, data_source == "ES") |> 
  dplyr::filter( date == ymd("2023-10-13") | date == ymd("2023-10-14")) |>
  dplyr::full_join(bw |> 
                     dplyr::filter( date == ymd("2023-10-13") | date == ymd("2023-10-14"))) |>
  dplyr::mutate(
    det_time = hms::as_hms(start_time),
    MaxEclipseTimeUTC = hms::as_hms(MaxEclipseTimeUTC),
    start = hms::as_hms( MaxEclipseTimeUTC - (2*60)), 
    end = hms::as_hms( MaxEclipseTimeUTC + (2*60))) |> 
  dplyr::filter(det_time >= start & det_time <= end) |> 
  dplyr::filter( date == ymd("2023-10-13") | date == ymd("2023-10-14")) |>
  dplyr::select( 
    lat = Latitude,
    lon = Longitude, 
    com = common_name, 
    sci = scientific_name, 
    det_time,
    MaxEclipseTimeUTC,
    date, 
    start, 
    end, 
    source = data_source, 
    alan = avg_rad,
    path_status, 
    dist_km, 
    coverage) |> 
  dplyr::filter(dist_km < 3000) |> 
  dplyr::filter(path_status == "within_path")

template <- dplyr::filter(birdnet, data_source == "ES") |> 
  dplyr::filter( date == ymd("2023-10-13") | date == ymd("2023-10-14")) |>
  dplyr::full_join(bw |> 
                     dplyr::filter( date == ymd("2023-10-13") | date == ymd("2023-10-14"))) |>
  dplyr::mutate(
    det_time = hms::as_hms(start_time),
    MaxEclipseTimeUTC = hms::as_hms(MaxEclipseTimeUTC),
    start = hms::as_hms( MaxEclipseTimeUTC - (30*60)), 
    end = hms::as_hms( MaxEclipseTimeUTC + (30*60))) |> 
  dplyr::filter(det_time >= start & det_time <= end) |> 
  dplyr::filter( date == ymd("2023-10-13") | date == ymd("2023-10-14")) |>
  dplyr::select( 
    lat = Latitude,
    lon = Longitude, 
    com = common_name, 
    sci = scientific_name,
    MaxEclipseTimeUTC,
    date, 
    source = data_source, 
    alan = avg_rad,
    path_status, 
    dist_km, 
    coverage) |> 
  dplyr::filter(dist_km < 3000) |> 
  dplyr::distinct() |>
  dplyr::filter(path_status == "within_path")

valid_combos <- dplyr::filter(birdnet, data_source == "ES") |>
  dplyr::filter(date == ymd("2023-10-13") | date == ymd("2023-10-14")) |> 
  dplyr::filter(path_status == "within_path") |> 
  dplyr::select( lat = Latitude, lon = Longitude, date) |> 
  dplyr::distinct() |> 
  dplyr::full_join(
    bw |> 
      dplyr::filter(date == ymd("2023-10-13") | date == ymd("2023-10-14")) |> 
      dplyr::filter( path_status == "within_path") |> 
      dplyr::select(lat = Latitude, lon = Longitude, date) |> 
      dplyr::distinct())

test <- df |>
  dplyr::filter(path_status == "within_path") |> 
  dplyr::select(lat, lon, coverage, dist_km, date, com, sci) |> 
  tibble::add_column(y = 1) |> 
  dplyr::distinct() |> 
  tidyr::pivot_wider(names_from = date, values_from = y)

final <- full_join(template, test) |> 
  dplyr::mutate(tm1 = tidyr::replace_na(`2023-10-13`, 0), 
                t = tidyr::replace_na(`2023-10-14`, 0)) |>
  dplyr::select(-`2023-10-13`, -`2023-10-14`) |> 
  tidyr::pivot_longer(tm1:t, names_to = "eclipse", values_to = "y") |> 
  dplyr::mutate(eclipse = factor(ifelse(eclipse == "t", 1, 0))) |> 
  dplyr::inner_join(valid_combos)

m <- glmmTMB::glmmTMB(
  y ~ 1 + eclipse + (1 + eclipse|com), 
  data = final,
  family = binomial(link = "logit"))

m_brms <- brms::brm(
  y ~ 1 + eclipse + (1+eclipse|com),
  data = final,
  family = bernoulli(link = "logit"),
  prior = c(prior(normal(0, 2), class = "Intercept"),
            prior(normal(0, 0.5), class = "b"),
            prior(exponential(1), class = "sd"),
            prior(lkj(2), class = cor)),
  control = list(adapt_delta = 0.95),
  chains = 4)

focal_time_detections <- dplyr::filter(birdnet, data_source == "ES") |> 
  dplyr::filter( date == ymd("2023-10-14")) |> 
  dplyr::full_join(bw |>
                     filter(date == ymd("2023-10-14"))) |> 
  dplyr::filter(path_status == "within_path") |> 
  dplyr::mutate( 
    det_time = hms::as_hms(start_time),
    MaxEclipseTimeUTC = hms::as_hms(MaxEclipseTimeUTC),
    start = hms::as_hms( MaxEclipseTimeUTC - (62*60)), 
    end = hms::as_hms( MaxEclipseTimeUTC + (62*60))) |> 
  dplyr::filter(det_time >= start & det_time <= end) |> 
  dplyr::filter( date == ymd("2023-10-14")) |> 
  dplyr::select( lat = Latitude,
                 lon = Longitude, 
                 MaxEclipseTimeUTC,
                 com = common_name, 
                 sci = scientific_name, 
                 det_time, 
                 date, 
                 start, 
                 end, 
                 source = data_source, 
                 alan = avg_rad)

bin_labs <- focal_time_detections |> 
  dplyr::mutate(time_rel_totality = as.numeric( difftime( det_time, MaxEclipseTimeUTC, units = "mins"))) |>
  dplyr::mutate(bin.lab = cut( time_rel_totality, 
                               breaks = seq(from = min(time_rel_totality), 
                                            to = max(time_rel_totality),
                                            by = 4),
                               include.lowest = TRUE)) |>
  dplyr::group_by(bin.lab) |> 
  dplyr::mutate(bin = dplyr::cur_group_id()) |> 
  dplyr::mutate(bin_rel = 4*(bin-16))

template <- bin_labs |> 
  dplyr::ungroup() |> 
  dplyr::select(lat, lon, com, sci) |> 
  dplyr::distinct() |> 
  dplyr::cross_join(
    bin_labs |> 
      dplyr::ungroup() |> 
      dplyr::select(bin.lab, bin, bin_rel) |> 
      dplyr::distinct())

bin_df <- bin_labs |>
  tibble::add_column(y01 = 1) |> 
  dplyr::group_by(com, sci, lat, lon, bin.lab, bin, bin_rel) |> 
  dplyr::summarise(y = unique(y01),
                   ndets = sum(y01)) |> 
  dplyr::full_join(template) |> 
  dplyr::mutate(y = tidyr::replace_na(y, 0),
                ndets = tidyr::replace_na(ndets, 0)) |>
  dplyr::group_by(com, sci) |> 
  dplyr::mutate(ny = sum(y)) |> 
  dplyr::arrange(lat, lon, com, bin)

final_bin <- bin_df |> 
  dplyr::filter(ny >= 5) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(bin.sc = as.numeric(scale(bin)),
                com = factor(com))

gs <- mgcv::bam(
  y ~ s(bin.sc, k = 8, m = 2) + s(bin.sc, com, bs = "fs", m = 2), 
  family = binomial(link = "logit"), 
  data = final_bin)

save(
  final,
  m, 
  m_brms,
  bin_df,
  final_bin, 
  gs,
  file = here::here("Results/annular_eclipse_results.RData"))