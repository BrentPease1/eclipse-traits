# coverage analysis for total and annular eclipses
library(tidyverse)
library(mgcv)
library(here)

birdnet <- readr::read_csv( here::here("Data/merged_es_bw_apr2025_viirs.csv"))

bw_loc <- dplyr::filter(birdnet, data_source == "BW") |> 
  dplyr::filter( date == ymd("2023-10-14") | date == ymd("2024-04-08")) |>
  dplyr::select(Station,
                date,
                Latitude, 
                Longitude) |> 
  dplyr::distinct() |> 
  dplyr::group_by(date, Station) |> 
  dplyr::slice(1) |> 
  dplyr::ungroup()

bw <- dplyr::filter(birdnet, data_source == "BW") |> 
  dplyr::filter( date == ymd("2023-10-14") | date == ymd("2024-04-08")) |>
  dplyr::select(-Latitude, -Longitude) |> 
  dplyr::left_join(bw_loc)

df_prog <- dplyr::filter(birdnet, data_source == "ES") |> 
  dplyr::filter( date == ymd("2023-10-14") | date == ymd("2024-04-08")) |> 
  dplyr::full_join(bw |> 
                     dplyr::filter( date == ymd("2023-10-14") | date == ymd("2024-04-08"))) |> 
  dplyr::mutate(
    det_time = hms::as_hms(start_time),
    MaxEclipseTimeUTC = hms::as_hms(MaxEclipseTimeUTC),
    start = hms::as_hms( MaxEclipseTimeUTC - (62*60)), 
    end = hms::as_hms( MaxEclipseTimeUTC + (62*60))) |> 
  dplyr::filter(det_time >= start & det_time <= end) |> 
  dplyr::filter( date == ymd("2023-10-14") | date == ymd("2024-04-08")) |> 
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
  dplyr::filter(dist_km < 3000)

bin_labs_an <- df_prog |>
  dplyr::filter(date == "2023-10-14") |> 
  dplyr::mutate(time_rel_totality = as.numeric( difftime( det_time, MaxEclipseTimeUTC, units = "mins"))) |>
  dplyr::mutate(bin.lab = cut( time_rel_totality, 
                               breaks = seq(from = min(time_rel_totality), 
                                            to = max(time_rel_totality),
                                            by = 4),
                               include.lowest = TRUE)) |>
  dplyr::group_by(bin.lab) |> 
  dplyr::mutate(bin = dplyr::cur_group_id()) |> 
  dplyr::mutate(bin_rel = 4*(bin-16))

bin_labs_to <- df_prog |>
  dplyr::filter(date == "2024-04-08") |> 
  dplyr::mutate(time_rel_totality = as.numeric( difftime( det_time, MaxEclipseTimeUTC, units = "mins"))) |>
  dplyr::mutate(bin.lab = cut( time_rel_totality, 
                               breaks = seq(from = min(time_rel_totality), 
                                            to = max(time_rel_totality),
                                            by = 4),
                               include.lowest = TRUE)) |>
  dplyr::group_by(bin.lab) |> 
  dplyr::mutate(bin = dplyr::cur_group_id()) |> 
  dplyr::mutate(bin_rel = 4*(bin-16))

template_an <- bin_labs_an |> 
  dplyr::ungroup() |> 
  dplyr::select(lat, lon, coverage, com, sci, MaxEclipseTimeUTC) |> 
  dplyr::distinct() |> 
  dplyr::cross_join(
    bin_labs_an |> 
      dplyr::ungroup() |> 
      dplyr::select(bin.lab, bin, bin_rel) |> 
      dplyr::distinct())

template_to <- bin_labs_to |> 
  dplyr::ungroup() |> 
  dplyr::select(lat, lon, coverage, com, sci, MaxEclipseTimeUTC) |> 
  dplyr::distinct() |> 
  dplyr::cross_join(
    bin_labs_to |> 
      dplyr::ungroup() |> 
      dplyr::select(bin.lab, bin, bin_rel) |> 
      dplyr::distinct())

bin_df_an <- bin_labs_an |>
  tibble::add_column(y01 = 1) |> 
  dplyr::group_by(com, sci, lat, lon, coverage, bin.lab, bin, bin_rel) |> 
  dplyr::summarise(y = unique(y01),
                   ndets = sum(y01)) |> 
  dplyr::full_join(template_an) |> 
  dplyr::mutate(y = tidyr::replace_na(y, 0),
                ndets = tidyr::replace_na(ndets, 0)) |>
  dplyr::group_by(com, sci) |> 
  dplyr::mutate(ny = sum(y)) |> 
  dplyr::arrange(lat, lon, com, bin) |> 
  dplyr::filter(ny >= 5) |> 
  dplyr::group_by(lat, lon, com) |> 
  dplyr::mutate(sp.station = dplyr::cur_group_id()) |> 
  dplyr::ungroup() |> 
  dplyr::mutate( bin.sc = as.numeric(scale(bin)), 
                 coverage.sc = as.numeric(scale(coverage)))

# an_sites <- bin_df_an |> 
#   dplyr::select(lat, lon, MaxEclipseTimeUTC, coverage) |> 
#   dplyr::distinct() |> 
#   add_column(date = ymd("2023-10-14"))
# 
# suncalc::getSunlightTimes( data = an_sites, keep = "sunrise") |>
#   dplyr::select(sunrise) |>
#   bind_cols(an_sites) |>
#   as_tibble() |>
#   mutate( et = as_datetime(paste(date, MaxEclipseTimeUTC))) |>
#   mutate( sr_et  = as.numeric(difftime( et, sunrise, units = "hours"))) |>
#   dplyr::filter(coverage < 33) |> 
#   dplyr::pull(sr_et) |> 
#   mean()
#   # select(sr_et, coverage) |>
#   # cor()

bin_df_to <- bin_labs_to |>
  tibble::add_column(y01 = 1) |> 
  dplyr::group_by(com, sci, lat, lon, coverage, bin.lab, bin, bin_rel) |> 
  dplyr::summarise(y = unique(y01),
                   ndets = sum(y01)) |> 
  dplyr::full_join(template_to) |> 
  dplyr::mutate(y = tidyr::replace_na(y, 0),
                ndets = tidyr::replace_na(ndets, 0)) |>
  dplyr::group_by(com, sci) |> 
  dplyr::mutate(ny = sum(y)) |> 
  dplyr::arrange(lat, lon, com, bin) |> 
  dplyr::filter(ny >= 5) |> 
  dplyr::group_by(lat, lon, com) |> 
  dplyr::mutate(sp.station = dplyr::cur_group_id()) |> 
  dplyr::ungroup() |> 
  dplyr::mutate( bin.sc = as.numeric(scale(bin)), 
                 coverage.sc = as.numeric(scale(coverage))) 

# write out unique locations for use in creating Figure 1
# setwd(here::here("data"))
# bin_df_to |> 
#   dplyr::select(lat, lon) |> 
#   dplyr::distinct() |> 
#   tibble::add_column( type = "total") |> 
#   dplyr::full_join(
#     bin_df_an |> 
#       dplyr::select(lat, lon) |> 
#       dplyr::distinct() |> 
#       tibble::add_column( type = "annular")) |> 
#   readr::write_csv( "unique_locations.csv" )

annular <- mgcv::bam(
  y ~ te(bin.sc, coverage.sc) + s(sp.station, bs = "re"),
  family = binomial(link = "logit"),
  data = bin_df_an)

total <- mgcv::bam(
  y ~ te(bin.sc, coverage.sc) + s(sp.station, bs = "re"), 
  family = binomial(link = "logit"), 
  data = bin_df_to)

save(
  bin_df_an, 
  bin_df_to, 
  annular,
  total, 
  file = here::here("Results/coverage_analysis_total_and_annular.RData"))