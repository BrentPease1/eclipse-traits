library(data.table)
library(lubridate)
library(here)
library(glmmTMB)
library(hms)
library(lutz)
library(suncalc)
library(janitor)
library(tidyverse)
library(mgcv)

# Read data
birdnet <- readr::read_csv( here::here("Data/merged_es_bw_apr2025_viirs.csv"))

# Fix BirdWeather location issues
# (each PUC will have multiple lat-lons due to GPS accuracy issues)
bw_loc <- dplyr::filter(birdnet, data_source == "BW") |> 
  dplyr::filter(date == ymd("2024-04-08")) |>
  dplyr::select(Station,
                date,
                Latitude, 
                Longitude) |> 
  dplyr::distinct() |> 
  dplyr::group_by(date, Station) |> 
  dplyr::slice(1) |> 
  dplyr::ungroup()

bw <- filter(birdnet, data_source == "BW") |> 
  dplyr::filter(date == ymd("2024-04-08")) |>
  dplyr::select(-Latitude, -Longitude) |> 
  dplyr::left_join(bw_loc)

# filter to day of eclipse, within path, within 62 minutes of peak eclipse time
focal_time_detections <- dplyr::filter(birdnet, data_source == "ES") |> 
  dplyr::filter( date == ymd("2024-04-08")) |> 
  dplyr::full_join(bw |>
                     filter(date == ymd("2024-04-08"))) |> 
  dplyr::filter(path_status == "within_path") |> 
  dplyr::mutate( 
    det_time = hms::as_hms(start_time),
    MaxEclipseTimeUTC = hms::as_hms(MaxEclipseTimeUTC),
    start = hms::as_hms( MaxEclipseTimeUTC - (62*60)), 
    end = hms::as_hms( MaxEclipseTimeUTC + (62*60))) |> 
  dplyr::filter(det_time >= start & det_time <= end) |> 
  dplyr::filter( date == ymd("2024-04-08")) |> 
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

# 4-minute time bins over the 124-minute time window centered on peak eclipse time
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

# template for species-site combinations
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
  dplyr::full_join(
    template) |> 
  dplyr::mutate(y = tidyr::replace_na(y, 0),
                ndets = tidyr::replace_na(ndets, 0)) |>
  dplyr::group_by(com, sci) |> 
  dplyr::mutate(ny = sum(y)) |> 
  dplyr::arrange(lat, lon, com, bin)

# elton traits for family name
elton <- data.table::fread(here::here('Data/elton.txt')) |> 
  dplyr::select(sci_elton = Scientific, noct = Nocturnal)

# key to resolve name idiosyncracies
key <- readr::read_csv( here::here("Data/birdweather_elton_botw_name_key.csv")) |> 
  dplyr::select( sci = sci_name_bw, sci_elton = sci_name_elton) |> 
  dplyr::mutate( sci_elton = stringr::str_replace_all(sci_elton, "\xa0", " "))

# eye size data 
eye.sp <- readr::read_csv( here::here("Data/ritland_clean.csv")) |>
  dplyr::select(sci_elton = species_jetz, cd1) |> 
  dplyr::left_join(key) |> 
  dplyr::filter(!is.na(sci)) |> 
  dplyr::select(sci, cd1) |> 
  dplyr::distinct()

# summarise eye data at fmaily level to impute for missing species
eye.fam <- readr::read_csv( here::here("Data/ritland_clean.csv")) |>
  dplyr::select(sci_elton = species_jetz, fam = family_jetz, cd1) |> 
  dplyr::left_join(key) |>
  dplyr::mutate(fam = ifelse(fam == "Emberizidae", "Passerellidae", fam)) |> 
  dplyr::mutate(fam = ifelse(fam == "Reguliidae", "Regulidae", fam)) |> 
  dplyr::mutate(fam = ifelse(sci_elton == "Pandion haliaetus", "Pandionidae", fam)) |> 
  dplyr::group_by(fam) |> 
  dplyr::summarise( cd1.fam = mean(cd1))

# avonet
avo <- readr::read_csv( here::here("Data/avonet.csv")) |> 
  dplyr::rename(sci_name = Species1) |> 
  dplyr::mutate( sci_name = ifelse(sci_name == "Passerella arborea", "Spizelloides arborea", sci_name)) |> 
  dplyr::mutate( sci_name = ifelse(sci_name =="Hesperiphona vespertina", "Coccothraustes vespertinus", sci_name)) |> 
  dplyr::mutate( sci_name = ifelse(sci_name =="Leuconotopicus villosus", "Dryobates villosus", sci_name)) |> 
  dplyr::mutate( sci_name = ifelse(sci_name =="Larus atricilla", "Leucophaeus atricilla", sci_name)) |> 
  dplyr::mutate( sci_name = ifelse(sci_name =="Hylatomus pileatus", "Dryocopus pileatus", sci_name)) |> 
  dplyr::mutate( sci_name = ifelse(sci_name =="Regulus calendula", "Corthylio calendula", sci_name)) |> 
  janitor::clean_names()

# nest type
cav <- readr::read_csv(here::here("Data/cavity.csv")) |> 
  dplyr::select(com = com_name, sci = sci_name_bw, cavity)

noct <- bin_df |> 
  dplyr::select(com, sci) |> 
  dplyr::distinct() |> 
  dplyr::left_join(key) |> 
  dplyr::left_join(elton) |> 
  dplyr::select(com, sci, noct) |> 
  dplyr::group_by(com, sci) |> 
  dplyr::summarise( noct = unique(noct)) |> 
  dplyr::ungroup()

# calculating proportion nocturnal activity 
# bw <- readr::read_csv( here::here("data/birdweather_apr24.csv"))
# es <- readr::read_csv( here::here("Data/birdnet_cleaned_v03.csv"))
# 
# combos <- bw |> 
#   dplyr::select( lat = Latitude, 
#                  lon = Longitude) |> 
#   dplyr::distinct() |>
#   dplyr::full_join(
#     es |> 
#       dplyr::select(lat = Latitude, 
#                     lon = Longitude) |> 
#       dplyr::distinct()) |> 
#   dplyr::cross_join(
#     tibble::tibble(
#       date = seq(from = lubridate::ymd("2024-03-31"), to = lubridate::ymd("2024-04-16"), by = 1)))
# 
# sr_ss <- suncalc::getSunlightTimes(
#   data = combos,
#   keep = c("sunrise",
#            "sunset"))
# 
# all_data <- full_join(
#   es |> 
#     janitor::clean_names() |> 
#     dplyr::select(com = common_name, 
#                   sci = scientific_name, 
#                   lat = latitude, 
#                   lon = longitude, 
#                   time = start_timestamp,
#                   conf = confidence),
#   
#   bw |> 
#     janitor::clean_names() |> 
#     dplyr::select(com = common_name, 
#                   sci = scientific_name, 
#                   lat = latitude, 
#                   lon = longitude, 
#                   time = timestamp,
#                   conf = confidence) |> 
#     dplyr::mutate(time = lubridate::parse_date_time(time, orders = "Ymd HMS z", tz = "UTC")))
# 
# tz <- all_data |> 
#   dplyr::select(lat, lon) |> 
#   dplyr::distinct() |> 
#   dplyr::mutate(tz = lutz::tz_lookup_coords(lat = lat, lon = lon, method = "fast"))
# 
# clean <- all_data |> 
#   dplyr::left_join(tz) |> 
#   dplyr::mutate(time_local = lubridate::with_tz(time, tzone = tz),
#                 date_local = lubridate::as_date(time)) |> 
#   dplyr::filter(!date_local == ymd("2024-04-08")) |> 
#   dplyr::select(-time_local,-date_local)
# 
# pnoct <- clean |> 
#   dplyr::mutate(date = lubridate::as_date(time)) |> 
#   dplyr::mutate(month = lubridate::month(date)) |> 
#   dplyr::filter(month == 4) |> 
#   dplyr::left_join(sr_ss) |> 
#   dplyr::filter(conf >= 0.65 ) |> 
#   dplyr::mutate( noct = ifelse( time < sunrise | time > sunset, 1, 0)) |>  
#   dplyr::filter(!is.na(noct)) |> 
#   dplyr::group_by(com, sci) |> 
#   dplyr::summarise( pnoct = sum(noct) / sum(!is.na(noct)))

# read in proportion nocturnal data
pnoct <- readr::read_csv( here::here("Data/pnoct_v01.csv"))

unique_sites <- focal_time_detections |> 
  # dplyr::filter(date == lubridate::ymd("2024-04-08")) |> 
  dplyr::select(lat, lon, MaxEclipseTimeUTC) |> 
  dplyr::distinct() |> 
  tibble::add_column(date = lubridate::ymd("2024-04-08")) |> 
  dplyr::select(lat, lon, date, MaxEclipseTimeUTC)

sr <- suncalc::getSunlightTimes(
  data = unique_sites, 
  keep = "sunrise",
  tz = "UTC")

# calculate time (relative to sunrise) that eclipse occurred
eclipse_time <- unique_sites |> 
  tibble::add_column(sunrise = sr$sunrise) |> 
  dplyr::mutate(eclipse = lubridate::as_datetime( paste(date, hms::as_hms(MaxEclipseTimeUTC)))) |> 
  dplyr::mutate( eclipse_dist_sunrise = as.numeric(difftime(eclipse,sunrise, units = "hours"))) |> 
  dplyr::select(lat, lon, eclipse_dist_sunrise) |> 
  dplyr::group_by(lat, lon) |> 
  dplyr::summarise( eclipse_dist_sunrise = mean(eclipse_dist_sunrise))

alan <- birdnet |> 
  dplyr::select(lat = Latitude, lon = Longitude, alan = avg_rad) |> 
  dplyr::distinct()

# final formatted data
final <- bin_df |> 
  dplyr::left_join(noct) |> 
  dplyr::left_join(avo |> 
                     dplyr::rename(sci = sci_name)) |> 
  dplyr::rename(fam = family1) |> 
  dplyr::left_join(eye.fam) |> 
  dplyr::left_join(eye.sp) |> 
  dplyr::left_join(pnoct) |> 
  dplyr::left_join(eclipse_time) |> 
  dplyr::left_join(cav) |> 
  dplyr::filter(ny >= 5) |>
  dplyr::ungroup() |> 
  dplyr::mutate(bin.sc = as.numeric(scale(bin))) |> 
  dplyr::mutate(com = factor(com)) |> 
  dplyr::group_by(lat, lon) |> 
  dplyr::mutate(site = dplyr::cur_group_id()) |> 
  dplyr::group_by(com) |> 
  dplyr::mutate(nsite = length(unique(site))) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(cd = ifelse(!is.na(cd1), cd1, cd1.fam)) |> 
  dplyr::mutate(cd_raw = cd) |> 
  dplyr::mutate( cd = as.numeric(scale(log(cd_raw)))) |> 
  dplyr::group_by( com, site) |> 
  dplyr::mutate(sp.station = dplyr::cur_group_id()) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(pnoct_raw = pnoct) |> 
  dplyr::mutate(pnoct = as.numeric(scale(pnoct))) |> 
  dplyr::mutate(migration = factor(migration)) |> 
  dplyr::mutate(hd = factor(habitat_density),
                cavity = factor(cavity)) |> 
  dplyr::left_join(alan) |> 
  dplyr::mutate(alan_raw = alan, 
                alan = as.numeric(scale(log1p(alan))),
                et = as.numeric(scale(eclipse_dist_sunrise)))

# took about 5 minutes on my fairly robust desktop
# general model with global smoother (across species) with species-level smooths
gs <- mgcv::bam(
  y ~ s(bin.sc, k = 8, m = 2) + s(bin.sc, com, bs = "fs", m = 2), 
  family = binomial(link = "logit"), 
  data = final)

# trait models
# these do NOT have species-specific smooths (species-station specific intercept though)
# instead the global smooth varies according to traits

# diel niche binary
dnb <- mgcv::bam(
  y ~ s(bin.sc, by = factor(noct), k = 8, bs = "fs") + factor(noct) + s(sp.station, bs = "re"),
  family = binomial(link = "logit"), 
  data = final)

# diel niche continuous
dnc <- mgcv::bam(
  y ~ s(bin.sc, by = pnoct, k = 8, bs = "tp") + pnoct + s(sp.station, bs = "re"),
  family = binomial(link = "logit"),
  data = final)

# eye size
es <- mgcv::bam(
  y ~ s(bin.sc, by = cd, k = 8, bs = "tp") + s(sp.station, bs = "re"),
  family = binomial(link = "logit"), 
  data = final)

# habitat density
hd <- mgcv::bam(
  y ~ s(bin.sc, by = hd, k = 8, bs = "tp", m = 1) + hd + s(sp.station, bs = "re"), 
  family = binomial(link = "logit"), 
  data = final)

# migraiton
mi <- mgcv::bam(
  y ~ s(bin.sc, by = migration, k = 8, bs = "tp", m = 1) + migration + s(sp.station, bs = "re"),
  family = binomial(link = "logit"),
  data = final)

# nest type
nt <- mgcv::bam(
  y ~ s(bin.sc, by = cavity, k = 8, bs = "tp", m = 1) + cavity + s(sp.station, bs = "re"), 
  family = binomial(link = "logit"), 
  data = final)

# light pollution
lp <- mgcv::bam(
  y ~ s(bin.sc, by = alan, k = 8, bs = "tp") + alan + s(sp.station, bs = "re"),
  family = binomial(link = "logit"), 
  data = final)

# eclipse time
et <- mgcv::bam(
  y ~ s(bin.sc, by = et, k = 8, bs = "tp") + et + s(sp.station, bs = "re"),
  family = binomial(link = "logit"), 
  data = filter(final, !is.na(et)))

# save model results
save(
  final,
  bin_df,
  gs,  # general model (global smooth with species-level smooths)
  dnb, # diel niche binary
  dnc, # diel niche continuous
  es,  # eye size
  hd,  # habitat density
  mi,  # migration
  nt,  # nest type
  lp,  # light pollution
  et,  # eclipse time
  file = here::here("Results/total_eclipse_vocalization_progression.RData"))