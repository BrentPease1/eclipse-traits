library(tidyverse)
library(glmmTMB)
library(brms)
library(posterior)
library(here)
library(sf)
library(rnaturalearth)
library(data.table)

conus84 <- rnaturalearth::ne_states(iso_a2 = "US") |> 
  dplyr::summarise(geometry = sf::st_union(geometry))

birdnet <- readr::read_csv( here::here("Data/merged_es_bw_apr2025_viirs.csv"))

# Fix BirdWeather location issues
# (each PUC will have multiple lat-lons due to GPS accuracy issues)
bw_loc <- dplyr::filter(birdnet, data_source == "BW") |> 
  dplyr::filter(date == ymd("2024-04-08") | date == ymd("2024-04-07")) |>
  dplyr::select(Station,
                date,
                Latitude, 
                Longitude) |> 
  dplyr::distinct() |> 
  dplyr::group_by(date, Station) |> 
  dplyr::slice(1) |> 
  dplyr::ungroup()

# patch in with udpated lat-lons
bw <- filter(birdnet, data_source == "BW") |> 
  dplyr::filter(date == ymd("2024-04-08") | date == ymd("2024-04-07")) |>
  dplyr::select(-Latitude, -Longitude) |> 
  dplyr::left_join(bw_loc)

# focus during 4-minute time period centered on maximum eclipse time on day of and previous day
focal_time_detections <- dplyr::filter(birdnet, data_source == "ES") |>
  dplyr::filter(date == ymd("2024-04-08") | date == ymd("2024-04-07")) |>
  dplyr::full_join(bw |>
                     filter(date == ymd("2024-04-08") | date == ymd("2024-04-07"))) |> 
  dplyr::filter(path_status == "within_path") |> 
  dplyr::mutate( 
    det_time = hms::as_hms(start_time),
    MaxEclipseTimeUTC = hms::as_hms(MaxEclipseTimeUTC),
    start = hms::as_hms( MaxEclipseTimeUTC - 120), 
    end = hms::as_hms( MaxEclipseTimeUTC + 120)) |> 
  dplyr::filter(det_time >= start & det_time <= end) |> 
  dplyr::filter( date == ymd("2024-04-07") | date == ymd("2024-04-08")) |> 
  dplyr::select( lat = Latitude,
                 lon = Longitude, 
                 com = common_name, 
                 sci = scientific_name, 
                 det_time, 
                 date, 
                 start, 
                 end, 
                 source = data_source, 
                 alan = avg_rad)

# detections during focal time window
test <- focal_time_detections |> 
  dplyr::select(lat, lon, com, sci, date) |> 
  dplyr::distinct() |> 
  tibble::add_column(y = 1) |> 
  tidyr::pivot_wider(names_from = date, values_from = y) |> 
  dplyr::rename( tm1 = `2024-04-07`,
                 t = `2024-04-08`) |> 
  dplyr::mutate(across(c(tm1, t), function(x) replace_na(x, 0)))

# unique locations
unique_sites <- dplyr::filter(birdnet, data_source == "ES") |>
  dplyr::filter(date == ymd("2024-04-08") | date == ymd("2024-04-07")) |>
  dplyr::full_join(bw |>
                     filter(date == ymd("2024-04-08") | date == ymd("2024-04-07"))) |> 
  dplyr::filter(path_status == "within_path") |> 
  dplyr::mutate( 
    det_time = hms::as_hms(start_time),
    MaxEclipseTimeUTC = hms::as_hms(MaxEclipseTimeUTC),
    start = hms::as_hms( MaxEclipseTimeUTC - 1800), 
    end = hms::as_hms( MaxEclipseTimeUTC + 1800)) |> 
  dplyr::filter(det_time >= start & det_time <= end) |> 
  dplyr::filter(date == ymd("2024-04-08") | date == ymd("2024-04-07")) |>
  dplyr::select(lat = Latitude,
                lon = Longitude, 
                date,
                MaxEclipseTimeUTC) |> 
  dplyr::distinct() 

# species-location combinations
valid_combos <- dplyr::filter(birdnet, data_source == "ES") |>
  dplyr::filter(date == ymd("2024-04-08") | date == ymd("2024-04-07")) |> 
  dplyr::filter(path_status == "within_path") |> 
  dplyr::select( lat = Latitude, lon = Longitude, date) |> 
  dplyr::distinct() |> 
  dplyr::full_join(
    bw |> 
      dplyr::filter(date == ymd("2024-04-08") | date == ymd("2024-04-07")) |> 
      dplyr::filter( path_status == "within_path") |> 
      dplyr::select(lat = Latitude, lon = Longitude, date) |> 
      dplyr::distinct())

# detection template: possible species-site combinations
# "eligible" species are those detected within 30 minutes of eclipse max on day of or day before
template <- dplyr::filter(birdnet, data_source == "ES") |>
  dplyr::filter(date == ymd("2024-04-08") | date == ymd("2024-04-07")) |>
  dplyr::full_join(bw |>
                     filter(date == ymd("2024-04-08") | date == ymd("2024-04-07"))) |> 
  dplyr::filter(path_status == "within_path") |> 
  dplyr::mutate( 
    det_time = hms::as_hms(start_time),
    MaxEclipseTimeUTC = hms::as_hms(MaxEclipseTimeUTC),
    start = hms::as_hms( MaxEclipseTimeUTC - 1800), 
    end = hms::as_hms( MaxEclipseTimeUTC + 1800)) |> 
  dplyr::filter(det_time >= start & det_time <= end) |> 
  dplyr::filter( date == ymd("2024-04-07") | date == ymd("2024-04-08")) |> 
  dplyr::select( lat = Latitude,
                 lon = Longitude, 
                 com = common_name, 
                 sci = scientific_name, 
                 MaxEclipseTimeUTC,
                 source = data_source, 
                 alan = avg_rad) |> 
  dplyr::distinct()

# elton traits for family name
elton <- data.table::fread(here::here('Data/elton.txt')) |> 
  dplyr::select(sci_elton = Scientific, noct = Nocturnal)

# crosswalk keys for resolving name idiosyncracies
key <- readr::read_csv( here::here("Data/birdweather_elton_botw_name_key.csv")) |> 
  dplyr::select( sci = sci_name_bw, sci_elton = sci_name_elton) |> 
  dplyr::mutate( sci_elton = stringr::str_replace_all(sci_elton, "\xa0", " "))

# Stanley Ritland's eye size data; did some cleaning/prep before hand
eye.sp <- readr::read_csv( here::here("Data/ritland_clean.csv")) |>
  dplyr::select(sci_elton = species_jetz, cd1) |> 
  dplyr::left_join(key) |> 
  dplyr::filter(!is.na(sci)) |> 
  dplyr::select(sci, cd1) |> 
  dplyr::distinct()

# summarise eye size data for family level for missing species
eye.fam <- readr::read_csv( here::here("Data/ritland_clean.csv")) |>
  dplyr::select(sci_elton = species_jetz, fam = family_jetz, cd1) |> 
  dplyr::left_join(key) |>
  dplyr::mutate(fam = ifelse(fam == "Emberizidae", "Passerellidae", fam)) |> 
  dplyr::mutate(fam = ifelse(fam == "Reguliidae", "Regulidae", fam)) |> 
  dplyr::mutate(fam = ifelse(sci_elton == "Pandion haliaetus", "Pandionidae", fam)) |> 
  dplyr::group_by(fam) |> 
  dplyr::summarise( cd1.fam = mean(cd1))

# avonet database; resolve some naming/taxonomy issues
avo <- readr::read_csv( here::here("Data/avonet.csv")) |> 
  dplyr::rename(sci_name = Species1) |> 
  dplyr::mutate( sci_name = ifelse(sci_name == "Passerella arborea", "Spizelloides arborea", sci_name)) |> 
  dplyr::mutate( sci_name = ifelse(sci_name =="Hesperiphona vespertina", "Coccothraustes vespertinus", sci_name)) |> 
  dplyr::mutate( sci_name = ifelse(sci_name =="Leuconotopicus villosus", "Dryobates villosus", sci_name)) |> 
  dplyr::mutate( sci_name = ifelse(sci_name =="Larus atricilla", "Leucophaeus atricilla", sci_name)) |> 
  dplyr::mutate( sci_name = ifelse(sci_name =="Hylatomus pileatus", "Dryocopus pileatus", sci_name)) |> 
  dplyr::mutate( sci_name = ifelse(sci_name =="Regulus calendula", "Corthylio calendula", sci_name)) |> 
  janitor::clean_names()

# nest type data; cavity scores from reviewing Birds of the World
cav <- readr::read_csv(here::here("Data/cavity.csv")) |> 
  dplyr::select(com = com_name, sci = sci_name_bw, cavity)

noct <- template |> 
  dplyr::select(com, sci) |> 
  dplyr::distinct() |> 
  dplyr::left_join(key) |> 
  dplyr::left_join(elton) |> 
  dplyr::select(com, sci, noct) |> 
  dplyr::group_by(com, sci) |> 
  dplyr::summarise( noct = unique(noct)) |> 
  dplyr::ungroup()

# join up the detections with the template
det <- full_join(
  test, template) |> 
  arrange(lat, lon, com) |>
  dplyr::mutate(across(c(tm1, t), function(x) replace_na(x, 0))) |> 
  tidyr::pivot_longer(tm1:t, names_to = "eclipse", values_to = "y") |>
  # dplyr::filter(!eclipse == "tp1") |> 
  dplyr::mutate(eclipse = ifelse(eclipse == "t", 1, 0)) |> # binary variable for whether it was eclipse day or not 
  dplyr::group_by(lat, lon, com) |> 
  dplyr::mutate(sp.station = dplyr::cur_group_id()) |> 
  dplyr::ungroup() |> 
  # join up with various trait tables
  dplyr::left_join(avo |> 
                     rename(sci = sci_name)) |> 
  dplyr::rename(fam = family1) |> 
  dplyr::left_join(eye.sp) |> 
  dplyr::left_join(eye.fam) |>
  dplyr::left_join(noct) |> 
  dplyr::mutate(cd = ifelse(!is.na(cd1), cd1, cd1.fam)) |> 
  dplyr::mutate(eclipse = factor(eclipse)) 

# calculating proportion nocturnal activity
# did this once & saved output which I'll read in below 
# the two files read in on the next two lines are big so they are not included in the rep
# but they are birdweather data for all of april and ES data for the broader time period
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
#   dplyr::left_join(sr_ss) |> 
#   dplyr::filter(conf >= 0.65 ) |> 
#   dplyr::mutate( noct = ifelse( time < sunrise | time > sunset, 1, 0)) |>  
#   dplyr::filter(!is.na(noct)) |> 
#   dplyr::group_by(com, sci) |> 
#   dplyr::summarise( pnoct = sum(noct) / sum(!is.na(noct)))

# proportion of nocturnal activity (derived from broader-scale BW and ES data)
pnoct <- readr::read_csv( here::here("Data/pnoct_v01.csv"))

# calculate what time of day a site experienced the eclipse
us2 <- template |> 
  dplyr::select(lat, lon, MaxEclipseTimeUTC) |> 
  dplyr::distinct() |> 
  tibble::add_column(date = ymd("2024-04-08"))

sr <- suncalc::getSunlightTimes(
  data = us2, 
  keep = "sunrise",
  tz = "UTC")

eclipse_time <- us2 |> 
  dplyr::bind_cols(sunrise = sr$sunrise) |> 
  dplyr::mutate(eclipse = lubridate::as_datetime( paste(date, hms::as_hms(MaxEclipseTimeUTC)))) |> 
  dplyr::mutate( eclipse_dist_sunrise = as.numeric(difftime(eclipse,sunrise, units = "hours"))) |> 
  dplyr::select(lat, lon, eclipse_dist_sunrise) |> 
  dplyr::group_by(lat, lon) |> 
  dplyr::summarise(eclipse_dist_sunrise = mean(eclipse_dist_sunrise))

# throw down a grid for random effect blocking
bounding_box <- sf::st_bbox(c(xmin = -180,
                              ymin = -90,
                              xmax = 180,
                              ymax = 90), crs = sf::st_crs(4326))


# Create a global polygon grid with cell_size spacing = 2 degree
global_grid_5 <- sf::st_make_grid(sf::st_as_sfc(bounding_box),
                                  cellsize = c(2, 2), crs = sf::st_crs(4326), what = "polygons")

grd_sf <- sf::st_sf(geometry = global_grid_5) |> 
  dplyr::mutate(ID = row_number())

stations <- template |> 
  dplyr::select(lat, lon) |> 
  dplyr::distinct() |> 
  sf::st_as_sf(coords = c("lon", "lat"), 
               crs = 4326)

stations.with.grd <- sf::st_join( stations, grd_sf, join = st_within) |> 
  sf::st_drop_geometry()

grd.info <- template |> 
  dplyr::select(lat, lon) |> 
  dplyr::distinct() |> 
  tibble::add_column(grd = stations.with.grd$ID)

# phew - finally, everything all together
final <- det |> 
  dplyr::left_join(eclipse_time |> 
                     dplyr::select(lat, lon, eclipse_time = eclipse_dist_sunrise)) |> 
  dplyr::left_join(pnoct) |> 
  dplyr::select(lat, lon, com, sci, fam, source, y,
                eclipse,
                alan,
                eclipse_time,
                migration,
                habitat,
                habitat_density,
                trophic_level, trophic_niche, 
                primary_lifestyle, range_size, 
                noct, pnoct, cd) |> 
  dplyr::mutate(cd = as.numeric(scale(log(cd))),
                eclipse = factor(eclipse), 
                eclise_time = as.numeric(scale(eclipse_time)), 
                habitat_density = factor(habitat_density), 
                trophic_level = factor(trophic_level), 
                trophic_niche = factor(trophic_niche), 
                migration = factor(migration),
                primary_lifestyle = factor(primary_lifestyle), 
                range_size = as.numeric(scale(log(range_size))), 
                # noct = factor(noct), 
                et_raw = eclipse_time,
                eclipse_time = as.numeric(scale(eclipse_time)),
                pnoct_raw = pnoct, 
                pnoct = as.numeric(scale(pnoct)),
                alan = as.numeric(scale(log1p(alan)))) |> 
  dplyr::group_by(lat, lon) |> 
  dplyr::mutate(station = dplyr::cur_group_id()) |> 
  dplyr::ungroup() |> 
  dplyr::left_join(grd.info) |> 
  dplyr::left_join(cav) |> 
  dplyr::group_by(com,grd) |> 
  dplyr::mutate(sp.grd = dplyr::cur_group_id()) |> 
  dplyr::mutate( date = as_date(ifelse(eclipse == 1, ymd("2024-04-08"), ymd("2024-04-07")))) |> 
  dplyr::inner_join(valid_combos) |> 
  dplyr::ungroup()

# base model: do birds vocalize more or less during maximum of total eclipse?  
m <- glmmTMB::glmmTMB(
  y ~ 1 + eclipse + (1+eclipse|com) + (1|sp.grd),
  data = final,
  family = binomial(link = "logit"))

# fit same model with brms -
# better for doing computations on model predictions & retaining uncertainty
m_brms <- brms::brm(
  y ~ 1 + eclipse + (1+eclipse|com) + (1|sp.grd),
  data = final,
  family = bernoulli(link = "logit"),
  prior = c(prior(normal(0, 1.5), class = "Intercept"),
            prior(normal(0, 0.5), class = "b"),
            prior(exponential(1), class = "sd"),
            prior(lkj(2), class = cor)),
  control = list(adapt_delta = 0.95),
  chains = 4)

# trait models

# diel niche (binary)
m1 <- glmmTMB::glmmTMB(
  y ~ 1 + eclipse*noct + (1+eclipse|com) + (1|sp.grd),
  data = filter(final, !is.na(noct)),
  family = binomial(link = "logit")) 

# diel niche (continuous)
m2 <- glmmTMB::glmmTMB(
  y ~ 1 + eclipse*pnoct + (1+eclipse|com) + (1|sp.grd),
  data = filter(final, !is.na(pnoct)),
  family = binomial(link = "logit"))

# eye size (cd = corneal diameter)
m3 <- glmmTMB::glmmTMB(
  y ~ 1 + eclipse*cd + (1+eclipse|com) + (1|sp.grd),
  data = filter(final, !is.na(cd)),
  family = binomial(link = "logit"))

# light pollution
m4 <- glmmTMB::glmmTMB(
  y ~ 1 + eclipse*alan + (1+eclipse|com) + (1|sp.grd),
  data = filter(final, !is.na(alan)),
  family = binomial(link = "logit"))

# eclipse time of day
m5 <- glmmTMB::glmmTMB(
  y ~ 1 + eclipse*eclipse_time + (1+eclipse|com) + (1|sp.grd),
  data = filter(final, !is.na(eclipse_time)),
  family = binomial(link = "logit"))

# species preferred habitat density
m6 <- glmmTMB::glmmTMB(
  y ~ 1 + eclipse*habitat_density + (1+eclipse|com) + (1|sp.grd),
  data = filter(final, !is.na(habitat_density)),
  family = binomial(link = "logit"))

# species migratory habit
m7 <- glmmTMB::glmmTMB(
  y ~ 1 + eclipse*migration + (1+eclipse|com) + (1|sp.grd),
  data = filter(final, !is.na(migration)),
  family = binomial(link = "logit"))

# nest type
m8 <- glmmTMB::glmmTMB(
  y ~ 1 + eclipse*cavity + (1+eclipse|com) + (1|sp.grd), 
  data = final, 
  family = binomial(link = "logit"))

# save all those model results
save(
  final, 
  m, m_brms, 
  m1, 
  m2, 
  m3,
  m4, 
  m5, 
  m6, 
  m7, 
  m8,
  file = here::here("Results/total_eclipse_maximum.RData"))