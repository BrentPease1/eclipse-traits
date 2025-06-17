library(tidyverse)
library(glmmTMB)
library(brms)
library(posterior)
library(MetBrewer)
library(here)
library(sf)
library(rnaturalearth)
library(data.table)
library(patchwork)
library(effects)
library(tidybayes)

conus84 <- rnaturalearth::ne_states(iso_a2 = "US") |> 
  # dplyr::filter(!name %in% c("Alaska", "Hawaii")) |> 
  dplyr::summarise(geometry = sf::st_union(geometry))

birdnet <- readr::read_csv( here::here("Data/merged_es_bw_apr2025_viirs.csv"))

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

bw <- filter(birdnet, data_source == "BW") |> 
  dplyr::filter(date == ymd("2024-04-08") | date == ymd("2024-04-07")) |>
  dplyr::select(-Latitude, -Longitude) |> 
  dplyr::left_join(bw_loc)

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

test <- focal_time_detections |> 
  dplyr::select(lat, lon, com, sci, date) |> 
  dplyr::distinct() |> 
  tibble::add_column(y = 1) |> 
  tidyr::pivot_wider(names_from = date, values_from = y) |> 
  dplyr::rename( tm1 = `2024-04-07`,
                 t = `2024-04-08`) |> 
  dplyr::mutate(across(c(tm1, t), function(x) replace_na(x, 0)))

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

key <- readr::read_csv( here::here("Data/birdweather_elton_botw_name_key.csv")) |> 
  dplyr::select( sci = sci_name_bw, sci_elton = sci_name_elton) |> 
  dplyr::mutate( sci_elton = stringr::str_replace_all(sci_elton, "\xa0", " "))

eye.sp <- readr::read_csv( here::here("Data/ritland_clean.csv")) |>
  dplyr::select(sci_elton = species_jetz, cd1) |> 
  dplyr::left_join(key) |> 
  dplyr::filter(!is.na(sci)) |> 
  dplyr::select(sci, cd1) |> 
  dplyr::distinct()

eye.fam <- readr::read_csv( here::here("Data/ritland_clean.csv")) |>
  dplyr::select(sci_elton = species_jetz, fam = family_jetz, cd1) |> 
  dplyr::left_join(key) |>
  dplyr::mutate(fam = ifelse(fam == "Emberizidae", "Passerellidae", fam)) |> 
  dplyr::mutate(fam = ifelse(fam == "Reguliidae", "Regulidae", fam)) |> 
  dplyr::mutate(fam = ifelse(sci_elton == "Pandion haliaetus", "Pandionidae", fam)) |> 
  dplyr::group_by(fam) |> 
  dplyr::summarise( cd1.fam = mean(cd1))

avo <- readr::read_csv( here::here("Data/avonet.csv")) |> 
  dplyr::rename(sci_name = Species1) |> 
  dplyr::mutate( sci_name = ifelse(sci_name == "Passerella arborea", "Spizelloides arborea", sci_name)) |> 
  dplyr::mutate( sci_name = ifelse(sci_name =="Hesperiphona vespertina", "Coccothraustes vespertinus", sci_name)) |> 
  dplyr::mutate( sci_name = ifelse(sci_name =="Leuconotopicus villosus", "Dryobates villosus", sci_name)) |> 
  dplyr::mutate( sci_name = ifelse(sci_name =="Larus atricilla", "Leucophaeus atricilla", sci_name)) |> 
  dplyr::mutate( sci_name = ifelse(sci_name =="Hylatomus pileatus", "Dryocopus pileatus", sci_name)) |> 
  dplyr::mutate( sci_name = ifelse(sci_name =="Regulus calendula", "Corthylio calendula", sci_name)) |> 
  janitor::clean_names()

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

det <- full_join(
  test, template) |> 
  arrange(lat, lon, com) |>
  dplyr::mutate(across(c(tm1, t), function(x) replace_na(x, 0))) |> 
  tidyr::pivot_longer(tm1:t, names_to = "eclipse", values_to = "y") |>
  # dplyr::filter(!eclipse == "tp1") |> 
  dplyr::mutate(eclipse = ifelse(eclipse == "t", 1, 0)) |> 
  dplyr::group_by(lat, lon, com) |> 
  dplyr::mutate(sp.station = dplyr::cur_group_id()) |> 
  dplyr::ungroup() |> 
  dplyr::left_join(avo |> 
                     rename(sci = sci_name)) |> 
  dplyr::rename(fam = family1) |> 
  dplyr::left_join(eye.sp) |> 
  dplyr::left_join(eye.fam) |>
  dplyr::left_join(noct) |> 
  dplyr::mutate(cd = ifelse(!is.na(cd1), cd1, cd1.fam)) |> 
  dplyr::mutate(eclipse = factor(eclipse)) 

bw <- readr::read_csv( here::here("data/birdweather_apr24.csv"))
es <- readr::read_csv( here::here("Data/birdnet_cleaned_v03.csv"))

combos <- bw |> 
  dplyr::select( lat = Latitude, 
                 lon = Longitude) |> 
  dplyr::distinct() |>
  dplyr::full_join(
    es |> 
      dplyr::select(lat = Latitude, 
                    lon = Longitude) |> 
      dplyr::distinct()) |> 
  dplyr::cross_join(
    tibble::tibble(
      date = seq(from = lubridate::ymd("2024-03-31"), to = lubridate::ymd("2024-04-16"), by = 1)))

sr_ss <- suncalc::getSunlightTimes(
  data = combos,
  keep = c("sunrise",
           "sunset"))

all_data <- full_join(
  es |> 
    janitor::clean_names() |> 
    dplyr::select(com = common_name, 
                  sci = scientific_name, 
                  lat = latitude, 
                  lon = longitude, 
                  time = start_timestamp,
                  conf = confidence),
  
  bw |> 
    janitor::clean_names() |> 
    dplyr::select(com = common_name, 
                  sci = scientific_name, 
                  lat = latitude, 
                  lon = longitude, 
                  time = timestamp,
                  conf = confidence) |> 
    dplyr::mutate(time = lubridate::parse_date_time(time, orders = "Ymd HMS z", tz = "UTC")))

tz <- all_data |> 
  dplyr::select(lat, lon) |> 
  dplyr::distinct() |> 
  dplyr::mutate(tz = lutz::tz_lookup_coords(lat = lat, lon = lon, method = "fast"))

clean <- all_data |> 
  dplyr::left_join(tz) |> 
  dplyr::mutate(time_local = lubridate::with_tz(time, tzone = tz),
                date_local = lubridate::as_date(time)) |> 
  dplyr::filter(!date_local == ymd("2024-04-08")) |> 
  dplyr::select(-time_local,-date_local)

pnoct <- clean |> 
  dplyr::mutate(date = lubridate::as_date(time)) |> 
  dplyr::left_join(sr_ss) |> 
  dplyr::filter(conf >= 0.65 ) |> 
  dplyr::mutate( noct = ifelse( time < sunrise | time > sunset, 1, 0)) |>  
  dplyr::filter(!is.na(noct)) |> 
  dplyr::group_by(com, sci) |> 
  dplyr::summarise( pnoct = sum(noct) / sum(!is.na(noct)))

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

bounding_box <- sf::st_bbox(c(xmin = -180,
                              ymin = -90,
                              xmax = 180,
                              ymax = 90), crs = sf::st_crs(4326))


# Create a global polygon grid with cell_size spacing
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
  
m <- glmmTMB::glmmTMB(
  y ~ 1 + eclipse + (1+eclipse|com) + (1|sp.grd),
  data = final,
  family = binomial(link = "logit"))

m_nd <- tibble(eclipse = unique(final$eclipse))

m_p <- effects::effect(m, term = "eclipse") |> 
  as_tibble() |> 
  dplyr::mutate(eclipse = ifelse(eclipse == 1, "eclipse", "normal")) |> 
  dplyr::mutate(eclipse = factor(eclipse, levels = c("eclipse", "normal")))

ggplot(m_p, aes(x = fit, y = eclipse)) +
  geom_errorbar(aes(xmin = fit - se, xmax = fit + se),
                width = 0, color = MetBrewer::MetPalettes$Isfahan1[[1]][6],
                linewidth = 2) +
  geom_point(size = 4,
             color = MetBrewer::MetPalettes$Isfahan1[[1]][6]) +
  labs(x = "probability of vocalization during 4-minute window") +
  theme_classic() +
  theme(strip.background = element_blank(),
        legend.position = "none",
        axis.text = element_text(size = 8.5, color = "black"), 
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 10, color = "black"), 
        axis.line = element_line(linewidth = 0.2, color = "black"),
        axis.ticks = element_line(linewidth = 0.2, color = "black"),,
        strip.text = element_text(color = "black", size = 10, face = "bold")) 


setwd(here::here("Results/figures"))
ggsave(
  filename = "figure_02a.png", 
  width = 3.5, 
  height = 1.1,
  units = "in", 
  dpi = 600)

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
  chains = 4,
  cores = 4)

pred_com <- tidyr::expand_grid(
  eclipse = unique(final$eclipse)) |> 
  tidybayes::add_epred_draws( m_brms, ndraws = 1000, re_formula = NA)

pred_com |> 
  dplyr::ungroup() |> 
  dplyr::select(eclipse, draw = .draw, p = .epred) |> 
  tidyr::pivot_wider(names_from = eclipse, values_from = p) |> 
  dplyr::mutate( pchange = (`1` - `0`) / `0`) |> 
  summarise( mean = mean(pchange), 
             l95 = quantile(pchange, c(0.025)), 
             u95 = quantile(pchange, c(0.975))) |> 
  mutate(across(mean:u95, function(x) round(100*x,)))

pred <- tidyr::expand_grid(
  com = unique(final$com), 
  eclipse = unique(final$eclipse)) |> 
  tidybayes::add_epred_draws(m_brms, ndraws = 1000, re_formula = "~(1 + eclipse | com)") 

# filter down to species detected at at least 9 sites to reduced species list
mult <- final |> 
  dplyr::group_by(com) |> 
  dplyr::count() |> 
  dplyr::filter(n > 18)

pred |> 
  dplyr::ungroup() |> 
  dplyr::select(com, eclipse, draw = .draw, p = .epred) |> 
  tidyr::pivot_wider(names_from = eclipse, values_from = p) |> 
  dplyr::mutate(diff = `1` - `0`) |> 
  dplyr::group_by(com) |> 
  dplyr::summarise( mean = mean(diff),
                    l68 = quantile(diff, c(0.160)), 
                    u68 = quantile(diff, c(0.840)),
                    l95 = quantile(diff, c(0.025)), 
                    u95 = quantile(diff, c(0.975))) |>
  dplyr::filter(com %in% mult$com) |> 
  dplyr::arrange(mean) |> 
  dplyr::mutate(com = factor(com, levels = unique(com))) |> 
  dplyr::mutate(col.lab = ifelse(mean > 0, "called more", "called less")) |> 
  
  ggplot(aes(x = mean, y = com, color = col.lab)) +
  geom_vline(xintercept = 0, color = MetBrewer::MetPalettes$Demuth[[1]][2],
             linetype = "dashed") +
  geom_errorbar(aes(xmin = l95, xmax = u95), linewidth = 0.5, width =0) +
  geom_errorbar(aes(xmin = l68, xmax = u68), linewidth = 1, width =0) +
  geom_point() +
  theme_classic() +
  labs(x = "change in vocalization probability",
       color = "eclipse effect") +
  scale_color_manual(values = MetBrewer::MetPalettes$Demuth[[1]][c(4,10)]) +
  theme(strip.background = element_blank(),
        legend.position = c(0.825, 0.05),
        legend.key = element_rect(fill = NA, color = NA),
        legend.key.width = unit(5, "pt"),
        legend.background = element_blank(),
        axis.text = element_text(size = 7.5, color = "black"), 
        axis.title.y = element_blank(),
        legend.title = element_text(size = 9, color = "black"),
        legend.text = element_text(size = 8, color = "black"),
        axis.title.x = element_text(size = 10, color = "black"), 
        axis.line = element_line(linewidth = 0.2, color = "black"),
        axis.ticks = element_line(linewidth = 0.2, color = "black"),,
        strip.text = element_text(color = "black", size = 10, face = "bold")) 

setwd(here::here("Results/Figures"))
ggsave(
  filename = "figure_02c.png", 
  width = 3.5, 
  height = 7, 
  units = "in",
  dpi = 600)

m1 <- glmmTMB::glmmTMB(
  y ~ 1 + eclipse*noct + (1+eclipse|com) + (1|sp.grd),
  data = filter(final, !is.na(noct)),
  family = binomial(link = "logit")) 

m2 <- glmmTMB::glmmTMB(
  y ~ 1 + eclipse*pnoct + (1+eclipse|com) + (1|sp.grd),
  data = filter(final, !is.na(pnoct)),
  family = binomial(link = "logit"))

m3 <- glmmTMB::glmmTMB(
  y ~ 1 + eclipse*cd + (1+eclipse|com) + (1|sp.grd),
  data = filter(final, !is.na(cd)),
  family = binomial(link = "logit"))

m4 <- glmmTMB::glmmTMB(
  y ~ 1 + eclipse*alan + (1+eclipse|com) + (1|sp.grd),
  data = filter(final, !is.na(alan)),
  family = binomial(link = "logit"))

m5 <- glmmTMB::glmmTMB(
  y ~ 1 + eclipse*eclipse_time + (1+eclipse|com) + (1|sp.grd),
  data = filter(final, !is.na(eclipse_time)),
  family = binomial(link = "logit"))

m6 <- glmmTMB::glmmTMB(
  y ~ 1 + eclipse*habitat_density + (1+eclipse|com) + (1|sp.grd),
  data = filter(final, !is.na(habitat_density)),
  family = binomial(link = "logit"))

m7 <- glmmTMB::glmmTMB(
  y ~ 1 + eclipse*migration + (1+eclipse|com) + (1|sp.grd),
  data = filter(final, !is.na(migration)),
  family = binomial(link = "logit"))

m8 <- glmmTMB::glmmTMB(
  y ~ 1 + eclipse*cavity + (1+eclipse|com) + (1|sp.grd), 
  data = final, 
  family = binomial(link = "logit"))

m1_nd <- tidyr::expand_grid(
  eclipse = unique(final$eclipse), 
  noct = c(0,1))

m1_p <- predict(m1, m1_nd, re.form = NA, se = TRUE, type = "response")

m1_pred <- m1_nd |> 
  tibble::add_column(fit= m1_p$fit, 
                     se = m1_p$se.fit) |> 
  tibble::add_column(predictor = "Diel niche (binary)") |> 
  dplyr::select(predictor, cov = noct, eclipse, fit, se)

pnoct.sc <- scale(final$pnoct_raw)

m2_nd <- tidyr::expand_grid(
  eclipse = unique(final$eclipse), 
  pnoct = c(0.0306, 0.5, 0.999)) |> 
  dplyr::mutate(pnoct = (pnoct - attr(pnoct.sc, "scaled:center")) /attr(pnoct.sc, "scaled:scale")) 

m2_p <- predict(m2, m2_nd, re.form = NA, se = TRUE, type = "response")

m2_pred <- m2_nd |> 
  tibble::add_column(fit = m2_p$fit, 
                     se = m2_p$se.fit) |> 
  dplyr::mutate(pnoct = pnoct*attr(pnoct.sc, "scaled:scale") + attr(pnoct.sc, "scaled:center")) |> 
  dplyr::mutate(pnoct = round(pnoct, 2)) |> 
  tibble::add_column(predictor = "Diel niche (continuous)") |> 
  dplyr::select(predictor, cov = pnoct, eclipse, fit, se)

m3_nd <- tidyr::expand_grid(
  eclipse = unique(final$eclipse), 
  cd = c(min(final$cd), max(final$cd)))

m3_p <- predict(m3, m3_nd, re.form = NA, se = TRUE, type = "response")

m3_pred <- m3_nd |> 
  tibble::add_column(fit = m3_p$fit, 
                     se = m3_p$se.fit) |> 
  tibble::add_column(predictor = "Eye size") |> 
  dplyr::select(predictor, cov = cd, eclipse, fit, se)

m4_nd <- tidyr::expand_grid(
  eclipse = unique(final$eclipse), 
  alan = c(min(final$alan), max(final$alan)))

m4_p <- predict(m4, m4_nd, re.form = NA, se = TRUE, type = "response")

m4_pred <- m4_nd |> 
  tibble::add_column(fit = m4_p$fit, 
                     se = m4_p$se.fit) |> 
  tibble::add_column(predictor = "Light pollution") |> 
  dplyr::select(predictor, cov = alan, eclipse, fit, se)

m5_nd <- tidyr::expand_grid(
  eclipse = unique(final$eclipse), 
  eclipse_time = c(min(final$eclipse_time), max(final$eclipse_time)))

m5_p <- predict(m5, m5_nd, re.form = NA, se = TRUE, type = "response")

m5_pred <- m5_nd |> 
  tibble::add_column(fit = m5_p$fit, 
                     se = m5_p$se.fit) |> 
  tibble::add_column(predictor = "Eclipse time") |> 
  dplyr::select(predictor, cov = eclipse_time, eclipse, fit, se)

m6_nd <- tidyr::expand_grid(
  eclipse = unique(final$eclipse), 
  habitat_density = c(1,3))

m6_p <- predict(m6, m6_nd, re.form = NA, se = TRUE, type = "response")

m6_pred <- m6_nd |> 
  tibble::add_column(fit = m6_p$fit, 
                     se = m6_p$se.fit) |> 
  tibble::add_column(predictor = "Habitat density") |> 
  dplyr::select(predictor, cov = habitat_density, eclipse, fit, se)

m7_nd <- tidyr::expand_grid(
  eclipse = unique(final$eclipse), 
  migration = c(1,3))

m7_p <- predict(m7, m7_nd, re.form = NA, se = TRUE, type = "response")

m7_pred <- m7_nd |> 
  tibble::add_column(fit = m7_p$fit, 
                     se = m7_p$se.fit) |> 
  tibble::add_column(predictor = "Migration") |> 
  dplyr::select(predictor, cov = migration, eclipse, fit, se)

m8_nd <- tidyr::expand_grid(
  eclipse = unique(final$eclipse), 
  cavity = c(0,1))

m8_p <- predict(m8, m8_nd, re.form = NA, se = TRUE, type = "response")

m8_pred <- m8_nd |> 
  tibble::add_column(fit = m8_p$fit, 
                     se = m8_p$se.fit) |> 
  tibble::add_column(predictor = "Nest type") |> 
  dplyr::select(predictor, cov = cavity, eclipse, fit, se)

all <- dplyr::full_join(m1_pred, m2_pred) |> 
  dplyr::full_join(m3_pred) |> 
  dplyr::full_join(m4_pred) |> 
  dplyr::full_join(m5_pred) |> 
  dplyr::full_join(m6_pred) |> 
  dplyr::full_join(m7_pred) |> 
  dplyr::full_join(m8_pred) |> 
  dplyr::mutate(eclipse = ifelse(eclipse == 1, "eclipse", "normal")) |> 
  dplyr::mutate(eclipse = factor(eclipse, levels = c("normal", "eclipse"))) |> 
  dplyr::mutate(predictor = tolower(predictor))

param <- 
  summary(m1)$coefficients$cond |> 
  tibble::as_tibble(rownames = "param") |> 
  janitor::clean_names() |> 
  tibble::add_column(predictor = "diel niche (binary)") |> 
  dplyr::full_join(
    summary(m2)$coefficients$cond |> 
      tibble::as_tibble(rownames = "param") |> 
      janitor::clean_names() |> 
      tibble::add_column(predictor = "diel niche (continuous)")) |> 
  dplyr::full_join(
    summary(m3)$coefficients$cond |> 
      tibble::as_tibble(rownames = "param") |> 
      janitor::clean_names() |> 
      tibble::add_column(predictor = "eye size")) |> 
  dplyr::full_join(
    summary(m4)$coefficients$cond |> 
      tibble::as_tibble(rownames = "param") |> 
      janitor::clean_names() |> 
      tibble::add_column(predictor = "light pollution")) |> 
  dplyr::full_join(
    summary(m5)$coefficients$cond |> 
      tibble::as_tibble(rownames = "param") |> 
      janitor::clean_names() |> 
      tibble::add_column(predictor = "eclipse time")) |> 
  dplyr::full_join(
    summary(m6)$coefficients$cond |> 
      tibble::as_tibble(rownames = "param") |> 
      janitor::clean_names() |> 
      tibble::add_column(predictor = "habitat density")) |> 
  dplyr::full_join(
    summary(m7)$coefficients$cond |> 
      tibble::as_tibble(rownames = "param") |> 
      janitor::clean_names() |> 
      tibble::add_column(predictor = "migration")) |> 
  dplyr::full_join(
    summary(m8)$coefficients$cond |> 
      tibble::as_tibble(rownames = "param") |> 
      janitor::clean_names() |> 
      tibble::add_column(predictor = "nest type"))

star.lab <- param |> 
  dplyr::filter(grepl(":", param)) |> 
  dplyr::filter(!(param == "eclipse1:habitat_density2")) |> 
  dplyr::filter(!(param == "eclipse1:migration2")) |> 
  dplyr::mutate(star.lab = ifelse(pr_z < 0.001, "***", 
                                  # ifelse(pr_z >= 0.001 & pr_z < 0.01, "***", 
                                  ifelse(pr_z >= 0.001 & pr_z < 0.05, "**", 
                                         ifelse(pr_z >= 0.05 & pr_z < 0.1, "*", "")))) |> 
  tibble::add_column(eclipse = factor("normal")) |> 
  tibble::add_column(fit = 0.35) |> 
  dplyr::select(predictor, eclipse, fit, star.lab, pr_z) |> 
  dplyr::mutate( p = sprintf("%.2f", round(pr_z, 2))) |> 
  dplyr::mutate(p.lab = ifelse(p > 0.01, paste0("p = ", p),
                               "p < 0.01"))

( dnb_plot <- ggplot() +
    geom_errorbar(
      data = filter(all, predictor == "diel niche (binary)"),
      aes(x = eclipse, ymin = fit - se, ymax = fit + se, color = factor(cov)), 
      width = 0, 
      linewidth = 1.25,
      position = position_dodge(width = 0.35)) +
    geom_point(
      data = filter(all, predictor == "diel niche (binary)"),
      aes(x = eclipse, y = fit, color = factor(cov)),
      size = 3,
      position = position_dodge(width = 0.35)) +
    ylim(c(-0.01, 0.4)) +
    geom_text(data = filter(star.lab, predictor == "diel niche (binary)"), 
              aes(x = eclipse, y = fit, label = p.lab),
              color = "black", 
              size = 3.25) +
    facet_wrap(~predictor) +
    theme_classic() +
    labs(y = "probability of vocalization during 4-minute window") +
    scale_color_manual(values = MetBrewer::MetPalettes$Kandinsky[[1]][c(2,4)]) +
    theme(strip.background = element_blank(),
          legend.position = "none",
          axis.text = element_text(size = 8.5, color = "black"), 
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 10, color = "black"), 
          axis.line = element_line(linewidth = 0.2, color = "black"),
          axis.ticks = element_line(linewidth = 0.2, color = "black"),,
          strip.text = element_text(color = "black", size = 10, face = "bold")) )

cp <- colorRampPalette(MetBrewer::MetPalettes$Kandinsky[[1]][c(2, 4)])

(dnc_plot <- ggplot() +
    geom_errorbar(
      data = filter(all, predictor == "diel niche (continuous)"),
      aes(x = eclipse, ymin = fit - se, ymax = fit + se, color = factor(cov)), 
      width = 0, 
      linewidth = 1.25,
      position = position_dodge(width = 0.35)) +
    geom_point(
      data = filter(all, predictor == "diel niche (continuous)"),
      aes(x = eclipse, y = fit, color = factor(cov)),
      size = 3,
      position = position_dodge(width = 0.35)) +
    ylim(c(-0.01, 0.4)) +
    geom_text(data = filter(star.lab, predictor == "diel niche (continuous)"), 
              aes(x = eclipse, y = fit, label = p.lab),
              color = "black", 
              size = 3.25) +
    facet_wrap(~predictor) +
    theme_classic() +
    labs(y = "probability of vocalization during 4-minute window") +
    scale_color_manual(values = cp(3)) +
    theme(strip.background = element_blank(),
          legend.position = "none",
          strip.clip = "off",
          axis.text = element_text(size = 8.5, color = "black"), 
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 10, color = "black"), 
          axis.line = element_line(linewidth = 0.2, color = "black"), 
          axis.ticks = element_line(linewidth = 0.2, color = "black"),,
          strip.text = element_text(color = "black", size = 10, face = "bold")) )

( cd_plot <- ggplot() +
    geom_errorbar(
      data = filter(all, predictor == "eye size"),
      aes(x = eclipse, ymin = fit - se, ymax = fit + se, color = factor(cov)), 
      width = 0, 
      linewidth = 1.25,
      position = position_dodge(width = 0.35)) +
    geom_point(
      data = filter(all, predictor == "eye size"),
      aes(x = eclipse, y = fit, color = factor(cov)),
      size = 3,
      position = position_dodge(width = 0.35)) +
    ylim(c(-0.01, 0.4)) +
    geom_text(data = filter(star.lab, predictor == "eye size"), 
              aes(x = eclipse, y = fit, label = p.lab),
              color = "black", 
              size = 3.25) +
    facet_wrap(~predictor) +
    theme_classic() +
    scale_color_manual(values = MetBrewer::MetPalettes$Hokusai1[[1]][c(2,7)]) +
    labs(y = "probability of vocalization during 4-minute window") +
    theme(strip.background = element_blank(),
          legend.position = "none",
          axis.text = element_text(size = 8.5, color = "black"), 
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 10, color = "black"), 
          axis.line = element_line(linewidth = 0.2, color = "black"),
          axis.ticks = element_line(linewidth = 0.2, color = "black"),,
          strip.text = element_text(color = "black", size = 10, face = "bold")) )

( hd_plot <- ggplot() +
    geom_errorbar(
      data = filter(all, predictor == "habitat density"),
      aes(x = eclipse, ymin = fit - se, ymax = fit + se, color = factor(cov)), 
      width = 0, 
      linewidth = 1.25,
      position = position_dodge(width = 0.35)) +
    geom_point(
      data = filter(all, predictor == "habitat density"),
      aes(x = eclipse, y = fit, color = factor(cov)),
      size = 3,
      position = position_dodge(width = 0.35)) +
    ylim(c(-0.01, 0.4)) +
    geom_text(data = filter(star.lab, predictor == "habitat density"), 
              aes(x = eclipse, y = fit, label = p.lab),
              color = "black", 
              size = 3.25) +
    facet_wrap(~predictor) +
    theme_classic() +
    scale_color_manual(values = MetBrewer::MetPalettes$Hokusai2[[1]][c(2,5)]) +
    labs(y = "probability of vocalization during 4-minute window") +
    theme(strip.background = element_blank(),
          legend.position = "none",
          axis.text = element_text(size = 8.5, color = "black"), 
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 10, color = "black"), 
          axis.line = element_line(linewidth = 0.2, color = "black"), 
          axis.ticks = element_line(linewidth = 0.2, color = "black"),,
          strip.text = element_text(color = "black", size = 10, face = "bold")) )

( mi_plot <- ggplot() +
    geom_errorbar(
      data = filter(all, predictor == "migration"),
      aes(x = eclipse, ymin = fit - se, ymax = fit + se, color = factor(cov)), 
      width = 0, 
      linewidth = 1.25,
      position = position_dodge(width = 0.35)) +
    geom_point(
      data = filter(all, predictor == "migration"),
      aes(x = eclipse, y = fit, color = factor(cov)),
      size = 3,
      position = position_dodge(width = 0.35)) +
    ylim(c(-0.01, 0.4)) +
    geom_text(data = filter(star.lab, predictor == "migration"), 
              aes(x = eclipse, y = fit, label = p.lab),
              color = "black", 
              size = 3.25) +
    facet_wrap(~predictor) +
    theme_classic() +
    scale_color_manual(values = MetBrewer::MetPalettes$Cassatt2[[1]][c(3,8)]) +
    labs(y = "probability of vocalization during 4-minute window") +
    theme(strip.background = element_blank(),
          legend.position = "none",
          axis.text = element_text(size = 8.5, color = "black"), 
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 10, color = "black"), 
          axis.line = element_line(linewidth = 0.2, color = "black"),
          axis.ticks = element_line(linewidth = 0.2, color = "black"),,
          strip.text = element_text(color = "black", size = 10, face = "bold")) )

( nt_plot <- ggplot() +
    geom_errorbar(
      data = filter(all, predictor == "nest type"),
      aes(x = eclipse, ymin = fit - se, ymax = fit + se, color = factor(cov)), 
      width = 0, 
      linewidth = 1.25,
      position = position_dodge(width = 0.35)) +
    geom_point(
      data = filter(all, predictor == "nest type"),
      aes(x = eclipse, y = fit, color = factor(cov)),
      size = 3,
      position = position_dodge(width = 0.35)) +
    ylim(c(-0.01, 0.4)) +
    geom_text(data = filter(star.lab, predictor == "nest type"), 
              aes(x = eclipse, y = fit, label = p.lab),
              color = "black", 
              size = 3.25) +
    facet_wrap(~predictor) +
    theme_classic() +
    scale_color_manual(values = MetBrewer::MetPalettes$Ingres[[1]][c(4,6)]) +
    labs(y = "probability of vocalization during 4-minute window") +
    theme(strip.background = element_blank(),
          legend.position = "none",
          axis.text = element_text(size = 8.5, color = "black"), 
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 10, color = "black"), 
          axis.line = element_line(linewidth = 0.2, color = "black"),
          axis.ticks = element_line(linewidth = 0.2, color = "black"),,
          strip.text = element_text(color = "black", size = 10, face = "bold")) )

( lp_plot <- ggplot() +
    geom_errorbar(
      data = filter(all, predictor == "light pollution"),
      aes(x = eclipse, ymin = fit - se, ymax = fit + se, color = factor(cov)), 
      width = 0, 
      linewidth = 1.25,
      position = position_dodge(width = 0.35)) +
    geom_point(
      data = filter(all, predictor == "light pollution"),
      aes(x = eclipse, y = fit, color = factor(cov)),
      size = 3,
      position = position_dodge(width = 0.35)) +
    ylim(c(-0.01, 0.4)) +
    geom_text(data = filter(star.lab, predictor == "light pollution"), 
              aes(x = eclipse, y = fit, label = p.lab),
              color = "black", 
              size = 3.25) +
    facet_wrap(~predictor) +
    theme_classic() +
    scale_color_manual(values = MetBrewer::MetPalettes$Manet[[1]][c(1,3)]) +
    labs(y = "probability of vocalization during 4-minute window") +
    theme(strip.background = element_blank(),
          legend.position = "none",
          axis.text = element_text(size = 8.5, color = "black"), 
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 10, color = "black"), 
          axis.line = element_line(linewidth = 0.2, color = "black"),
          axis.ticks = element_line(linewidth = 0.2, color = "black"),,
          strip.text = element_text(color = "black", size = 10, face = "bold")) )


( et_plot <- ggplot() +
    geom_errorbar(
      data = filter(all, predictor == "eclipse time"),
      aes(x = eclipse, ymin = fit - se, ymax = fit + se, color = factor(cov)), 
      width = 0, 
      linewidth = 1.25,
      position = position_dodge(width = 0.35)) +
    geom_point(
      data = filter(all, predictor == "eclipse time"),
      aes(x = eclipse, y = fit, color = factor(cov)),
      size = 3,
      position = position_dodge(width = 0.35)) +
    ylim(c(-0.01, 0.4)) +
    geom_text(data = filter(star.lab, predictor == "eclipse time"), 
              aes(x = eclipse, y = fit, label = p.lab),
              color = "black", 
              size = 3.25) +
    facet_wrap(~predictor) +
    theme_classic() +
    scale_color_manual(values = MetBrewer::MetPalettes$Navajo[[1]][c(2,4)]) +
    labs(y = "probability of vocalization during 4-minute window") +
    theme(strip.background = element_blank(),
          legend.position = "none",
          axis.text = element_text(size = 8.5, color = "black"), 
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 10, color = "black"), 
          axis.line = element_line(linewidth = 0.2, color = "black"),
          axis.ticks = element_line(linewidth = 0.2, color = "black"),,
          strip.text = element_text(color = "black", size = 10, face = "bold")) )

dnc_plot + dnb_plot + cd_plot + hd_plot + mi_plot + 
  nt_plot + lp_plot + et_plot +  plot_layout(ncol = 2,
                                             axis_titles = "collect")

setwd(here::here("Results/Figures"))
ggsave(
  filename = "figure_02b.png", 
  width = 4, 
  height = 6, 
  units = "in",
  dpi = 600)
