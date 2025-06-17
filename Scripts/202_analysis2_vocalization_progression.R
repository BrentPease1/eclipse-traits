library(data.table)
library(lubridate)
library(here)
library(ggplot2)
library(glmmTMB)
library(hms)
library(lutz)
library(suncalc)
library(janitor)
library(tidyverse)
library(mgcv)
library(patchwork)

# elton traits for family name
elton <- fread(here::here('Data/elton.txt'))
# Read data
birdnet <- readr::read_csv( here::here("Data/merged_es_bw_apr2025_viirs.csv"))

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
  dplyr::full_join(
    template) |> 
  dplyr::mutate(y = tidyr::replace_na(y, 0),
                ndets = tidyr::replace_na(ndets, 0)) |>
  dplyr::group_by(com, sci) |> 
  dplyr::mutate(ny = sum(y)) |> 
  dplyr::arrange(lat, lon, com, bin)

( figure_03a <- bin_df |> 
    dplyr::group_by( bin.lab, bin, bin_rel) |> 
    dplyr::summarise(tot = sum(y),
                     ny = sum(!is.na(y))) |>
    dplyr::mutate(prop = tot / ny) |> 
    # pivot_longer(y:n, names_to = "type", values_to = "val") |> 
    # dplyr::filter(type == "y") |> 
    
    ggplot(aes(x = bin_rel, y = prop)) +
    geom_point(size = 2,
               color = MetBrewer::MetPalettes$Isfahan1[[1]][c(6)]) +
    geom_line(linewidth = 0.3,
              alpha = 0.5,
              color = MetBrewer::MetPalettes$Isfahan1[[1]][c(6)]) +
    labs(x = "time from peak (min)",
         y = "proportion of community vocalizing",
         title = "(a)") +
    theme_classic() +
    theme(axis.line = element_line(color = "black", linewidth = 0.2), 
          axis.ticks = element_line(color = "black", linewidth = 0.2), 
          axis.text = element_text(color = "black", size = 8), 
          plot.title = element_text(color = "black", size = 9, vjust = -2.5, hjust = -0.15),
          axis.title = element_text(color = "black", size = 9),
          panel.background = element_blank(),
          plot.background = element_blank()) ) 

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

noct <- bin_df |> 
  dplyr::select(com, sci) |> 
  dplyr::distinct() |> 
  dplyr::left_join(key) |> 
  dplyr::left_join(elton) |> 
  dplyr::select(com, sci, noct) |> 
  dplyr::group_by(com, sci) |> 
  dplyr::summarise( noct = unique(noct)) |> 
  dplyr::ungroup()

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
  dplyr::mutate(month = lubridate::month(date)) |> 
  dplyr::filter(month == 4) |> 
  dplyr::left_join(sr_ss) |> 
  dplyr::filter(conf >= 0.65 ) |> 
  dplyr::mutate( noct = ifelse( time < sunrise | time > sunset, 1, 0)) |>  
  dplyr::filter(!is.na(noct)) |> 
  dplyr::group_by(com, sci) |> 
  dplyr::summarise( pnoct = sum(noct) / sum(!is.na(noct)))

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

# works, but takes a good 10-15 minutes
gs <- mgcv::bam(
  y ~ s(bin.sc, k = 8, m = 2) + s(bin.sc, com, bs = "fs", m = 2), 
  family = binomial(link = "logit"), 
  data = final)

nd_com <- tibble(bin.sc = sort(unique(final$bin.sc)))

pred.com <-  predict(gs, nd_com, exclude = "s(bin.sc,com)", se = TRUE, type = "response", newdata.guaranteed = TRUE)

com <- nd_com |> 
  add_column(fit = pred.com$fit,
             se = pred.com$se.fit) |>
  dplyr::left_join(
    final |> 
      dplyr::select(
        bin.sc, 
        bin.lab,
        bin, 
        bin_rel) |>
      dplyr::distinct())

nd_sp <- expand_grid(
  bin.sc = sort(unique(final$bin.sc)), 
  com = sort(unique(final$com)))

pred.sp <- predict(gs, nd_sp, se = TRUE, type = "response", newdata.guaranteed = TRUE)

# 45/100species are "bimodal" with peaks of vocalization prior to and after totality
# of these, the pre-totality peak is higher than the post-totality peak for 31
# and for 14 the post-totality peak is higher than the pre-totality peak
# 2/97  of species are "unimodal" with a peak during totality (highest pvoc within 10 minutes of max eclipse time)
# remaining species don't fit either pattern (e.g., vocalization prob is flat over time or decreases linearly)
types <-
  nd_sp |> 
  add_column(fit = pred.sp$fit, 
             se = pred.sp$se.fit) |> 
  dplyr::left_join(
    final |> 
      dplyr::ungroup() |> 
      dplyr::select(
        bin.sc, 
        bin.lab,
        bin, 
        bin_rel) |>
      dplyr::distinct()) |>
  dplyr::mutate( period = ifelse(bin_rel < -12, "pre", ifelse( bin_rel > 12, "post", "tot"))) |> 
  dplyr::group_by(com, period) |> 
  dplyr::filter(fit == max(fit)) |>
  dplyr::select(com, fit, period) |>
  tidyr::pivot_wider(names_from = period, values_from = fit) |> 
  dplyr::mutate( type = ifelse(tot > pre & tot > post, "unimodal",
                               ifelse( tot < pre & tot < post, "bimodal", "other"))) |> 
  dplyr::mutate(type2 = type) |> 
  dplyr::mutate(type2 = ifelse(type2 == "other" & pre > post, "pre-eclise highest",
                               ifelse(type2 == "other" & pre < post, "post-eclipse highest", type2)))


sp <- nd_sp |> 
  add_column(fit = pred.sp$fit, 
             se = pred.sp$se.fit) |> 
  dplyr::left_join(
    final |> 
      dplyr::select(
        bin.sc, 
        bin.lab,
        bin, 
        bin_rel) |>
      dplyr::distinct()) |> 
  dplyr::group_by(com) |> 
  dplyr::mutate(ma = max(fit)) |>
  dplyr::group_by(com, bin_rel) |> 
  dplyr::mutate( bin_w_peak = max(fit)) |> 
  dplyr::left_join(types) #|> 

( figure_03b <- ggplot() +
    geom_line(data = filter(sp, !(com %in% c("Barred Owl", "Tufted Titmouse")) & ma < 0.4),
              aes(x = bin_rel, y = fit, group = com), linewidth = 0.3,
              color = MetBrewer::MetPalettes$Ingres[[1]][4],
              alpha = 0.2) +
    geom_line(data = filter(sp, com %in% c("Barred Owl","Tufted Titmouse", "American Robin")),
              aes(x = bin_rel, y = fit, group = com),
              color = MetBrewer::MetPalettes$Ingres[[1]][4],
              alpha = 1, linewidth = 1) +
    geom_ribbon(data = com,
                aes(x = bin_rel, ymin = fit - se, ymax = fit + se), color = NA, alpha = 0.3,
                fill = MetBrewer::MetPalettes$Ingres[[1]][7]) +
    geom_line(data = com, aes(x = bin_rel, y = fit), linewidth = 1.5,
              color = MetBrewer::MetPalettes$Ingres[[1]][7]) +
    theme_classic() +
    labs(x = "time from peak (min)",
         y = "probability of vocalization during 4-minute window",
         title = "(b)") +
    theme(axis.line = element_line(color = "black", linewidth = 0.2), 
          axis.ticks = element_line(color = "black", linewidth = 0.2), 
          axis.text = element_text(color = "black", size = 8), 
          plot.title = element_text(color = "black", size = 9, vjust = -2.5, hjust = -0.15),
          axis.title = element_text(color = "black", size = 9),
          panel.background = element_blank(),
          plot.background = element_blank() ) ) 

dnb <- mgcv::bam(
  y ~ s(bin.sc, by = factor(noct), k = 8, bs = "fs") + factor(noct) + s(sp.station, bs = "re"),
  family = binomial(link = "logit"), 
  data = final)

dnc <- mgcv::bam(
  y ~ s(bin.sc, by = pnoct, k = 8, bs = "tp") + pnoct + s(sp.station, bs = "re"),
  family = binomial(link = "logit"),
  data = final)

es <- mgcv::bam(
  y ~ s(bin.sc, by = cd, k = 8, bs = "tp") + s(sp.station, bs = "re"),
  family = binomial(link = "logit"), 
  data = final)

hd <- mgcv::bam(
  y ~ s(bin.sc, by = hd, k = 8, bs = "tp", m = 1) + hd + s(sp.station, bs = "re"), 
  family = binomial(link = "logit"), 
  data = final)

mi <- mgcv::bam(
  y ~ s(bin.sc, by = migration, k = 8, bs = "tp", m = 1) + migration + s(sp.station, bs = "re"),
  family = binomial(link = "logit"),
  data = final)

nt <- mgcv::bam(
  y ~ s(bin.sc, by = cavity, k = 8, bs = "tp", m = 1) + cavity + s(sp.station, bs = "re"), 
  family = binomial(link = "logit"), 
  data = final)

lp <- mgcv::bam(
  y ~ s(bin.sc, by = alan, k = 8, bs = "tp") + alan + s(sp.station, bs = "re"),
  family = binomial(link = "logit"), 
  data = final)

et <- mgcv::bam(
  y ~ s(bin.sc, by = et, k = 8, bs = "tp") + et + s(sp.station, bs = "re"),
  family = binomial(link = "logit"), 
  data = filter(final, !is.na(et)))

pn.sc <- scale( final$pnoct_raw)

dnb_nd <- tidyr::expand_grid(bin.sc = sort(unique(final$bin.sc)), noct = unique(final$noct))
dnc_nd <- tidyr::expand_grid(bin.sc = sort(unique(final$bin.sc)), pnoct = (c(0.01, 0.25, 0.6, 0.99) - attr(pn.sc, "scaled:center"))/attr(pn.sc, "scaled:scale"))
es_nd <- tidyr::expand_grid(bin.sc = sort(unique(final$bin.sc)), cd = c(min(final$cd), max(final$cd)))
hd_nd <- tidyr::expand_grid(bin.sc = sort(unique(final$bin.sc)), hd = unique(final$hd))
mi_nd <- tidyr::expand_grid(bin.sc = sort(unique(final$bin.sc)), migration = unique(final$migration))
nt_nd <- tidyr::expand_grid(bin.sc = sort(unique(final$bin.sc)), cavity = unique(final$cavity))
lp_nd <- tidyr::expand_grid(bin.sc = sort(unique(final$bin.sc)), alan = c(min(final$alan), max(final$alan)))
et_nd <- tidyr::expand_grid(bin.sc = sort(unique(final$bin.sc)), et = c(min(final$et, na.rm = TRUE), max(final$et, na.rm = TRUE))) 

dnb_p <- predict(dnb, dnb_nd, exclude = "s(sp.station)", se = TRUE, type = "response", newdata.guaranteed = TRUE)
dnc_p <- predict(dnc, dnc_nd, exclude = "s(sp.station)", se = TRUE, type = "response", newdata.guaranteed = TRUE)
es_p <- predict(es, es_nd, exclude = "s(sp.station)", se = TRUE, type = "response", newdata.guaranteed = TRUE)
hd_p <- predict(hd, hd_nd, exclude = "s(sp.station)", se = TRUE, type = "response", newdata.guaranteed = TRUE)
mi_p <- predict(mi, mi_nd, exclude = "s(sp.station)", se = TRUE, type = "response", newdata.guaranteed = TRUE)
nt_p <- predict(nt, nt_nd, exclude = "s(sp.station)", se = TRUE, type = "response", newdata.guaranteed = TRUE)
lp_p <- predict(lp, lp_nd, exclude = "s(sp.station)", se = TRUE, type = "response", newdata.guaranteed = TRUE)
et_p <- predict(et, et_nd, exclude = "s(sp.station)", se = TRUE, type = "response", newdata.guaranteed = TRUE)

all_p <- dnb_nd |>
  dplyr::rename(cov = noct) |> 
  tibble::add_column(
    fit = dnb_p$fit,
    se = dnb_p$se.fit,
    predictor = "diel niche (binary)") |> 
  dplyr::full_join(
    dnc_nd |> 
      dplyr::rename(cov = pnoct) |> 
      tibble::add_column(
        fit = dnc_p$fit,
        se = dnc_p$se.fit,
        predictor = "diel niche (continuous)")) |> 
  dplyr::full_join(
    es_nd |>
      dplyr::rename(cov = cd) |> 
      tibble::add_column(
        fit = es_p$fit,
        se = es_p$se.fit,
        predictor = "eye size")) |> 
  dplyr::full_join(
    hd_nd |>
      dplyr::rename(cov = hd) |>
      dplyr::mutate(cov = as.numeric(cov)) |> 
      tibble::add_column(
        fit = hd_p$fit,
        se = hd_p$se.fit,
        predictor = "habitat density")) |> 
  dplyr::full_join(
    mi_nd |>
      dplyr::rename(cov = migration) |> 
      dplyr::mutate(cov = as.numeric(cov)) |> 
      tibble::add_column(
        fit = mi_p$fit,
        se = mi_p$se.fit,
        predictor = "migration")) |> 
  dplyr::full_join(
    nt_nd |>
      dplyr::rename(cov = cavity) |> 
      dplyr::mutate(cov = as.numeric(cov)) |> 
      tibble::add_column(
        fit = nt_p$fit,
        se = nt_p$se.fit,
        predictor = "nest type")) |> 
  dplyr::full_join(
    lp_nd |>
      dplyr::rename(cov = alan) |> 
      tibble::add_column(
        fit = lp_p$fit,
        se = lp_p$se.fit,
        predictor = "light pollution")) |> 
  dplyr::full_join(
    et_nd |>
      dplyr::rename(cov = et) |> 
      tibble::add_column(
        fit = et_p$fit,
        se = et_p$se.fit,
        predictor = "eclipse time")) |> 
  dplyr::left_join(
    final |> 
      dplyr::select(bin.sc, bin, bin.lab, bin_rel) |> 
      dplyr::distinct()) |> 
  dplyr::mutate(upper = fit + se, 
                lower = fit - se) |> 
  dplyr::mutate(lower = ifelse(lower < 0, 0, lower))

( dnb_plot <- ggplot( data = filter(all_p, predictor == "diel niche (binary)"),
                      aes(x = bin_rel, y = fit, color = factor(cov), fill = factor(cov))) +
    facet_wrap(~predictor) +
    ylim(c(0, 0.20)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, color = NA) +
    geom_line(size = 1) +
    theme_classic() +
    scale_color_manual(values = MetBrewer::MetPalettes$Kandinsky[[1]][c(2,4)]) +
    scale_fill_manual(values = MetBrewer::MetPalettes$Kandinsky[[1]][c(2,4)]) +
    labs(x = "time from peak (min)",
         y = "probability of vocalization during 4-minute window",
         title = "(d)") +
    theme(axis.line = element_line(color = "black", linewidth = 0.2), 
          axis.ticks = element_line(color = "black", linewidth = 0.2), 
          axis.text = element_text(color = "black", size = 8), 
          axis.title = element_text(color = "black", size = 9),
          strip.background = element_blank(), 
          strip.text = element_text(color = "black", size = 9, face = "bold"),
          plot.title = element_text(color = "black", size = 9, vjust = -2.5, hjust = -0.15),
          panel.background = element_blank(),
          plot.background = element_blank(),
          legend.position = "none") )

cp <- colorRampPalette(MetBrewer::MetPalettes$Kandinsky[[1]][c(2, 4)])

( dnc_plot <- ggplot( data = filter(all_p, predictor == "diel niche (continuous)" & ( cov < 0 | cov > 3)),
                      aes(x = bin_rel, y = fit, color = factor(cov), fill = factor(cov))) +
    facet_wrap(~predictor) +
    ylim(c(0, 0.20)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, color = NA) +
    geom_line(size = 1) +
    theme_classic() +
    scale_color_manual(values = cp(3)) +
    scale_fill_manual(values = cp(3)) +
    labs(x = "time from peak (min)",
         y = "probability of vocalization during 4-minute window",
         title = "(c)") +
    theme(axis.line = element_line(color = "black", linewidth = 0.2), 
          axis.ticks = element_line(color = "black", linewidth = 0.2), 
          axis.text = element_text(color = "black", size = 8), 
          axis.title = element_text(color = "black", size = 9),
          strip.background = element_blank(), 
          strip.text = element_text(color = "black", size = 9, face = "bold"),
          plot.title = element_text(color = "black", size = 9, vjust = -2.5, hjust = -0.15),
          strip.clip = "off",
          panel.background = element_blank(),
          plot.background = element_blank(),
          legend.position = "none") )

( es_plot <- ggplot( data = filter(all_p, predictor == "eye size"),
                     aes(x = bin_rel, y = fit, color = factor(cov), fill = factor(cov))) +
    facet_wrap(~predictor) +
    ylim(c(0, 0.20)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, color = NA) +
    geom_line(size = 1) +
    theme_classic() +
    scale_color_manual(values = MetBrewer::MetPalettes$Hokusai1[[1]][c(2,7)]) +
    scale_fill_manual(values = MetBrewer::MetPalettes$Hokusai1[[1]][c(2,7)]) +
    labs(x = "time from peak (min)",
         y = "probability of vocalization during 4-minute window",
         title = "(e)") +
    theme(axis.line = element_line(color = "black", linewidth = 0.2), 
          axis.ticks = element_line(color = "black", linewidth = 0.2), 
          axis.text = element_text(color = "black", size = 8), 
          axis.title = element_text(color = "black", size = 9),
          plot.title = element_text(color = "black", size = 9, vjust = -2.5, hjust = -0.15),
          strip.background = element_blank(), 
          strip.text = element_text(color = "black", size = 9, face = "bold"),
          panel.background = element_blank(),
          plot.background = element_blank(),
          legend.position = "none") )

hd_cp <- colorRampPalette(MetBrewer::MetPalettes$Hokusai2[[1]][c(2, 5)])

( hd_plot <- ggplot( data = filter(all_p, predictor == "habitat density" & cov %in% c(1,3)),
                     aes(x = bin_rel, y = fit, color = factor(cov), fill = factor(cov))) +
    facet_wrap(~predictor) +
    ylim(c(0, 0.20)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, color = NA) +
    geom_line(size = 1) +
    theme_classic() +
    scale_color_manual(values = hd_cp(2)) +
    scale_fill_manual(values = hd_cp(2)) +
    labs(x = "time from peak (min)",
         y = "probability of vocalization during 4-minute window",
         title = "(f)") +
    theme(axis.line = element_line(color = "black", linewidth = 0.2), 
          axis.ticks = element_line(color = "black", linewidth = 0.2), 
          axis.text = element_text(color = "black", size = 8), 
          axis.title = element_text(color = "black", size = 9),
          plot.title = element_text(color = "black", size = 9, vjust = -2.5, hjust = -0.15),
          strip.background = element_blank(), 
          strip.text = element_text(color = "black", size = 9, face = "bold"),
          panel.background = element_blank(),
          plot.background = element_blank(),
          legend.position = "none") )


mi_cp <- colorRampPalette( MetBrewer::MetPalettes$Cassatt2[[1]][c(3,8)])

( mi_plot <- ggplot( data = filter(all_p, predictor == "migration" & cov %in% c(1,3)),
                     aes(x = bin_rel, y = fit, color = factor(cov), fill = factor(cov))) +
    facet_wrap(~predictor) +
    ylim(c(0, 0.20)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, color = NA) +
    geom_line(size = 1) +
    theme_classic() +
    scale_color_manual(values = mi_cp(2)) +
    scale_fill_manual(values = mi_cp(2)) +
    labs(x = "time from peak (min)",
         y = "probability of vocalization during 4-minute window",
         title = "(g)") +
    theme(axis.line = element_line(color = "black", linewidth = 0.2), 
          axis.ticks = element_line(color = "black", linewidth = 0.2), 
          axis.text = element_text(color = "black", size = 8), 
          axis.title = element_text(color = "black", size = 9),
          strip.background = element_blank(), 
          plot.title = element_text(color = "black", size = 9, vjust = -2.5, hjust = -0.15),
          strip.text = element_text(color = "black", size = 9, face = "bold"),
          panel.background = element_blank(),
          plot.background = element_blank(),
          legend.position = "none") )

( nt_plot <- ggplot( data = filter(all_p, predictor == "nest type"),
                     aes(x = bin_rel, y = fit, color = factor(cov), fill = factor(cov))) +
    facet_wrap(~predictor) +
    ylim(c(0, 0.20)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, color = NA) +
    geom_line(size = 1) +
    theme_classic() +
    scale_color_manual(values = MetBrewer::MetPalettes$Ingres[[1]][c(4,6)]) +
    scale_fill_manual(values = MetBrewer::MetPalettes$Ingres[[1]][c(4,6)]) +
    labs(x = "time from peak (min)",
         y = "probability of vocalization during 4-minute window",
         title = "(h)") +
    theme(axis.line = element_line(color = "black", linewidth = 0.2), 
          axis.ticks = element_line(color = "black", linewidth = 0.2), 
          axis.text = element_text(color = "black", size = 8), 
          plot.title = element_text(color = "black", size = 9, vjust = -2.5, hjust = -0.15),
          axis.title = element_text(color = "black", size = 9),
          strip.background = element_blank(), 
          strip.text = element_text(color = "black", size = 9, face = "bold"),
          panel.background = element_blank(),
          plot.background = element_blank(),
          legend.position = "none") )

( lp_plot <- ggplot( data = filter(all_p, predictor == "light pollution"),
                     aes(x = bin_rel, y = fit, color = factor(cov), fill = factor(cov))) +
    facet_wrap(~predictor) +
    ylim(c(0, 0.20)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, color = NA) +
    geom_line(size = 1) +
    theme_classic() +
    scale_color_manual(values = MetBrewer::MetPalettes$Manet[[1]][c(1,3)]) +
    scale_fill_manual(values = MetBrewer::MetPalettes$Manet[[1]][c(1,3)]) +
    labs(x = "time from peak (min)",
         y = "probability of vocalization during 4-minute window",
         title = "(i)") +
    theme(axis.line = element_line(color = "black", linewidth = 0.2), 
          axis.ticks = element_line(color = "black", linewidth = 0.2), 
          axis.text = element_text(color = "black", size = 8), 
          axis.title = element_text(color = "black", size = 9),
          plot.title = element_text(color = "black", size = 9, vjust = -2.5, hjust = -0.15),
          strip.background = element_blank(), 
          strip.text = element_text(color = "black", size = 9, face = "bold"),
          panel.background = element_blank(),
          plot.background = element_blank(),
          legend.position = "none") )

( et_plot <- ggplot( data = filter(all_p, predictor == "eclipse time"),
                     aes(x = bin_rel, y = fit, color = factor(cov), fill = factor(cov))) +
    facet_wrap(~predictor) +
    ylim(c(0, 0.20)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, color = NA) +
    geom_line(size = 1) +
    theme_classic() +
    scale_color_manual(values = MetBrewer::MetPalettes$Navajo[[1]][c(2,4)]) +
    scale_fill_manual(values = MetBrewer::MetPalettes$Navajo[[1]][c(2,4)]) +
    labs(x = "time from peak (min)",
         y = "probability of vocalization during 4-minute window",
         title = "(j)") +
    theme(axis.line = element_line(color = "black", linewidth = 0.2), 
          axis.ticks = element_line(color = "black", linewidth = 0.2), 
          axis.text = element_text(color = "black", size = 8), 
          axis.title = element_text(color = "black", size = 9),
          plot.title = element_text(color = "black", size = 9, vjust = -2.5, hjust = -0.15),
          strip.background = element_blank(), 
          strip.text = element_text(color = "black", size = 9, face = "bold"),
          panel.background = element_blank(),
          plot.background = element_blank(),
          legend.position = "none") )

( traits <- dnc_plot + dnb_plot + 
    es_plot + hd_plot +
    mi_plot + nt_plot +
    lp_plot + et_plot +
    plot_layout(ncol = 2, axis_titles = "collect") &
    theme(plot.background = element_rect(fill = NA, color = NA)))

setwd(here::here("Results/Figures"))
ggsave(
  filename = "figure_03c-j.png", 
  width = 3.5, 
  height = 6, 
  units = "in", 
  dpi = 600)

( general <- figure_03a + figure_03b + plot_layout(ncol =1) &
    theme(plot.background = element_rect(fill = NA, color = NA)))

setwd(here::here("Results/Figures"))
ggsave(
  filename = "figure_03a-b.png", 
  width = 2.2, 
  height = 6, 
  units = "in", 
  dpi = 600)
