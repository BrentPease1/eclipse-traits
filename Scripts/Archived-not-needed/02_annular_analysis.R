# Analyses for annular eclipse
## conduct Analysis 1 (logistic regression comparing eclipse day versus preceding day)
## conduct Analysis 2 (logistic GAM quantifying vocalization progression)
## generate Figure S01

library(tidyverse)
library(mgcv)
library(MetBrewer)
library(suncalc)
library(patchwork)
library(glmmTMB)
library(brms)
library(tidybayes)
library(hms)
library(effects)
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

( figure_s01a <- effects::effect( term = c("eclipse"), mod = m) |> 
  tibble::as_tibble() |> 
  dplyr::mutate(eclipse = ifelse(eclipse == 1, "eclipse", "normal")) |> 
  dplyr::mutate(eclipse = factor(eclipse, levels = c("eclipse", "normal"))) |> 
  dplyr::mutate(lo = fit - se,
                hi = fit + se) |> 
  dplyr::mutate(lo = ifelse(lo < 0, 0, lo)) |> 

  ggplot(aes(x = fit, y = eclipse)) + 
  geom_errorbar(aes(xmin = lo, xmax = hi), width = 0, linewidth = 2,
                color = MetBrewer::MetPalettes$Isfahan1[[1]][5]) +
  geom_point(size = 4, color = MetBrewer::MetPalettes$Isfahan1[[1]][5]) +
  labs(x = "p(vocalization) during 4-minute bin") +
  theme_classic() +
  theme(strip.background = element_blank(),
        legend.position = "none",
        plot.background = element_rect(fill = NA, color = NA), 
        panel.background = element_rect(fill = NA, color = NA),
        axis.text = element_text(size = 8, color = "black"), 
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 9, color = "black"), 
        axis.line = element_line(linewidth = 0.2, color = "black"),
        axis.ticks = element_line(linewidth = 0.2, color = "black"),,
        strip.text = element_text(color = "black", size = 10, face = "bold")) )

m_brms <- brms::brm(
  y ~ 1 + eclipse + (1+eclipse|com),
  data = final,
  family = bernoulli(link = "logit"),
  prior = c(prior(normal(0, 2), class = "Intercept"),
            prior(normal(0, 0.5), class = "b"),
            prior(exponential(1), class = "sd"),
            prior(lkj(2), class = cor)),
  control = list(adapt_delta = 0.95),
  chains = 4,
  cores = 4)

pred <- tidyr::expand_grid(
  com = unique(final$com), 
  eclipse = unique(final$eclipse)) |> 
  tidybayes::add_epred_draws(m_brms, ndraws = 1000, re_formula = "~(1 + eclipse | com)") 

( figure_s01c <- pred |> 
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
        legend.position = c(0.815, 0.15),
        axis.text = element_text(size = 7.5, color = "black"), 
        axis.title.y = element_blank(),
        legend.title = element_text(size = 9, color = "black"),
        legend.text = element_text(size = 8, color = "black"),
        axis.title.x = element_text(size = 9, color = "black"), 
        axis.line = element_line(linewidth = 0.2, color = "black"),
        axis.ticks = element_line(linewidth = 0.2, color = "black"),,
        strip.text = element_text(color = "black", size = 10, face = "bold")) )

setwd(here::here("Results/Figures"))
ggsave(
  filename = "figure_s01c.png", 
  width = 3.5, 
  height = 7,
  units = "in", 
  dpi = 600)

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

( figure_s01b <- bin_df |> 
    dplyr::group_by( bin.lab, bin, bin_rel) |> 
    dplyr::summarise(tot = sum(y),
                     ny = sum(!is.na(y))) |>
    dplyr::mutate(prop = tot / ny) |> 
    ggplot(aes(x = bin_rel, y = prop)) +
    geom_point(size = 3,
               color = MetBrewer::MetPalettes$Isfahan1[[1]][c(6)]) +
    geom_line(linewidth = 0.75,
              alpha = 0.5,
              color = MetBrewer::MetPalettes$Isfahan1[[1]][c(6)]) +
    labs(x = "time from peak (min)",
         y = "proportion of community vocalizing",
         title = "(a)") +
    theme_classic() +
    theme(axis.line = element_line(color = "black", linewidth = 0.2), 
          axis.ticks = element_line(color = "black", linewidth = 0.2), 
          axis.text = element_text(color = "black", size = 8), 
          plot.title = element_blank(),
          axis.title = element_text(color = "black", size = 9),
          panel.background = element_blank(),
          plot.background = element_blank()) ) 

final <- bin_df |> 
  dplyr::filter(ny >= 5) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(bin.sc = as.numeric(scale(bin)),
                com = factor(com))

gs <- mgcv::bam(
  y ~ s(bin.sc, k = 8, m = 2) + s(bin.sc, com, bs = "fs", m = 2), 
  family = binomial(link = "logit"), 
  data = final)

nd_com <- tibble::tibble(bin.sc = sort(unique(final$bin.sc)))

pred.com <-  predict(gs, nd_com, exclude = "s(bin.sc,com)", se = TRUE, type = "response", newdata.guaranteed = TRUE)

com <- nd_com |> 
  tibble::add_column(fit = pred.com$fit,
             se = pred.com$se.fit) |>
  dplyr::left_join(
    final |> 
      dplyr::select(
        bin.sc, 
        bin.lab,
        bin, 
        bin_rel) |>
      dplyr::distinct())

nd_sp <- tidyr::expand_grid(
  bin.sc = sort(unique(final$bin.sc)), 
  com = sort(unique(final$com)))

pred.sp <- predict(gs, nd_sp, se = TRUE, type = "response", newdata.guaranteed = TRUE)

sp <- nd_sp |> 
  tibble::add_column(fit = pred.sp$fit, se = pred.sp$se.fit) |> 
  dplyr::left_join(
    final |> 
      dplyr::select(
        bin.sc, 
        bin.lab,
        bin, 
        bin_rel) |>
      dplyr::distinct())

( figure_s01c <- ggplot() +
    geom_line(data = sp,
              aes(x = bin_rel, y = fit, group = com), linewidth = 0.5,
              color = MetBrewer::MetPalettes$Ingres[[1]][4],
              alpha = 0.5) +
    geom_line(data = filter(sp, com %in% c("Barred Owl","Tufted Titmouse", "American Robin")),
              aes(x = bin_rel, y = fit, group = com),
              color = MetBrewer::MetPalettes$Ingres[[1]][4],
              alpha = 1, linewidth = 1.2) +
    geom_ribbon(data = com,
                aes(x = bin_rel, ymin = fit - se, ymax = fit + se), color = NA, alpha = 0.3,
                fill = MetBrewer::MetPalettes$Ingres[[1]][7]) +
    geom_line(data = com, aes(x = bin_rel, y = fit), linewidth = 1.75,
              color = MetBrewer::MetPalettes$Ingres[[1]][7]) +
    theme_classic() +
    labs(x = "time from peak (min)",
         y = "p(vocalization) during 4-minute bin",
         title = "(b)") +
    theme(axis.line = element_line(color = "black", linewidth = 0.2), 
          axis.ticks = element_line(color = "black", linewidth = 0.2), 
          axis.text = element_text(color = "black", size = 8), 
          plot.title = element_blank(),
          axis.title = element_text(color = "black", size = 9),
          panel.background = element_blank(),
          plot.background = element_blank() ) ) 

figure_s01a + figure_s01b + figure_s01c + plot_layout(ncol = 1, heights = c(1, 3, 3)) &
  theme(plot.background = element_rect(color = NA, fill = NA))

setwd(here::here("Results/Figures"))
ggsave(
  "figure_s01a-c.png", 
  width = 3, 
  height = 7, 
  units = "in", 
  dpi = 600)