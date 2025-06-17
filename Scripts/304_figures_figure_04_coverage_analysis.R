library(tidyverse)
library(mgcv)
library(MetBrewer)
library(patchwork)

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

bw <- filter(birdnet, data_source == "BW") |> 
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

cov.sc <- scale(bin_df_to$coverage)
cov.sc.an <- scale(bin_df_an$coverage)
cov.p.an <- 17:90
cov.p.to <- 17:100

annular_nd <- expand_grid(
  bin.sc = sort(unique(bin_df_an$bin.sc)), 
  coverage.sc = (cov.p.an - attr(cov.sc.an, "scaled:center"))/attr(cov.sc.an, "scaled:scale"))

annular_p <- predict(annular, annular_nd, exclude = "s(sp.station)", type = "response", se = TRUE, newdata.guaranteed = TRUE)

total_nd <- expand_grid(
  bin.sc = sort(unique(bin_df_to$bin.sc)), 
  coverage.sc = (cov.p.to - attr(cov.sc, "scaled:center") ) / attr(cov.sc, "scaled:scale"))

total_p <- predict(total, total_nd, exclude = "s(sp.station)", type = "response", se = TRUE, newdata.guaranteed = TRUE)

p_peak <- annular_nd |> 
  tibble::add_column(fit = annular_p$fit, 
                     se = annular_p$se.fit) |> 
  dplyr::mutate(cov = coverage.sc * attr(cov.sc.an, "scaled:scale") + attr(cov.sc.an, "scaled:center")) |> 
  dplyr::mutate(cov = round(cov, 1)) |> 
  dplyr::left_join(
    bin_df_an |> 
      dplyr::select(bin, bin.sc, bin.lab, bin_rel) |>
      dplyr::distinct()) |> 
  dplyr::filter(bin_rel %in% c(0)) |> 
  dplyr::select(bin_rel, cov, fit, se) |> 
  tibble::add_column(eclipse = "annular") |> 
  dplyr::full_join(
    total_nd |> 
      tibble::add_column(fit = total_p$fit, 
                         se = total_p$se.fit) |>
      dplyr::mutate(cov = coverage.sc * attr(cov.sc, "scaled:scale") + attr(cov.sc, "scaled:center")) |> 
      dplyr::mutate(cov = round(cov, 1)) |> 
      dplyr::left_join(
        bin_df_to |> 
          dplyr::select(bin, bin.sc, bin.lab, bin_rel) |>
          dplyr::distinct())  |>
      dplyr::filter(bin_rel == 0) |>
      # dplyr::filter(bin_rel %in% c(-32, 0)) |> 
      dplyr::select(bin_rel, cov, fit, se) |> 
      tibble::add_column(eclipse = "total")) |> 
  dplyr::group_by(eclipse, bin_rel) |> 
  dplyr::mutate( id = dplyr::cur_group_id())

( panela <- ggplot(p_peak, aes(x = cov, y = fit, fill = eclipse, color = eclipse)) +
    geom_ribbon(aes(ymin = fit - se, ymax = fit + se), alpha = 0.2, color = NA) +
    geom_line(linewidth = 1.25) +
    # scale_color_manual(values = c("#D04E00", "#863300", "#0086A8", "#005268" )) +
    # scale_fill_manual(values = c("#D04E00", "#863300", "#0086A8", "#005268" )) +
    scale_color_manual(values = MetBrewer::MetPalettes$Johnson[[1]][c(2,4)]) +
    scale_fill_manual(values = MetBrewer::MetPalettes$Johnson[[1]][c(2,4)]) +
    theme_classic() +
    labs(fill = "eclipse type", 
         color = "eclipse type",
         x = "obscuration (%)",
         y = "p(vocalization) during 4-minute bin",
         title = "vocalization at peak eclipse") +
    theme(axis.line = element_line(color = "black", linewidth = 0.2), 
          axis.ticks = element_line(color = "black", linewidth = 0.2), 
          axis.text = element_text(color = "black", size = 8), 
          axis.title = element_text(color = "black", size = 9),
          plot.title = element_text(color = "black", size = 9, hjust = 0.5, face = "bold"),
          strip.background = element_blank(), 
          strip.text = element_text(color = "black", size = 9, face = "bold"),
          panel.background = element_blank(),
          plot.background = element_blank(),
          legend.title = element_text(color = "black", size = 9), 
          legend.text = element_text(color = "black", size = 8),
          legend.position = "none") )

# visualize differences at peak versus 32 minutes before for differing levels of coverage
( panelb <- annular_nd |> 
    tibble::add_column(fit = annular_p$fit, 
                       se = annular_p$se.fit) |> 
    dplyr::mutate(cov = coverage.sc * attr(cov.sc.an, "scaled:scale") + attr(cov.sc.an, "scaled:center")) |> 
    dplyr::mutate(cov = round(cov, 1)) |> 
    dplyr::left_join(
      bin_df_an |> 
        dplyr::select(bin, bin.sc, bin.lab, bin_rel) |>
        dplyr::distinct()) |> 
    dplyr::filter(bin_rel %in% c(0, -32)) |> 
    dplyr::select(cov, fit, bin_rel) |> 
    pivot_wider(names_from = bin_rel, values_from = fit) |> 
    dplyr::mutate( diff = `0` - `-32`) |>
    add_column(type = "annular") |> 
    full_join(
      total_nd |> 
        tibble::add_column(fit = total_p$fit, 
                           se = total_p$se.fit) |>
        dplyr::mutate(cov = coverage.sc * attr(cov.sc, "scaled:scale") + attr(cov.sc, "scaled:center")) |> 
        dplyr::mutate(cov = round(cov, 1)) |> 
        dplyr::left_join(
          bin_df_to |> 
            dplyr::select(bin, bin.sc, bin.lab, bin_rel) |>
            dplyr::distinct()) |> 
        dplyr::filter(bin_rel %in% c(0, -32)) |> 
        dplyr::select(cov, fit, bin_rel) |> 
        pivot_wider(names_from = bin_rel, values_from = fit) |> 
        dplyr::mutate( diff = `0` - `-32`) |> 
        add_column(type = "total")) |> 
    ggplot(aes(x = cov, y = diff, color = type)) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.25) +
    
    # geom_line(linewidth = 1) +
    geom_point(size = 0.75) +
    # geom_line() +
    theme_classic() +
    scale_color_manual(values = MetBrewer::MetPalettes$Johnson[[1]][c(2,4)]) +
    scale_fill_manual(values = MetBrewer::MetPalettes$Johnson[[1]][c(2,4)])  +
    labs(fill = "eclipse type", 
         color = "eclipse type",
         x = "obscuration (%)",
         y = "difference in p(vocalization)",
         title = "peak relative to 32 minutes prior")  +
    theme(axis.line = element_line(color = "black", linewidth = 0.2), 
          axis.ticks = element_line(color = "black", linewidth = 0.2), 
          axis.text = element_text(color = "black", size = 8), 
          axis.title = element_text(color = "black", size = 9),
          plot.title = element_text(color = "black", size = 9, hjust = 0.5, face = "bold"),
          strip.background = element_blank(), 
          strip.text = element_text(color = "black", size = 9, face = "bold"),
          panel.background = element_blank(),
          plot.background = element_blank(),
          legend.title = element_text(color = "black", size = 9), 
          legend.text = element_text(color = "black", size = 8),
          legend.position = "none") )

cp <- colorRampPalette(c(MetBrewer::MetPalettes$Manet[[1]][4], "black"))

# figuring out what colors for the different coverage levels for consistency
tibble(coverage = 17:100) |> add_column(col = cp(84)) |> filter(coverage %in% c(17, 83, 100)) |> pull(col) |> dput()
tibble(coverage = 17:100) |> add_column(col = cp(84)) |> filter(coverage %in% c(17, 80, 90)) |> pull(col) |> dput()

( panelc <- total_nd |> 
    tibble::add_column(fit = total_p$fit, 
                       se = total_p$se.fit) |>
    dplyr::mutate(cov = coverage.sc * attr(cov.sc, "scaled:scale") + attr(cov.sc, "scaled:center")) |> 
    dplyr::mutate(cov = round(cov, 1)) |> 
    dplyr::left_join(
      bin_df_to |> 
        dplyr::select(bin, bin.sc, bin.lab, bin_rel) |>
        dplyr::distinct()) |> 
    dplyr::filter(cov %in% c(17, 83, 100)) |> 
    
    ggplot(aes(x = bin_rel, y = fit, color = factor(cov), fill = factor(cov))) +
    geom_ribbon(aes(ymin = fit - se, ymax = fit + se), color = NA, alpha = 0.2) +
    geom_line(linewidth = 1.25) +
    scale_color_manual( values = c("#EBC174", "#302717", "#000000")) +
    scale_fill_manual( values = c("#EBC174", "#302717", "#000000")) +
    # scale_color_manual(values = cp(3)) +
    # scale_fill_manual(values = cp(3)) +
    theme_classic() +
    labs(fill = "obscuration (%)", 
         color = "obscuration (%)",
         x = "time from peak (min)",
         y = "p(vocalization) during 4-minute bin",
         title = "total eclipse") +
    theme(axis.line = element_line(color = "black", linewidth = 0.2), 
          axis.ticks = element_line(color = "black", linewidth = 0.2), 
          axis.text = element_text(color = "black", size = 8), 
          axis.title = element_text(color = "black", size = 9),
          plot.title = element_text(color = "black", size = 9, hjust = 0.5, face = "bold"),
          strip.background = element_blank(), 
          strip.text = element_text(color = "black", size = 9, face = "bold"),
          panel.background = element_blank(),
          plot.background = element_blank(),
          legend.title = element_text(color = "black", size = 9), 
          legend.text = element_text(color = "black", size = 8),
          legend.position = "none") )

( paneld <- annular_nd |> 
    tibble::add_column(fit = annular_p$fit, 
                       se = annular_p$se.fit) |> 
    dplyr::mutate(cov = coverage.sc * attr(cov.sc.an, "scaled:scale") + attr(cov.sc.an, "scaled:center")) |> 
    dplyr::mutate(cov = round(cov, 1)) |> 
    dplyr::left_join(
      bin_df_an |> 
        dplyr::select(bin, bin.sc, bin.lab, bin_rel) |>
        dplyr::distinct()) |> 
    dplyr::filter(cov %in% c(17, 80, 90)) |> 
    ggplot(aes(x = bin_rel, y = fit, color = factor(cov), fill = factor(cov))) +
    geom_ribbon(aes(ymin = fit - se, ymax = fit + se), color = NA, alpha = 0.2) +
    geom_line(linewidth = 1.25) +
    scale_color_manual(values = c("#EBC174", "#382E1B", "#1C170D")) +
    scale_fill_manual(values = c("#EBC174", "#382E1B", "#1C170D")) +
    theme_classic() +
    labs(fill = "obscuration (%)", 
         color = "obscuration (%)",
         x = "time from peak (min)",
         y = "p(vocalization) during 4-minute bin",
         title = "annular eclipse") +
    theme(axis.line = element_line(color = "black", linewidth = 0.2), 
          axis.ticks = element_line(color = "black", linewidth = 0.2), 
          axis.text = element_text(color = "black", size = 8), 
          axis.title = element_text(color = "black", size = 9),
          plot.title = element_text(color = "black", size = 9, hjust = 0.5, face = "bold"),
          strip.background = element_blank(), 
          strip.text = element_text(color = "black", size = 9, face = "bold"),
          panel.background = element_blank(),
          plot.background = element_blank(),
          legend.title = element_text(color = "black", size = 9), 
          legend.text = element_text(color = "black", size = 8),
          legend.position = "none") )

panela + panelb + panelc + paneld + plot_layout(ncol = 2) &
  theme(plot.background = element_rect(color = NA, fill = NA))

setwd(here::here("Results/Figures"))
ggsave(
  filename = "figure_04.png", 
  width = 5, 
  height = 5, 
  units = "in", 
  dpi = 600)