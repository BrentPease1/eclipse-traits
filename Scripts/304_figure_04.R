library(here)
library(tidyverse)
library(mgcv)
library(MetBrewer)
library(patchwork)

load( here::here("Results/coverage_analysis_total_and_annular.RData"))

cov.sc <- scale(bin_df_to$coverage)
cov.sc.an <- scale(bin_df_an$coverage)
cov.p.an <- 17:90
cov.p.to <- 17:100

annular_nd <- tidyr::expand_grid(
  bin.sc = sort(unique(bin_df_an$bin.sc)), 
  coverage.sc = (cov.p.an - attr(cov.sc.an, "scaled:center"))/attr(cov.sc.an, "scaled:scale"))

annular_p <- predict(annular, annular_nd, exclude = "s(sp.station)", type = "response", se = TRUE, newdata.guaranteed = TRUE)

total_nd <- tidyr::expand_grid(
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
         title = "vocalization at eclipse maximum") +
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
    scale_x_continuous(limits = c(10, 100)) +
    
    # geom_line(linewidth = 1) +
    geom_line(linewidth = 1.25) +
    # geom_point(size = 0.75) +
    # geom_line() +
    theme_classic() +
    scale_color_manual(values = MetBrewer::MetPalettes$Johnson[[1]][c(2,4)]) +
    scale_fill_manual(values = MetBrewer::MetPalettes$Johnson[[1]][c(2,4)])  +
    labs(fill = "eclipse type", 
         color = "eclipse type",
         x = "obscuration (%)",
         y = "difference in p(vocalization)",
         title = "maximum relative to 32 min prior")  +
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
         x = "time from maximum (min)",
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
         x = "time from maximum (min)",
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

ggsave(
  filename = here::here("Figures/figure_04.png"), 
  width = 5, 
  height = 5, 
  units = "in", 
  dpi = 600)
