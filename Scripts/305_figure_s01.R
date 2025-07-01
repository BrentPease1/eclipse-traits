library(here)
library(tidyverse)
library(MetBrewer)
library(patchwork)
library(effects)
library(glmmTMB)

load( here::here("Results/annular_eclipse_results.RData"))

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
    labs(x = "time from eclipse maximum (min)",
         y = "proportion of community vocalizing",
         title = "(b)") +
    theme_classic() +
    theme(axis.line = element_line(color = "black", linewidth = 0.2), 
          axis.ticks = element_line(color = "black", linewidth = 0.2), 
          axis.text = element_text(color = "black", size = 8), 
          plot.title = element_blank(),
          axis.title = element_text(color = "black", size = 9),
          panel.background = element_blank(),
          plot.background = element_blank()) ) 

nd_com <- tibble::tibble(bin.sc = sort(unique(final_bin$bin.sc)))

pred.com <-  predict(gs, nd_com, exclude = "s(bin.sc,com)", se = TRUE, type = "response", newdata.guaranteed = TRUE)

com <- nd_com |> 
  tibble::add_column(fit = pred.com$fit,
                     se = pred.com$se.fit) |>
  dplyr::left_join(
    final_bin |> 
      dplyr::select(
        bin.sc, 
        bin.lab,
        bin, 
        bin_rel) |>
      dplyr::distinct())

nd_sp <- tidyr::expand_grid(
  bin.sc = sort(unique(final_bin$bin.sc)), 
  com = sort(unique(final_bin$com)))

pred.sp <- predict(gs, nd_sp, se = TRUE, type = "response", newdata.guaranteed = TRUE)

sp <- nd_sp |> 
  tibble::add_column(fit = pred.sp$fit, se = pred.sp$se.fit) |> 
  dplyr::left_join(
    final_bin |> 
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
    labs(x = "time from eclipse maximum (min)",
         y = "p(vocalization) during 4-minute bin",
         title = "(b)") +
    theme(axis.line = element_line(color = "black", linewidth = 0.2), 
          axis.ticks = element_line(color = "black", linewidth = 0.2), 
          axis.text = element_text(color = "black", size = 8), 
          plot.title = element_blank(),
          axis.title = element_text(color = "black", size = 9),
          panel.background = element_blank(),
          plot.background = element_blank() ) )


pred <- tidyr::expand_grid(
  com = unique(final$com), 
  eclipse = unique(final$eclipse)) |> 
  tidybayes::add_epred_draws(m_brms, ndraws = 1000, re_formula = "~(1 + eclipse | com)") 

( figure_s01d <- pred |> 
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

( figure_s01a_c <- figure_s01a + figure_s01b + figure_s01c + plot_layout(ncol = 1, heights = c(1, 3, 3)) &
  theme(plot.background = element_rect(color = NA, fill = NA)) )

ggsave(
  filename = here::here("Figures/figure_s01a-c.png"), 
  plot = figure_s01a_c, 
  width = 3, 
  height = 7, 
  units = "in", 
  dpi = 600)

ggsave(
  filename = here::here("Figures/figure_s01d.png"), 
  plot = figure_s01d,
  width = 3.5, 
  height = 7,
  units = "in", 
  dpi = 600)
