# Create Figure 2

library(tidyverse)
library(here)
library(glmmTMB)
library(tidybayes)
library(janitor)
library(MetBrewer)
library(patchwork)
library(effects)

load( here::here("Results/total_eclipse_maximum.RData"))

m_nd <- tibble::tibble(eclipse = unique(final$eclipse))

m_p <- effects::effect(m, term = "eclipse") |> 
  tibble::as_tibble() |> 
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

ggsave(
  filename = here::here("Figures/figure_02a.png"), 
  width = 3.5, 
  height = 1.1,
  units = "in", 
  dpi = 600)

pred_com <- tidyr::expand_grid(
  eclipse = unique(final$eclipse)) |> 
  tidybayes::add_epred_draws( m_brms, ndraws = 1000, re_formula = NA)

# get 95% CI of percent change in pvoc
pred_com |> 
  dplyr::ungroup() |> 
  dplyr::select(eclipse, draw = .draw, p = .epred) |> 
  tidyr::pivot_wider(names_from = eclipse, values_from = p) |> 
  dplyr::mutate( pchange = (`1` - `0`) / `0`) |> 
  dplyr::summarise( mean = mean(pchange), 
             l95 = quantile(pchange, c(0.025)), 
             u95 = quantile(pchange, c(0.975))) |> 
  dplyr::mutate(across(mean:u95, function(x) round(100*x,)))

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

ggsave(
  filename = here::here("Figures/figure_02c.png"), 
  width = 3.5, 
  height = 7, 
  units = "in",
  dpi = 600)

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

ggsave(
  filename = here::here("Figures/figure_02b.png"), 
  width = 4, 
  height = 6, 
  units = "in",
  dpi = 600)