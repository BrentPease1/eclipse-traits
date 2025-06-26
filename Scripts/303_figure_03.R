# Create Figure 3
library(here)
library(tidyverse)
library(MetBrewer)
library(mgcv)

load( here::here("Results/total_eclipse_vocalization_progression.RData"))

( figure_03a <- bin_df |> 
  dplyr::group_by( bin.lab, bin, bin_rel) |> 
  dplyr::summarise(tot = sum(y),
                   ny = sum(!is.na(y))) |>
  dplyr::mutate(prop = tot / ny) |> 
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

# 45/100species are "bimodal" with peaks of vocalization prior to and after totality
# of these, the pre-totality peak is higher than the post-totality peak for 31
# and for 14 the post-totality peak is higher than the pre-totality peak
# 2/97  of species are "unimodal" with a peak during totality (highest pvoc within 10 minutes of max eclipse time)
# remaining species don't fit either pattern (e.g., vocalization prob is flat over time or decreases linearly)
types <-
  nd_sp |> 
  tibble::add_column(fit = pred.sp$fit, 
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
  tibble::add_column(fit = pred.sp$fit, 
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


( general <- figure_03a + figure_03b + plot_layout(ncol =1) &
    theme(plot.background = element_rect(fill = NA, color = NA)))

ggsave(
  filename = here::here("Figures/figure_03a-b.png"), 
  plot = general,
  width = 2.2, 
  height = 6, 
  units = "in", 
  dpi = 600
)

( traits <- dnc_plot + dnb_plot + 
    es_plot + hd_plot +
    mi_plot + nt_plot +
    lp_plot + et_plot +
    plot_layout(ncol = 2, axis_titles = "collect") &
    theme(plot.background = element_rect(fill = NA, color = NA)))

ggsave(
  filename = here::here("Figures/figure_03c-j.png"), 
  plot = traits,
  width = 3.5, 
  height = 6, 
  units = "in", 
  dpi = 600)