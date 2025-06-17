library(tidyverse)
library(here)

setwd(here::here("data"))

d <- read_csv("birdnet_cleaned_v03_long.csv")

glimpse(d)

combos <- d |> 
  group_by( ESIDNumber, common_name) |> 
  summarise(y = sum(detection)) |> 
  dplyr::filter(y > 0) |> 
  dplyr::select(ESIDNumber, common_name) |> 
  dplyr::ungroup()

df <- d |> 
  dplyr::right_join(combos) |> 
  dplyr::filter(TimeSinceFirstContact <= 155) |> 
  dplyr::mutate(date = as.character(date)) |> 
  dplyr::filter(!path_status == "outside_path") |> 
  dplyr::mutate( eclipse = ifelse(date == "2024-04-08",2,1)) |> 
  dplyr::group_by(common_name) |> 
  dplyr::mutate(sp = dplyr::cur_group_id()) |> 
  dplyr::ungroup() |> 
  dplyr::rename(y = detection, 
                time = TimeSinceFirstContact) |> 
  dplyr::mutate(across(c(eclipse, sp), function(x) factor(x)))


glimpse(df)

m <- mgcv::bam(
  y ~ s(time,eclipse,bs = "fs",k = 6) + s(sp,bs = "re"),
  family = binomial(link = "logit"), 
  data = df)

newdata <- expand_grid(
  time = sort(unique(df$time)), 
  eclipse = unique(df$eclipse),
  sp = sort(unique(df$sp))[1] # even if you exclude the species smooth, predict() gets made if there isn't a sp column
)

p <- predict(m, newdata, exclude = "s(sp)", se = TRUE, type = "response")

key <- df |>
    dplyr::select(common = common_name, sp) |> 
    dplyr::distinct()

pdf <- tibble(
  mean = p$fit, 
  se = p$se.fit) |> 
  cbind(newdata) |> 
  as_tibble() |> 
  left_join(key) |> 
  dplyr::mutate( day_lab = ifelse(eclipse == 2, "Eclipse", "Normal"))

ggplot(pdf, aes(x = time, y = mean, color = day_lab)) +
  geom_ribbon(aes(ymin = mean - se, ymax = mean + se, fill = day_lab), alpha = 0.2, color = NA) +
  geom_line(linewidth = 2) +
  scale_color_manual(values = MetBrewer::MetPalettes$Isfahan1[[1]][c(6,3)]) +
  scale_fill_manual(values = MetBrewer::MetPalettes$Isfahan1[[1]][c(6,3)]) +
  # facet_wrap(~sp) +
  labs( x = "Time since first contact", 
        y = "Probability of vocalization\n within a 1-minute interval") +
  theme_minimal() +
  theme(axis.line = element_line(linewidth = 0.1, color = "black"),
        panel.background = element_rect(fill = "white", color = NA), 
        plot.background = element_rect(fill = "white", color = NA),
        legend.title = element_blank())

setwd(here::here("Results/Figures"))
ggsave(
  filename = "pvocalize.png", 
  width = 4, 
  height = 3, 
  units = "in", 
  dpi = 600)

props <- df |>
  dplyr::group_by(ESIDNumber) |> 
  # dplyr::mutate(nsp = length(unique(common_name))) |> 
  dplyr::group_by(date, time) |> 
  dplyr::summarise( ndet = sum(y > 0)) |> 
  dplyr::distinct() |> 
  filter(time <= 155) |> 
  dplyr::mutate(lab = ifelse(date == "2024-04-08", "Eclipse", "Normal"))


ggplot(props, aes(x = time, y = ndet, color = lab, fill = lab)) + 
  scale_color_manual(values = MetBrewer::MetPalettes$Isfahan1[[1]][c(6,3)]) +
  scale_fill_manual(values = MetBrewer::MetPalettes$Isfahan1[[1]][c(6,3)]) +
  geom_point(size = 3, alpha = 0.4) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 6)) +
  labs( x = "Time since first contact", 
        y = "Number of species with detections\n per 1-minute bin (across sites)") +
  theme_minimal() +
  theme(axis.line = element_line(linewidth = 0.1, color = "black"),
        panel.background = element_rect(fill = "white", color = NA), 
        plot.background = element_rect(fill = "white", color = NA),
        legend.title = element_blank())

setwd(here::here("Results/Figures"))
ggsave(
  filename = "proportion_figure.png", 
  width = 5, 
  height = 4, 
  units = "in", 
  dpi = 600
)

site_df <- d |> 
  dplyr::select(ESIDNumber, Latitude, Longitude) |> 
  dplyr::distinct()

setwd(here::here("data"))
write_csv(site_df, "site_coordinates.csv")
