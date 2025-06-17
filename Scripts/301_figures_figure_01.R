library(here)
library(tidyverse)
library(sf)
library(rnaturalearth)

annular <- sf::st_read( here::here("Data/2023eclipse_shapefiles/upath_lo.shp"))

total <- sf::st_read( here::here("Data/2024eclipse_shapefiles/upath_lo.shp"))

sites <- readr::read_csv( here::here("Data/unique_locations.csv")) |> 
  sf::st_as_sf(coords = c("lon", "lat"), 
               crs = 4326)

world <- rnaturalearth::ne_countries(scale = "small",
                                     returnclass = "sf") 

lakes <- rnaturalearth::ne_download(scale = 110, 
                                    type = 'lakes', 
                                    category = 'physical') |> 
  sf::st_as_sf(lakes110, crs = 4269)

bbox_coords <- c( -127, 19, -61, 50)

names(bbox_coords) <- c("xmin", "ymin", "xmax", "ymax")

bbp <- sf::st_as_sf( sf::st_as_sfc( sf::st_bbox( bbox_coords) ), crs = 4326) |> 
  sf::st_transform( sf::st_crs(world)) 

sf::sf_use_s2(FALSE)
# crop everything
cont_crop <- sf::st_crop(world, bbp)

lake_crop <- sf::st_crop(lakes, bbp) |> 
  dplyr::filter(name_alt == "Great Lakes")
an_crop <- sf::st_crop(annular, bbp)
to_crop <- sf::st_crop(total, bbp)

site_an <- sites |> 
  filter(type == "annular") |> 
  sf::st_crop( bbp ) 

site_to <- sites |> 
  dplyr::filter(type == "total") |> 
  sf::st_crop( bbp )

in_path <- as.numeric( st_within( site_an, an_crop ))

site_an <- site_an |> 
  cbind(path_status = in_path) |> 
  mutate( path_status = tidyr::replace_na(path_status, 0)) |> 
  dplyr::mutate( path_status = ifelse(path_status == 1, "within path", "outside path"))

in_path_to <- as.numeric( sf::st_within( site_to, to_crop))

site_to <- site_to |> 
  cbind(path_status = in_path_to) |> 
  mutate( path_status = tidyr::replace_na(path_status, 0)) |> 
  dplyr::mutate( path_status = ifelse(path_status == 1, "within path", "outside path"))

lab <- tribble(
  ~lat, ~lon, ~label,
  22.9, -118.98, "annular eclipse",
  20.9, -118.5, "14 october 2023") |> 
  sf::st_as_sf( coords = c("lon", "lat"), 
                crs = 4326)

lab_to <- tribble(
  ~lat, ~lon, ~label,
  22.9, -118.98, "total eclipse",
  20.9, -118.5, "08 april 2024") |> 
  sf::st_as_sf( coords = c("lon", "lat"), 
                crs = 4326)

( plot_annular <- ggplot() +
    geom_sf( data = bbp, aes(geometry = x), fill = "gray60", color = NA) +
    geom_sf( data = cont_crop, fill = "gray10", color = NA) +
    geom_sf( data = lake_crop, aes(geometry = geometry),
             fill = "gray60", color = NA) +
    geom_sf( data = an_crop, aes(geometry = geometry), fill = "gray90", color = "gray90") +
    geom_sf( data = site_an, aes(geometry = geometry, color = factor(path_status)),
             size = 1) +
    theme_void() +
    scale_color_manual(values = MetBrewer::MetPalettes$Hiroshige[[1]][c(2,8)]) +
    theme(legend.position = "none",
          legend.title = element_blank(),
          legend.key.spacing.y = unit(0, "pt"),
          legend.spacing = unit(-10, "pt"),
          legend.text = element_text(color = "black", size = 9,
                                     margin = margin(l = -2, unit = "pt")),
          plot.margin = margin(0,0,0,0)) )

( plot_total <- ggplot() +
    geom_sf( data = bbp, aes(geometry = x), fill = "gray60", color = NA) +
    geom_sf( data = cont_crop, fill = "gray10", color = NA) +
    geom_sf( data = lake_crop, aes(geometry = geometry),
             fill = "gray60", color = NA) +
    geom_sf( data = to_crop, aes(geometry = geometry), fill = "gray90", color = "gray90") +
    geom_sf( data = site_to, aes(geometry = geometry, color = factor(path_status)),
             size = 1) +
    theme_void() +
    scale_color_manual(values = MetBrewer::MetPalettes$Hiroshige[[1]][c(2,8)]) +
    theme(legend.position = "none",
          legend.title = element_blank(),
          legend.key.spacing.y = unit(0, "pt"),
          legend.spacing = unit(-10, "pt"),
          legend.text = element_text(color = "black", size = 9,
                                     margin = margin(l = -2, unit = "pt")),
          plot.margin = margin(0,0,0,0)) )

setwd(here::here("Results/Figures"))
ggsave(
  plot_annular,
  filename = "figure_01b.png", 
  width = 4, 
  height = 2.5, 
  units = "in", 
  dpi = 600)

ggsave(
  plot_total,
  filename = "figure_01c.png", 
  width = 4, 
  height = 2.5, 
  units = "in", 
  dpi = 600)