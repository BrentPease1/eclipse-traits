require(tidyverse)
require(sf)
require(patchwork)
require(magrittr)
require(ggforce)
require(cowplot)
require(terra)
require(tidyterra)
library(geodata)
library(here)
library(basemaps)
library(ggthemes)
library(data.table)
library(MetBrewer)
# get usa basemap
usa <- geodata::gadm(country = "US", path = here('Data/mapping_data/'))
usa <- usa |>
  terra::subset(!usa$NAME_1 %in% c("Alaska", "Hawaii")) |>
  st_as_sf() |>
  st_transform(crs = 3857)

# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
# read in eclipse shapefile
total_center <- st_read(here('Data/2024eclipse_shapefiles/center.shp'))
total_upath_lo <- st_read(here('Data/2024eclipse_shapefiles/upath_lo.shp'))

# read in eclipse shapefile
annular_center <- st_read(here('Data/2023eclipse_shapefiles/center.shp'))
annular_upath_lo <- st_read(here('Data/2023eclipse_shapefiles/upath_lo.shp'))

# get in same crs as usa
annular_center <- st_transform(x = annular_center, crs = 3857)
annular_upath_lo <- st_transform(x = annular_upath_lo, crs = 3857)
total_center <- st_transform(x = total_center, crs = 3857)
total_upath_lo <- st_transform(x = total_upath_lo, crs = 3857)

annular_upath_lo <- st_crop(x = annular_upath_lo, y = annular_center)
total_upath_lo <- st_crop(x = total_upath_lo, y = total_center)
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

# read in complete dataset
deployments <- fread(here('Data/merged_es_bw_apr2025_viirs.csv'))
deployments <- deployments[, .(Latitude, Longitude, eclipse_event, coverage, data_source,
                               path_status, month)]

# filter to annular
annular_times <- deployments[month == '10']
annular_times <- annular_times[, index := .GRP, .(Latitude, Longitude)]
annular_times <- annular_times[!duplicated(index)]
annular_times <- st_as_sf(annular_times, coords = c('Longitude', "Latitude"), crs = 4326)
annular_times <- st_transform(x = annular_times, crs = 3857)

annular_times <- st_join(annular_times, usa["NAME_1"], left = TRUE)
annular_times <- annular_times %>%
  filter(NAME_1 != "Alkasa" & NAME_1 != "Hawaii")
# filter to total
total_times <- deployments[month == '4']
total_times <- total_times[, index := .GRP, .(Latitude, Longitude)]
total_times <- total_times[!duplicated(index)]
total_times <- st_as_sf(total_times, coords = c('Longitude', "Latitude"), crs = 4326)
total_times <- st_transform(x = total_times, crs = 3857)

total_times <- st_join(total_times, usa["NAME_1"], left = TRUE)
total_times <- total_times %>%
  filter(NAME_1 != "Alkasa" & NAME_1 != "Hawaii")

# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
# ANNULAR ####
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
annular_times_bbox <- st_bbox(annular_times)
annular_upath_bbox <- st_bbox(annular_upath_lo)

# Define how far to nudge the bbox in meters (adjust this value as needed)
x_shift <- 100000  # shifts 50 km to the right if your CRS is in meters (e.g., EPSG:3857)
y_shift <- 100000
# Shift bbox to the right
merged_bbox <- sf::st_bbox(c(
  xmin = min(annular_times_bbox[1], annular_upath_bbox[1]) - x_shift,
  ymin = min(annular_times_bbox[2], annular_upath_bbox[2]),
  xmax = max(annular_times_bbox[3], annular_upath_bbox[3]) + x_shift,
  ymax = max(annular_times_bbox[4], annular_upath_bbox[4]) + y_shift
), crs = 3857)

# path only
ann_path <- ggplot() + 
  basemap_gglayer(ext = merged_bbox, map_service = 'carto', map_type = 'dark', force = F) +
  ggspatial::layer_spatial(data = annular_upath_lo)+
  ggspatial::layer_spatial(data = annular_center)+
 # ggspatial::layer_spatial(data = annular_times, color = 'orange', size = 2)+
  scale_fill_identity() +
  coord_sf() +
#  ggspatial::annotation_north_arrow(location = "bl", width = unit(.8, "cm"), height = unit(.8, "cm"),
 #                                   pad_x = unit(1.5, "cm"),
 #                                   pad_y = unit(2, "cm")) +
  #ggspatial::annotation_scale(location = "bl",pad_x = unit(1.5, "cm"),
  #                            pad_y = unit(1.5, "cm")) +
  geom_text(aes(x = -15400000, y = 3000000, label = "2023 Annular Eclipse\n2023-Oct-14"), 
            color = "black", size = 8, hjust = 0) +
  theme_map()

ggsave(ann_path, filename = here("Results/Figures/Maps/annular_path.png"), width = 12, height = 12, dpi = 300)

# with deployments
ann_map <- ggplot() + 
  basemap_gglayer(ext = merged_bbox, map_service = 'carto', map_type = 'dark', force = FALSE) +
  ggspatial::layer_spatial(data = annular_upath_lo) +
  ggspatial::layer_spatial(data = annular_center) +
  geom_sf(data = annular_times, 
          aes(color = path_status, shape = data_source), 
          size = 3) +
  scale_color_manual(values = MetBrewer::MetPalettes$Navajo[[1]][c(2,4)]) +
  scale_shape_manual(values = c(16,17)) + # adjust depending on number of sources
  scale_fill_identity() +
  coord_sf() + 
  geom_text(aes(x = merged_bbox[1]+50000, y = merged_bbox[2]+500000, label = "2023 Annular Eclipse\n2023-Oct-14"), 
            color = "black", size = 8, hjust = 0) +
  theme_map() +
  theme(legend.position = 'none')
ggsave(ann_map, filename = here("Results/Figures/Maps/annular_deployments_merged.png"), 
       width = 12, height = 8, dpi = 300)


# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
# TOTAL ####
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

total_times_bbox <- st_bbox(total_times)
total_upath_bbox <- st_bbox(total_upath_lo)
x_shift <- 1000000 
y_shift <- 100000
merged_bbox <- sf::st_bbox(c(
  xmin = min(total_times_bbox[1], total_upath_bbox[1]) - x_shift,
  ymin = min(total_times_bbox[2], total_upath_bbox[2]),
  xmax = max(total_times_bbox[3], total_upath_bbox[3]) - x_shift,
  ymax = max(total_times_bbox[4], total_upath_bbox[4]) + y_shift
), crs = 3857)
flush_cache()

tot_path <- ggplot() + 
  basemap_gglayer(ext = merged_bbox, map_service = 'carto', map_type = 'dark', force = T) +
  ggspatial::layer_spatial(data = total_upath_lo)+
  ggspatial::layer_spatial(data = total_center)+
 # ggspatial::layer_spatial(data = total_times, color = 'orange', size = 2)+
  scale_fill_identity() +
  coord_sf() +
  # ggspatial::annotation_north_arrow(location = "br", width = unit(.8, "cm"), height = unit(.8, "cm"),
  #                                   pad_x = unit(1.5, "cm"),
  #                                   pad_y = unit(2, "cm")) +
  # ggspatial::annotation_scale(location = "br",pad_x = unit(1.5, "cm"),
  #                             pad_y = unit(1.5, "cm")) +
  geom_text(aes(x = -6500000, y = 3000000, label = "2024 Total Eclipse\n2024-Apr-8"), 
            color = "black", size = 8) +
  theme_map()

ggsave(tot_path, filename = here("Results/Figures/Maps/total_path.png"), width = 12, height = 12, dpi = 300)
flush_cache()

# fix plotting
total_times <- total_times %>%
  mutate(path_status = if_else(index %in% c(272, 275, 276), 'within_path', path_status))


tot_map <- ggplot() + 
  basemap_gglayer(ext = merged_bbox, map_service = 'carto', map_type = 'dark', force = FALSE) +
  ggspatial::layer_spatial(data = total_upath_lo) +
  ggspatial::layer_spatial(data = total_center) +
  geom_sf(data = total_times, 
          aes(color = path_status, shape = data_source), 
          size = 3) +
  scale_color_manual(values = MetBrewer::MetPalettes$Navajo[[1]][c(2,4)]) +
  scale_shape_manual(values = c(16,17)) + # adjust depending on number of sources
  scale_fill_identity() +
  coord_sf() + 
  geom_text(aes(x = merged_bbox[3]-2300000, y = merged_bbox[2]+1000000, label = "2024 Total Eclipse\n2024-Apr-8"), 
            color = "black", size = 8, hjust = 0) +
  theme_map() +
  theme(legend.position = 'none')

ggsave(tot_map, filename = here("Results/Figures/Maps/total_deployments.png"), 
       width = 12, height = 8, dpi = 300)
flush_cache()

son_map <- ggplot() + 
  basemap_gglayer(ext = merged_bbox, map_service = 'carto', map_type = 'dark', force = T) +
  ggspatial::layer_spatial(data = total_upath_lo)+
  ggspatial::layer_spatial(data = total_center)+
  ggspatial::layer_spatial(data = total_times, color = 'orange', size = 2)+
  ggspatial::layer_spatial(data = son_times, color = '#1E90FF', size = 2)+
  scale_fill_identity() +
  coord_sf() +
  ggspatial::annotation_north_arrow(location = "br", width = unit(.8, "cm"), height = unit(.8, "cm"),
                                    pad_x = unit(1.5, "cm"),
                                    pad_y = unit(2, "cm")) +
  ggspatial::annotation_scale(location = "br",pad_x = unit(1.5, "cm"),
                              pad_y = unit(1.5, "cm")) +
  geom_text(aes(x = -6500000, y = 3000000, label = "2024 Total Eclipse\nApr 8, 2024"), 
            color = "black", size = 8) +
  theme_map()

ggsave(son_map, filename = here("Results/Figures/Maps/son_deployments.png"), width = 12, height = 12, dpi = 300)
