# merge all datasets
library(here)
library(data.table)


if(!file.exists(here('Data/merged_es_bw_apr2025.csv'))){
  # -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
  library(lubridate)
  library(hms)
  library(sf)
  library(mapview)
  library(units)
  # Define a function to safely convert to hms, coercing invalid formats to NA
  safe_as_hms <- function(x) {
    suppressWarnings(as_hms(ifelse(grepl("^\\d{2}:\\d{2}:\\d{2}$", x), x, NA)))
  }
  
  # -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
  
  # read in eclipse shapefile
  total_center <- st_read(here('Data/2024eclipse_shapefiles/center.shp'))
  total_upath_lo <- st_read(here('Data/2024eclipse_shapefiles/upath_lo.shp'))
  
  
  # read in eclipse shapefile
  annular_center <- st_read(here('Data/2023eclipse_shapefiles/center.shp'))
  annular_upath_lo <- st_read(here('Data/2023eclipse_shapefiles/upath_lo.shp'))
  # mapview(annular_center) + mapview(annular_upath_lo) + 
  #   mapview(total_center) + mapview(total_upath_lo)
  # -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
  
  
  
  
  # complete annular and total eclipse soundscapes dataset
  es <- fread(here::here("Data/birdnet_cleaned_v03.csv"))
  setnames(es, old = c('start_timestamp', 'within_path', "TotalEclipseTimeUTC" ), 
           new = c('timestamp', 'path_status', "MaxEclipseTimeUTC"))
  # fix timestamps
  time_cols <- c('start_time', 'end_time', "annular_start",
                 "annular_max","annular_end","FirstContactTimeUTC",
                 "SecondContactTimeUTC","ThirdContactTimeUTC",
                 "FourthContactTimeUTC", "MaxEclipseTimeUTC")
  es[, (time_cols) := lapply(.SD, function(x) as_hms(x)), 
     .SDcols = time_cols]
  
  # going to get rid of the annular columns, and just use first, fourth, total for start, end, and max, respectively
  es[, FirstContactTimeUTC := fcoalesce(annular_start, FirstContactTimeUTC)]
  es[, FourthContactTimeUTC := fcoalesce(annular_end, FourthContactTimeUTC)]
  es[, MaxEclipseTimeUTC := fcoalesce(annular_max, MaxEclipseTimeUTC)]
  
  # birdweather apr24 - total eclipse
  bw_tot <- fread(here('Data/birdweather_apr24_with_contact_times.csv'))
  setnames(bw_tot, old = c('com_name', "Scientific Name", "Timestamp", "Confidence", "CoveragePercent", "TotalEclipseTimeUTC"),
           new = c('common_name', 'scientific_name', 'timestamp', 'confidence', 'coverage', "MaxEclipseTimeUTC"))
  bw_tot <- bw_tot[, .(timestamp, common_name, scientific_name, Latitude, Longitude, confidence, date,
                       coverage, FirstContactDate, FirstContactTimeUTC, SecondContactTimeUTC, ThirdContactTimeUTC,
                       FourthContactTimeUTC, MaxEclipseTimeUTC, Station)]
  # fix time columns
  
  
  
  # Ensure all relevant columns in bw_tot are character type
  cols <- c("FirstContactTimeUTC", "SecondContactTimeUTC", "ThirdContactTimeUTC", 
            "FourthContactTimeUTC", "MaxEclipseTimeUTC")
  
  # Convert non-character columns to character
  bw_tot[, (cols) := lapply(.SD, as.character), .SDcols = cols]
  
  # Convert to hms, coercing bad values to NA
  bw_tot[, (cols) := lapply(.SD, safe_as_hms), .SDcols = cols]
  
  # get time columns
  # day, hour, start_time
  bw_tot[, `:=`(day = day(timestamp), hour = hour(timestamp), 
                start_time = as_hms(timestamp))]
  
  # need to calculate distance to center of path
  # call column dist_to_center
  # conver to dist_km
  
  
  # categorical for within_path
  # get proximity to center of path
  # also get indicator for whether within path
  
  # get pucs, remove duplicated station id, bring back into bw_apr so I have just a single lat/lon
  # pucs <- bw_tot[Station %like% "PUC"]
  # pucs <- pucs[!duplicated(Station)]
  # bw_tot <- bw_tot[!(Station %like% "PUC")]
  # bw_tot <- rbindlist(list(bw_tot, pucs))
  
  # get lat lon stuff going for getting unique stations
  # bw_apr[,  `:=`(lon_round = round(Longitude, 3),
  #                lat_round = round(Latitude, 3))] 
  
  bw_tot[, lat_lon_index := .GRP, .(Longitude, Latitude)]
  
  # if we need contact times, we can do the following:
  
  bw_tot_locs <- bw_tot[!duplicated(lat_lon_index),.(lat_lon_index, Longitude, Latitude)]
  bw_tot_locs <- st_as_sf(bw_tot_locs, coords = c("Longitude", "Latitude"), crs = 4326)
  
  total_dist_mat <- st_distance(bw_tot_locs, total_center)
  bw_tot_locs$dist_to_center <- total_dist_mat[,1]
  
  bw_tot_locs$path_status <- ifelse(st_intersects(bw_tot_locs, total_upath_lo), 'within_path', 'outside_path')
  bw_tot_locs$path_status <- ifelse(is.na(bw_tot_locs$path_status), "outside_path", bw_tot_locs$path_status)
  bw_tot_locs <- as.data.table(st_drop_geometry(bw_tot_locs))
  
  bw_tot <- merge(bw_tot, bw_tot_locs)
  
  bw_tot[, dist_km := set_units(dist_to_center, km)]
  bw_tot[, dist_km := as.numeric(dist_km)]
  
  # also need categorical for spp activity pattern
  # quickly skimmed list of species in birdnet for nocturnals detected
  noc_spp <- c("Barred Owl","Barn Owl","Black-crowned Night-Heron",
               "Chuck-will's-widow", "Common Nighthawk" ,"Common Pauraque",
               "Common Poorwill","Eastern Screech-Owl","Eastern Whip-poor-will",
               'Great Horned Owl',"Yellow-crowned Night-Heron", "Northern Pygmy-Owl",
               "Western Screech-Owl")
  bw_tot[, activity := ifelse(common_name %in% noc_spp, "nocturnal", "diurnal")] 
  
  # a few misc columns for the merge
  bw_tot[, `:=`(eclipse_event = 'total')]
  # -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
  # -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
  
  # birdweather oct23 - annular eclipse
  bw_ann <- fread(here('Data/birdweather_oct23_with_contact_times.csv'))
  
  
  setnames(bw_ann, old = c('com_name', "Scientific Name", "Timestamp", "Confidence", "CoveragePercent", "TotalEclipseTimeUTC"),
           new = c('common_name', 'scientific_name', 'timestamp', 'confidence', 'coverage', "MaxEclipseTimeUTC"))
  bw_ann <- bw_ann[, .(timestamp, common_name, scientific_name, Latitude, Longitude, confidence, date,
                       coverage, FirstContactDate, FirstContactTimeUTC, SecondContactTimeUTC, ThirdContactTimeUTC,
                       FourthContactTimeUTC, MaxEclipseTimeUTC, Station)]
  # fix time cols
  cols <- c("FirstContactTimeUTC", "SecondContactTimeUTC", "ThirdContactTimeUTC", 
            "FourthContactTimeUTC", "MaxEclipseTimeUTC")
  
  # Convert non-character columns to character
  bw_ann[, (cols) := lapply(.SD, as.character), .SDcols = cols]
  
  # Convert to hms, coercing bad values to NA
  bw_ann[, (cols) := lapply(.SD, safe_as_hms), .SDcols = cols]
  # get time columns
  bw_ann[, `:=`(day = day(timestamp), hour = hour(timestamp), 
                start_time = as_hms(timestamp))]
  # get pucs, remove duplicated station id, bring back into bw_apr so I have just a single lat/lon
  # pucs <- bw_ann[Station %like% "PUC"]
  # pucs <- pucs[!duplicated(Station)]
  # bw_ann <- bw_ann[!(Station %like% "PUC")]
  # bw_ann <- rbindlist(list(bw_ann, pucs))
  # index for getting locations
  bw_ann[, lat_lon_index := .GRP, .(Longitude, Latitude)]
  
  # distance based measurements
  bw_ann_locs <- bw_ann[!duplicated(lat_lon_index),.(lat_lon_index, Longitude, Latitude)]
  bw_ann_locs <- st_as_sf(bw_ann_locs, coords = c("Longitude", "Latitude"), crs = 4326)
  
  annular_dist_mat <- st_distance(bw_ann_locs, annular_center)
  bw_ann_locs$dist_to_center <- annular_dist_mat[,1]
  
  bw_ann_locs$path_status <- ifelse(st_intersects(bw_ann_locs, annular_upath_lo), 'within_path', 'outside_path')
  bw_ann_locs$path_status <- ifelse(is.na(bw_ann_locs$path_status), "outside_path", bw_ann_locs$path_status)
  bw_ann_locs <- as.data.table(st_drop_geometry(bw_ann_locs))
  
  # bring it together
  bw_ann <- merge(bw_ann, bw_ann_locs)
  
  # new dist columns
  bw_ann[, dist_km := set_units(dist_to_center, km)]
  bw_ann[, dist_km := as.numeric(dist_km)]
  
  # also need categorical for spp activity pattern
  bw_ann[, activity := ifelse(common_name %in% noc_spp, "nocturnal", "diurnal")] 
  
  # a few misc columns for the merge
  bw_ann[, `:=`(eclipse_event = 'annular')]
  # -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
  # -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
  # -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
  # -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
  es[, data_source := "ES"]
  bw_ann[, data_source := "BW"]
  bw_tot[, data_source := "BW"]
  es[, FirstContactDate:= as.IDate(as.POSIXct(FirstContactDate, format = "%m/%d/%Y"))]
  es[, dist_to_center  := set_units(dist_to_center, km)]
  # drop some columns then merge
  es <- es[, !c("end_time","end_timestamp", "eclipse_type" , "annular_start", "annular_end"  ,
                "annular_max"  , "duplicateID",   "annular_date" , "total_date"), with = F]
  out <- rbindlist(list(es, bw_tot, bw_ann), fill = T)
  rm(list = setdiff(ls(), "out"))
  gc()
  fwrite(out, file = here('Data/merged_es_bw_apr2025.csv'))
} else{
  out <- fread(here('Data/merged_es_bw_apr2025.csv'))
}

