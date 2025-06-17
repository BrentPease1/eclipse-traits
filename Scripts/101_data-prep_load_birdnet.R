library(here)
library(data.table)
library(lubridate)
library(hms)
library(stringr)
library(sf)
library(mapview)
library(units)
if(file.exists(here('Data/birdnet_cleaned_v03.csv'))){
  birdnet <- fread(here('Data/birdnet_cleaned_v03.csv'))
} else{
  # read in eclipse shapefile
  total_center <- st_read(here('Data/2024eclipse_shapefiles/center.shp'))
  total_upath_lo <- st_read(here('Data/2024eclipse_shapefiles/upath_lo.shp'))
  
  
  # read in eclipse shapefile
  annular_center <- st_read(here('Data/2023eclipse_shapefiles/center.shp'))
  annular_upath_lo <- st_read(here('Data/2023eclipse_shapefiles/upath_lo.shp'))
  mapview(annular_center) + mapview(annular_upath_lo) + 
    mapview(total_center) + mapview(total_upath_lo)
  
  
  # Load ES locations and times
  total_times <- fread(here('Data/metadata/es2024_locations_timesv02.csv'))
  total_times <- total_times[Latitude != 0]
  total_times[, ESIDNumber := str_pad(string = ESIDNumber,
                                      width = 3,
                                      side = 'left',
                                      pad = '0')]
  # --
  annular_times <- fread(here('Data/metadata/2023_annular_locations_times.csv'))
  annular_times <- annular_times[Latitude != 0]
  annular_times[, ESIDNumber := str_pad(string = `AudioMoth ES ID Number`,
                                        width = 3,
                                        side = 'left',
                                        pad = '0')]
  annular_times <- annular_times[, .(ESIDNumber, Latitude, Longitude, `Eclipse Type\n`, `Coverage Percent`, `2023 Eclipse Start (UTC)`, `2023 Eclipse End (UTC)`,
                                     `2023 Eclipse Maximum (UTC)`)]
  # update names
  setnames(annular_times, old = c("Eclipse Type\n", "Coverage Percent",
                                  "2023 Eclipse Start (UTC)",
                                  "2023 Eclipse End (UTC)",
                                  "2023 Eclipse Maximum (UTC)"),
           new = c('eclipse_type', 'coverage', 'annular_start', 'annular_end',
                   'annular_max'))
  
  # -- -- -- -- -- -- -- --
  # list all BirdNET files
  # EDIT: Feb 11, 2025
  # I ran birdnet using two approaches
  # for the approach prior to Feb 2025, this script is perfect
  # for feb 2025 and ff, I am going to pull it in in a different way
  # after the loop below
  # -- -- -- -- -- -- -- --
  
  bn_files <- list.files(here('Data/Birdnet'), pattern = "*_birdnetpredictions.csv", full.names = T, recursive = T)
  
  # read in files, do some time manipulation, and then bind together
  holder <- list()
  for(i in 1:length(bn_files)){
    # isolate file
    this_file <- bn_files[i]
    
    # read in a deployment's birdnet predictions
    dat <- fread(this_file)
    
    # add ESIDNumber as a column
    dat[, ESIDNumber := sub(".*ESID#(\\d{3}).*", "\\1", this_file)]
    
    # filter confidence scores
    dat <- dat[confidence >= 0.65,]
    
    # add some lubridate measures
    dat[, day := day(start_timestamp)]
    dat[, hour := hour(start_timestamp)]
    dat[, start_time := as_hms(start_timestamp)]
    dat[, end_time := as_hms(end_timestamp)]
    dat[, date := date(start_timestamp)]
    
    # get target dates (throw out 1970s)
    good_days <- c("2023-10-12", "2023-10-13", "2023-10-14" ,"2023-10-15", "2023-10-16",
                   "2024-04-06", "2024-04-07", "2024-04-08","2024-04-09", "2024-04-10")
    dat <- dat[date %in% good_days,]
    # if bad date, will go to 0 rows, throw out here
    if(nrow(dat) == 0){
      next
    }
    
    # join in deployment information from appropriate table
    # Duplicate ESIDs in 2023 and 2024 (e.g., 002 and 002 can exist from annular and total deployment but they are different)
    if(year(dat$start_timestamp[1]) == 2023){
      dat <- merge(dat, annular_times, by = 'ESIDNumber')
    } else{
      dat <- merge(dat, total_times, by = 'ESIDNumber')
    }
    
    # convert characters to time
    if(year(dat$start_timestamp[1]) == 2024){
      time_cols <- tail(names(dat), 5)  # Select the last five columns
      dat[, (time_cols) := lapply(.SD, function(x) as.POSIXct(paste(FirstContactDate, x), format = "%m/%d/%Y %H:%M:%S")), 
          .SDcols = time_cols]
      dat[, (time_cols) := lapply(.SD, function(x) as_hms(x)), 
          .SDcols = time_cols]
    } else{
      time_cols <- c('annular_start', 'annular_end', 'annular_max')
      dat[, (time_cols) := lapply(.SD, function(x) as.POSIXct(paste("10/14/2023", x), format = "%m/%d/%Y %H:%M:%S")), 
          .SDcols = time_cols]
      dat[, (time_cols) := lapply(.SD, function(x) as_hms(x)), 
          .SDcols = time_cols]
    }
    
    # store in holder
    holder[[i]] <- dat
    # update impatient user
    if(i %% 10 == 0){
      cat(i, 'of', length(bn_files), 'completed\n')
    }
  }
  
  # bring it all together
  birdnet <- rbindlist(holder, fill = T)
  birdnet$label <- NULL # don't need and helps with merge below
  # -- -- -- -- -- -- -- --
  # -- -- -- -- -- -- -- --
  # BRING IN FEB 2025 and ff files
  bn_files2 <- list.files(here('Data/Birdnet'), pattern = ".csv", full.names = T, recursive = T)
  bn_files2 <- setdiff(x = bn_files2, y = bn_files)
  source(here('Scripts/101b_data-prep_load_birdnet.R'))
  
  # combine with older files
  # before adding together, I want to double check that the same deployment didn't end up
  # in birdnet and birdnet2.
  # will create a column indicator
  birdnet[, duplicateID := paste0(ESIDNumber, "-", ifelse(month(start_timestamp) == 10, "annular", "total"))]
  b1IDS <- unique(birdnet$duplicateID)
  birdnet2[, duplicateID := paste0(ESIDNumber, "-", ifelse(month(start_timestamp) == 10, "annular", "total"))]
  # Identify duplicate deployments in birdnet2
  birdnet2 <- birdnet2[!(duplicateID %in% b1IDS)]
  
  # bind
  birdnet <- rbindlist(list(birdnet, birdnet2), fill = T)
  rm(birdnet2)
  # assign event
  birdnet[, eclipse_event := ifelse(month(start_timestamp) == 10, "annular", "total")]
  birdnet[, annular_date := ymd(as.POSIXct("10/14/2023", format = '%m/%d/%Y'))]
  birdnet[, total_date := ymd(as.POSIXct("04/08/2024", format = '%m/%d/%Y'))]
  
  # -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
  # -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
  # get proximity to center of path
  # also get indicator for whether within path
  total_locs <- birdnet[!duplicated(ESIDNumber) & eclipse_event == 'total', .(ESIDNumber, Longitude, Latitude)]
  total_locs <- st_as_sf(total_locs, coords = c("Longitude", "Latitude"), crs = 4326)
  
  total_dist_mat <- st_distance(total_locs, total_center)
  total_locs$dist_to_center <- total_dist_mat[,1]
  
  total_locs$within_path <- ifelse(st_intersects(total_locs, total_upath_lo), 'within_path', 'outside_path')
  total_locs$within_path <- ifelse(is.na(total_locs$within_path), "outside_path", total_locs$within_path)
  total_locs <- as.data.table(st_drop_geometry(total_locs))
  
  birdnet_total <- merge(birdnet, total_locs)
  
  # annular
  annular_locs <- birdnet[!duplicated(ESIDNumber) & eclipse_event == 'annular', .(ESIDNumber, Longitude, Latitude)]
  annular_locs <- st_as_sf(annular_locs, coords = c("Longitude", "Latitude"), crs = 4326)
  
  annular_dist_mat <- st_distance(annular_locs, annular_center)
  annular_locs$dist_to_center <- annular_dist_mat[,1]
  
  annular_locs$within_path <- ifelse(st_intersects(annular_locs, annular_upath_lo), 'within_path', 'outside_path')
  annular_locs$within_path <- ifelse(is.na(annular_locs$within_path), "outside_path", annular_locs$within_path)
  annular_locs <- as.data.table(st_drop_geometry(annular_locs))
  
  
  birdnet_annular <- merge(birdnet, annular_locs)
  
  birdnet <- rbindlist(list(birdnet_total, birdnet_annular))
  
  birdnet[, dist_km := set_units(dist_to_center, km)]
  birdnet[, dist_km := as.numeric(dist_km)]
  # -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
  # -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
  
  # some traits
  # quickly skimmed list of species in birdnet for nocturnals detected
  noc_spp <- c("Barred Owl","Barn Owl","Black-crowned Night-Heron",
               "Chuck-will's-widow", "Common Nighthawk" ,"Common Pauraque",
               "Common Poorwill","Eastern Screech-Owl","Eastern Whip-poor-will",
               'Great Horned Owl',"Yellow-crowned Night-Heron", "Northern Pygmy-Owl",
               "Western Screech-Owl")
  birdnet[, activity := ifelse(common_name %in% noc_spp, "nocturnal", "diurnal")] 
  
  
  # clean up
  rm(dat, annular_times, total_times, bn_files, i, this_file, good_days,
     time_cols, total_locs, annular_locs, total_dist_mat, 
     annular_dist_mat, birdnet_annular, birdnet_total, start_date_time,
     bn_files2)
  
  fwrite(birdnet, file = here('Data/birdnet_cleaned_v03.csv'))
  
}
