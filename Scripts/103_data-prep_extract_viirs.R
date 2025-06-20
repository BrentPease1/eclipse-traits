library(here)
library(data.table)
library(lubridate)
library(sf)
library(terra)
library(rworldmap)
setDTthreads(0)
# script outline:
#   - read in vocalization activity measures calculated with Scripts/101_data-prep_calculate_vocal_activity_v02.R
#   - read in corresponding month of VIIRS nighttime_light values
#   - extract point-level values at each station



# Read in calculated vocalization activity ####

if(!file.exists(here('Data/merged_es_bw_apr2025_viirs.csv'))){
  
  source(here('Scripts/102_data-prep_merge_es_and_bw.R'))
  
  # index for getting locations
  out[, lat_lon_index := .GRP, .(Longitude, Latitude)]
  out[, `:=`(year = year(timestamp), month = month(timestamp))]
  out[, timestamp_group := .GRP, .(month, year)]
  out_locs <- out[!duplicated(lat_lon_index), .(year, month, timestamp_group, lat_lon_index, Longitude, Latitude)]
  out_locs <- st_as_sf(out_locs, coords = c('Longitude', 'Latitude'), crs = 4326)
  
  # Read in corresponding nighttime_lights ####
  base_file <- "D:/nighttime_data"
  
  va_holder <- list()
  va_counter = 0
  
  for(i in unique(out_locs$timestamp_group)){
    this_group <- out_locs[out_locs$timestamp_group == i,]
    this_month <- stringr::str_pad(unique(this_group$month), 2, pad = '0')
    this_year <- unique(this_group$year)
    year_num <- paste0(this_year,this_month)
    year_num_file <- paste0(base_file, "/", this_year, 
                            "/",year_num, "/","vcmslcfg")
    nt_files <- list.files(year_num_file, 
                           pattern = "\\.avg_rade9h\\.tif$", 
                           full.names = TRUE, 
                           recursive = TRUE)
    
    # read in viirs files and stack with `terra`
    # Read each raster individually because of different extents
    nt_rast <- lapply(nt_files, rast)
    
    # loop through each file and try to extract values
    # will return NAs for non-overlapping points
    nt_holder <- list()
    for(i in 1:length(nt_rast)){
      this_rast <- nt_rast[[i]]
      nt_holder[[i]] <- extract(this_rast, vect(this_group))
      names(nt_holder[[i]]) <- c('ID', paste0('avg_rad_',i))
    }
    
    # bring together
    combined_df <- Reduce(function(x, y) merge(x, y, by = "ID", all = TRUE), nt_holder)
    
    # Ensure the result is always a data.frame
    avg_rad_cols <- grep("avg_rad", names(combined_df), value = TRUE)
    
    if (length(avg_rad_cols) == 1) {
      combined_df$avg_rad <- combined_df[[avg_rad_cols]]
    } else {
      combined_df$avg_rad <- apply(combined_df[ , avg_rad_cols, drop = FALSE], 1, function(x) {
        x[which(!is.na(x))[1]]
      })
    }
    
    # keep the two columns 
    nt_estimates <- combined_df[ , c("ID", "avg_rad")]
    setDT(nt_estimates)
    setkey(nt_estimates, "ID")
    
    # stash  
    va_counter = va_counter + 1
    va_holder[[va_counter]] <- nt_estimates
  }

  nt_estimates <- rbindlist(va_holder)
  nt_estimates <- nt_estimates[!duplicated(ID)]
  
  out <- merge(out, nt_estimates, by.x = 'lat_lon_index', by.y = 'ID')
  # clear out memory
  rm(list=ls()[!ls() %in% c("out")])
  gc()
  
  fwrite(out, here('Data/merged_es_bw_apr2025_viirs.csv'))
}else{
  out <- fread(here('Data/merged_es_bw_apr2025_viirs.csv'))
}
