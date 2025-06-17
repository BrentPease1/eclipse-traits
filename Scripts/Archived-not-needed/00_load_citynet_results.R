library(here)
library(data.table)
library(lubridate)
library(hms)
library(stringr)
library(sf)
library(mapview)
library(units)
# read in total eclipse shapefile
total_center <- st_read(here('Data/2024eclipse_shapefiles/center.shp'))
total_upath_lo <- st_read(here('Data/2024eclipse_shapefiles/upath_lo.shp'))


# read in annular eclipse shapefile
annular_center <- st_read(here('Data/2023eclipse_shapefiles/center.shp'))
annular_upath_lo <- st_read(here('Data/2023eclipse_shapefiles/upath_lo.shp'))
mapview(annular_center) + mapview(annular_upath_lo) + 
  mapview(total_center) + mapview(total_upath_lo)


# Load ES locations and times
total_times <- fread(here('Data/metadata/es2024_locations_timesv02.csv'))
total_times <- total_times[Latitude != 0]
total_times <- total_times[SecondContactTimeUTC != 0]
total_times[, ESIDNumber := str_pad(string = ESIDNumber,
                                    width = 3,
                                    side = 'left',
                                    pad = '0')]
total_times <- total_times[!duplicated(ESIDNumber)]
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
# list all citynet files
cn_files <- list.files(here('Data/CityNet'), pattern = ".csv", full.names = T, recursive = T)

holder <- list()
for(i in 1:length(cn_files)){
  # isolate file
  this_file <- cn_files[i]
  
  # read in a deployment's citynet predictions
  dat <- fread(this_file)
  
  # add ESIDNumber as a column
  dat[, ESIDNumber := sub(".*ESID#(\\d{3}).*", "\\1", this_file)]
  
  # update names
  setnames(dat, old = c("Average biotic sound", "Average anthropogenic sound"),
           new = c('avg_biotic', 'avg_anthro'))
  
  # make recognizable timestamp using lubridate from wav file name
  dat[, Timestamp := ymd_hms(paste0(
    substr(Filename, 1, 8), " ",  # Extract date (YYYYMMDD)
    substr(Filename, 10, 15)      # Extract time (HHMMSS)
  ))]
  
  # add some lubridate measures
  dat[, day := day(Timestamp)]
  dat[, hour := hour(Timestamp)]
  dat[, time := as_hms(Timestamp)]
  dat[, date := date(Timestamp)]
  
  # get target dates (throw out 1970s)
  good_days <- c("2023-10-12", "2023-10-13", "2023-10-14" ,"2023-10-15", "2023-10-16",
                 "2024-04-06", "2024-04-07", "2024-04-08","2024-04-09", "2024-04-10")
  dat <- dat[date %in% good_days,]
  # if bad date, will go to 0 rows, throw out here
  if(nrow(dat) == 0){
    next
  }
  
  if(dat$ESIDNumber[1] %in% c('932', '117','118', '122', "139", "144")){
    next #missing deployment info
  }
  # join in deployment information from appropriate table
  # Duplicate ESIDs in 2023 and 2024 (e.g., 002 and 002 can exist but they are different)
  if (year(dat$Timestamp[1]) == 2023) {
    if (!any(dat$ESIDNumber %in% annular_times$ESIDNumber)) next
    dat <- merge(dat, annular_times, by = "ESIDNumber")
  } else {
    if (!any(dat$ESIDNumber %in% total_times$ESIDNumber)) next
    dat <- merge(dat, total_times, by = "ESIDNumber")
  }
  
  # convert characters to time
  if(year(dat$Timestamp[1]) == 2024){
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
    cat(i, 'of', length(cn_files), 'completed\n')
  }
}

# bring it all together
citynet <- rbindlist(holder, fill = T)

# assign event
citynet[, eclipse_event := ifelse(month(Timestamp) == 10, "annular", "total")]
citynet[, annular_date := ymd(as.POSIXct("10/14/2023", format = '%m/%d/%Y'))]
citynet[, total_date := ymd(as.POSIXct("04/08/2024", format = '%m/%d/%Y'))]

# add a flag for when the eclipse occurred
citynet[, eclipse_time := ifelse(time >= FirstContactTimeUTC & 
                                   time <= FourthContactTimeUTC, "yes", "no")]
citynet[, eclipse_datetime := ifelse(time >= FirstContactTimeUTC & 
                                       time <= FourthContactTimeUTC &
                                       day == 8, "yes", "no")]

# get proximity to center of path
total_locs <- citynet[!duplicated(ESIDNumber) & eclipse_event == 'total', .(ESIDNumber, Longitude, Latitude)]
total_locs <- st_as_sf(total_locs, coords = c("Longitude", "Latitude"), crs = 4326)

total_dist_mat <- st_distance(total_locs, total_center)
total_locs$dist_to_center <- total_dist_mat[,1]
total_locs <- as.data.table(st_drop_geometry(total_locs))

citynet_total <- merge(citynet, total_locs)

# annular
annular_locs <- citynet[!duplicated(ESIDNumber) & eclipse_event == 'annular', .(ESIDNumber, Longitude, Latitude)]
annular_locs <- st_as_sf(annular_locs, coords = c("Longitude", "Latitude"), crs = 4326)

annular_dist_mat <- st_distance(annular_locs, annular_center)
annular_locs$dist_to_center <- annular_dist_mat[,1]
annular_locs <- as.data.table(st_drop_geometry(annular_locs))

citynet_annular <- merge(citynet, annular_locs)

citynet <- rbindlist(list(citynet_total, citynet_annular))

citynet[, dist_km := units::set_units(dist_to_center, km)]
citynet[, dist_km := as.numeric(dist_km)]
# clean up
rm(dat, total_times, cn_files, i, this_file, good_days,
   time_cols, total_locs, annular_locs, total_dist_mat, 
   annular_dist_mat, citynet_annular, citynet_total) # I keep holder in memory 
                                                     # so when I mess up the reset is faster

# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
