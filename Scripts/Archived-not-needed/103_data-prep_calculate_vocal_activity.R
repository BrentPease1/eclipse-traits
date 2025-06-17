library(ggplot2)
library(data.table)
library(here)
library(hms)
library(lubridate)
library(stringr)
library(suncalc)
# Read data
birdnet <- fread(here::here("Data/birdnet_cleaned_v03.csv"))
# fix timestamps

time_cols <- c('start_time', 'end_time', "FirstContactTimeUTC",
               "SecondContactTimeUTC","ThirdContactTimeUTC",
               "FourthContactTimeUTC", "TotalEclipseTimeUTC",
               "annular_start","annular_max","annular_end") 
birdnet[, (time_cols) := lapply(.SD, function(x) as_hms(x)), 
        .SDcols = time_cols]
birdnet[common_name == "Woodhouse's Scrub-Jay", common_name := "California Scrub-Jay"] #fuckit
birdnet[common_name == "Pacific Wren", common_name := "Winter Wren"] #fuckit
birdnet[common_name == "Green-winged Teal", common_name := "Common Teal"] #fuckit
birdnet[common_name == "Wilson's Snipe", common_name := "Common Snipe"] #fuckit

birdnet[, quick_index := 1:nrow(birdnet)]


# elton traits for family name
elton <- fread(here('Data/elton.txt'))

# fix some names manually
elton[English == 'Whip-poor-will', English := 'Eastern Whip-poor-will']
elton[English == 'Blue-grey Gnatcatcher', English := 'Blue-gray Gnatcatcher']
elton[English == 'Bare-throated Tiger-heron', English := 'Bare-throated Tiger-Heron']
elton[English == 'Black-bellied Whistling-duck', English := 'Black-bellied Whistling-Duck']
elton[English == 'Black-crowned Night-heron', English := 'Black-crowned Night-Heron']
elton[English == 'Yellow-crowned Night-heron', English := 'Yellow-crowned Night-Heron']
elton[English == 'American Treecreeper', English := 'Brown Creeper']
elton[English == 'Eastern Screech-owl', English := 'Eastern Screech-Owl']
elton[English == 'Western Screech-owl', English := 'Western Screech-Owl']
elton[English == 'Whiskered Screech-owl', English := 'Whiskered Screech-Owl']
elton[English == "Eastern Wood-pewee", English := "Eastern Wood-Pewee"]
elton[English == "Brent Goose", English := "Brant"]
elton[English == "Western Scrub-jay", English := "California Scrub-Jay"]
elton[English == "Clay-coloured Sparrow", English := "Clay-colored Sparrow"]
elton[English == "Tricoloured Heron", English := "Tricolored Heron"]
elton[English == "Grey Plover", English := "Black-bellied Plover"]
elton[English == "American Swallow-tailed Kite", English := "Swallow-tailed Kite"]
elton[English == "Greyish Saltator", English := "Cinnamon-bellied Saltator"] #fuckit
elton[English == "Collared Forest-falcon", English := "Collared Forest-Falcon"]
elton[English == "Common Moorhen", English := "Common Gallinule"] #fuckit
elton[English == "Common Ground-dove", English := "Common Ground Dove"]
elton[English == "Eurasian Collared-dove", English := "Eurasian Collared-Dove"]
elton[English == "Grey Catbird", English := "Gray Catbird"]
elton[English == "Common Starling", English := "European Starling"]
elton[English == "Northern Pygmy-owl", English := "Northern Pygmy-Owl"]
elton[English == "Rusty-crowned Ground-sparrow", English := "Rusty-crowned Ground-Sparrow"]
elton[English == "Leach's Storm-petrel", English := "Leach's Storm-Petrel"]
elton[English == "Common Pheasant", English := "Ring-necked Pheasant"]
elton[English == "Northern Beardless-tyrannulet", English := "Northern Beardless-Tyrannulet"]
elton[English == "Purple Swamphen", English := "Purple Gallinule"]
elton[English == "Ruddy Ground-dove", English := "Ruddy Ground Dove"]
elton[English == "Yellow-headed Amazon", English := "Yellow-headed Parrot"]


# !! missing rows !! (FIXED!)
# these_indices <- which(!(birdnet$quick_index %in% birdnet2$quick_index))
# test <- sort(birdnet[these_indices,unique(common_name)])
# test

# !! testing zone !!
# elton[English %like% "Pheasant", .(Scientific, English)]
# elton[Scientific %like% "Amazona", .(Scientific, English)]


# english loses fewer rows than scientific
birdnet <- merge(birdnet, elton[, .(BLFamilyLatin, English)], by.x = "common_name", by.y = 'English')
setnames(birdnet, old = 'BLFamilyLatin', new = 'family')
birdnet <- birdnet[!duplicated(quick_index),]
rm(elton)



birdnet[, station_date := .GRP, .(ESIDNumber, date)]
total_spp <- birdnet[, .N, by = .(common_name, station_date)]


total_spp <- total_spp[N > 10,] #min observations per species per station_date
focal_spp <- total_spp[, unique(common_name)]
focal_spp <- sort(focal_spp)


species_holder <- list()
species_counter = 0

for(f in focal_spp){
  species_counter = species_counter + 1
  single_spp_dets <- birdnet[common_name == f,]
  
  
  
  # Generate the sunlight times
  time_frame <- getSunlightTimes(
    data = single_spp_dets[, .(date = date, lat = Latitude, lon = Longitude)],
    keep = c("dusk", "night", "dawn", "nightEnd","sunrise",
             "nauticalDawn",  "sunriseEnd", "sunset", "solarNoon", "nadir"),
    tz = "UTC"
  )
  
  # bring everything together and bind
  
  single_spp_dets <- cbind(single_spp_dets, time_frame[, 4:ncol(time_frame)])

  # --- # --- # --- # --- # --- # --- # --- # --- # --- # --- 
  # total_cessation: final vocalization event relative to totality ####
  # --- # --- # --- # --- # --- # --- # --- # --- # --- # --- 

  total_cessation <- single_spp_dets[date == "2024-04-08" & start_timestamp >= sunrise & start_timestamp <= sunset,]
  if(nrow(total_cessation) != 0){
    # store the vocalization event closest to totality
    total_cessation[,total_cessation := int_length(interval(TotalEclipseTimeUTC , start_time ))/60, .(station_date)]
    total_cessation <- total_cessation[total_cessation < 0, .(total_cessation, station_date)]
    
    total_cessation <- total_cessation[, total_cessation := max(total_cessation), by = station_date]
    total_cessation <- total_cessation[!duplicated(station_date),]
    setkey(total_cessation, station_date)
    total_cessation[, category := "total_cessation"]
    setnames(total_cessation, old = 'total_cessation', new = 'value')
  } else{
    total_cessation <- data.table(value = NA, station_date = NA,
                              category = "total_cessation")
  }
  
  # --- # --- # --- # --- # --- # --- # --- # --- # --- # --- 
  #total onset: initial vocalization following totality ####
  # --- # --- # --- # --- # --- # --- # --- # --- # --- # --- 
  total_onset <- single_spp_dets[date == "2024-04-08" & start_timestamp >= sunrise & start_timestamp <= sunset,]
  if(nrow(total_onset) != 0){
    # store the vocalization event closest to totality
    total_onset[,total_onset := int_length(interval(TotalEclipseTimeUTC , start_time ))/60, .(station_date)]
    total_onset <- total_onset[total_onset >= 0, .(total_onset, station_date)]
    
    total_onset <- total_onset[, total_onset := min(total_onset), by = station_date]
    total_onset <- total_onset[!duplicated(station_date),]
    
    setkey(total_onset, station_date)
    total_onset[, category := "total_onset"]
    setnames(total_onset, old = 'total_onset', new = 'value')
  } else{
    total_onset <- data.table(value = NA, station_date = NA,
                                  category = "total_onset")
  }
  
  # bring together individual calculations
  out <- rbindlist(l = list(total_cessation, total_onset))
  out <- out[!is.na(station_date),] # throw out measures we couldn't calculate
  if(nrow(out) == 0){
    next
  }
  out[, common_name := f,] #add species name to out
  setkey(out, station_date)
  out <- merge(out, single_spp_dets[!duplicated(station_date), 
                                    .(scientific_name, start_timestamp, date,
                                      Latitude, Longitude,
                                      station_date, dist_km, within_path, eclipse_event,
                                      sunrise, sunset, TotalEclipseTimeUTC)])
  # drop station_date grouping variable
  out <- out[, !("station_date"), with = FALSE]
  species_holder[[species_counter]] <- out
  
  cat('\n\n',f, 'completed','at', as.character(Sys.time()), "\n\n")
  
  
} #species
species_holder <- rbindlist(species_holder)

# write file to directory
fwrite(species_holder, 
       file = here("Results/vocal_activity/vocal_activity_20250422.csv"))

