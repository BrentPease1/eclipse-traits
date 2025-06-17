library(here)
library(data.table)
library(rnaturalearth)
library(stringr)
library(sf)
library(mapview)
setDTthreads(0) # use it all

# get the data
bw_oct <- fread(here('Data/North_Ameica_oct2023.csv'))
setnames(bw_oct, old = "Common Name", new = 'com_name')
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
# doing a bunch of filtering so we only retain the stations we need
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
bw_oct[, date := as.IDate(Timestamp)]
bw_oct <- bw_oct[date %in% as.IDate(c('2023-10-12','2023-10-13','2023-10-14','2023-10-15','2023-10-16'))]

# remove noise, mammals, insects, and amphibians
not_interested <- c("Engine", "Siren", "Coyote", "Dog", 
                    "Eastern Gray Squirrel", "Red Squirrel",
                    "Power tools", "Fireworks", "Gray Wolf", "Gun",
                    "Honey Bee",
                    "Spring Peeper")
bw_oct <- bw_oct[!(com_name %in% not_interested),]
# this should keep frogmouths but drop anuras
bw_oct <- bw_oct[!(str_detect(com_name, "frog(?!mouth)") | str_detect(com_name, "Frog(?!mouth)")),]
bw_oct <- bw_oct[!(com_name %like% "Treefrog"),]
bw_oct <- bw_oct[!(com_name %like% "Bullfrog"),]
bw_oct <- bw_oct[!(com_name %like% "Cricket"),]
bw_oct <- bw_oct[!(com_name %like% "Toad"),]
bw_oct <- bw_oct[!(com_name %like% "Trig"),]
bw_oct <- bw_oct[!(com_name %like% "Katydid"),]
bw_oct <- bw_oct[!(com_name %like% "Chipmunk"),]
bw_oct <- bw_oct[!(com_name %like% "Conehead"),]
bw_oct <- bw_oct[!(com_name %like% "Gryllus assimilis"),]
bw_oct <- bw_oct[!(com_name %like% "Human"),]
bw_oct <- bw_oct[!(com_name %like% "Monkey"),]

# blanket conservative confidence score
# same as eclipse soundscapes
bw_oct <- bw_oct[Confidence >= 0.65,]

# PUC GPS is shit, so the same puc appears as a unique location but its just getting poor fixes


# get pucs, remove duplicated station id, bring back into bw_oct so I have just a single lat/lon
# pucs <- bw_oct[Station %like% "PUC"]
# pucs <- pucs[!duplicated(Station)]
# bw_oct <- bw_oct[!(Station %like% "PUC")]
# bw_oct <- rbindlist(list(bw_oct, pucs))

# get lat lon stuff going for getting unique stations
# bw_oct[,  `:=`(lon_round = round(Longitude, 3),
#                lat_round = round(Latitude, 3))] 

bw_oct[, lat_lon_index := .GRP, .(Longitude, Latitude)]

# if we need contact times, we can do the following:

bw_oct_locs <- bw_oct[!duplicated(lat_lon_index),]
bw_oct_locs <- st_as_sf(bw_oct_locs, coords = c('Longitude', 'Latitude'), crs = 4326)


# north american
north_america <- rnaturalearth::ne_countries(continent = "North America", returnclass = "sf")
north_america_union <- st_union(north_america)

bw_oct_locs <- st_transform(bw_oct_locs, crs = st_crs(north_america_union))

bw_oct_locs <- st_intersection(bw_oct_locs, north_america_union)
mapview(bw_oct_locs[bw_oct_locs$Station %like% "PUC",], zcol = 'Station', legend = F)
mapview(bw_oct_locs, zcol = 'Station', legend = F)

bw_oct_locs$Longitude <- st_coordinates(bw_oct_locs)[,1]
bw_oct_locs$Latitude <- st_coordinates(bw_oct_locs)[,2]
bw_oct_locs <- st_drop_geometry(bw_oct_locs)
bw_oct_locs <- bw_oct_locs[, c('lat_lon_index', 'date', 'Longitude', 'Latitude')]
setDT(bw_oct_locs)
bw_oct_locs[, date := as.IDate("2023-10-14")]
#fwrite(bw_oct_locs, file = here('Data/birdweather_oct23_unique_locs.csv'))
fwrite(bw_oct_locs[, .(Latitude, Longitude)], file = here('Data/birdweather_oct23_unique_locs_lat_lon_only.csv'))


# from here, run the locs through the eclipse data tool main program to extract contact times
# data tool lives in repo
# first open in visual code studio and delete the last empty row in dataset
# or data tool throws error
contact_times <- fread(here('Data/birdweather_oct23_unique_locs_lat_lon_only_annotated_v02.csv'))
contact_times <- contact_times[, !c('ZipCode', 'Longitude', 'Latitude'), with = F]
bw_oct_locs <- cbind(bw_oct_locs, contact_times)
bw_oct_locs <- bw_oct_locs[, !c('date', 'Longitude', 'Latitude'), with = F]

# join with bw
bw_oct <- merge.data.table(x = bw_oct, y = bw_oct_locs, by = "lat_lon_index")
fwrite(bw_oct, file = here('Data/birdweather_oct23_with_contact_times.csv'))


# quick test
# seems like coverage column is messed up?
# tt <- bw_oct[!duplicated(lat_lon_index)]
# #tt <- tt[, CoveragePercent := ifelse(CoveragePercent < 1, CoveragePercent*100, CoveragePercent)]
# tt <- st_as_sf(bw_oct, coords = c("Longitude", "Latitude"), crs = 4326)
# mapview(tt, zcol = "CoveragePercent")
