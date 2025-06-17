library(here)
library(data.table)
library(rnaturalearth)
library(stringr)
library(sf)
library(mapview)
setDTthreads(0) # use it all

# get the data
bw_apr <- fread(here('Data/birdweather_apr24.csv'))
setnames(bw_apr, old = "Common Name", new = 'com_name')
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
# doing a bunch of filtering so we only retain the stations we need
# -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
bw_apr[, date := as.IDate(Timestamp)]
bw_apr <- bw_apr[date %in% as.IDate(c('2024-04-06','2024-04-07','2024-04-08','2024-04-09','2024-04-10'))]

# remove noise, mammals, insects, and amphibians
not_interested <- c("Engine", "Siren", "Coyote", "Dog", 
                    "Eastern Gray Squirrel", "Red Squirrel",
                    "Power tools", "Fireworks", "Gray Wolf", "Gun",
                    "Honey Bee",
                    "Spring Peeper")
bw_apr <- bw_apr[!(com_name %in% not_interested),]
# this should keep frogmouths but drop anuras
bw_apr <- bw_apr[!(str_detect(com_name, "frog(?!mouth)") | str_detect(com_name, "Frog(?!mouth)")),]
bw_apr <- bw_apr[!(com_name %like% "Treefrog"),]
bw_apr <- bw_apr[!(com_name %like% "Bullfrog"),]
bw_apr <- bw_apr[!(com_name %like% "Cricket"),]
bw_apr <- bw_apr[!(com_name %like% "Toad"),]
bw_apr <- bw_apr[!(com_name %like% "Trig"),]
bw_apr <- bw_apr[!(com_name %like% "Katydid"),]
bw_apr <- bw_apr[!(com_name %like% "Chipmunk"),]
bw_apr <- bw_apr[!(com_name %like% "Conehead"),]
bw_apr <- bw_apr[!(com_name %like% "Gryllus assimilis"),]
bw_apr <- bw_apr[!(com_name %like% "Human"),]
bw_apr <- bw_apr[!(com_name %like% "Monkey"),]

# same as eclipse soundscapes
bw_apr <- bw_apr[Confidence >= 0.65,]

# PUC GPS is shit, so the same puc appears as a unique location but its just getting poor fixes


# get pucs, remove duplicated station id, bring back into bw_apr so I have just a single lat/lon
# pucs <- bw_apr[Station %like% "PUC"]
# pucs <- pucs[!duplicated(Station)]
# bw_apr <- bw_apr[!(Station %like% "PUC")]
# bw_apr <- rbindlist(list(bw_apr, pucs))

# get lat lon stuff going for getting unique stations
# bw_apr[,  `:=`(lon_round = round(Longitude, 3),
#                lat_round = round(Latitude, 3))] 

bw_apr[, lat_lon_index := .GRP, .(Longitude, Latitude)]

# if we need contact times, we can do the following:

bw_apr_locs <- bw_apr[!duplicated(lat_lon_index),]
bw_apr_locs <- st_as_sf(bw_apr_locs, coords = c('Longitude', 'Latitude'), crs = 4326)


# north american
north_america <- ne_countries(continent = "North America", returnclass = "sf")
north_america_union <- st_union(north_america)

bw_apr_locs <- st_transform(bw_apr_locs, crs = st_crs(north_america_union))

bw_apr_locs <- st_intersection(bw_apr_locs, north_america_union)
#mapview(bw_apr_locs[bw_apr_locs$Station %like% "PUC",], zcol = 'Station', legend = F)
#mapview(bw_apr_locs, zcol = 'Station', legend = F)

bw_apr_locs$Longitude <- st_coordinates(bw_apr_locs)[,1]
bw_apr_locs$Latitude <- st_coordinates(bw_apr_locs)[,2]
bw_apr_locs <- st_drop_geometry(bw_apr_locs)
bw_apr_locs <- bw_apr_locs[, c('lat_lon_index', 'date', 'Longitude', 'Latitude')]
setDT(bw_apr_locs)
bw_apr_locs[, date := as.IDate("2024-04-08")]
#fwrite(bw_apr_locs, file = here('Data/birdweather_apr24_unique_locs.csv'))
fwrite(bw_apr_locs[, .(Latitude, Longitude)], file = here('Data/birdweather_apr24_unique_locs_lat_lon_only.csv'))


# from here, run the locs through the eclipse data tool main program to extract contact times
# data tool lives in repo
contact_times <- fread(here('Data/birdweather_apr24_unique_locs_lat_lon_only_annotated_v02.csv'))
contact_times <- contact_times[, !c('ZipCode', 'Longitude', 'Latitude'), with = F]
bw_apr_locs <- cbind(bw_apr_locs, contact_times)
bw_apr_locs <- bw_apr_locs[, !c('date', 'Longitude', 'Latitude'), with = F]


# join with bw
bw_apr <- merge.data.table(x = bw_apr, y = bw_apr_locs, by = "lat_lon_index")
fwrite(bw_apr, file = here('Data/birdweather_apr24_with_contact_times.csv'))
       