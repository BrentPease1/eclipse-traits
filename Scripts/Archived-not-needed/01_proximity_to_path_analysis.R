library(ggplot2)
library(data.table)
library(here)
library(hms)
library(lubridate)
library(stringr)
interval_minutes = 1 # this didn't work but leaving in anyway

# read in birdnet output
birdnet <- fread(here('Data/birdnet_cleaned_v03.csv'))
# fix timestamps
time_cols <- c('start_time', 'end_time', "FirstContactTimeUTC",
               "SecondContactTimeUTC","ThirdContactTimeUTC",
               "FourthContactTimeUTC", "TotalEclipseTimeUTC") 
birdnet[, (time_cols) := lapply(.SD, function(x) as_hms(x)), 
        .SDcols = time_cols]

# just focus on total eclipse right now
birdnet <- birdnet[eclipse_event == 'total']
birdnet <- birdnet[activity == 'diurnal']
# Load ES locations and times
total_times <- fread(here('Data/metadata/es2024_locations_times.csv'))
total_times <- total_times[Latitude != 0]
total_times[, ESIDNumber := str_pad(string = ESIDNumber,
                                    width = 3,
                                    side = 'left',
                                    pad = '0')]
# reduce to three focal columns
total_times <- total_times[, .(ESIDNumber, FirstContactDate, FirstContactTimeUTC, SecondContactTimeUTC,ThirdContactTimeUTC, FourthContactTimeUTC, TotalEclipseTimeUTC)]
# fix timestamps
time_cols <- tail(names(total_times), 5)  # Select the last five columns
total_times[, (time_cols) := lapply(.SD, function(x) as.POSIXct(paste(FirstContactDate, x), format = "%m/%d/%Y %H:%M:%S", tz = "UTC")), 
            .SDcols = time_cols]

# calculate relative times in minutes
total_times[, `:=`(
  TimeSinceFirstContact = as.numeric(difftime(FirstContactTimeUTC, FirstContactTimeUTC, units = "mins")),
  TimeSinceSecondContact = as.numeric(difftime(SecondContactTimeUTC, FirstContactTimeUTC, units = "mins")),
  TimeSinceThirdContact = as.numeric(difftime(ThirdContactTimeUTC, FirstContactTimeUTC, units = "mins")),
  TimeSinceFourthContact = as.numeric(difftime(FourthContactTimeUTC, FirstContactTimeUTC, units = "mins")),
  TimeSinceTotality = as.numeric(difftime(TotalEclipseTimeUTC, FirstContactTimeUTC, units = "mins"))
)]

# Compute relative time since first contact for bird detections
birdnet[, TimeSinceFirstContact := as.numeric(difftime(start_time, FirstContactTimeUTC, units = "mins")), by = ESIDNumber]

## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 

# GET ARRAY DIMENSIONS
# max relative time that occurred between fourth and first time
total_min <- floor(max(total_times$TimeSinceFourthContact, na.rm = T))

# total ESIDs
total_sites <- length(unique(total_times$ESIDNumber))

# assign esid a number
birdnet[, ESID := .GRP, ESIDNumber]

## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 

# TOTAL 100% coverage ####
total_100 <- birdnet[eclipse_event == 'total' & date == "2024-04-08" & coverage == 100]
total_100 <- total_100[start_time >= FirstContactTimeUTC & start_time <= FourthContactTimeUTC]


total_100[, SamplePeriod := cut(
  x = TimeSinceFirstContact,
  breaks = seq(
    from = min(total_times$TimeSinceFirstContact, na.rm = T),
    to = max(total_times$TimeSinceFourthContact, na.rm = T),
    by = interval_minutes
  ),
  labels = FALSE
)]


# precompute the counts of index combinations before loop
total_100_counts <- total_100[, .N, by = .(SamplePeriod, ESID)]
total_100_counts <- total_100_counts[!is.na(SamplePeriod),]
# create an array to hold output
total_100_array <- array(data = 0, dim = c(total_min, total_sites))

# loop through and update array
for (i in seq_len(nrow(total_100_counts))) {
  time_idx <- total_100_counts$SamplePeriod[i]
  site_idx <- total_100_counts$ESID[i]
  activity_count <- total_100_counts$N[i]
  
  if (!is.na(time_idx) && !is.na(site_idx)) {
    if (time_idx > 0 && time_idx <= dim(total_100_array)[1] &&
        site_idx > 0 && site_idx <= dim(total_100_array)[2]) {
      total_100_array[time_idx, site_idx] <- activity_count
    }
  }
}

non_zero_rows <- rowSums(total_100_array) != 0
total_100_counts <- apply(total_100_array[non_zero_rows, , drop = FALSE], MARGIN = 1, FUN = mean)

#total_100_counts <- apply(total_100_array, MARGIN = 1, FUN = function(x) mean(x))
#plot(x = 1:nrow(total_100_array), y = total_100_counts)

## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
# TOTAL 99% coverage ####
total_99 <- birdnet[eclipse_event == 'total' & date == "2024-04-08" & coverage >= 99 & coverage < 100]
total_99 <- total_99[start_time >= FirstContactTimeUTC & start_time <= FourthContactTimeUTC]

total_99[, SamplePeriod := cut(
  x = TimeSinceFirstContact,
  breaks = seq(
    from = min(total_times$TimeSinceFirstContact, na.rm = T),
    to = max(total_times$TimeSinceFourthContact, na.rm = T),
    by = interval_minutes
  ),
  labels = FALSE
)]






# precompute the counts of index combinations before loop
total_99_counts <- total_99[, .N, by = .(SamplePeriod, ESID)]

# create an array to hold output
total_99_array <- array(data = 0, dim = c(total_min, total_sites))

# loop through and update array
for (i in seq_len(nrow(total_99_counts))) {
  time_idx <- total_99_counts$SamplePeriod[i]
  site_idx <- total_99_counts$ESID[i]
  activity_count <- total_99_counts$N[i]
  
  if (!is.na(time_idx) && !is.na(site_idx)) {
    if (time_idx > 0 && time_idx <= dim(total_99_array)[1] &&
        site_idx > 0 && site_idx <= dim(total_99_array)[2]) {
      total_99_array[time_idx, site_idx] <- activity_count
    }
  }
}

#total_99_counts <- apply(total_99_array, MARGIN = 1, FUN = function(x) mean(x))
#plot(x = 1:nrow(total_99_array), y = total_99_counts)

# I think some sites do not fall into this 99 category, so dropping those
non_zero_rows <- rowSums(total_99_array) != 0
total_99_counts <- apply(total_99_array[non_zero_rows, , drop = FALSE], MARGIN = 1, FUN = mean)

## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 

# TOTAL 98% coverage ####
total_98 <- birdnet[eclipse_event == 'total' & date == "2024-04-08" & coverage >= 98 & coverage < 99]
total_98 <- total_98[start_time >= FirstContactTimeUTC & start_time <= FourthContactTimeUTC]

total_98[, SamplePeriod := cut(
  x = TimeSinceFirstContact,
  breaks = seq(
    from = min(total_times$TimeSinceFirstContact, na.rm = T),
    to = max(total_times$TimeSinceFourthContact, na.rm = T),
    by = interval_minutes
  ),
  labels = FALSE
)]






# precompute the counts of index combinations before loop
total_98_counts <- total_98[, .N, by = .(SamplePeriod, ESID)]

# create an array to hold output
total_98_array <- array(data = 0, dim = c(total_min, total_sites))

# loop through and update array
for (i in seq_len(nrow(total_98_counts))) {
  time_idx <- total_98_counts$SamplePeriod[i]
  site_idx <- total_98_counts$ESID[i]
  activity_count <- total_98_counts$N[i]
  
  if (!is.na(time_idx) && !is.na(site_idx)) {
    if (time_idx > 0 && time_idx <= dim(total_98_array)[1] &&
        site_idx > 0 && site_idx <= dim(total_98_array)[2]) {
      total_98_array[time_idx, site_idx] <- activity_count
    }
  }
}

# total_98_counts <- apply(total_98_array, MARGIN = 1, FUN = function(x) mean(x))
# plot(x = 1:nrow(total_98_array), y = total_98_counts)
# 

# I think some sites do not fall into this 99 category, so dropping those
non_zero_rows <- rowSums(total_98_array) != 0
total_98_counts <- apply(total_98_array[non_zero_rows, , drop = FALSE], MARGIN = 1, FUN = mean)


## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
# TOTAL 97% coverage ####
total_97 <- birdnet[eclipse_event == 'total' & date == "2024-04-08" & coverage >= 97 & coverage < 98]
total_97 <- total_97[start_time >= FirstContactTimeUTC & start_time <= FourthContactTimeUTC]

total_97[, SamplePeriod := cut(
  x = TimeSinceFirstContact,
  breaks = seq(
    from = min(total_times$TimeSinceFirstContact, na.rm = T),
    to = max(total_times$TimeSinceFourthContact, na.rm = T),
    by = interval_minutes
  ),
  labels = FALSE
)]


# precompute the counts of index combinations before loop
total_97_counts <- total_97[, .N, by = .(SamplePeriod, ESID)]

# create an array to hold output
total_97_array <- array(data = 0, dim = c(total_min, total_sites))

# loop through and update array
for (i in seq_len(nrow(total_97_counts))) {
  time_idx <- total_97_counts$SamplePeriod[i]
  site_idx <- total_97_counts$ESID[i]
  activity_count <- total_97_counts$N[i]
  
  if (!is.na(time_idx) && !is.na(site_idx)) {
    if (time_idx > 0 && time_idx <= dim(total_97_array)[1] &&
        site_idx > 0 && site_idx <= dim(total_97_array)[2]) {
      total_97_array[time_idx, site_idx] <- activity_count
    }
  }
}

# total_97_counts <- apply(total_97_array, MARGIN = 1, FUN = function(x) mean(x))
# plot(x = 1:nrow(total_97_array), y = total_97_counts)
# 

# I think some sites do not fall into this 99 category, so dropping those
non_zero_rows <- rowSums(total_97_array) != 0
total_97_counts <- apply(total_97_array[non_zero_rows, , drop = FALSE], MARGIN = 1, FUN = mean)


## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
# SUMMARIZING/PLOTTING ####
out <- data.table(Vocal_Detections = c(total_100_counts, 
                                       total_99_counts,
                                       total_98_counts, 
                                       total_97_counts),
                  period = c(rep('100% Coverage', length(total_100_counts)),
                             rep('99% Coverage', length(total_99_counts)),
                             rep('98% Coverage', length(total_98_counts)),
                             rep('97% Coverage', length(total_97_counts))),
                  xaxis = c(1:length(total_100_counts),
                            1:length(total_99_counts),
                            1:length(total_98_counts),
                            1:length(total_97_counts)))

# make it relative
out[, relative_vocal_detections := Vocal_Detections / max(Vocal_Detections), period]
# 
# ggplot(out, aes(x = xaxis, y = Vocal_Detections,color = period)) +
#   geom_point(alpha = 0.2) + 
#   geom_smooth(method = "loess", span = 0.5)
# 
# (p1 <- ggplot(out, aes(x = xaxis, y = Vocal_Detections,color = period)) +
#   geom_point(alpha = 0.2) + 
#   geom_smooth(method = 'loess', span = 0.5))  # Increasing k increases wiggliness


# Generate the ggplot object
(p1 <- ggplot(out, aes(x = xaxis, y = relative_vocal_detections, color = period)) +
    geom_smooth(method = "gam", formula = y ~ s(x, k = 6),aes(fill = period), alpha = 0.3))# +
   # scale_color_manual(values = c("random_nonday" = 'gold', "within_path" = "black"))

(p1 <- ggplot(out, aes(x = xaxis, y = Vocal_Detections, color = period)) +
  scale_color_manual(values = c("random_nonday" = 'gold', "within_path" = "black")) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 6), aes(fill = period), alpha = 0.3) +
  scale_fill_manual(values = c("random_nonday" = 'gold', "within_path" = "black")) +
    labs(x = "Time Relative to Eclipse", y = "Average Number of Vocalizations Per Minute")+
    theme_bw()+
    theme(axis.title = element_text(size = 18),
          axis.text = element_text(size = 14),
          legend.position="none"))

# Extract smoothed data
smoothed_data <- as.data.table(ggplot_build(p1)$data[[1]])

# Filter for the "within_path" period (blue line)
blue_line <- smoothed_data[colour == "#00BFC4"]  # Default ggplot blue

# Compute slope (change in y)
blue_line[, slope := c(NA, diff(y))]


# Find where slope changes from positive to negative (local maximum)
rise_x <- blue_line[which(diff(sign(slope)) == -2), x]
rise_x <- rise_x[1:2]

# slope negative to positive
drop_x <- blue_line[which(diff(sign(slope)) == 2), x]

eclipse <- cut(
  x = mean(total_times$TimeSinceTotality, na.rm = T),
  breaks = seq(
    from = min(total_times$TimeSinceFirstContact, na.rm = T),
    to = max(total_times$TimeSinceFourthContact, na.rm = T),
    by = interval_minutes
  ),
  labels = FALSE
)
first <- cut(
  x = mean(total_times$TimeSinceFirstContact, na.rm = T),
  breaks = seq(
    from = min(total_times$TimeSinceFirstContact, na.rm = T),
    to = max(total_times$TimeSinceFourthContact, na.rm = T),
    by = interval_minutes
  ),
  labels = FALSE
)



second <- cut(
  x = mean(total_times$TimeSinceSecondContact, na.rm = T),
  breaks = seq(
    from = min(total_times$TimeSinceFirstContact, na.rm = T),
    to = max(total_times$TimeSinceFourthContact, na.rm = T),
    by = interval_minutes
  ),
  labels = FALSE
)

third <- cut(
  x = mean(total_times$TimeSinceThirdContact, na.rm = T),
  breaks = seq(
    from = min(total_times$TimeSinceFirstContact, na.rm = T),
    to = max(total_times$TimeSinceFourthContact, na.rm = T),
    by = interval_minutes
  ),
  labels = FALSE
)

fourth <- cut(
  x = mean(total_times$TimeSinceFourthContact, na.rm = T),
  breaks = seq(
    from = min(total_times$TimeSinceFirstContact, na.rm = T),
    to = max(total_times$TimeSinceFourthContact, na.rm = T),
    by = interval_minutes
  ),
  labels = FALSE
)



p1 + 
  geom_vline(xintercept = 0, linetype = "solid", color = "black", size = 0.75,alpha = 0.3) +
  geom_vline(xintercept = second, linetype = "dotted", color = "black", size = 0.75,alpha = 0.3) +
  geom_vline(xintercept = eclipse, linetype = "dashed", color = "black", size = 0.75,alpha = 0.3) +
  geom_vline(xintercept = third, linetype = "dotted", color = "black", size = 0.75,alpha = 0.3) +
  geom_vline(xintercept = fourth, linetype = "solid", color = "black", size = 0.75,alpha = 0.3) +
 # geom_vline(xintercept = rise_x[1], linetype = 5, color = "black", size = 0.5) +
 # geom_vline(xintercept = rise_x[2], linetype = 5, color = "black", size = 0.5) +
 # geom_vline(xintercept = drop_x[2], linetype = 5, color = "black", size = 0.5) +
  geom_point(alpha = 0.5)

ggsave(plot = last_plot(), filename = here('Results/Figures/detections_per_minute_relative.png'),
       width = 12, height = 8, dpi = 300)


p1 + 
  geom_vline(xintercept = 0, linetype = "solid", color = "black", size = 0.75,alpha = 0.3) +
  geom_vline(xintercept = second, linetype = "dotted", color = "black", size = 0.75,alpha = 0.3) +
  geom_vline(xintercept = eclipse, linetype = "dashed", color = "black", size = 0.75,alpha = 0.3) +
  geom_vline(xintercept = third, linetype = "dotted", color = "black", size = 0.75,alpha = 0.3) +
  geom_vline(xintercept = fourth, linetype = "solid", color = "black", size = 0.75,alpha = 0.3) +
 # geom_vline(xintercept = rise_x[1], linetype = 5, color = "black", size = 0.5) +
  geom_vline(xintercept = 56, linetype = 5, color = "black", size = 0.5) +
  geom_vline(xintercept = rise_x[2], linetype = 5, color = "black", size = 0.5) +
 # geom_vline(xintercept = drop_x[2], linetype = 5, color = "black", size = 0.5) +
  geom_point(alpha = 0.5)

ggsave(plot = last_plot(), filename = here('Results/Figures/detections_per_minute_relative_MARKERS.png'),
       width = 12, height = 8, dpi = 300)

#effect of eclipse starts
eclipse - rise_x[1]

# effect dissipates
rise_x[2] - eclipse

  
(p2 <- ggplot(out, aes(x = xaxis, y = Vocal_Detections, color = period)) +
    geom_vline(xintercept = 0, linetype = "solid", color = "black", size = 0.75,alpha = 0.3) +
    geom_vline(xintercept = second, linetype = "dotted", color = "black", size = 0.75,alpha = 0.3) +
    geom_vline(xintercept = eclipse, linetype = "dashed", color = "black", size = 0.75,alpha = 0.3) +
    geom_vline(xintercept = third, linetype = "dotted", color = "black", size = 0.75,alpha = 0.3) +
    geom_vline(xintercept = fourth, linetype = "solid", color = "black", size = 0.75,alpha = 0.3) +
    # geom_vline(xintercept = rise_x[1], linetype = 5, color = "black", size = 0.5) +
   # geom_vline(xintercept = 56, linetype = 5, color = "black", size = 0.5) +
   # geom_vline(xintercept = rise_x[2], linetype = 5, color = "black", size = 0.5) +
    scale_color_manual(values = c("random_nonday" = 'gold', "within_path" = "black")) +
   # geom_smooth(method = "gam", formula = y ~ s(x, k = 6), aes(fill = period), alpha = 0.3) +
    scale_fill_manual(values = c("random_nonday" = 'gold', "within_path" = "black")) +
    labs(x = "Time Relative to Eclipse", y = "Average Number of Vocalizations Per Minute")+
    theme_bw()+
    theme(axis.title = element_text(size = 18),
          axis.text = element_text(size = 14),
          legend.position="none"))
ggsave(plot = p2, filename = here('Results/Figures/detections_per_minute_relative_MARKERS_ONLY.png'),
       width = 12, height = 8, dpi = 300)
