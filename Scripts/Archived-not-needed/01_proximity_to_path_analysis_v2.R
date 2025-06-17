# Define the observed coverage levels
coverage_levels <- sort(unique(round(birdnet$coverage, 0)))

# Create a list to store results
total_counts_list <- list()

for (cov in coverage_levels) {
  
  # Define the subset of birdnet for the current coverage range
  if (cov == 100) {
    total_data <- birdnet[eclipse_event == 'total' & date == "2024-04-08" & coverage == 100]
  } else {
    total_data <- birdnet[eclipse_event == 'total' & date == "2024-04-08" & coverage >= cov & coverage < cov + 1]
  }
  
  # Filter within eclipse event times
  total_data <- total_data[start_time >= FirstContactTimeUTC & start_time <= FourthContactTimeUTC]
  
  # Assign sample periods
  total_data[, SamplePeriod := cut(
    x = TimeSinceFirstContact,
    breaks = seq(
      from = min(total_times$TimeSinceFirstContact, na.rm = T),
      to = max(total_times$TimeSinceFourthContact, na.rm = T),
      by = interval_minutes
    ),
    labels = FALSE
  )]
  
  # Precompute counts
  total_counts <- total_data[, .N, by = .(SamplePeriod, ESID)]
  
  # Create an array to hold output
  total_array <- array(data = 0, dim = c(total_min, total_sites))
  
  # Loop through and update the array
  for (i in seq_len(nrow(total_counts))) {
    time_idx <- total_counts$SamplePeriod[i]
    site_idx <- total_counts$ESID[i]
    activity_count <- total_counts$N[i]
    
    if (!is.na(time_idx) && !is.na(site_idx)) {
      if (time_idx > 0 && time_idx <= dim(total_array)[1] &&
          site_idx > 0 && site_idx <= dim(total_array)[2]) {
        total_array[time_idx, site_idx] <- activity_count
      }
    }
  }
  
  # Remove empty rows and compute the mean
  # non_zero_rows <- rowSums(total_array) != 0
  # total_counts_final <- apply(total_array[non_zero_rows, , drop = FALSE], MARGIN = 1, FUN = mean)
  total_counts_final <- apply(total_array, MARGIN = 1, FUN = function(x) mean(x))
  
  # Store results in the list
  total_counts_list[[as.character(cov)]] <- total_counts_final
}


# Convert the list into a long-format data.table
out <- rbindlist(lapply(names(total_counts_list), function(cov) {
  data.table(
    Vocal_Detections = total_counts_list[[cov]],  # Extract values
    period = paste0(cov, "% Coverage"),  # Create period labels
    xaxis = seq_along(total_counts_list[[cov]])  # Generate x-axis indices
  )
}))

# Convert period to a factor to maintain order
out[, period := factor(period, levels = paste0(sort(as.numeric(names(total_counts_list)), decreasing = TRUE), "% Coverage"))]
out[, relative_vocal_detections := Vocal_Detections / max(Vocal_Detections), period]
out[, relative_vocal_detections := ifelse(is.nan(relative_vocal_detections), 0, relative_vocal_detections)]


(p1 <- ggplot(out, aes(x = xaxis, y = relative_vocal_detections, color = period)) +
    geom_smooth(method = "gam", formula = y ~ s(x, k = 6),aes(fill = period), alpha = 0.3))
p1 + facet_wrap(~period)
# Access results for a specific coverage level, e.g., 95%
total_95_counts <- total_counts_list[["95"]]
