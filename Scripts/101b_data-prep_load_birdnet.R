library(data.table)
library(lubridate)
library(stringr)
library(hms)
library(future.apply)
plan(multisession, workers = parallel::detectCores() - 2)

# focal deployment dates
good_days <- as.Date(c("2023-10-12", "2023-10-13", "2023-10-14" ,"2023-10-15", "2023-10-16",
                       "2024-04-06", "2024-04-07", "2024-04-08","2024-04-09", "2024-04-10"))


process_file <- function(this_file) {
  dat <- fread(this_file)
  if (nrow(dat) == 0) return(NULL)  # Skip empty files
  
  start_date_time <- gsub(".BirdNET.results.csv", "", basename(this_file))
  dat[, initial_date_time := ymd_hms(gsub("_", "", start_date_time))]
  dat[, start_timestamp := initial_date_time + seconds(`Start (s)`)]
  dat[, end_timestamp := initial_date_time + seconds(`End (s)`)]
  dat[, ESIDNumber := str_extract(File, "(?<=ESID#)\\d{3}")]
  
  # Filter confidence scores
  dat <- dat[Confidence >= 0.65,]
  if (nrow(dat) == 0) return(NULL)  # Skip low-confidence files
  
  # Add time-related columns
  dat[, `:=`(day = day(start_timestamp), hour = hour(start_timestamp), 
             start_time = as_hms(start_timestamp), end_time = as_hms(end_timestamp), 
             date = date(start_timestamp))]
  

  dat <- dat[date %in% good_days,]
  if (nrow(dat) == 0) return(NULL)  # Skip non-matching dates
  
  # Merge with deployment data
  deploy_table <- if (year(dat$start_timestamp[1]) == 2023) annular_times else total_times
  dat <- merge(dat, deploy_table, by = "ESIDNumber", all.x = TRUE)
  
  # Convert characters to time
  time_cols <- if (year(dat$start_timestamp[1]) == 2024) tail(names(dat), 5) else c('annular_start', 'annular_end', 'annular_max')
  dat[, (time_cols) := lapply(.SD, function(x) as_hms(as.POSIXct(paste(ifelse(year(dat$start_timestamp[1]) == 2024, FirstContactDate, "10/14/2023"), x), format = "%m/%d/%Y %H:%M:%S"))), .SDcols = time_cols]
  
  # Remove unnecessary columns
  dat[, c("Start (s)", "End (s)", "File", "initial_date_time") := NULL]
  setnames(dat, old = c("Common name", "Confidence", "Scientific name"), 
           new = c("common_name", "confidence", "scientific_name"))
  
  return(dat)
}

# Process all files in parallel
holder2 <- future_lapply(bn_files2, process_file)

# Combine results into one data.table
birdnet2 <- rbindlist(holder2, fill = TRUE)
