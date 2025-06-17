# Load necessary library
library(stringr)
# -- -- -- -- -- -- -- -- -- -- -- --
# MANUALLY MODIFY FOLDER PATH AND START_DATETIME

# Define the folder containing the sound files
folder_path <- "D:/2024_Total_Eclipse_Data_Rd_1/ESID#313"

# Actual deployment start date_time
# usually listed in spreadsheet
start_datetime <- as.POSIXct("2024-04-06 13:48:00", tz = "UTC") # Adjust this as needed

# PRESS SOURCE
# -- -- -- -- -- -- -- -- -- -- -- --


# List all .wav files in the folder
wav_files <- list.files(folder_path, pattern = "\\.WAV$", full.names = TRUE)

# Sort files to ensure chronological order
wav_files <- sort(wav_files)

# Function to calculate new timestamps
shift_timestamps <- function(file_name, start_datetime, last_timestamp) {
  # Extract the time portion from the file name
  original_time <- str_extract(file_name, "\\d{8}_\\d{6}")
  
  if (is.na(original_time)) {
    stop(paste("File name does not match expected pattern:", file_name))
  }
  
  # Parse original time to seconds since epoch
  original_datetime <- as.POSIXct(
    paste0("1970-01-01 ", substr(original_time, 10, 17)),
    format = "%Y-%m-%d %H%M%S", tz = "UTC"
  )
  
  # If it's the first file, start from the actual deployment start time
  if (is.null(last_timestamp)) {
    return(start_datetime)
  }
  
  # Calculate elapsed time in seconds between the current and previous file
  elapsed_seconds <- as.numeric(difftime(original_datetime, as.POSIXct("1970-01-01 00:00:00", tz = "UTC"), units = "secs"))
  
  # Add elapsed time to the previous timestamp
  new_timestamp <- last_timestamp + elapsed_seconds
  
  return(new_timestamp)
}

# Initialize variables
last_timestamp <- NULL
new_file_paths <- character()

# Process each file and rename
for (file in wav_files) {
  # Get the new timestamp
  new_timestamp <- shift_timestamps(basename(file), start_datetime, last_timestamp)
  
  # Format the new datetime for the file name
  new_time <- format(new_timestamp, "%Y%m%d_%H%M%S")
  
  # Create the new file path
  new_file_name <- file.path(dirname(file), paste0(new_time, ".WAV"))
  
  # Rename the file
  file.rename(file, new_file_name)
  
  # Update last_timestamp and save new file path
  last_timestamp <- new_timestamp
  new_file_paths <- c(new_file_paths, new_file_name)
}

cat("Timestamps shifted successfully! New files:\n")
print(new_file_paths)
