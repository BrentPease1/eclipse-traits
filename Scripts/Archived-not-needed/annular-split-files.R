# Load necessary libraries
library(tuneR)
library(seewave)
library(here)
library(data.table)
library(stringr)
# Function to split a WAV file into 1-minute segments
split_wav_file <- function(file_path, file_folder) {
  tryCatch({
    # Read the WAV file
    tic <- Sys.time()
    soundfile <- readWave(file_path)
    cat("Read file in", round(difftime(Sys.time(), tic, units = "secs"), 2), "seconds.\n")
    
    # Get the total number of seconds
    totsec <- length(soundfile@left) / soundfile@samp.rate
    
    if (totsec <= 60) {
      cat("File is less than 60 seconds: ", file_path, "\n")
      return(NULL)
    }
    
    # Define the segment length (60 seconds)
    seglen <- 60
    
    # Define the break points
    breaks <- unique(c(seq(0, totsec, seglen), totsec))
    index <- 1:(length(breaks) - 1)
    
    # Create the left matrix
    leftmat <- matrix(soundfile@left, ncol = (length(breaks) - 2), nrow = seglen * soundfile@samp.rate)
    
    # Convert to list of Wave objects
    split_audio <- lapply(1:ncol(leftmat), function(x) Wave(left = leftmat[, x], samp.rate = soundfile@samp.rate, bit = soundfile@bit))
    
    # Create output file names and save each segment
    file_base <- tools::file_path_sans_ext(basename(file_path))
    file_dir <- file_folder
    
    for (i in seq_along(split_audio)) {
      start_time <- as.POSIXct(file_base, format = "%Y%m%d_%H%M%S") + (i - 1) * 60
      output_filename <- paste0(file_dir, "/", format(start_time, "%Y%m%d_%H%M%S"), ".wav")
      writeWave(split_audio[[i]], output_filename)
    }
    
    # Delete the original file
    file.remove(file_path)
    gc()
  }, error = function(e) {
    cat("Error: soundfile corrupt or unreadable: ", file_path, "\n", conditionMessage(e), "\n")
  })
}

## - ## - ## - ## - ## - ## - ## - ## - ## - ## - ## - ## - ## - ## - ## - ## - 
## - ## - ## - ## - ## - ## - ## - ## - ## - ## - ## - ## - ## - ## - ## - ## - 

# read in deployment information
good_deploys <- fread(here('Data/2023_annualar_good_deployments.csv'))
good_deploys <- good_deploys[Notes == "Good Deployment" & `Needs split` == 'Yes']
good_deploys[, ESIDNumber := str_pad(`AudioMoth ES ID Number`,
                                     side = 'left',
                                     width = 3,
                                     pad = '0')]


# Set your main directory containing the folders
main_dir <- "E:/2023_Annular_Eclipse_Data_Rd_1"

# folders within directory
folders <- list.dirs(main_dir, recursive = F)

# Extract the 3-digit codes from folder names using regular expressions
folder_codes <- sub(".*ESID#(\\d{3}).*", "\\1", folders)

# Filter folders where the 3-digit code matches the ESIDNumber
filtered_folders <- folders[folder_codes %in% good_deploys$ESIDNumber]

# 002 is messed up
filtered_folders <- filtered_folders[2:length(filtered_folders)]

# Process each folder
for (folder in filtered_folders) {
  # Get all WAV files in the folder
  wav_files <- list.files(folder, pattern = "\\.WAV$", full.names = TRUE)
  
  # Process each WAV file
  for (wav_file in wav_files) {
    split_wav_file(wav_file, folder)
    gc()
  }
  
  # update deployments sheet
  good_deploys[ESIDNumber == sub(".*ESID#(\\d{3}).*", "\\1", folder), `Needs split` := "NO"]
  #overwrite deployment sheet
  fwrite(good_deploys, here('Data/2023_Annular_Eclipse_split_info.csv'))
  cat(folder, 'completed\n')
  gc()
}

cat("All files have been processed, split, and original files deleted where applicable.")
