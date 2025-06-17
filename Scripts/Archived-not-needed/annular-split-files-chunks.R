library(tuneR)
library(seewave)
library(here)
library(data.table)
library(stringr)

# Function to split a WAV file into 1-minute segments
split_wav_file <- function(file_path, file_folder) {
  tryCatch({
    # Read the WAV file to learn about length; does not read in actual sound file
    soundfile <- readWave(file_path, header = T)

    # number of samples in a 1 hour recording
    hour_len <- soundfile$sample.rate * 3600
    
    for(i in 1:ceiling(soundfile$samples/hour_len)){
      if(i == 1){
        part_sound <- readWave(filename = file_path, from = 1, to = i*hour_len, units = 'samples')
        
        # Get the total number of seconds
        totsec <- length(part_sound@left) / part_sound@samp.rate
        
        if (totsec < 60) {
          cat("File is less than 60 seconds: ", file_path, "\n")
          return(NULL)
        }
        
        # Define the segment length (60 seconds)
        seglen <- 60
        
        # Define the break points
        breaks <- unique(c(seq(0, totsec, seglen), totsec))
        index <- 1:(length(breaks) - 1)
        
        # Create the left matrix
        leftmat <- matrix(part_sound@left, ncol = (length(breaks) - 2), nrow = seglen * part_sound@samp.rate)
        
        # Convert to list of Wave objects
        split_audio <- lapply(1:ncol(leftmat), function(x) Wave(left = leftmat[, x], samp.rate = part_sound@samp.rate, bit = part_sound@bit))
        
        # Create output file names and save each segment
        file_base <- tools::file_path_sans_ext(basename(file_path))
        file_dir <- file_folder
        
        for (j in seq_along(split_audio)) {
          start_time <- as.POSIXct(file_base, format = "%Y%m%d_%H%M%S") + (j - 1) * 60
          output_filename <- paste0(file_dir, "/", format(start_time, "%Y%m%d_%H%M%S"), ".wav")
          writeWave(split_audio[[j]], output_filename)
        }
        
        # Delete the original file
        file.remove(file_path)
        gc()
        
      } else{
        
        # after i = 1, modify the from/to in readwave otherwise all the same
        
        # read in sound file
        part_sound <- readWave(filename = file_path, from = (i-1)*hour_len, to = i*hour_len, units = 'samples')
        
        # Get the total number of seconds
        totsec <- length(part_sound@left) / part_sound@samp.rate
        
        if (totsec < 60) {
          cat("File is less than 60 seconds: ", file_path, "\n")
          return(NULL)
        }
        
        # Define the segment length (60 seconds)
        seglen <- 60
        
        # Define the break points
        breaks <- unique(c(seq(0, totsec, seglen), totsec))
        index <- 1:(length(breaks) - 1)
        
        # Create the left matrix
        leftmat <- matrix(part_sound@left, ncol = (length(breaks) - 2), nrow = seglen * part_sound@samp.rate)
        
        # Convert to list of Wave objects
        split_audio <- lapply(1:ncol(leftmat), function(x) Wave(left = leftmat[, x], samp.rate = part_sound@samp.rate, bit = part_sound@bit))
        
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
      }
    }

    

  }, error = function(e) {
    cat("Error: soundfile corrupt or unreadable: ", file_path, "\n", conditionMessage(e), "\n")
  })
}

## - ## - ## - ## - ## - ## - ## - ## - ## - ## - ## - ## - ## - ## - ## - ## - 
## - ## - ## - ## - ## - ## - ## - ## - ## - ## - ## - ## - ## - ## - ## - ## - 

# Set main directory containing the folders
main_dir <- "E:/2023_Annular_Eclipse_Data_Rd_1"

# folders within directory
folders <- list.dirs(main_dir, recursive = F)

# filter down to good deployments
good_deploys <- fread(here('Data/2023_annualar_good_deployments.csv'))
good_deploys <- good_deploys[Notes == "Good Deployment" & `Needs split` == 'Yes']
good_deploys[, ESIDNumber := str_pad(`AudioMoth ES ID Number`,
                                     side = 'left',
                                     width = 3,
                                     pad = '0')]

# Extract the 3-digit codes from folder names using regular expressions
folder_codes <- sub(".*ESID#(\\d{3}).*", "\\1", folders)

# Filter folders where the 3-digit code matches the ESIDNumber
filtered_folders <- folders[folder_codes %in% good_deploys$ESIDNumber]


# Process each folder
for (folder in filtered_folders[1]) {
  # Get all WAV files in the folder
  wav_files <- list.files(folder, pattern = "\\.WAV$", full.names = TRUE)
  
  # Process each WAV file
  for (wav_file in wav_files) {
    split_wav_file(wav_file, folder)
  }
  

  cat(folder, 'completed\n')
  gc()
}

cat("All files have been processed, split, and original files deleted where applicable.")
