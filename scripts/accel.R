
  library(tidyverse)
  library(readxl)
  library(janitor)
  library(viridis)
  
  repeat.before = function(x) {   # repeats the last non NA value. Keeps leading NA
    ind = which(!is.na(x))        # get positions of nonmissing values
    if(is.na(x[1]))               # if it begins with a missing, add the
      ind = c(1,ind)              # first position to the indices
    rep(x[ind], times = diff(     # repeat the values at these indices
      c(ind, length(x) + 1) ))    # diffing the indices + length yields how often
  }

# Extracting accelerometer data ------------------------------------------------
  
# Unzip all the files in the project shared drive and put into data folder ----
  
  zips <- "G:/Shared drives/FCB Shared Drive/SIVUH Pre Intervention/SIVUH Pre Intervention accelerometers/"
  tar <- list.files(zips)[grepl(".zip", list.files(zips))]
  out_ <- "G:/Shared drives/SDAU/studies/open/19_01_01_fcb/data/accel"
  for(i in tar){
    zip_file <- paste0(zips, i)
    unzip(zip_file, exdir = out_)
  }
  
# Pull out the actual accelerometer data ----
  tar <- list.files("data/accel/")[!grepl("_MACOSX", list.files("data/accel/"))]

# 10 sec epochs ----  
  accel_10secs <- data_frame()
  for(i in tar){
    
    sub_ <- list.files(paste0("data/accel/", i)) # Subfolder with all the data
    
    files_2 <- paste0("data/accel/", i, "/", sub_, "/10 Sec Bin/")
    
    for(j in list.files(files_2)){
      file_ <- paste0(files_2, j)
      accel_10secs <- bind_rows(
        read_csv(file_), 
        accel_10secs
      ) %>%
        mutate(time = "10 secs")
    }
  }
  
  accel_10secs <- map_df(accel_10secs, repeat.before) %>%
    clean_names() %>%
    arrange(date) %>%
    group_by(ssid) %>%
    mutate(cum_binned = cumsum(binned_steps_steps_in_bin))
  
# 1 minute epochs ----  
  accel_1min <- data_frame()
  for(i in tar){
    
    sub_ <- list.files(paste0("data/accel/", i)) # Subfolder with all the data
    
    files_1 <- paste0("data/accel/", i, "/", sub_, "/1 Min Bin/")
    files_2 <- paste0("data/accel/", i, "/", sub_, "/10 Sec Bin/")
    
    for(j in list.files(files_1)){
      file_ <- paste0(files_1, j)
      accel_1min <- bind_rows(
        read_csv(file_), 
        accel_1min
      ) %>%
        mutate(time = "1 min")
    }
  }
  
  accel_1min <- map_df(accel_1min, repeat.before) %>%
    clean_names() %>%
    arrange(date) %>%
    group_by(ssid) %>%
    mutate(cum_binned = cumsum(binned_steps_steps_in_bin))
  

  ggplot(accel_1min, aes(x = binned_steps_timestamp, y = cum_binned, 
                    group = ssid, color = ssid)) +
    geom_line(size = 1) +
    scale_color_viridis(discrete = TRUE, guide = FALSE, end = 0.8) +
    facet_wrap(~ssid, scales = "free_x") +
    theme(axis.text.x = element_text(size = 6)) +
    ylab("Cummulative steps (1 minute bins)") +
    xlab("")
  
  ggsave("summary_steps.png")
  
  save(accel_1min, accel_10secs, file = "data/accel_data.RData")
    
  