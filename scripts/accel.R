
  library(tidyverse)
  library(readxl)
  library(janitor)
  
  repeat.before = function(x) {   # repeats the last non NA value. Keeps leading NA
    ind = which(!is.na(x))        # get positions of nonmissing values
    if(is.na(x[1]))               # if it begins with a missing, add the
      ind = c(1,ind)              # first position to the indices
    rep(x[ind], times = diff(     # repeat the values at these indices
      c(ind, length(x) + 1) ))    # diffing the indices + length yields how often
  }

# Extracting accelerometer data

  tar <- list.files("data/accel/") # Individual patient folders
  
  accel <- data_frame()
  for(i in tar){
    
    sub_ <- list.files(paste0("data/accel/", i)) # Subfolder with all the data
    
    files_1 <- paste0("data/accel/", i, "/", sub_, "/1 Min Bin/")
    files_2 <- paste0("data/accel/", i, "/", sub_, "/10 Sec Bin/")
    
    for(j in list.files(files_1)){
      file_ <- paste0(files_1, j)
      accel <- bind_rows(
        read_csv(file_), 
        accel
      ) %>%
        mutate(time = "1 min")
    }
    
#   for(k in list.files(files_2)){
#     file_ <- paste0(files_2, k)
#     accel <- bind_rows(
#       read_csv(file_), 
#       accel
#     ) %>%
#       mutate(time = "10 sec")
#   }
  }
  
  accel <- map_df(accel, repeat.before) %>%
    clean_names() %>%
    arrange(date) %>%
    group_by(ssid) %>%
    mutate(cum_binned = cumsum(binned_steps_steps_in_bin))
  
  ggplot(accel, aes(x = binned_steps_timestamp, y = cum_binned, 
                    group = ssid, color = ssid)) +
    geom_line() +
    facet_wrap(~ssid)
  
  