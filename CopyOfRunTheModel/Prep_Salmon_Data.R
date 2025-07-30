# Script to create salmon arrival data
library(tidyr)
library(dplyr)
library(anytime)
library(lubridate)

#### Arrival Data ----

# case.study <- "Base"
# run_count <- 3
# case.study <- "N1"
# case.study <- "B1"

# Case Base
if(case.study == "Base"){
  run_info <- read.csv("Data/salmon_run_info.csv")
  run_info <- run_info[1:run_count,]
  run_group <- run_info[,"CopyCat"]
  
  run_info$Escape <- 1/run_info$Residence
  sd.end <- max(run_info$Residence) * 4
  
  # create arrival data frame
  salmon_arrival <- create_salmon_arrival(5, sd.end, run_info) # width of days range
  colnames(salmon_arrival) <- c("Day", run_info$Run)
  n_days <- nrow(salmon_arrival)
  data_start <- salmon_arrival$Day[1]
  data_end <- max(salmon_arrival$Day)
  
  # bioenergetics parameters
  run_kcal <- salmon_kcal[run_group]
  low_mat <- matrix(ncol = 3, nrow = 1, data = low_energetics[,run_group])
  colnames(low_mat) <- c("Pv", "Zc", "Ej")
  rownames(low_mat) <- run_info$Run
  # high_mat <- matrix(ncol = 3, nrow = 1, data = high_energetics[,run_group])
  # colnames(high_mat) <- c("Pv", "Zc", "Ej")
  # rownames(high_mat) <- run_info$Run
  high_mat <- matrix(ncol = 3, nrow = 1, data = high_energetics[,"Base"])
  colnames(high_mat) <- c("Pv", "Zc", "Ej")
  rownames(high_mat) <- "Base"
  alpha_mat <- alpha_all["Base",] ### Look here! Not flexible !!!
  colnames(run_kcal) <- run_info$Run
  # rownames(alpha_mat) <- run_info$Run
  
  # days with boats and days with fish
  boat_days <- array(dim = n_days, data = 0)
  salmon_arrival_daily_total <- data.frame(Day = salmon_arrival$Day, Fish = rep(0, n_days))
  for(i in 1:run_count){
    salmon_arrival_daily_total$Fish <- salmon_arrival_daily_total$Fish + salmon_arrival[,(i+1)]
  }
  fishy_days <- which(salmon_arrival_daily_total$Fish > 0)
  
  salmon_catch_rates <- data.frame(matrix(data = 0, nrow = n_days, 
                                          ncol = ncol(salmon_arrival), 
                                          dimnames = dimnames(salmon_arrival)))
  colnames(salmon_catch_rates) <- colnames(salmon_arrival)
  salmon_catch_rates$Day <- salmon_arrival$Day
  
  for(i in 1:run_count){
    if(run_info$Fish_Rate[i] > 0){
      fishery_dates <- (run_info$Fishery_Open[i]:run_info$Fishery_Close[i]) - (data_start-1)
      salmon_catch_rates[fishery_dates, i+1] <- run_info$Fish_Rate[i]
      boat_days[fishery_dates] <- 1
    }
  }
  
  sealion_arrival <- 1
  
  min_fishers <- 15
  max_fishers <- 15
  num_harvesters <- sampleBespoke(x = min_fishers:max_fishers, size = n_days, replace = TRUE)
  max_harvesters <- max(num_harvesters)
  
}
# Case N1
if(case.study == "N1"){
  run_info <- read.csv("Data/salmon_run_info_N.csv")
  run_group <- run_info$Run
  
  run_info$Escape <- 1/run_info$Residence
  n_species <- nrow(run_info)
  run_count <- length(run_group)
  
  # create arrival data frame
  salmon_arrival <- create_salmon_arrival(7, 10, run_info)
  colnames(salmon_arrival) <- c("Day", run_info$Run)
  n_days <- nrow(salmon_arrival)
  data_start <- salmon_arrival$Day[1]
  data_end <- max(salmon_arrival$Day)
  
  # bioenergetics parameters
  run_kcal <- salmon_kcal[run_group]
  low_mat <- low_energetics[,run_group]
  high_mat <- high_energetics[,run_group]
  alpha_mat <- t(alpha_all[run_group,])
  high_flow_alpha_adjust <- 0.5
  high_flow_alphas <- alpha_mat * high_flow_alpha_adjust
  high_flow_t <- yday("2024-10-01") - data_start
  
  # catch data --> rates
  salmon_catch_rates <- data.frame(matrix(data = 0, nrow = n_days, 
                                          ncol = ncol(salmon_arrival), 
                                          dimnames = dimnames(salmon_arrival)))
  colnames(salmon_catch_rates) <- colnames(salmon_arrival)
  salmon_catch_rates$Day <- salmon_arrival$Day
  
  # days with fish
  salmon_arrival_daily_total <- data.frame(Day = salmon_arrival$Day, Fish = rep(0, n_days))
  for(i in 1:run_count){
    salmon_arrival_daily_total$Fish <- salmon_arrival_daily_total$Fish + salmon_arrival[,(i+1)]
  }
  fishy_days <- which(salmon_arrival_daily_total$Fish > 0)[1]:255 # approx end day
  
  
  # fishery days
  if(chum_fishery == TRUE){
    catch_info <- read.csv("Data/salmon_catch_info_N_withChum.csv")
    catch_info$Day <- yday(anydate(catch_info$Dates))
    catch_info$Day[which(catch_info$Day < catch_info$Day[1])] <- 
      catch_info$Day[which(catch_info$Day < catch_info$Day[1])] + 366 #wrap year
    fishery_dates <- which(catch_info[,2] + catch_info[,3] + catch_info[,4] > 0) # loopdays
    
    salmon_catch_rates$Chum[which(catch_info$Chum > 0)] <- 0.1
    salmon_catch_rates$ChinookLN[which(catch_info$LocNis > 0)] <- 0.08
    salmon_catch_rates$ChinookGR[which(catch_info$GR > 0)] <- 0.07
    
    num_harvesters <- c(catch_info$Boats, rep(0, n_days - nrow(catch_info)))
    max_harvesters <- max(num_harvesters)
    
    # for learning cues
    boat_days <- array(dim = n_days, data = 0)
    boat_days[fishery_dates] <- 1
  }
  
  if(chum_fishery == FALSE){
    catch_info <- read.csv("Data/salmon_catch_info_N.csv")
    catch_info$Day <- yday(anydate(x = catch_info$Dates))
    catch_info$Day[which(catch_info$Day < catch_info$Day[1])] <- 
      catch_info$Day[which(catch_info$Day < catch_info$Day[1])] + 366 #wrap year
    fishery_dates <- which(catch_info[,2] + catch_info[,3] + catch_info[,4] > 0) # loopdays
    
    salmon_catch_rates$Chum[which(catch_info$Chum > 0)] <- 0
    salmon_catch_rates$ChinookLN[which(catch_info$LocNis > 0)] <- 0.08
    salmon_catch_rates$ChinookGR[which(catch_info$GR > 0)] <- 0.07
    
    num_harvesters <- c(catch_info$Boats, rep(0, n_days - length(catch_info$Boats)))
    max_harvesters <- max(num_harvesters)
    
    # for learning cues
    boat_days <- array(dim = n_days, data = 0)
    boat_days[fishery_dates] <- 1
  }
  
  
}
# Case B1
if(case.study == "B1"){
  run_info <- read.csv("Data/salmon_run_info_B.csv")
  run_group <- run_info$Run
  n_species <- nrow(run_info)
  run_count <- length(run_group)
  
  # create arrival data frame
  salmon_arrival <- create_salmon_arrival(7, 10, run_info)
  colnames(salmon_arrival) <- c("Day", run_info$Run)
  n_days <- nrow(salmon_arrival)
  data_start <- salmon_arrival$Day[1]
  data_end <- max(salmon_arrival$Day)
  
  run_info$Escape <- 1/run_info$Residence
  temp_slow_factor <- 1.5
  hot_days <- (yday("01/07/2024")-data_start):(yday("01/09/2024")-data_start)
  
  # bioenergetics parameters
  run_kcal <- salmon_kcal[run_group]
  low_mat <- low_energetics[,run_group]
  high_mat <- high_energetics[,run_group]
  alpha_mat <- t(alpha_all[run_group,])
  
  # catch data --> rates
  salmon_catch_rates <- data.frame(matrix(data = 0, nrow = n_days, ncol = ncol(salmon_arrival), dimnames = dimnames(salmon_arrival)))
  colnames(salmon_catch_rates) <- colnames(salmon_arrival)
  salmon_catch_rates$Day <- salmon_arrival$Day
  
  # days with boats and days with fish
  salmon_arrival_daily_total <- data.frame(Day = salmon_arrival$Day, Fish = rep(0, n_days))
  for(i in 1:run_count){
    salmon_arrival_daily_total$Fish <- salmon_arrival_daily_total$Fish + salmon_arrival[,(i+1)]
  }
  fishy_days <- which(salmon_arrival_daily_total$Fish > 0)[1]:202 # approx end day
  
  catch_info <- read.csv("Data/salmon_catch_info_B.csv")
  catch_info$Day <- yday(anydate(catch_info$Dates))
  fishery_dates <- catch_info$Day[which(catch_info[,2] + catch_info[,3] + catch_info[,4] > 0)] - data_start # turned into loopdays
  
  # sockeye rate is 0, bycatch minimal
  salmon_catch_rates$ChinookB[fishery_dates] <- 0.02
  salmon_catch_rates$CohoB[fishery_dates] <- 0.04
  
  min_harvesters <- 13
  max_harvesters <- 25
  num_harvesters <- sampleBespoke(min_harvesters:max_harvesters, n_days, replace = T)
  
  # for learning cues
  boat_days <- array(dim = n_days, data = 0)
  boat_days[fishery_dates] <- 1
  
}






