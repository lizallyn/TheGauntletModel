# Script to create salmon arrival data
library(tidyr)
library(dplyr)
library(anytime)
library(lubridate)

#### Arrival Data ----

# Case Base
if(case.study == "Base"){
  run_info <- read.csv("Data/salmon_run_info.csv")
  run_info <- run_info[1:run_count,]
  run_group <- run_info[,"CopyCat"]
  
  run_info$Escape <- 1/run_info$Residence
  sd.end <- max(run_info$Residence) * 4 #makes sure dataframe is big enough for full run arrive and leave
  
  # create arrival data frame
  salmon_arrival <- create_salmon_arrival(5, sd.end, run_info) # width of days range
  colnames(salmon_arrival) <- c("Day", run_info$Run)
  n_days <- nrow(salmon_arrival)
  data_start <- salmon_arrival$Day[1]
  data_end <- max(salmon_arrival$Day)
  
  # bioenergetics parameters
  run_kcal <- salmon_kcal[run_group]
  colnames(run_kcal) <- run_info$Run
  low_mat <- matrix(ncol = 3, nrow = 1, data = low_energetics[,run_group])
  colnames(low_mat) <- c("Pv", "Zc", "Ej")
  rownames(low_mat) <- run_info$Run
  high_mat <- matrix(ncol = 3, nrow = 1, data = high_energetics[,"Base"])
  colnames(high_mat) <- c("Pv", "Zc", "Ej")
  rownames(high_mat) <- "Base"
  alpha_mat <- alpha_all["Base",]
  
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
  
  min_fishers <- 15
  max_fishers <- 15
  num_harvesters <- sampleBespoke(x = min_fishers:max_fishers, size = n_days, replace = TRUE)
  max_harvesters <- max(num_harvesters)
  
}



