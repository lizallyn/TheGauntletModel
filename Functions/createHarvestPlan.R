# Create some harvest_plan scenarios


createHarvestPlan <- function(scenario = c("None", "Trap", "Boat"), timing, empty.array = oneDzeroes) {
  harvest_plan <- empty.array
  if(scenario == "None") {
    harvest_plan <- harvest_plan
  } else {
    if(timing == "During"){harvest_days <- fishy_days}
    if(timing == "Before"){harvest_days <- c((fishy_days[1]-11):(fishy_days[1]+9), sealion_arrival:(sealion_arrival+10))}
    if(timing == "Fishing"){harvest_days <- fishery_dates}
    if(timing == "Trap During"){harvest_days <- which(trap_days == 1)}
    if(timing == "Weekly"){harvest_days <- which(harvest_schedule == 1)}
    if(timing == "All"){harvest_days <- 1:days}
    
    harvest_plan[harvest_days] <- scenario
  }
    
  #   if(scenario == "Boat") {
  #   harvest
  #   harvest_plan[harvest_days] <- scenario
  # } else if(scenario == "Boat") {
  #   harvest_plan[harvest_days] <- scenario
  # } else {
  #   return("Not a valid scenario option. Try Again!")
  # }
  return(harvest_plan)
}

