# Create some harvest_plan scenarios


createHarvestPlan <- function(scenario = c("None", "Trap", "Boat"), timing, empty.array = oneDzeroes) {
  harvest_plan <- empty.array
  if(scenario == "None") {
    harvest_plan <- harvest_plan
  } else {
    if(timing == "During"){harvest_days <- fishy_days}
    if(timing == "Fishing"){harvest_days <- fishery_dates}
    if(timing == "Weekly"){harvest_days <- which(harvest_schedule == 1)}
    if(timing == "All"){harvest_days <- 1:days}
    
    harvest_plan[harvest_days] <- scenario
  }
    
  return(harvest_plan)
}

