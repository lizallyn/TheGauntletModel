# harvest as of Feb 2025



simpleHarvest <- function(day_plan, num_gauntlet_seals, num_fishers, max_fishers, 
                          catchability, overlap, participation, trap_level) {
  if(day_plan == 0){
    res <- 0
  }
  if(day_plan == "Boat"){
    q <- catchability * overlap
    res <- num_gauntlet_seals * (num_fishers * (1/max_fishers)) * participation * q
    res <- round(res)
  }
  if(day_plan == "Trap"){
    if(num_gauntlet_seals > trap_level) {
      trapped <- trap_level
    } else {trapped <- num_gauntlet_seals}
    res <- trapped
  }
  return(res)
}

# simpleHarvest(1, 10, 14, 25, 0.5, 0.25, 1)

