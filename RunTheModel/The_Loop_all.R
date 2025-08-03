# Runs the daily loop
# for 3 species
# January 2025

for(t in 1:(days - 1)) {
  
  # salmon arrive at the gauntlet
  daily_salmon_list <- salmon_list[t,]
  daily_update <- salmonSpeciesUpdate(day = t, 
                                      salmon = daily_salmon_list, 
                                      arrive_data = salmon_arrival)
  salmon_list[t,] <- daily_update
  
  # decide where each seal goes that day
  seal_forage_loc[,t] <- sapply(X = seal_prob_gauntlet[,t], FUN = decideForagingDestination)
  
  # round of copying for seals
  receptivity_y[,t] <- sapply(X = y[,t], FUN = receptivityX, pars = rec_y_pars, baseline = base_y)
  receptivity_x[,t] <- sapply(X = x[,t], FUN = receptivityX, pars = rec_x_pars, 
                              baseline = base_x)
  receptivity[,t] <- receptivity_x[,t] * receptivity_y[,t]
  for(seal in 1:num_seals){
    social_info <- socialInfo(network_pv[,seal], receptivity = receptivity[seal,t], 
                              probs = seal_prob_gauntlet[,t], self = seal)
    P_social[seal,t] <- (1-receptivity[seal,t]) * seal_prob_gauntlet[seal,t] + 
      receptivity[seal,t] * social_info
    seal_forage_loc[,t] <- sapply(X = P_social[,t], FUN = decideForagingDestination)
  }
  
  # sea lions only make decisions if they have arrived
  if(t >= sealion_arrival) {
    
    ## Californias
    # initial decision
    zc_forage_loc[,t] <- sapply(X = zc_prob_gauntlet[,t], FUN = decideForagingDestination)
    
    receptivity_y_zc[,t] <- sapply(X = y_zc[,t], FUN = receptivityX, pars = rec_y_pars, baseline = base_y)
    receptivity_x_zc[,t] <- sapply(X = x_zc[,t], FUN = receptivityX, pars = rec_x_pars,
                                   baseline = base_x_sl)
    receptivity_zc[,t] <- receptivity_x_zc[,t] * receptivity_y_zc[,t]
    for(csl in 1:num_zc){
      social_info_zc <- socialInfo(network = network_zc[,csl], receptivity = receptivity_zc[csl,t], 
                                   probs = zc_prob_gauntlet[,t], self = csl)
      P_social_zc[csl,t] <- (1-receptivity_zc[csl,t]) * zc_prob_gauntlet[csl,t] + 
        receptivity_zc[csl,t] * social_info_zc
    }
    zc_forage_loc[,t] <- sapply(X = P_social_zc[,t], FUN = decideForagingDestination)
    
    ## Stellers
    # initial decision
    ej_forage_loc[,t] <- sapply(X = ej_prob_gauntlet[,t], FUN = decideForagingDestination)
    
    # copying
    receptivity_y_ej[,t] <- sapply(X = y_ej[,t], FUN = receptivityX, pars = rec_y_pars, baseline = base_y)
    receptivity_x_ej[,t] <- sapply(X = x_ej[,t], FUN = receptivityX, pars = rec_x_pars,
                                   baseline = base_x_sl)
    receptivity_ej[,t] <- receptivity_x_ej[,t] * receptivity_y_ej[,t]
    for(ssl in 1:num_ej){
      social_info_ej <- socialInfo(network = network_ej[,ssl], receptivity = receptivity_ej[ssl,t], 
                                   probs = ej_prob_gauntlet[,t], self = ssl)
      P_social_ej[ssl,t] <- (1-receptivity_ej[ssl,t]) * ej_prob_gauntlet[ssl,t] + 
        receptivity_ej[ssl,t] * social_info_ej
    }
    ej_forage_loc[,t] <- sapply(X = P_social_ej[,t], FUN = decideForagingDestination)
    
  } else {
    zc_forage_loc[,t] <- 0
    P_social_zc[,t] <- NA
    
    ej_forage_loc[,t] <- 0
    P_social_ej[,t] <- NA
  }
  
  
  # calculate salmon mortality 
  seals_at_gauntlet <- which(seal_forage_loc[,t] == 1)
  zc_at_gauntlet <- which(zc_forage_loc[,t] == 1)
  ej_at_gauntlet <- which(ej_forage_loc[,t] == 1)
  
  seals_at_gauntlet_save[[t]] <- seals_at_gauntlet
  zc_at_gauntlet_save[[t]] <- zc_at_gauntlet
  ej_at_gauntlet_save[[t]] <- ej_at_gauntlet
  
  num_seals_at_gauntlet <- length(seals_at_gauntlet)
  num_zc_at_gauntlet <- length(zc_at_gauntlet)
  num_ej_at_gauntlet <- length(ej_at_gauntlet)
  
  salmon_result <- run_rungeKutta(salmon = daily_update, 
                                  Cmax = Cmax_mat["Pv",], 
                                  Nseal = num_seals_at_gauntlet, 
                                  alpha = alpha_mat["Pv",], 
                                  gamma = gamma, 
                                  Y = Y,
                                  NCSL = num_zc_at_gauntlet, 
                                  Cmax_CSL = Cmax_mat["Zc",], 
                                  alpha_CSL = alpha_mat["Zc",], 
                                  gamma_CSL = gamma, 
                                  Y_CSL = Y,
                                  NSSL = num_ej_at_gauntlet, 
                                  Cmax_SSL = Cmax_mat["Ej",], 
                                  alpha_SSL = alpha_mat["Ej",], 
                                  gamma_SSL = gamma, 
                                  Y_SSL = Y,
                                  F_catch = as.numeric(salmon_catch_rates[t, 2:ncol(salmon_catch_rates)]), 
                                  M = natural_mort, 
                                  E = run_info$Escape, 
                                  deltat = deltat_val)
  
  # assign escape and gauntlet updates
  salmon_list[t+1, 2:ncol(salmon_list)] <- salmon_result[, "Ns"]
  escape_salmon[t+1, 2:ncol(escape_salmon)] <- salmon_result[, "E"]
  fished_salmon[t, 2:ncol(escape_salmon)] <- salmon_result[, "Catch"]
  eaten_salmon[t, 2:ncol(escape_salmon)] <- salmon_result[, "C"] +
    salmon_result[, "C_CSL"] + salmon_result[, "C_SSL"]
  consumed_total[t] <- sum(eaten_salmon[t, 2:ncol(escape_salmon)])
  
  # assign consumed salmon to gauntlet pinnipeds
  
  consumed_by_pv <- sum(salmon_result[,"C"])
  consumed_by_zc <- sum(salmon_result[,"C_CSL"])
  consumed_by_ej <- sum(salmon_result[,"C_SSL"])
  
  #pv
  if(num_seals_at_gauntlet == 0 | consumed_by_pv == 0) {
    salmon_consumed_pv[,t] <- 0
  } else {
    if(num_seals_at_gauntlet > consumed_by_pv){
      seals_that_eat <- sampleBespoke(seals_at_gauntlet, consumed_by_pv)
      salmon_consumed_pv[seals_that_eat, t] <- 1
    } else {
      salmon_consumed_pv[seals_at_gauntlet, t] <- floor(consumed_by_pv/num_seals_at_gauntlet)
      remainder <- consumed_by_pv %% num_seals_at_gauntlet
      seals_that_eat_extra <- sampleBespoke(seals_at_gauntlet, remainder)
      salmon_consumed_pv[seals_that_eat_extra, t] <- salmon_consumed_pv[seals_that_eat_extra, t] + 1
    }
  }
  
  if(num_zc_at_gauntlet == 0 | consumed_by_zc == 0){
    salmon_consumed_zc[,t] <- 0
  } else {
    if(num_zc_at_gauntlet > consumed_by_zc){
      zc_that_eat <- sampleBespoke(zc_at_gauntlet, consumed_by_zc)
      salmon_consumed_zc[zc_at_gauntlet,t] <- 0
      salmon_consumed_zc[zc_that_eat,t] <- 1
    } else {
      salmon_consumed_zc[zc_at_gauntlet, t] <- floor(consumed_by_zc/num_zc_at_gauntlet)
      remainder_zc <- consumed_by_zc %% num_zc_at_gauntlet
      zc_that_eat_extra <- sampleBespoke(zc_at_gauntlet, remainder_zc)
      salmon_consumed_zc[zc_that_eat_extra, t] <- salmon_consumed_zc[zc_that_eat_extra, t] + 1
    }
  }
  
  if(num_ej_at_gauntlet == 0 | consumed_by_ej == 0){
    salmon_consumed_ej[,t] <- 0
  } else {
    if(num_ej_at_gauntlet > consumed_by_ej){
      ej_that_eat <- sampleBespoke(ej_at_gauntlet, consumed_by_ej)
      salmon_consumed_ej[ej_at_gauntlet,t] <- 0
      salmon_consumed_ej[ej_that_eat,t] <- 1
    } else {
      salmon_consumed_ej[ej_at_gauntlet, t] <- floor(consumed_by_ej/num_ej_at_gauntlet)
      remainder_ej <- consumed_by_ej %% num_ej_at_gauntlet
      ej_that_eat_extra <- sampleBespoke(ej_at_gauntlet, remainder_ej)
      salmon_consumed_ej[ej_that_eat_extra,t] <- salmon_consumed_ej[ej_that_eat_extra,t] + 1
    }
  }
  
  # seal harvest
  H[t] <- simpleHarvest(day_plan = harvest_plan_pv[t], 
                        num_gauntlet_seals = num_seals_at_gauntlet, 
                        num_fishers = num_harvesters[t], 
                        max_fishers = max_harvesters, 
                        catchability = catchability, 
                        overlap = overlap_pv, 
                        participation = participation)
  H_zc[t] <- simpleHarvest(day_plan = harvest_plan_zc[t], 
                          num_gauntlet_seals = num_zc_at_gauntlet, 
                          num_fishers = num_harvesters[t], 
                          max_fishers = max_harvesters, 
                          catchability = catchability, 
                          overlap = overlap_sl, 
                          participation = participation)
  H_ej[t] <- simpleHarvest(day_plan = harvest_plan_ej[t], 
                          num_gauntlet_seals = num_ej_at_gauntlet, 
                          num_fishers = num_harvesters[t], 
                          max_fishers = max_harvesters, 
                          catchability = catchability, 
                          overlap = overlap_sl, 
                          participation = participation)
  
  # seals only develop fear if fear methods used & they're exposed
  hunt_pv <- rep(0, num_seals)
  hunt_zc <- rep(0, num_zc)
  hunt_ej <- rep(0, num_ej)
  
  if(t %in% harvest_days_all && fear == "Fear"){
    num_seals_exposed_fear <- ceiling(num_seals_at_gauntlet * overlap_pv)
    num_zc_exposed_fear <- ceiling(num_zc_at_gauntlet * overlap_sl)
    num_ej_exposed_fear <- ceiling(num_ej_at_gauntlet * overlap_sl)
    if(num_seals_exposed_fear > 0){
      seals_exposed_fear <- sampleBespoke(size = num_seals_exposed_fear, 
                                   x = seals_at_gauntlet)
      hunt_pv[seals_exposed_fear] <- 1
    }
    if(num_zc_exposed_fear > 0){
      zc_exposed_fear <- sampleBespoke(size = num_zc_exposed_fear, 
                                x = zc_at_gauntlet)
      hunt_zc[zc_exposed_fear] <- 1
    }
    if(num_ej_exposed_fear > 0){
      ej_exposed_fear <- sampleBespoke(size = num_ej_exposed_fear, 
                                   x = ej_at_gauntlet)
      hunt_ej[ej_exposed_fear] <- 1
    }
  }
  # calculate harvested pinnipeds
  if(H[t] > 0){
    killed <- sampleBespoke(seals_at_gauntlet, H[t])
    kill_list <- c(kill_list, killed)
  }
  if(H_zc[t] > 0){
    killed <- sampleBespoke(zc_at_gauntlet, H_zc[t])
    kill_list_zc <- c(kill_list_zc, killed)
  }
  if(H_ej[t] > 0){
    killed <- sampleBespoke(ej_at_gauntlet, H_ej[t])
    kill_list_ej <- c(kill_list_ej, killed)
  }
  
  ## calculate x, y and prob_gauntlet for next time step
  
  # calculate salmon consumed as kcal
  if(consumed_by_pv > 0){
    props_pv_consumed<- salmon_result[,"C"]/consumed_by_pv
    pv_kcal <- sum(run_kcal * props_pv_consumed) * salmon_consumed_pv[,t]
  } else {pv_kcal <- rep(0, num_seals)}
  if(consumed_by_zc > 0){
    props_zc_consumed <- salmon_result[,"C_CSL"]/consumed_by_zc
    zc_kcal <- sum(run_kcal * props_zc_consumed) * salmon_consumed_zc[,t]
  } else {zc_kcal <- rep(0, num_zc)}
  if(consumed_by_ej > 0){
    props_ej_consumed <- salmon_result[,"C_SSL"]/consumed_by_ej
    ej_kcal <- sum(run_kcal * props_ej_consumed) * salmon_consumed_ej[,t]
  } else {ej_kcal <- rep(0, num_ej)}
  
  # seals
  
  for(seal in 1:num_seals){

    if(seal %in% kill_list){
      seal_prob_gauntlet[seal, t+1] <- NA
      seal_forage_loc[seal, t+1] <- NA
      x[seal, t+1] <- NA
      y[seal, t+1] <- NA
      C[seal, t] <- NA
      P_x[seal, t+1] <- NA
      P_y[seal, t+1] <- NA
    } else {
      update_output <- updateLearning(salmon_consumed = pv_kcal[seal], 
                                      boats = boat_days[t], 
                                      rho = rho, 
                                      learn_rate = learn_rate,
                                      w = w["Pv"], 
                                      hunting = hunt_pv[seal],
                                      x_t = x[seal, t], 
                                      x_pars = x_pars,
                                      step = step[seal], 
                                      decay = decay,
                                      forage_loc = seal_forage_loc[seal, t], 
                                      dead = seal %in% kill_list,
                                      baseline_x = base_x[seal], 
                                      w1 = risk_boat_pv[seal, t],
                                      w2 = risk_hunt_pv[seal, t], 
                                      w3 = risk_g_pv[seal, t],
                                      specialist = seal %in% specialist_seals, 
                                      bundle_x = bundle_x, 
                                      bundle_x_spec = bundle_x_spec, 
                                      bundle_y = bundle_y,
                                      bundle_y_spec = bundle_y_spec)
      
      x[seal, t+1] <- as.numeric(update_output["x_t1"])
      y[seal, t+1] <- as.numeric(update_output["y_t1"])
      P_x[seal, t+1] <- as.numeric(update_output["P_x"])
      P_y[seal, t+1] <- as.numeric(update_output["P_y"])
      risk_boat_pv[seal, t+1] <- as.numeric(update_output["w1"])
      risk_hunt_pv[seal, t+1] <- as.numeric(update_output["w2"])
      risk_g_pv[seal, t+1] <- as.numeric(update_output["w3"])
      seal_prob_gauntlet[seal, t+1] <- P_x[seal, t+1] * P_y[seal, t+1]
    }

  }
  
  # californias
  
  for(csl in 1:num_zc){
    
    if(csl %in% kill_list_zc){
      zc_prob_gauntlet[csl, t+1] <- NA
      zc_forage_loc[csl, t+1] <- NA
      x_zc[csl, t+1] <- NA
      y_zc[csl, t+1] <- NA
      C_zc[csl, t] <- NA
      P_x_zc[csl, t+1] <- NA
      P_y_zc[csl, t+1] <- NA
    } else {
      update_output <- updateLearning(salmon_consumed = zc_kcal[csl], 
                                      w = w["Zc"], 
                                      hunting = hunt_zc[csl], 
                                      boats = boat_days[t],
                                      x_t = x_zc[csl, t], 
                                      rho = rho, 
                                      step = step_spec, 
                                      decay = decay, 
                                      learn_rate = learn_rate,
                                      forage_loc = zc_forage_loc[csl, t], 
                                      x_pars = x_pars,
                                      dead = csl %in% kill_list_zc,
                                      w1 = risk_boat_zc[csl, t],
                                      w2 = risk_hunt_zc[csl, t], 
                                      w3 = risk_g_zc[csl, t],
                                      baseline_x = base_x_sl,
                                      specialist = csl %in% specialist_zc, 
                                      bundle_x = bundle_x, 
                                      bundle_x_spec = bundle_x_sl, 
                                      bundle_y = bundle_y,
                                      bundle_y_spec = bundle_y_sl)
      
      x_zc[csl, t+1] <- as.numeric(update_output["x_t1"])
      y_zc[csl, t+1] <- as.numeric(update_output["y_t1"])
      P_x_zc[csl, t+1] <- as.numeric(update_output["P_x"])
      P_y_zc[csl, t+1] <- as.numeric(update_output["P_y"])
      risk_boat_zc[csl, t+1] <- as.numeric(update_output["w1"])
      risk_hunt_zc[csl, t+1] <- as.numeric(update_output["w2"])
      risk_g_zc[csl, t+1] <- as.numeric(update_output["w3"])
      zc_prob_gauntlet[csl, t+1] <- P_x_zc[csl, t+1] * P_y_zc[csl, t+1]
    }
    
  }
  
  # stellers
  
  for(ssl in 1:num_ej){
    
    if(ssl %in% kill_list_ej){
      ej_prob_gauntlet[ssl, t+1] <- NA
      ej_forage_loc[ssl, t+1] <- NA
      x_ej[ssl, t+1] <- NA
      y_ej[ssl, t+1] <- NA
      C_ej[ssl, t] <- NA
      P_x_ej[ssl, t+1] <- NA
      P_y_ej[ssl, t+1] <- NA
    } else {
      update_output <- updateLearning(salmon_consumed = ej_kcal[ssl], 
                                      w = w["Ej"], 
                                      hunting = hunt_ej[ssl], 
                                      boats = boat_days[t],
                                      x_t = x_ej[ssl, t], 
                                      rho = rho, 
                                      step = step_spec, 
                                      decay = decay, 
                                      learn_rate = learn_rate,
                                      forage_loc = ej_forage_loc[ssl, t], 
                                      x_pars = x_pars,
                                      dead = ssl %in% kill_list_ej,
                                      w1 = risk_boat_ej[ssl, t],
                                      w2 = risk_hunt_ej[ssl, t], 
                                      w3 = risk_g_ej[ssl, t],
                                      baseline_x = base_x_sl,
                                      specialist = ssl %in% specialist_ej, 
                                      bundle_x = bundle_x, 
                                      bundle_x_spec = bundle_x_sl, 
                                      bundle_y = bundle_y,
                                      bundle_y_spec = bundle_y_sl)
      
      x_ej[ssl, t+1] <- as.numeric(update_output["x_t1"])
      y_ej[ssl, t+1] <- as.numeric(update_output["y_t1"])
      P_x_ej[ssl, t+1] <- as.numeric(update_output["P_x"])
      P_y_ej[ssl, t+1] <- as.numeric(update_output["P_y"])
      risk_boat_ej[ssl, t+1] <- as.numeric(update_output["w1"])
      risk_hunt_ej[ssl, t+1] <- as.numeric(update_output["w2"])
      risk_g_ej[ssl, t+1] <- as.numeric(update_output["w3"])
      ej_prob_gauntlet[ssl, t+1] <- P_x_ej[ssl, t+1] * P_y_ej[ssl, t+1]
    }

  }
  
  if((t+1) >= high_flow_t){
    alpha_mat <- high_flow_alphas
  }
  
  print(c(rep, t))
  
} # days loop


