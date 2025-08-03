### For creating, storing, editing data on pinniped species
# Feb 2025

if(case.study == "Base"){
  # Pinniped input
  num_seals <- 100
  num_zc <- 0
  num_ej <- 0
  pinnipeds <- data.frame(Pv = num_seals, Zc = num_zc, Ej = num_ej)
  
  num_pinn_sp <- length(which(pinnipeds > 0))
  list_of_pinns <- colnames(pinnipeds[which(pinnipeds > 0)])
  
  # num_haulouts <- 2 # for Pv
  
  ## Specialist Behavior Prevalence
  prop_specialists <- 0.3
  base_x <- 0.01
  base_x_spec <- 0.1
  base_x_sl <- 0.25
  step <- 0.15
  step_spec <- 0.3
  decay <- 0.1
  
  ## Seasonality
  sealion_arrival <- 1

}

if(case.study == "N1"){
  # Pinniped input
  num_seals <- 200
  num_zc <- 50
  num_ej <- 15
  pinnipeds <- data.frame(Pv = num_seals, Zc = num_zc, Ej = num_ej)
  
  num_pinn_sp <- length(which(pinnipeds > 0))
  list_of_pinns <- colnames(pinnipeds[which(pinnipeds > 0)])
  
  ## Specialist Behavior Prevalence
  prop_specialists <- 0.1
  num_specialists <- round(num_seals * prop_specialists)
  if(num_specialists == 0){
    specialist_seals <- NA
  } else {
    specialist_seals <- sampleBespoke(1:num_seals, num_specialists)
  }
  
  base_x <- 0.001
  base_x_spec <- 0.1
  base_x_sl <- 0.1
  step <- 0.05
  step_spec <- 0.3
  decay <- 0.1
  
  ## Seasonality
  sealion_arrival <- yday("2024-12-01") - data_start
  sealion_days <- sealion_arrival:data_end
  
}

if(case.study == "B1"){
  # Pinniped input
  num_seals <- 100
  num_zc <- 20
  num_ej <- 0
  pinnipeds <- data.frame(Pv = num_seals, Zc = num_zc, Ej = num_ej)
  
  num_pinn_sp <- length(which(pinnipeds > 0))
  list_of_pinns <- colnames(pinnipeds[which(pinnipeds > 0)])
  
  ## Specialist Behavior Prevalence
  prop_specialists <- 0.2
  num_specialists <- round(num_seals * prop_specialists)
  if(num_specialists == 0){
    specialist_seals <- NA
  } else {
    specialist_seals <- sampleBespoke(1:num_seals, num_specialists)
  }
  
  base_x <- 0.001
  base_x_spec <- 0.1
  base_x_sl <- 0.1
  step <- 0.05
  step_spec <- 0.3
  decay <- 0.1
  
  ## Seasonality
  sealion_leave <- yday(as.Date("2024-06-10")) - data_start
  sealion_arrival <- yday(as.Date("2024-08-25")) - data_start
  sealion_days <- c(1:sealion_leave, sealion_arrival:n_days)
  
  ## fall Zc return
  gone_forever <- NA
  num_fall_zc <- 5
  fall_zc <- sampleBespoke(1:num_zc, num_fall_zc)

}


## consumption parameters

if(bounds == "High Consumption"){
  Cmax_mat <- ceiling(high_mat) # whole fish have to die
}
if(bounds == "Low Consumption"){
  Cmax_mat <- ceiling(low_mat) # whole fish have to die
}

gamma <- -1 # pred dep, this expects something between -1, 0
Y <- 0 # always 0 for this.

error_msg <- "Error in pinniped accounting! They cannot count and neither can you!"

## Social Associations

# See "social_associations_cleanup.R" for more details
# pulled from association matrix from Zac monitoring Zc at EMB
sim_network <- read.csv("Data/simulated_association_network_1.csv")
network_pv <- matrix(data = sim_network[1:(num_seals * num_seals), "vals"], 
                     nrow = num_seals, ncol = num_seals)
colnames(network_pv) <- 1:num_seals
rownames(network_pv) <- 1:num_seals
# make self-associations = 0
# and mirror across 1:1 axis
for(i in 1:num_seals){
  network_pv[i,i] <- 0
  for(j in 1:(i-1)){
    network_pv[j,i] <- network_pv[i,j]
  }
}

if(num_zc > 0){
  # create associations matrix for Zc
  sim_network_2 <- read.csv("Data/simulated_association_network_2.csv")
  network_zc <- matrix(data = sim_network_2[1:(num_zc * num_zc), "vals"], 
                       nrow = num_zc, ncol = num_zc)
  colnames(network_zc) <- 1:num_zc
  rownames(network_zc) <- 1:num_zc
  # make self-associations = 0
  # and mirror across 1:1 axis
  for(i in 1:num_zc){
    network_zc[i,i] <- 0
    for(j in 1:(i-1)){
      network_zc[j,i] <- network_zc[i,j]
    }
  }
}


if(num_ej > 0){
  # create associations matrix for Ej
  sim_network_3 <- read.csv("Data/simulated_association_network_3.csv")
  network_ej <- matrix(data = sim_network_3[1:(num_ej * num_ej), "vals"], 
                       nrow = num_ej, ncol = num_ej)
  colnames(network_ej) <- 1:num_ej
  rownames(network_ej) <- 1:num_ej
  # make self-associations = 0
  # and mirror across 1:1 axis
  for(i in 1:num_ej){
    network_ej[i,i] <- 0
    for(j in 1:(i-1)){
      network_ej[j,i] <- network_ej[i,j]
    }
  }
}

