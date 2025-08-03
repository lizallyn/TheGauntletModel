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

