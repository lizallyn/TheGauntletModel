# assemble all the components to run the model
# Jan 2026

# Case study in this version is always Base with 1 salmon run
case.study <- "Base"
run_count <- 1

# Set predator consumption values (Cmax) to high (gorging) or 
# low (diet proportion) bounds
bounds <- "High Consumption"
# bounds <- "Low Consumption"

# Load Set-Up Functions
source("Functions/makeArray.R")
source("Functions/sampleBespoke.R")
source("Functions/createHarvestPlan.R")
source("Functions/createSalmonArrival.R")

# Prep Salmon Data
source("RunTheModel/Prep_Bioenergetics.R")
source("RunTheModel/Prep_Salmon_Data.R")

# Prep Pinniped Data
source("RunTheModel/Prep_Pinniped_Data.R")

## Set Harvest Regime
source("RunTheModel/Choose_Harvest_Options.R")

# Set Parameters
source("RunTheModel/set_pars.R")

# Initialize Variables
source("RunTheModel/initialize_variables.R")

# Load Loop Functions
source("RunTheModel/loadLoopFunctions.R")

# Run The Loop 
source("Functions/rungeKutta.R")
source("RunTheModel/The_Loop_pv.R")


# 08 Plots
source("Functions/Plots_Pv.R")
source("Functions/Plots_salmon.R")