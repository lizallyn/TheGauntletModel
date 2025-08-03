case.study <- "Base"
run_count <- 1
# case.study <- "N1"
# case.study <- "B1"
# temps <- "Boring"
# temps <- "Hot!"

# bounds <- "Made-Up"
# bounds <- "High Consumption"
bounds <- "Low Consumption"

# 01 Set-Up Functions
source("Functions/makeArray.R")
source("Functions/createHarvestPlan.R")
source("Functions/createSalmonArrival.R")

# 02 Prep Salmon Data
source("CopyOfRunTheModel/Prep_Bioenergetics.R")
source("CopyOfRunTheModel/Prep_Salmon_Data.R")

# 03 Prep Pinniped Data
source("CopyOfRunTheModel/Prep_Pinniped_Data.R")

# Set Harvest Regime

scenario <- "None"
# scenario <- "Boat"
# timing <- "During"
# fear <- "Fear"
# participation <- 0.5
# catchability <- 0.25
# overlap_pv <- 0.1
# overlap_sl <- 0

# Haulout Reduction?
reduction <- 0
source("Functions/hauloutReduction_N.R")

# 04 Set Pars
source("CopyOfRunTheModel/set_pars.R")

# 05 Initialize Variables
source("CopyOfRunTheModel/initialize_variables.R")

# 06 Loop Functions
source("CopyOfRunTheModel/loadLoopFunctions.R")

# 07 Run The Loop
if(no_seals == F && no_zc == F && no_ej == F){
  source("Functions/rungeKutta_3.R")
  source("CopyOfRunTheModel/The_Loop_all.R")
  source("Functions/Plots_Ej.R")
  source("Functions/Plots_Zc.R")
} else if(no_seals == F && no_zc == F && no_ej == T && case.study == "B1"){
  source("Functions/rungeKutta_2.R")
  source("CopyOfRunTheModel/The_Loop_pv_zc_B.R")
  source("Functions/Plots_Zc.R")
} else if(no_seals == F && no_zc == T && no_ej == T){
  source("Functions/rungeKutta.R")
  source("CopyOfRunTheModel/The_Loop_pv.R")
} else {print(error_msg)}

# 08 Plots
source("Functions/Plots_Pv.R")
source("Functions/Plots_salmon.R")
# source("Functions/Plots_responses.R")
