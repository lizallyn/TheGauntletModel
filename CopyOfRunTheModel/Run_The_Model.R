# Run the model for all flexible pinnipeds and flexible salmon runs
# winter 2025


# clear environment
rm(list=ls())

rep <- 1

## Run it Manually

  
# case.study <- "Base"
# run_count <- 1
# case.study <- "N1"
# chum_fishery <- FALSE
case.study <- "B1"
temps <- "Boring"
# temps <- "Hot!"

# bounds <- "Made-Up"
# bounds <- "High Consumption"
bounds <- "Low Consumption"

# 01 Set-Up Functions
source("Functions/makeArray.R")
source("Functions/sampleBespoke.R")
source("Functions/createHarvestPlan.R")
source("Functions/createSalmonArrival.R")

# 02 Prep Salmon Data
source("CopyOfRunTheModel/Prep_Bioenergetics.R")
source("CopyOfRunTheModel/Prep_Salmon_Data.R")

# 03 Prep Pinniped Data
source("CopyOfRunTheModel/Prep_Pinniped_Data.R")

# Set Harvest Regime
scenario <- "None"
# scenario <- "Trap"
# scenario <- "Boat"
# timing <- "During"
# fear <- "No Fear"
# min_harvesters <- min_fishers
# max_harvesters <- max_fishers
# num_harvesters <- rep(min_harvesters:max_harvesters, n_days, replace = T)
# participation <- 0.5
# catchability <- 0.1
# overlap_pv <- 0.5
# overlap_sl <- 0.5

# Haulout Reduction?
reduction <- 0
source("Functions/hauloutReduction.R")

# 04 Set Pars
source("CopyOfRunTheModel/set_pars.R")

# 05 Initialize Variables
source("CopyOfRunTheModel/initialize_variables_B.R")

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
  source("CopyOfRunTheModel/The_Loop_pv_zc_B_2.R")
  source("Functions/Plots_Zc.R")
} else if(no_seals == F && no_zc == T && no_ej == T){
  source("Functions/rungeKutta.R")
  source("CopyOfRunTheModel/The_Loop_pv.R")
} else {print(error_msg)}

# 08 Plots
source("Functions/Plots_Pv.R")
source("Functions/Plots_salmon.R")
# source("Functions/Plots_responses.R")




# Look at the Results
plot_seals
plot_H/plot_y/plot_probs/plot_seals + plot_layout(guides = "collect", axis_titles = "collect")
eaten_sp_plot
plot_eaten / plot_eaten_zc /plot_eaten_ej + plot_layout(guides = "collect")
salmon_eaten

arrive_plot
gauntlet_plot
plot_seals / plot_ej / plot_zc + plot_layout(axis_titles = "collect")
# plot_zc
escape_plot
eaten_sp_plot
plot_eaten / plot_eaten_zc /plot_eaten_ej + plot_layout(guides = "collect")
# fished_plot
plot_H #+ plot_H_ej + plot_H_zc
salmon_catch
salmon_escapement
salmon_eaten
# escape_plot / eaten_sp_plot / fished_plot + plot_layout(axis_titles = "collect", guides = "collect")

# check colorblind grid:
# cvd_grid(eaten_sp_plot)

gauntlet_plot / plot_probs / plot_Psoc + plot_layout(guides = "collect", axes = "collect")
gauntlet_plot / plot_probs_zc / plot_Psoc_zc + plot_layout(guides = "collect", axes = "collect")
gauntlet_plot / plot_probs_ej / plot_Psoc_ej + plot_layout(guides = "collect", axes = "collect")
plot_x
plot_x_zc
plot_x_ej
plot_Px
plot_H / plot_y
plot_Py
plot_Psoc
length(kill_list)
length(kill_list_zc)
length(kill_list_ej)

plot_H / plot_y / plot_probs / plot_seals / gauntlet_plot + plot_layout(guides = "collect", axes = "collect")
plot_H_zc / plot_y_zc / plot_probs_zc / plot_zc / gauntlet_plot + plot_layout(guides = "collect", axes = "collect")
plot_H_ej / plot_y_ej / plot_probs_ej / plot_ej / gauntlet_plot + plot_layout(guides = "collect", axes = "collect")

plot(risk_hunt_zc[5,])
plot(risk_boat_zc[5,])
plot(risk_g_zc[5,])
plot(zc_prob_gauntlet[5,])
plot(zc_forage_loc[5,])
plot(H_zc)
harvest_days_all
harvest_plan_zc

plot(salmon_consumed_pv[2,])
plot(seal_forage_loc[2,])
plot(x[2,])
plot(seal_prob_gauntlet[2,])

plot(eaten_salmon_pv)

eaten_salmon_pv_long <- melt(data = eaten_salmon_pv, id.vars = colnames(eaten_salmon_pv)[1], 
     variable.name = "Species", value.name = "Eaten")
ggplot(data = eaten_salmon_pv_long) +
  geom_line(aes(x = Day, y = Eaten, color = Species), linewidth = 1.5)



