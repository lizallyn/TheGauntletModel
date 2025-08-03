# Run the model for all flexible pinnipeds and flexible salmon runs
# winter 2025


# clear environment
rm(list=ls())

rep <- 1

## Run it Manually

# Case studies in this version are always Base with 1 salmon run
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
source("CopyOfRunTheModel/Prep_Bioenergetics.R")
source("CopyOfRunTheModel/Prep_Salmon_Data.R")

# Prep Pinniped Data
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

# Include Haulout Reduction management?
# reduction factor = % reduction in total population
reduction <- 0
source("Functions/hauloutReduction.R")

# Set Parameters
source("CopyOfRunTheModel/set_pars.R")

# Initialize Variables
source("CopyOfRunTheModel/initialize_variables.R")

# Load Loop Functions
source("CopyOfRunTheModel/loadLoopFunctions.R")

# Run The Loop 
source("Functions/rungeKutta.R")
source("CopyOfRunTheModel/The_Loop_pv.R")


# 08 Plots
source("Functions/Plots_Pv.R")
source("Functions/Plots_salmon.R")


# Look at the Results
plot_seals
plot_H/plot_y/plot_probs/plot_seals + plot_layout(guides = "collect", axis_titles = "collect")
eaten_sp_plot
plot_eaten
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



