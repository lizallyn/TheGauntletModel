# This script is for setting Harvest options

## No pinniped removals:
scenario <- "None"

## Removals on a weekly schedule, days/week
# scenario <- "Boat"
# timing = "Weekly"
# days_per_week <- 7
# harvest_schedule <- rep(c(rep(0, 7-days_per_week), rep(1, days_per_week)), n_days/7)
# fear <- "Fear"
# participation <- 1
# catchability <- 0.1
# overlap_pv <- 0.5

## Removals on a schedule with set gap days between single removal days
# scenario <- "Boat"
# timing = "Weekly"
# days_gap <- range_gap[rep]
# harvest_schedule <- rep_len(c(rep(0, days_gap), 1), n_days)
# fear <- "Fear"
# participation <- 1
# catchability <- 0.1
# overlap_pv <- 0.5

## Removals during entire salmon presence
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

## Include Haulout Reduction management?
# reduction factor = % reduction in total population
reduction <- 0
source("Functions/hauloutReduction.R")
