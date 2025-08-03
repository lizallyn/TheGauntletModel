## Set Up Parameters ----
# with no Px anymore

library(lubridate)

# loop parameters
start_loop <- data_start
end_loop <- data_end
day_range <- start_loop:end_loop
days <- length(day_range)

# consumption parameters
deltat_val <- 1/24

# x parameters
xmin <- 0
xmax <- 1
base_y <- 0

# receptivity pars
rec.height <- sqrt(0.5)
rec.width <- 15

# individual learning parameters
rho <- 0.05
learn_rate <- 0.15

# salmon parameters
natural_mort <- 0.0005








