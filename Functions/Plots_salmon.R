# salmon plots
library(viridis)

salmon_escapement <- colSums(round(escape_salmon[2:ncol(escape_salmon)]))
salmon_catch <- colSums(round(fished_salmon[2:ncol(fished_salmon)]))
salmon_eaten <- colSums(round(eaten_salmon[2:ncol(eaten_salmon)]))

plot_consumed <- makePlot_2(x = 1:days + (start_loop - 1), x.name = "Day", y = consumed_total, y.name = "Daily Salmon Consumed", 
                            color = "dodgerblue")

# Plots of Salmon Species data


arrive_plot <- makePlot_4(data = salmon_arrival, variable.name = "Run", 
                          value.name = "Daily Salmon Arrived", colors = colors_salmon,
                          loop_days = F, start_loop = start_loop, line = T, legend.inc = T)
escape_plot <- makePlot_4(data = round(escape_salmon), variable.name = "Run", 
                          value.name = "Daily Salmon Escaped", colors = colors_salmon,
                          loop_days = F, start_loop = start_loop)
eaten_sp_plot <- makePlot_4(data = round(eaten_salmon), variable.name = "Run", 
                          value.name = "Daily Salmon Eaten", colors = colors_salmon,
                          loop_days = F, start_loop = start_loop)
gauntlet_plot <- makePlot_4(data = round(salmon_list), variable.name = "Run", 
                            value.name = "Daily Salmon at Gauntlet", colors = colors_salmon,
                            loop_days = T, start_loop = start_loop)
fished_plot <- makePlot_4(data = round(fished_salmon), variable.name = "Run", 
                          value.name = "Daily Salmon Fished", colors = colors_salmon,
                          loop_days = F, start_loop = start_loop)

# LocNis_arrival_plot <- plot(salmon_arrival$Day, salmon_arrival$ChinookLN, type = "l")
# LocNis_gauntlet <- data.frame(Day = salmon_list$Day, ChinookLN = salmon_list$ChinookLN)
# LocNis_gauntlet_plot <- plot(LocNis_gauntlet$Day, LocNis_gauntlet$ChinookLN)
# eaten_sp_plot <- makePlot_3(x = 1:days + (start_loop - 1), data = cbind(eaten_chinook, eaten_sockeye, eaten_coho),
#                             col.names = c("Day", "Chinook", "Sockeye", "Coho"), variable.name = "Species", 
#                             value.name = "Daily Salmon Eaten", colors = colors_salmon)
# gauntlet_plot <- makePlot_3(x = 1:days + (start_loop - 1), data = cbind(gauntlet_chinook, gauntlet_sockeye, gauntlet_coho),
#                             col.names = c("Day", "Chinook", "Sockeye", "Coho"), variable.name = "Species", 
#                             value.name = "Daily Salmon at Gauntlet", colors = colors_salmon)
# fished_plot <- makePlot_3(x = 1:days + (start_loop - 1), data = cbind(fished_chinook, fished_sockeye, fished_coho), 
#                           col.names = c("Day", "Chinook", "Sockeye", "Coho"), variable.name = "Species",
#                           value.name = "Daily Salmon Fished", colors = colors_salmon)

# Ch1_arrival_plot <- ggplot(data = salmon_arrival) +
#   geom_line(aes(x = Day - 49, y = Run1), linewidth = 2) +
#   theme_classic(base_size = 14) +
#   labs(x = "Day", y = "Arriving Salmon Abundance")
# ggsave(filename = "Ch1_salmon_arrival_plot",
#        plot = Ch1_arrival_plot,
#        device = "png", width = 10, height = 5, units = "in",
#        path = "ParameterManipulations/Plots")


colors_N <- c("Hatchery \nChinook" = "skyblue2", "Wild \nChinook" = "darkslategray3", "Chum" = "lightsalmon3")
Nisqually_arrival_plot <- ggplot(data = salmon_arrival) +
  geom_line(aes(x = Day - 49, y = ChinookGR, color = "Hatchery \nChinook"), linewidth = 2) +
  geom_line(aes(x = Day - 49, y = ChinookLN, color = "Wild \nChinook"), linewidth = 2) +
  geom_line(aes(x = Day - 49, y = Chum, color = "Chum"), linewidth = 2) +
  theme_classic(base_size = 12) +
  labs(x = "Day", y = "Daily Arriving Salmon", color = "Salmon Run") + 
  scale_color_manual(values = colors_N) + 
  theme(legend.key.spacing.y = unit(12, "pt"))
Nisqually_arrival_plot
# ggsave(filename = "Nisqually_arrival_plot.png",
#        plot = Nisqually_arrival_plot,
#        device = "png", width = 8, height = 3, units = "in",
#        path = "NisquallyManipulations/Plots")

colors_B <- c("Sockeye" = "firebrick3", "Chinook" = "darkslategray3", "Coho" = "darkolivegreen4")
Ballard_arrival_plot <- ggplot(data = salmon_arrival) +
  geom_line(aes(x = Day - 49, y = Sockeye,color = "Sockeye"), linewidth = 2) +
  geom_line(aes(x = Day - 49, y = ChinookB, color = "Chinook"), linewidth = 2) +
  geom_line(aes(x = Day - 49, y = CohoB, color = "Coho"), linewidth = 2) +
  xlim(c(90, 275)) +
  theme_classic(base_size = 12) +
  labs(x = "Day", y = "Daily Arriving Salmon", color = "Salmon Run") + 
  scale_color_manual(values = colors_B)
Ballard_arrival_plot
# ggsave(filename = "Ballard_arrival_plot.png",
#        plot = Ballard_arrival_plot,
#        device = "png", width = 8, height = 3, units = "in",
#        path = "BallardManipulations/Plots")


