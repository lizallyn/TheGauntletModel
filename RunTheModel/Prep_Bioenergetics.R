# separate script for diet energetics stuff
# alllll the runs here, subset by model run on Prep_Salmon_Data.R

#### Parameter Cmax ####
# see Obsidian note "Parameter - Cmax"
# energy estimates from Oneill 2014
Pv_kcal <- 3550
Zc_kcal <- 22803
Ej_kcal <- 35851
pinn_kcal <- data.frame(Pv = Pv_kcal, Zc = Zc_kcal, Ej = Ej_kcal)

chinookGR_kcal <- 7304
chinookLN_kcal <- 12403
chinookB_kcal <- 8428
chinook_kcal <- 13409 # region avg
chum_kcal <- 4200
coho_kcal <- 4982 # region avg
cohoB_kcal <- 2868
sockeye_kcal <- 4264 # region avg
salmon_kcal <- data.frame(ChinookGR = chinookGR_kcal, ChinookLN = chinookLN_kcal, 
                          ChinookB = chinookB_kcal,
                          Chinook = chinook_kcal, Chum = chum_kcal, 
                          Coho = coho_kcal, CohoB = cohoB_kcal,
                          Sockeye = sockeye_kcal)
energetics <- data.frame(matrix(data = NA, nrow = length(salmon_kcal), 
                                ncol = length(pinn_kcal)))
for(i in 1:length(pinn_kcal)){
  for(j in 1:length(salmon_kcal))
    energetics[j,i] <- pinn_kcal[i]/salmon_kcal[j]
}
colnames(energetics) <- c("Pv", "Zc", "Ej")
rownames(energetics) <- c("ChinookGR", "ChinookLN", "ChinookB", "Chinook", 
                          "Chum", "Coho", "CohoB", "Sockeye")

# diet proportions from Thomas 2017
Pv_diet_props <- data.frame(ChinookGR = 0.15,
                            ChinookLN = 0.15,
                            ChinookB = 0.15,
                            Chinook = 0.15,
                            Chum = 0.52, 
                            Coho = 0.034,
                            CohoB = 0.034,
                            Sockeye = 0.24)
# diet proportions from Scordino 2022
Ej_diet_props <- data.frame(ChinookGR = 0.068,
                            ChinookLN = 0.068,
                            ChinookB = 0.068,
                            Chinook = 0.068,
                            Chum = 0.205, 
                            Coho = 0.474,
                            CohoB = 0.474,
                            Sockeye = 0.01)
# diet proportions from Scordino 2022
Zc_diet_props <- data.frame(ChinookGR = 0.057,
                            ChinookLN = 0.057,
                            ChinookB = 0.057,
                            Chinook = 0.057,
                            Chum = 0.229, 
                            Coho = 0.543,
                            CohoB = 0.543,
                            Sockeye = 0.057)

# low case consumption Cmax estimates (via scat samples)
low_energetics <- energetics[,"Pv"] * Pv_diet_props
low_energetics[2,] <- energetics[, "Zc"] * Zc_diet_props
low_energetics[3,] <- energetics[, "Ej"] * Ej_diet_props
rownames(low_energetics) <- c("Pv", "Zc", "Ej")
low_energetics <- ceiling(data.frame(low_energetics))

# high case consumption Cmax estimates (high-grading, belly-biting)
high_energetics <- data.frame(matrix(data = NA, nrow = length(pinn_kcal), 
                                     ncol = length(salmon_kcal)))
base_high_grade_factor <- 7
high_grade_factor <- 3
high_energetics[1,] <- energetics[,"Pv"] * high_grade_factor
high_energetics[2,] <- energetics[, "Zc"] * high_grade_factor
high_energetics[3,] <- energetics[, "Ej"] * high_grade_factor
rownames(high_energetics) <- c("Pv", "Zc", "Ej")
colnames(high_energetics) <- c("ChinookGR", "ChinookLN", "ChinookB", "Chinook", 
                               "Chum", "Coho", "CohoB", "Sockeye")
high_energetics$Base <- as.numeric(energetics["Sockeye",] * base_high_grade_factor)
# high_energetics <- t(high_energetics)
high_energetics <- ceiling(high_energetics) # need to kill whole fish

#### Parameter w ####

# see Obsidian note Parameter-w
# from Olesiuk 1990 and scordino 2022 mostly
# have to eat more salmon than expected from an open water diet
# these are based on avg diet proportions

# in kcal:
if(case.study == "Base"){
  w <- data.frame(Pv = Pv_kcal * 0.10)
} else {
  w <- data.frame(Pv = Pv_kcal * 0.103, 
                        Zc = Zc_kcal * 0.131, 
                        Ej = Ej_kcal * 0.116)
}


#### Parameter alpha ####

# see Obsidian note Parameter-alpha

alpha_csv <- read.csv("Data/alpha_vals.csv", row.names = 1)
alpha_all <- rbind(alpha_csv["Chinook",],
                   alpha_csv["Chinook",],
                   alpha_csv["Chinook",],
                   alpha_csv["Chinook",],
                   alpha_csv["Chum",],
                   alpha_csv["Coho",],
                   alpha_csv["Coho",],
                   alpha_csv["Sockeye",],
                   alpha_csv["Base",])
rownames(alpha_all) <- c("ChinookGR", "ChinookLN", "ChinookB", "Chinook", 
                               "Chum", "Coho", "CohoB", "Sockeye", "Base")
alpha_all["ChinookLN",] <- alpha_all["ChinookGR",]/1.5


