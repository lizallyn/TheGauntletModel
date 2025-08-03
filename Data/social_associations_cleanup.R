# for messing with the network association matrix from Zac
# Feb 2025
library(tidyr)

set.seed <- 5

## load associations from Zac for 735 EMB Zc over all years of monitoring
# 0s mean those individuals never overlapped (between or within years)
network <- read.csv("Data/social_network_associations_zac_EMB.csv")

# wide to long format
long_network <- network %>% 
  pivot_longer(!ID, names_to = "Buddy_ID", values_to = "Association")

# remove self-associations and make 0s non-zero
long_network <- long_network[-which(long_network$ID == long_network$Buddy_ID),]
long_network$Association[which(long_network$Association == 0)] <- 0.0000001

# check stats and viz
max(long_network$Association)
mean(long_network$Association)
hist(long_network$Association, breaks = seq(0, 0.3, 0.01))

# fit a beta dist to it
start_vals <- list(shape1 = 0.1, shape2 = 10)
fit <- MASS::fitdistr(x = long_network$Association, densfun = "beta", start = start_vals)

# simulate fake data from the fitted beta and viz it
# round so almost-zeroes become zeroes again - janky zi workaround
fake <- round(rbeta(500000, fit$estimate[1], fit$estimate[2]), digits = 3)
max(fake)
mean(fake)
hist(fake, breaks = seq(0, 1, 0.01), xlim = c(0, 0.3))
# fit isn't perfect. 
# This is a messy janky way to do this but it'll do for now
# Don't want to break out the zero inflated BS

## Create a dummy matrix to mess with learning integration

# define parameters
max.sociality <- 0.5 # m in functional response for receptivity
num_pinns <- 10 # number of pinnipeds in matrix

# create associations matrix
associations <- matrix(data = fake[1:(num_pinns*num_pinns)], nrow = num_pinns, ncol = num_pinns)
colnames(associations) <- 1:num_pinns
rownames(associations) <- 1:num_pinns
# keep <- associations

# make self-associations = 0
# and mirror across 1:1 axis
for(i in 1:num_pinns){
  associations[i,i] <- 0
  for(j in 1:(i-1)){
    associations[j,i] <- associations[i,j]
  }
}
# keep.2 <- associations

# scale so max total associations for any individual is max.sociality = 0.5
sociality <- colSums(associations)
sociality.scaled <- sociality * (max.sociality/max(sociality))
for(i in 1:num_pinns){
  for(k in 1:num_pinns){
    associations[k,i] <- associations[k,i] * (sociality.scaled[i]/max(sociality[i]))
  }
}
# check and viz
hist(sociality)
hist(associations, breaks = seq(0, 1, 0.01))

# create fake probs matrix
probs <- data.frame(matrix(data = rnorm(10, 0.5, 0.2), nrow = 10, ncol = 1))
probs <- probs # this is for continuity between runs bc set.seed isn't working

R <- sociality.scaled * (4 * (probs - (probs^2)))^3 # from Andrew desmos, pseudo-beta-shaped function

P_G <- NA
P_S <- NA
for(seal in 1:10){
  P_S[seal] <- sum(associations[,seal] * probs)
  P_G[seal] <- (1-R[seal,]) * probs[seal,] + R[seal,] * P_S[seal]
}
P_G
P_S
# seems to work!


#####-----------------------------------------------
# March 2025

# But really I'm just using the matrix as a binary connected/not
# So should change to 0 & 1 and draw from a binomial distribution
# not the pseudo-beta shenanigans

## load associations from Zac for 735 EMB Zc over all years of monitoring
# 0s mean those individuals never overlapped (between or within years)
network <- read.csv("Data/social_network_associations_zac_EMB.csv")

# wide to long format
long_network <- network %>% 
  pivot_longer(!ID, names_to = "Buddy_ID", values_to = "Association")

# remove self-associations and make all 1 and 0
long_network <- long_network[-which(long_network$ID == long_network$Buddy_ID),]
long_network$Association[which(long_network$Association > 0)] <- 1

# check stats and viz
max(long_network$Association)
mean(long_network$Association)
hist(long_network$Association)

# describe a binomial dist to it
num_connections <- length(which(long_network$Association == 1))
total_nodes <- nrow(long_network)
fake <- rbinom(1000, 1, num_connections/total_nodes)


