
# Select exposures to run. An integer vector with values 1-12.
# 1 - bioldust, 2 - minedust, 3 - gasfumes, 4 - vgdf, 
# 5 - all (pesticides), 6 - herbi, 7 - insec, 8 - fungi, 
# 9 - aromatic, 10 - chlorinated, 11 - other, 12 - metals

exposures_to_run <- c(1:3, 5:12)


# Dataset to run on. "ecrhs", "sapaldia" or "pooled"

dataset_to_run <- "pooled"


# Model to run (1 to 4)
# (1 = unstratified, 2 = stratified by sex,
#  3 = stratified by ever smoking, 4 = stratified by both)

model_to_run <- 1


# The following lines can optionally be uncommented
# to adjust number of iterations and/or thinning

# iterations_burnin <- 500
# iterations_sampling <- 20000
# iterations_thinning <- 10


# Uncomment one of the two lines below, to run linear or log-linear model
# (Log-linear is to estimate effects on FEV1/FVC)

source("code/jags.R")
#source("code/jagsLog.R")

