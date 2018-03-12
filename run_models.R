
# Select exposures to run. An integer vector with values 1-12.
# 1 - bioldust, 2 - minedust, 3 - gasfumes, 4 - vgdf, 
# 5 - all (pesticides), 6 - herbi, 7 - insec, 8 - fungi, 
# 9 - aromatic, 10 - chlorinated, 11 - other, 12 - metals

exposures_to_run <- c(1:3, 5:12)


# Dataset to run on. "ecrhs", "sapaldia" or "pooled"

dataset_to_run <- "pooled"


# Model to run (set to 1 for now - unstratified model)

model_to_run <- 1



# Don't modify below this line

source("code/jags.R")

