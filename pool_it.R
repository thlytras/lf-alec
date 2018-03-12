if (file.exists("../input/procData-ECRHS.RData")) {
  cat("Loading ECRHS data...\n")
  load("../input/procData-ECRHS.RData")
} else {
  cat("Preparing ECRHS data...\n")
  source("prep-ECRHS.R")
}

if (file.exists("../input/procData-SAPALDIA.RData")) {
  cat("Loading SAPALDIA data...\n")
  load("../input/procData-SAPALDIA.RData")
} else {
  cat("Preparing SAPALDIA data...\n")
  source("prep-SAPALDIA.R")
}


# Pool everything together
cat("Pooling...\n")

common_wide_names <- c("pid", "study", "centre", "gender", "SES", "disadv",
    "followedInE1", "followedInE2", "followedInE3", "dateE1", "dateE2", "dateE3", 
    "ageE1", "ageE2", "ageE3", "timeE1", "timeE2", "timeE3", 
    "heightE1", "heightE2", "heightE3", "coughE1", "coughE2", "coughE3", 
    "phlegmE1", "phlegmE2", "phlegmE3", "asthmaE1", "asthmaE2", "asthmaE3", 
    "cursmokeE1", "cursmokeE2", "cursmokeE3", "packyrsE1", "packyrsE2", "packyrsE3", 
    "pybE1", "pybE2", "pybE3", "passiveE1", "passiveE2", "passiveE3", 
    "FEV1.E1", "FEV1.E2", "FEV1.E3", "FVC.E1", "FVC.E2", "FVC.E3", 
    "FEV1FVC.LLN.E1","FEV1FVC.LLN.E2","FEV1FVC.LLN.E3", 
    "bioldust.E2", "minedust.E2", "gasfumes.E2", "vgdf.E2", "all.E2", "herbi.E2", 
    "insec.E2", "fungi.E2", "aromatic.E2", "chlorinated.E2","other.E2", "metals.E2", 
    "bioldust.E3", "minedust.E3", "gasfumes.E3", "vgdf.E3", "all.E3", "herbi.E3", 
    "insec.E3", "fungi.E3", "aromatic.E3", "chlorinated.E3","other.E3", "metals.E3")

data.wide.ECRHS$study <- factor("ECRHS", levels=c("ECRHS","SAPALDIA"))
data.long.ECRHS$study <- factor("ECRHS", levels=c("ECRHS","SAPALDIA"))
data.wide.sap$study <- factor("SAPALDIA", levels=c("ECRHS","SAPALDIA"))
data.long.sap$study <- factor("SAPALDIA", levels=c("ECRHS","SAPALDIA"))

levels(data.wide.ECRHS$SES) <- levels(data.wide.sap$SES)
levels(data.long.ECRHS$SES) <- levels(data.long.sap$SES)

data.wide.pool <- rbind(data.wide.ECRHS[,common_wide_names], data.wide.sap[,common_wide_names])
data.long.pool <- rbind(data.long.ECRHS, data.long.sap)

cat("Saving...\n")
save(data.wide.pool, data.long.pool, file="../input/procData-pool.RData")
