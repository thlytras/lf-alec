library(readstata13)

sap.main <- read.dta13("../input/main.dta", nonint.factors=TRUE)
sap.jobs <- read.dta13("../input/occupational.dta", nonint.factors=TRUE)

sap.main$SES <- sap.main$educ_s1
sap.main$SES[is.na(sap.main$SES)] <- sap.main$educ_s2[is.na(sap.main$SES)]
sap.main$SES <- factor(sap.main$SES, levels=rev(levels(sap.main$SES)))
levels(sap.main$SES) <- c("High","Middle","Low")

NA0 <- function(x) { x[is.na(x)] <- 0; x }
sap.main$disadv <- with(sap.main, NA0(elm01100_s1==" Yes") + NA0(elm01300_s1==" Yes") + 
      NA0(matsmok_s1==" Yes") + NA0(earlinf_s1==" Yes") + NA0(rem04500_s1<=10))

# Convert height from cm to m
sap.main$height_s1 <- sap.main$height_s1/100
sap.main$height_s2 <- sap.main$height_s2/100
sap.main$height_s3 <- sap.main$height_s3/100

# Code the cough & phlegm variables (yes if most days for 3m per year, no otherwise)
sap.main$coughE1 <- as.integer(factor(sap.main$rem02400_s1, levels=c(" No"," Yes"))) - 1
sap.main$coughE2 <- as.integer(factor(sap.main$rem02400_s2, levels=c(" No"," Yes"))) - 1
sap.main$coughE3 <- as.integer(factor(sap.main$rem02400_s3, levels=c(" No"," Yes"))) - 1
sap.main$coughE1[sap.main$rem02300_s1==" No"] <- 0   # Set cough>3mpy to no if no cough at all
sap.main$coughE2[sap.main$rem02300_s2==" No"] <- 0
sap.main$coughE3[sap.main$rem02300_s3==" No"] <- 0
sap.main$phlegmE1 <- as.integer(factor(sap.main$rem03000_s1, levels=c(" No"," Yes"))) - 1
sap.main$phlegmE2 <- as.integer(factor(sap.main$rem03000_s2, levels=c(" No"," Yes"))) - 1
sap.main$phlegmE3 <- as.integer(factor(sap.main$rem03000_s3, levels=c(" No"," Yes"))) - 1
sap.main$phlegmE1[sap.main$rem02900_s1==" No"] <- 0   # Set phlegm>3mpy to no if no phlegm at all
sap.main$phlegmE2[sap.main$rem02900_s2==" No"] <- 0
sap.main$phlegmE3[sap.main$rem02900_s3==" No"] <- 0

# Code asthma variables in the same way
sap.main$asthmaE1 <- with(sap.main, rem05900_s1== " Yes" | rem06700_s1==" Yes" | rem01200_s1==" Yes")
sap.main$asthmaE1[with(sap.main, rem01200_s1==" No")] <- FALSE
sap.main$asthmaE2 <- with(sap.main, rem05900_s2== " Yes" | rem06700_s2==" Yes" | rem01200_s2==" Yes")
sap.main$asthmaE2[with(sap.main, rem01200_s2==" No")] <- FALSE
sap.main$asthmaE3 <- with(sap.main, rem05900_s3== " Yes" | rem06700_s3==" Yes" | rem01200_s3==" Yes")
sap.main$asthmaE3[with(sap.main, rem01200_s3==" No")] <- FALSE

# Variables for current smoking
sap.main$cursmokeE1 <- sap.main$smok_s1==" Current"
sap.main$cursmokeE2 <- sap.main$smok_s2==" Current"
sap.main$cursmokeE3 <- sap.main$smok_s3==" Current"




# Select variables to match the ECRHS wide dataset
data.wide.sap <- sap.main[,c("alec_id","area_s1", "sex_s1", "SES", "disadv", 
    "doi_s1", "age_s1", "doi_s2", "age_s2", "doi_s3", "age_s3", 
    "height_s1", "height_s2", "height_s3", "coughE1", "coughE2", "coughE3", 
    "phlegmE1", "phlegmE2", "phlegmE3", "asthmaE1", "asthmaE2", "asthmaE3",
    "cursmokeE1", "cursmokeE2", "cursmokeE3", "packyrsa_s1", "packyrsa_s2", "packyrsa_s3",
    "ETS12m_s1", "ETS12m_s2", "ETS12m_s3", "FEV1_s1", "FEV1_s2", "FEV1_corr_s3",
    "FVC_s1", "FVC_s2", "FVC_corr_s3", "FEV1_FVC_llg_s1", "FEV1_FVC_llg_s2", "FEV1_FVC_llg_s3")]
# Give variables the same name
colnames(data.wide.sap) <- c("pid", "centre", "gender", "SES", "disadv", "dateE1", "ageE1", "dateE2", "ageE2", "dateE3", "ageE3", "heightE1", "heightE2", "heightE3", "coughE1", "coughE2", "coughE3", "phlegmE1", "phlegmE2", "phlegmE3", "asthmaE1", "asthmaE2", "asthmaE3", "cursmokeE1", "cursmokeE2", "cursmokeE3", "packyrsE1", "packyrsE2", "packyrsE3", "passiveE1", "passiveE2", "passiveE3", "FEV1.E1", "FEV1.E2", "FEV1.E3", "FVC.E1", "FVC.E2", "FVC.E3", "FEV1FVC.LLN.E1", "FEV1FVC.LLN.E2", "FEV1FVC.LLN.E3")


# Fill in remaining columns
data.wide.sap$spiroTypeE1 <- data.wide.sap$spiroTypeE2 <- data.wide.sap$spiroTypeE3 <- NA
data.wide.sap$followedInE1 <- as.integer(!is.na(data.wide.sap$dateE1))
data.wide.sap$followedInE2 <- as.integer(!is.na(data.wide.sap$dateE2))
data.wide.sap$followedInE3 <- as.integer(!is.na(data.wide.sap$dateE3))
data.wide.sap$wup <- with(data.wide.sap, paste(followedInE1, followedInE2, followedInE3, sep="-"))
data.wide.sap$pybE1 <- data.wide.sap$packyrsE1
data.wide.sap$pybE2 <- data.wide.sap$packyrsE2 - data.wide.sap$packyrsE1
data.wide.sap$pybE3 <- data.wide.sap$packyrsE3 - data.wide.sap$packyrsE2
# Record whether the participant has at least one job at each wave
data.wide.sap$jobsIn2 <- (data.wide.sap$pid %in% unique(subset(sap.jobs, study_wave==2)$alec_id))
data.wide.sap$jobsIn3 <- (data.wide.sap$pid %in% unique(subset(sap.jobs, study_wave==3)$alec_id))


# Prepare the jobs dataset and calculate exposures
source("prep-jobsSap.R")


# Merge the occupational exposures on the wide dataset, setting exposures @E1 = 0
data.wide.sap[,paste(expNames, "E1", sep=".")] <- 0
data.wide.sap <- merge(data.wide.sap, dataOccExpCum, all.x=TRUE)

# Keep only those that have been followed up
data.wide.sap <- subset(data.wide.sap, followedInE2 | followedInE3)

# Make a variable for follow-up time
data.wide.sap$timeE1 <- 0
data.wide.sap$timeE2 <- with(data.wide.sap, ageE2-ageE1)
data.wide.sap$timeE3 <- with(data.wide.sap, ageE3-ageE1)

# When missing, fill in the height from other follow-up visits
data.wide.sap$heightE1[is.na(data.wide.sap$heightE1)] <- apply(data.wide.sap[is.na(data.wide.sap$heightE1), c("heightE2","heightE3")], 1, mean, na.rm=TRUE)
data.wide.sap$heightE2[is.na(data.wide.sap$heightE2)] <- apply(data.wide.sap[is.na(data.wide.sap$heightE2), c("heightE1","heightE3")], 1, mean, na.rm=TRUE)
data.wide.sap$heightE3[is.na(data.wide.sap$heightE3)] <- apply(data.wide.sap[is.na(data.wide.sap$heightE3), c("heightE1","heightE2")], 1, mean, na.rm=TRUE)

data.long.sap <- reshape(data.wide.sap,
        varying=c(list(
            c("followedInE1", "followedInE2", "followedInE3"),
            c("dateE1","dateE2","dateE3"), 
            c("ageE1","ageE2","ageE3"), 
            c("timeE1", "timeE2", "timeE3"),
            c("heightE1", "heightE2", "heightE3"),
            c("coughE1", "coughE2", "coughE3"),
            c("phlegmE1", "phlegmE2", "phlegmE3"),
            c("asthmaE1", "asthmaE2", "asthmaE3"),
            c("cursmokeE1", "cursmokeE2", "cursmokeE3"),
            c("packyrsE1", "packyrsE2", "packyrsE3"),
            c("pybE1", "pybE2", "pybE3"),
            c("passiveE1", "passiveE2", "passiveE3"),
            c("FEV1.E1","FEV1.E2","FEV1.E3"), 
            c("FVC.E1","FVC.E2","FVC.E3"),
            c("FEV1FVC.LLN.E1", "FEV1FVC.LLN.E2", "FEV1FVC.LLN.E3")
            ),
            lapply(expNames, function(x)paste(x, ".E", 1:3, sep=""))
            ),
        v.names=c("followed", "date", "age", "time", "height", "cough", "phlegm", "asthma",
            "cursmoke", "packyrs", "pyb", "passive", "FEV1", "FVC", "FEV1FVC.LLN", expNames), 
        direction="long", idvar="pid", timevar="survey")

data.long.sap <- data.long.sap[,c("pid","centre","gender","SES","disadv","survey", 
      "followed", "date", "age", "time", "height", "cough", "phlegm", "asthma", 
      "cursmoke", "packyrs", "pyb", "passive", "FEV1", "FVC", "FEV1FVC.LLN", expNames)]

data.long.sap <- subset(data.long.sap, followed==1)
rownames(data.long.sap) <- NULL

# Keep only those with LF measurements
data.long.sap <- subset(data.long.sap, !is.na(FEV1) & !is.na(FVC))


save(data.wide.sap, data.long.sap, file="../input/procData-SAPALDIA.RData")

