load("../input/data.ECRHS.RData")
load("../input/jem.aloha.RData")

# Correct formatting for a few variables, to make them mergeable with SAPALDIA
data.wide.ECRHS$cursmokeE1 <- data.wide.ECRHS$cursmokeE1=="cur"
data.wide.ECRHS$cursmokeE2 <- data.wide.ECRHS$cursmokeE2=="cur"
data.wide.ECRHS$cursmokeE3 <- data.wide.ECRHS$cursmokeE3=="cur"
data.wide.ECRHS$coughE1 <- data.wide.ECRHS$coughE1=="Yes"
data.wide.ECRHS$coughE2 <- data.wide.ECRHS$coughE2=="Yes"
data.wide.ECRHS$coughE3 <- data.wide.ECRHS$coughE3=="Yes"
data.wide.ECRHS$phlegmE1 <- data.wide.ECRHS$phlegmE1=="Yes"
data.wide.ECRHS$phlegmE2 <- data.wide.ECRHS$phlegmE2=="Yes"
data.wide.ECRHS$phlegmE3 <- data.wide.ECRHS$phlegmE3=="Yes"


data.long.ECRHS <- reshape(data.wide.ECRHS,
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

data.long.ECRHS <- data.long.ECRHS[,c("pid","centre","gender","SES","disadv","survey", 
      "followed", "date", "age", "time", "height", "cough", "phlegm", "asthma", 
      "cursmoke", "packyrs", "pyb", "passive", "FEV1", "FVC", "FEV1FVC.LLN", expNames)]
            
data.long.ECRHS <- subset(data.long.ECRHS, followed==1)
rownames(data.long.ECRHS) <- NULL

# Keep only those with LF measurements
data.long.ECRHS <- subset(data.long.ECRHS, !is.na(FEV1) & !is.na(FVC))


save(data.wide.ECRHS, data.long.ECRHS, file="../input/procData-ECRHS.RData")
