library(rjags)
library(tidyr)
load("input/jem.aloha.RData")

# Set up analysis options

modelfile <- c("code/models/model1-unstratified.jag",
    "code/models/model2-strBySex.jag",
    "code/models/model3-strBySmoking.jag",
    "code/models/model4-strByBoth.jag")[model_to_run]

if (tolower(dataset_to_run)=="ecrhs") {
  load("input/procData-ECRHS.RData")
  data.wide <- data.wide.ECRHS
  data.long <- data.long.ECRHS
  outname <- sprintf("output/resLog-ECRHS-%s-", model_to_run)
} else if (tolower(dataset_to_run)=="sapaldia") {
  load("input/procData-SAPALDIA.RData")
  data.wide <- data.wide.sap
  data.long <- data.long.sap
  outname <- sprintf("output/resLog-SAPALDIA-%s-", model_to_run)
} else if (tolower(dataset_to_run)=="pooled") {
  load("input/procData-pool.RData")
  data.wide <- data.wide.pool
  data.long <- data.long.pool
  outname <- sprintf("output/resLog-POOLED-%s-", model_to_run)
}

if (!exists("iterations_burnin")) iterations_burnin <- 500
if (!exists("iterations_sampling")) iterations_sampling <- 20000
if (!exists("iterations_thinning")) iterations_thinning <- 10


for (x in expNames) data.long[[x]][is.na(data.long[[x]])] <- 0

data.wide <- subset(data.wide, pid %in% data.long$pid)

data.long2 <- gather(data.long, key = "trait", value = "Y", FEV1, FVC)

for (xp in expNames[exposures_to_run]) {

cat(sprintf("Doing exposure %s...\n\n", xp))

dat <- list(
    pid = as.integer(factor(data.long2$pid)), 
    Y = log(data.long2$Y) - 1.3, 
    FEV1 = as.integer(data.long2$trait=="FEV1"),
    FVC = as.integer(data.long2$trait=="FVC"),
    X = data.long2[,xp],
    age = data.long2$age - 42,
    female = as.integer(data.long2$gender=="Female"),
    height = data.long2$height - 1.7,
    asthma = data.long2$asthma,
    cursmoke = as.integer(data.long2$cursmoke),
    pyb = with(data.wide, cbind(pybE1, pybE2, pybE3)),
    smoked = with(data.wide, cbind(as.integer(pybE1>0), as.integer(pybE2>0), as.integer(pybE3>0))),
    disadv = data.long2$disadv,
    SES = as.integer(data.wide$SES),
    survey = data.long2$survey,
    weight = rep(1, nrow(data.long2)),  # not weighting observations
    
    N = nrow(data.long2),                 # Number of rows in the dataset
    #K = length(unique(data.long2$pid))    # Number of persons (pids) in the data
    K = nrow(data.wide)    # Number of persons (pids) in the data
)

dat$gsd <- sapply(1:3, function(i) sd(dat$pyb[,i][which(dat$pyb[,i]>0)]))
dat$gmean <- sapply(1:3, function(i) mean(dat$pyb[,i][which(dat$pyb[,i]>0)]))

ini <- function(chain) {
  ini.values <- list(
    beta = rep(0,c(26,28,28,30)[model_to_run]),
    sigma.a1=0.1, sigma.a2=0.1, sigma.b1=0.1, sigma.b2=0.1, sigma.res=0.1,
    rho.u1=0, rho.u2=0, rho.Q2=0
  )
  return(ini.values)
}



t1 <- system.time(
    model1 <- jags.model(file = modelfile, data = dat, inits=ini,
                        n.chains = 4, n.adapt = iterations_burnin)
)
cat("now sampling...\n")
t2 <- system.time(
    res <- coda.samples(model1, var=c("beta", "sigma.res", 
        "sigma.a1", "sigma.b1", "sigma.a2", "sigma.b2", 
        "rho.u1", "rho.u2", "rho.ua", "rho.ub", "rho.Q1", "rho.Q2",
        "p.SES", "theta.smoke", "theta.asthma", "pSmk"), 
        n.iter=iterations_sampling, thin=iterations_thinning)
)

save(t1,t2,res, file=sprintf("%s%s.RData", outname, xp))

cat(sprintf("Completed exposure %s for dataset %s...\n\n", xp, dataset_to_run))

}
