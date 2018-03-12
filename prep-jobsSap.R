load("../input/jem.aloha.RData")

# Process the jobs dataset 

# Add exposure for each job
for(x in expNames) {
  sap.jobs[,x] <- jem.aloha[,x][match(sap.jobs$isco, jem.aloha$ISCO.code)]
}
# Fill in study visit dates (we get them from the processed main dataset)
sap.jobs$dateE1 <- data.wide.sap$dateE1[match(sap.jobs$alec_id, data.wide.sap$pid)]
sap.jobs$dateE2 <- data.wide.sap$dateE2[match(sap.jobs$alec_id, data.wide.sap$pid)]
sap.jobs$dateE3 <- data.wide.sap$dateE3[match(sap.jobs$alec_id, data.wide.sap$pid)]

# Function to truncate dates to the middle of the month
truncDate <- function(x, trunc=15) {
   as.Date(paste(substr(as.character(x), 1, 7), trunc, sep="-"))
}

sap.jobs$start <- with(sap.jobs, as.Date(paste(start_year, start_month, 15, sep="-")))
sap.jobs$stop <- with(sap.jobs, as.Date(paste(end_year, end_month, 15, sep="-")))

# Truncate jobs start dates to the SAP1 visit date, and discard earlier jobs
sap.jobs$startTrunc <- truncDate(pmax(sap.jobs$dateE1, sap.jobs$start))
sap.jobs <- subset(sap.jobs, stop>truncDate(dateE1) & !is.na(startTrunc))
rownames(sap.jobs)<-NULL

sap.jobs$stopTrunc <- truncDate(sap.jobs$stop)
sap.jobs$dateE2trunc <- truncDate(sap.jobs$dateE2)



# Now calculate exposures

maxi <- FALSE # For overlapping jobs. If TRUE, max exposure. If FALSE, mean exposure.
names(sap.jobs)[names(sap.jobs)=="alec_id"] <- "pid"   # Use this ID name for consistency with ECRHS

# Show a progress bar
cat("Now calculating occupational exposures per participant. Please wait...\n")
pb <- txtProgressBar(min=0, max=length(unique(sap.jobs$pid)))
counter=0

# Iterating over every participant
dataOccExp <- lapply(unique(sap.jobs$pid), function(x){
  counter <<- counter+1
  setTxtProgressBar(pb, counter)
  b <- subset(sap.jobs, pid==x) # b is a list of jobs for the participant
  b$stop <- truncDate(b$stop)
  d <- sort(unique(c(b$startTrunc, b$stopTrunc, b$dateE2trunc))) # d is a sorted vector of start, stop dates plus the SAP2 visit date. We will iterate over each period, find jobs performed over each period, and calculate exposures
  dE2 <- unique(b$dateE2trunc)
# Function to identify all jobs (from b) performed during a given period
  mtch <- function(start, stop) {
    which((b$startTrunc <= start) & (b$stopTrunc >= stop))
  }
# Function to assign a coefficient for cumulative exposures: low=1, high=4
  expToInt <- function(x) c(0,1,4)[as.integer(x)]
# Function to calculate duration in months
  monthDur <- function(start, stop, round=TRUE) {
  d <- as.integer(stop-start)/(365.25636/12)
    ifelse(round, round(d)+0.5*(round(d)==0), d)
  }
# Iterate over all time segments, and calculate the exposures for each segment
  r <- lapply(2:length(d), function(i){
    m <- mtch(d[i-1], d[i])
    if (length(m)==0) return(list())
    res <- sapply(b[m, expNames], expToInt)
    if (!is.null(nrow(res))) res <- apply(res, 2, ifelse(maxi, max, mean))
    res <- res * monthDur(d[i-1], d[i])
    if (is.na(dE2) || d[i-1]>=dE2) {
      names(res) <- paste(names(res),"E3", sep=".")
    } else {
      names(res) <- paste(names(res),"E2", sep=".")
    }
    res
  })
# Sum exposures over all time segments before and after SAP2
  expE2 <- do.call(rbind, r[sapply(r, function(x)sum(grepl(".E2",names(x), fixed=TRUE))>0)])
  if (!is.null(nrow(expE2))) expE2 <- apply(expE2, 2, sum)
  if (is.null(expE2)) {
    expE2 <- rep(NA, length(expNames))
    names(expE2) <- paste(expNames, "E2", sep=".")
  }
  expE3 <- do.call(rbind, r[sapply(r, function(x)sum(grepl(".E3",names(x), fixed=TRUE))>0)])
  if (!is.null(nrow(expE3))) expE3 <- apply(expE3, 2, sum)
  if (is.null(expE3)) {
    expE3 <- rep(NA, length(expNames))
    names(expE3) <- paste(expNames, "E3", sep=".")
  }
  return(c(expE2, expE3))
})

close(pb)

dataOccExp <- as.data.frame.matrix(cbind(pid=unique(sap.jobs$pid), do.call(rbind, dataOccExp)))

dataOccExpCum <- dataOccExp
for (i in 2:ncol(dataOccExpCum)) dataOccExpCum[,i][is.na(dataOccExpCum[,i])] <- 0
for (x in expNames) {
  dataOccExpCum[,sprintf("%s.E3",x)] <- dataOccExpCum[,sprintf("%s.E3",x)] + dataOccExpCum[,sprintf("%s.E2",x)]
}

