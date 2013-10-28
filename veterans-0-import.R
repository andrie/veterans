datafile <- "data/cup98LRN.txt"
rdsfile  <- "data/cup98LRN.rds"

dat <- read.csv(datafile)
dim(dat)
saveRDS(dat, file=rdsfile)

