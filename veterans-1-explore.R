datafile <- "data/cup98LRN.rds"

dat <- readRDS(datafile)
dim(dat)
names(dat)
head(dat)

table(sapply(dat, class))
fcols <- sapply(dat, is.factor)
sapply(dat[, fcols], nlevels)
sort(sapply(dat, function(x)sum(is.na(x))))


### target variables: 
### 97NK: promotion of interest
### TARGET_B: Responded to 97NK
### TARGET_D: Amount given in response to 97NK
### unique identifier: CONTROLN

interest <- c("ADATE_2", "RFA_2", "TARGET_B", "TARGET_D")
interest %in% names(dat)
summary(dat[, interest])

library(ggplot2)
with(dat, qplot(ADATE_2, as.logical(TARGET_B), geom="point", stat="sum"))
with(dat, table(ADATE_2, as.logical(TARGET_B)))

names(dat)

frm <- as.formula(TARGET_B ~ HC1)
library(microbenchmark)
microbenchmark(
  lm(frm, data=dat),
  rxLinMod(frm, data=dat),
  times=10
)

x <- rxLinMod(frm, dat)$adj.r.squared
rxLinMod(frm, dat, reportProgress=0)$adj.r.squared
str(x)

coef(x)
str(summary(x))

library(foreach)
library(doRSR)
rxOptions(numCoresToUse=8)
registerDoRSR(computeContext=RxLocalParallel())

foo <- function(z, dat, target="TARGET_B"){
  impossible <- FALSE
  message(z)
  if(is.factor(dat[[z]])) {
    if(nlevels(dat[[z]]) > 50 | nlevels(dat[[z]]) == 1) impossible <- TRUE
  }
  
  if(impossible) NULL 
  else {
    frm <- as.formula(paste0(target, " ~ ", z))
    summary(lm(frm, data=dat))$r.squared
    #   rxLinMod(frm, dat, reportProgress=0)$adj.r.squared
  }
  
}

system.time({
  cors <- foreach(z=iter(names(dat)), .inorder=FALSE) %dopar% {
    foo(z, dat, target="TARGET_B")
  }
})


system.time({
  cors <- rxExec(foo, z=rxElemArg(names(dat)), dat=dat, target="TARGET_B")
})
cors