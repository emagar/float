library(lubridate)
library(plyr)

## clean mem
rm(list=ls())

## data path
dd <- "/home/eric/Dropbox/data/rollcall/comm-floaters/data/"

## read 64 comm info
cm <- read.csv(paste0(dd,"64comm.csv"))
## read 64 dip info
dp <- read.csv(paste0(dd,"licencias/","dip64.csv"))

## retain finished comms only
sel.r <- grep("frontera norte|frontera sur|economia comercio|pesca", cm$committee)
cm <- cm[sel.r,]

## numeric n
cm$ord <- as.numeric(cm$ord)
cm$n   <- as.numeric(cm$n) 
cm$ymd <- as.numeric(cm$ymd)
## format dates
cm$fch <- ymd(cm$ymd)
cm[1,]

## generate tscs
tmp1 <- ymd("20180901")
tmp2 <- ymd("20210831")
tmp3 <- seq(tmp1, tmp2, by = 1)
t <- data.frame(ord=1:length(tmp3))
t <- cbind(t, fch=tmp3)
tail(t)
rm(tmp1,tmp2,tmp3)

## pesca
pesca <- t
table(unique(cm$n[grep("pesca", cm$committee)]))

tmp <- rep(NA, times=nrow(t))

## committee and var names
tmp1 <- revalue(cm$party, c("morena" = "mrn",
                           "pvem"   = "ver"))
tmp2 <- sprintf("%04.1f", cm$n)
tmp3 <- revalue(cm$rank, c("integrante" = "in",
                          "secretario" = "se",
                          "presidente" = "pr"))
seats <- paste0(tmp3,tmp2,tmp1)
cm$seats <- seats
rm(tmp1,tmp2,tmp3)

## pesca
names <- unique(seats[cm$committee=="pesca"])
pesca <- matrix(tmp, nrow=nrow(t), ncol=length(names))
colnames(pesca) <- names
pesca <- cbind(t, pesca)
## subset comm data
cm.s <- cm[cm$committee=="pesca",]
##

for (i in 3:ncol(pesca)){
    #i <- 3 # debug
    sel.r <- which(cm.s$seats %in% colnames(pesca)[i])
    fch1 <- ymd("20180901")
    for (j in 1:length(sel.r)){
        #j <- 1 # debug
        fch2 <- cm.s$fch[sel.r[j]]
        if (cm.s$out.id[sel.r[j]]=="start") {
            pesca[which(pesca$fch %within% interval(fch1,fch2)),i] <- "." ## fill initial vacancy
        } else if (cm.s$in.id [sel.r[j]]=="end"  ) {
            in1 <- cm.s$out.id[sel.r[j]]
            pesca[which(pesca$fch %within% interval(fch1,fch2)),i] <- in1 ## fill member's id
        } else {
            in1 <- cm.s$out.id[sel.r[j]]
            pesca[which(pesca$fch %within% interval(fch1,fch2)),i] <- in1 ## fill member's id
        }
        fch1 <- fch2 ## prep next loop
    }
}
##
rm(i,j,in1,sel.r)

write.csv(pesca, file = paste0(dd, "pesca.csv"), row.names=FALSE)

ls()
    
cm[1,]

pesca[1,]
dim(pesca)

