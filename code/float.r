library(lubridate)
library(plyr)

## clean mem
rm(list=ls())
##
## data path
dd <- "/home/eric/Dropbox/data/rollcall/comm-floaters/data/"
##
## read 64 comm info
cm <- read.csv(paste0(dd,"64comm.csv"))
## read 64 dip info
dp <- read.csv(paste0(dd,"licencias/","dip64.csv"))
##
## retain finished comms only
sel.r <- grep("frontera norte|frontera sur|economia comercio|pesca", cm$committee)
cm <- cm[sel.r,]
##
## numeric n
cm$ord <- as.numeric(cm$ord)
cm$n   <- as.numeric(cm$n) 
cm$ymd <- as.numeric(cm$ymd)
## format dates
cm$fch <- ymd(cm$ymd)
## shorten labels
cm$party <- revalue(cm$party, c("morena" = "mrn",
                                "pvem"   = "ver"))
cm$out.id <- revalue(cm$out.id, c("vacant" = "vac"))
## sort comm-seat-chrono
cm <- cm[order(cm$committee, cm$n, cm$ymd),]
##
## committee and var names
tmp1 <- cm$party
tmp2 <- sprintf("%04.1f", cm$n)
tmp3 <- revalue(cm$rank, c("integrante" = "in",
                            "secretario" = "se",
                            "presidente" = "pr"))
cm$seats <- paste0(tmp3,tmp2,tmp1)
rm(tmp1,tmp2,tmp3)
##
## session dates to subset
session.days <- ymd(
    c(20180901, 20180904, 20180906, 20180911, 20180913, 20180918, 20180919, 20180920, 20180925, 20180927, 20181002, 20181003, 20181004, 20181009, 20181010, 20181011, 20181012, 20181015, 20181016, 20181017, 20181018, 20181019, 20181022, 20181023, 20181024, 20181025, 20181026, 20181029, 20181030, 20181031, 20181101, 20181105, 20181106, 20181107, 20181108, 20181109, 20181112, 20181113, 20181114, 20181115, 20181116, 20181120, 20181121, 20181122, 20181123, 20181126, 20181127, 20181128, 20181129, 20181130, 20181203, 20181204, 20181205, 20181206, 20181210, 20181211, 20181212, 20181213, 20181214, 20181217, 20181218, 20181219, 20181220, 20181221, 20181222, 20181223, 20190201, 20190206, 20190207, 20190212, 20190214, 20190219, 20190221, 20190226, 20190228, 20190305, 20190307, 20190312, 20190314, 20190402, 20190403, 20190404, 20190408, 20190409, 20190410, 20190411, 20190423, 20190424, 20190425, 20190426, 20190429, 20190430, 20190831, 20190901, 20190903, 20190905, 20190910, 20190918, 20190919, 20190924, 20190926, 20191001, 20191002, 20191003, 20191008, 20191009, 20191010, 20191015, 20191016, 20191017, 20191022, 20191023, 20191024, 20191029, 20191031, 20191105, 20191106, 20191126, 20191128, 20191203, 20191205, 20191210, 20191211, 20191212, 20200201, 20200205, 20200206, 20200211, 20200213, 20200218, 20200220, 20200225, 20200227, 20200303, 20200305, 20200310, 20200312, 20200318, 20200319, 20200831, 20200901, 20200902, 20200908, 20200909, 20200914, 20200915, 20200922, 20200923, 20200929, 20200930, 20201001, 20201006, 20201013, 20201014, 20201015, 20201019, 20201022, 20201027, 20201028, 20201104, 20201105, 20201110, 20201111, 20201118, 20201119, 20201124, 20201125, 20201126, 20201201, 20201202, 20201203, 20201208, 20201209, 20201210, 20201214, 20201215, 20210201, 20210203, 20210204, 20210209, 20210210, 20210216, 20210217, 20210223, 20210302, 20210303, 20210308, 20210309, 20210310, 20210317, 20210318, 20210323, 20210324, 20210325, 20210407, 20210408, 20210413, 20210414, 20210420, 20210421, 20210422, 20210428, 20210429, 20210430, 20210831)
    )
##
## generate tscs
tmp1 <- ymd("20180901")
tmp2 <- ymd("20210831")
tmp3 <- seq(tmp1, tmp2, by = 1)
t <- data.frame(ord=1:length(tmp3))
t <- cbind(t, fch=tmp3)
tail(t)
rm(tmp1,tmp2,tmp3)
## empty vector to replicate
tmp.t <- rep(NA, times=nrow(t))

####################
## frontera norte ##
####################
tmp.work <- "asuntos frontera norte"
table(unique(cm$n[grep(tmp.work, cm$committee)]))
##
names <- unique(cm$seats[cm$committee==tmp.work])
t2 <- matrix(tmp.t, nrow=nrow(t), ncol=length(names))  # duplicate
colnames(t2) <- names
t2 <- cbind(t, t2)
## subset comm data
cm.s <- cm[cm$committee==tmp.work,]
##
for (i in 3:ncol(t2)){
    #i <- 3 # debug
    sel.r <- which(cm.s$seats %in% colnames(t2)[i])
    fch1 <- ymd("20180901")
    for (j in 1:length(sel.r)){
        #j <- 1 # debug
        fch2 <- cm.s$fch[sel.r[j]]
        if (cm.s$out.id[sel.r[j]]=="start") {
            t2[which(t2$fch %within% interval(fch1,fch2)),i] <- "vac" ## fill initial vacancy
        } else if (cm.s$in.id [sel.r[j]]=="end"  ) {
            in1 <- cm.s$out.id[sel.r[j]]
            t2[which(t2$fch %within% interval(fch1,fch2)),i] <- in1 ## fill member's id
        } else {
            in1 <- cm.s$out.id[sel.r[j]]
            t2[which(t2$fch %within% interval(fch1,fch2)),i] <- in1 ## fill member's id
        }
        fch1 <- fch2 ## prep next loop
    }
}
##
## add junta totals
t3 <- t2[, grep(pattern="^pr|^se", x=colnames(t2))]
for (i in 1:ncol(t3)){
    t3[,i] <- ifelse(t3[,i]=="vac", 0, 1)
}
t2$junta.mrn <- round(rowSums(t3[,grep(pattern="mrn$",                x=colnames(t3))]) / rowSums(t3),2)
t2$junta.jhh <- round(rowSums(t3[,grep(pattern="mrn$|pt$|pvem$|pes$", x=colnames(t3))]) / rowSums(t3),2)
##
## ## keep session days only
## t2 <- t2[which(t2$fch %in% session.days),]
## duplicate with names instead of ids
t2.n <- t2
for (i in 1:ncol(t2.n)){
    t2.n[,i] <- mapvalues(t2.n[,i], from = dp$id, to = dp$short, warn_missing = FALSE)
}
##
## export
write.csv(t2  [,-1], file = paste0(dd, "com-by-com/norte64ids.csv"), row.names=FALSE)
write.csv(t2.n[,-1], file = paste0(dd, "com-by-com/norte64nom.csv"), row.names=FALSE)


##################
## frontera sur ##
##################
tmp.work <- "asuntos frontera sur"
table(unique(cm$n[grep(tmp.work, cm$committee)]))
##
names <- unique(cm$seats[cm$committee==tmp.work])
t2 <- matrix(tmp.t, nrow=nrow(t), ncol=length(names))  # duplicate
colnames(t2) <- names
t2 <- cbind(t, t2)
## subset comm data
cm.s <- cm[cm$committee==tmp.work,]
##
for (i in 3:ncol(t2)){
    #i <- 3 # debug
    sel.r <- which(cm.s$seats %in% colnames(t2)[i])
    fch1 <- ymd("20180901")
    for (j in 1:length(sel.r)){
        #j <- 1 # debug
        fch2 <- cm.s$fch[sel.r[j]]
        if (cm.s$out.id[sel.r[j]]=="start") {
            t2[which(t2$fch %within% interval(fch1,fch2)),i] <- "vac" ## fill initial vacancy
        } else if (cm.s$in.id [sel.r[j]]=="end"  ) {
            in1 <- cm.s$out.id[sel.r[j]]
            t2[which(t2$fch %within% interval(fch1,fch2)),i] <- in1 ## fill member's id
        } else {
            in1 <- cm.s$out.id[sel.r[j]]
            t2[which(t2$fch %within% interval(fch1,fch2)),i] <- in1 ## fill member's id
        }
        fch1 <- fch2 ## prep next loop
    }
}
## add junta totals
t3 <- t2[, grep(pattern="^pr|^se", x=colnames(t2))]
for (i in 1:ncol(t3)){
    t3[,i] <- ifelse(t3[,i]=="vac", 0, 1)
}
t2$junta.mrn <- round(rowSums(t3[,grep(pattern="mrn$",                x=colnames(t3))]) / rowSums(t3),2)
t2$junta.jhh <- round(rowSums(t3[,grep(pattern="mrn$|pt$|pvem$|pes$", x=colnames(t3))]) / rowSums(t3),2)
##
## ## keep session days only
## t2 <- t2[which(t2$fch %in% session.days),]
## duplicate with names instead of ids
t2.n <- t2
for (i in 1:ncol(t2.n)){
    t2.n[,i] <- mapvalues(t2.n[,i], from = dp$id, to = dp$short, warn_missing = FALSE)
}
##
##
## export
write.csv(t2  [,-1], file = paste0(dd, "com-by-com/sur64ids.csv"), row.names=FALSE)
write.csv(t2.n[,-1], file = paste0(dd, "com-by-com/sur64nom.csv"), row.names=FALSE)

#######################
## economia comercio ##
#######################
tmp.work <- "economia comercio y competitividad"
table(unique(cm$n[grep(tmp.work, cm$committee)]))
##
names <- unique(cm$seats[cm$committee==tmp.work])
t2 <- matrix(tmp.t, nrow=nrow(t), ncol=length(names))  # duplicate
colnames(t2) <- names
t2 <- cbind(t, t2)
## subset comm data
cm.s <- cm[cm$committee==tmp.work,]
##
for (i in 3:ncol(t2)){
    #i <- 3 # debug
    sel.r <- which(cm.s$seats %in% colnames(t2)[i])
    fch1 <- ymd("20180901")
    for (j in 1:length(sel.r)){
        #j <- 1 # debug
        fch2 <- cm.s$fch[sel.r[j]]
        if (cm.s$out.id[sel.r[j]]=="start") {
            t2[which(t2$fch %within% interval(fch1,fch2)),i] <- "vac" ## fill initial vacancy
        } else if (cm.s$in.id [sel.r[j]]=="end"  ) {
            in1 <- cm.s$out.id[sel.r[j]]
            t2[which(t2$fch %within% interval(fch1,fch2)),i] <- in1 ## fill member's id
        } else {
            in1 <- cm.s$out.id[sel.r[j]]
            t2[which(t2$fch %within% interval(fch1,fch2)),i] <- in1 ## fill member's id
        }
        fch1 <- fch2 ## prep next loop
    }
}
##
## add junta totals
t3 <- t2[, grep(pattern="^pr|^se", x=colnames(t2))]
for (i in 1:ncol(t3)){
    t3[,i] <- ifelse(t3[,i]=="vac", 0, 1)
}
t2$junta.mrn <- round(rowSums(t3[,grep(pattern="mrn$",                x=colnames(t3))]) / rowSums(t3),2)
t2$junta.jhh <- round(rowSums(t3[,grep(pattern="mrn$|pt$|pvem$|pes$", x=colnames(t3))]) / rowSums(t3),2)
##
## ## keep session days only
## t2 <- t2[which(t2$fch %in% session.days),]
## duplicate with names instead of ids
t2.n <- t2
for (i in 1:ncol(t2.n)){
    t2.n[,i] <- mapvalues(t2.n[,i], from = dp$id, to = dp$short, warn_missing = FALSE)
}
##
## export
write.csv(t2  [,-1], file = paste0(dd, "com-by-com/ecocom64ids.csv"), row.names=FALSE)
write.csv(t2.n[,-1], file = paste0(dd, "com-by-com/ecocom64nom.csv"), row.names=FALSE)

###########
## pesca ##
###########
tmp.work <- "pesca"
table(unique(cm$n[grep(tmp.work, cm$committee)]))
##
names <- unique(cm$seats[cm$committee==tmp.work])
t2 <- matrix(tmp.t, nrow=nrow(t), ncol=length(names))  # duplicate
colnames(t2) <- names
t2 <- cbind(t, t2)
## subset comm data
cm.s <- cm[cm$committee==tmp.work,]
cm.s[1,]
##
for (i in 3:ncol(t2)){
    #i <- 3 # debug
    sel.r <- which(cm.s$seats %in% colnames(t2)[i])
    fch1 <- ymd("20180901")
    for (j in 1:length(sel.r)){
        #j <- 1 # debug
        fch2 <- cm.s$fch[sel.r[j]]
        if (cm.s$out.id[sel.r[j]]=="start") {
            t2[which(t2$fch %within% interval(fch1,fch2)),i] <- "vac" ## fill initial vacancy
        } else if (cm.s$in.id [sel.r[j]]=="end"  ) {
            in1 <- cm.s$out.id[sel.r[j]]
            t2[which(t2$fch %within% interval(fch1,fch2)),i] <- in1 ## fill member's id
        } else {
            in1 <- cm.s$out.id[sel.r[j]]
            t2[which(t2$fch %within% interval(fch1,fch2)),i] <- in1 ## fill member's id
        }
        fch1 <- fch2 ## prep next loop
    }
}
##
## add junta totals
t3 <- t2[, grep(pattern="^pr|^se", x=colnames(t2))]
for (i in 1:ncol(t3)){
    t3[,i] <- ifelse(t3[,i]=="vac", 0, 1)
}
t2$junta.mrn <- round(rowSums(t3[,grep(pattern="mrn$",                x=colnames(t3))]) / rowSums(t3),2)
t2$junta.jhh <- round(rowSums(t3[,grep(pattern="mrn$|pt$|pvem$|pes$", x=colnames(t3))]) / rowSums(t3),2)
##
## ## keep session days only
## t2 <- t2[which(t2$fch %in% session.days),]
## duplicate with names instead of ids
t2.n <- t2
for (i in 1:ncol(t2.n)){
    t2.n[,i] <- mapvalues(t2.n[,i], from = dp$id, to = dp$short, warn_missing = FALSE)
}
##
## export
write.csv(t2  [,-1], file = paste0(dd, "com-by-com/pesca64ids.csv"), row.names=FALSE)
write.csv(t2.n[,-1], file = paste0(dd, "com-by-com/pesca64nom.csv"), row.names=FALSE)

##
## clean    
ls()
rm(cm.s, fch1, fch2, i, j, in1, names, sel.r, tmp.t, tmp.work, t, t2, t2.n, t3)


