library(lubridate)

dd <- "/home/eric/Dropbox/data/rollcall/comm-floaters/data/"

## read 64 comm info
d <- read.csv(paste0(dd,"64comm.csv"))

## retain finished comms only
sel.r <- grep("frontera norte|frontera sur|economia comercio|pesca", d$committee)
d <- d[sel.r,]
table(d$committee)

## format dates
d$fch <- ymd(d$ymd)
## numeric n
d$n <- as.numeric(d$n)
str(d)
d[1,]


## generate tscs
tmp1 <- min(d$fch) #ymd("20180901")
tmp2 <- max(d$fch) #ymd("20210831")
tmp3 <- seq(tmp1, tmp2, by = 1)
t <- data.frame(ord=1:length(tmp3))
t <- cbind(t, fch=tmp3)
tail(t)

## pesca
pesca <- t
tmp <- rep(".", times=nrow(t))
unique(d$n)[grep("pesca", d$committee)])
head(t)

