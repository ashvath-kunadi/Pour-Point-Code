library(ggplot2)
library(reshape2)
library(plyr)
library(dplyr)
setwd("H:/My DocCircments/Project and related work/banksia at gingin")
dat <- read.csv("Vegetationdetails.csv")
View(dat)
names(dat)[5] <- "Circ"
findat <- mmoddat[complete.cases(mmoddat),]
ggplot(dat,aes(Species)) + geom_histogram(stat= "coCircnt") + theme_bw()
table(dat$Species)
dat$Circ <- as.nCircmeric(dat$Circ)
dat$TID <- paste(dat$Row,dat$Plot, dat$Tree..)
sCircb1 <- dat[dat$Species == "BA" | dat$Species == "BM", ]
tapply(sub1$X2, sub1$Species, FUN = function(X) {sum(is.na(X))/length(X)*100})
moddat1 <- sub1[,c(27,4:20,23)]
mmoddat <- melt(moddat1, id = c("TID", "Height", "Species"))
mmoddat <- mmoddat[complete.cases(mmoddat),]
stch <- tapply(mmoddat$variable, mmoddat$Species, FCircN = fCircnction(X) {table(X)})
fstch <- stch


for(j in (length(fstch$BA)-1):1){fstch$BA[j] <- stch$BA[j]-stch$BA[j+1]}
for(j in (length(fstch$BM)-1):1){fstch$BM[j] <- stch$BM[j]-stch$BM[j+1]}
stch <- data.frame(sp = rep(c("BA","BM"), each = 7), stems = rep(1:7,2),cnt = c(fstch$BA[1:7], fstch$BM[1:7]))

findat <- moddat1[is.na(moddat1$X3), c(1,21, 2:4, 19)]
findat$Stem <- 1
findat$Stem[!is.na(findat$X2)] <- 2
tags <- c("[0-10)", "[10,20)","[20-30)", "[30-40)","[40-50)", "[50,60)","[60-70)", "[70-80)","[80-90)", "[90-100)","[100-151)")
try2grp <- as_tibble(moddat1) %>%
  mutate(tag = case_when(
    Circ < 10 ~ tags[1],
    Circ >= 10 & Circ < 20 ~ tags[2],
    Circ >= 20 & Circ < 30 ~ tags[3],
    Circ >= 30 & Circ < 40 ~ tags[4],
    Circ >= 40 & Circ < 50 ~ tags[5],
    Circ >= 50 & Circ < 60 ~ tags[6],
    Circ >= 60 & Circ < 70 ~ tags[7],
    Circ >= 70 & Circ < 80 ~ tags[8],
    Circ >= 80 & Circ < 90 ~ tags[9],
    Circ >= 90 & Circ < 100 ~ tags[10],
    Circ >= 100 ~ tags[11],
  ))
moddat1$tag <- factor(try2grp$tag, levels = tags, ordered = FALSE)
lb <- function(x) mean(x) - sd(x)
ub <- function(x) mean(x) + sd(x)
sumld <- ddply(moddat1, ~tag, summarise, mean = mean(Height), median = median(Height), lower = lb(Height), upper = ub(Height))

g1 <- ggplot(data = moddat1, mapping = aes(x=tag,y=Height)) + 
  geom_jitter(aes(color='blue'),alpha=0.2) +
  facet_wrap(~Species)+
  geom_violin(fill="bisque",color="black",alpha=0.3) + 
  geom_point(data = sumld, aes(x = tag, y = mean), size = 2.5) +
  geom_errorbar(data = sumld, aes(ymin = lower, ymax = upper, y = mean), width = 0) +
  labs(x='Circumference [cm]', y = 'Height [m]') +
  guides(color=FALSE) +
  theme_minimal()

ggplot(stch,aes(stems, cnt, fill = sp)) + geom_histogram(stat= "identity", position = "dodge") + theme_bw()

ggplot(dat, aes(Circ, Height)) + geom_point(aes(col = Species, shape = Species)) + theme_bw()
