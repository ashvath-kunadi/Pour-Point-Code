library(ggplot2)
tot <- read.csv("H:/My Documents/Project and related work/banksia at gingin/Volumedetermination.csv")

for(i in 1:(nrow(tot)/5)){
  tot$avgrate[((i-1)*5+1):(i*5)] <- tot$Volume[((i-1)*5+1):(i*5)]/cumsum(tot$Seconds[((i-1)*5+1):(i*5)])
}

ggplot(tot, aes(Volume, avgrate, group = Trial)) + geom_point(aes(col = Trial)) + geom_line(aes(col = Trial)) + 
  theme_classic() + geom_hline(yintercept = (mean(tot$avgrate)), linetype = 3) + 
  geom_hline(yintercept = (mean(tot$avgrate[tot$Volume == 50])), linetype = 2) + xlab("volume [L]") + ylab("Average Rate [L/sec]")

hrdiff <- read.csv("H:/My Documents/Project and related work/banksia at gingin/Uniformitycoeff.csv")
nozloc <- data.frame(yin=rep(2.1, 3), yend= rep(1.9, 3), xin = c(0.4, 1.534, 2.368), xend = c(0.4, 1.534, 2.368))
ggplot(hrdiff, aes(xloc, yloc4)) + geom_bar(position = "stack", stat = "identity", aes(fill = Intensity)) +
  theme_classic() + xlab("Location from edge[m]") + ylab("Height above ground [m]") +
  geom_segment(data= nozloc, aes(x = xin, xend = xend, y = yin, yend = yend), size = 1, arrow = arrow(length(unit(1,"cm")))) + 
  geom_text(aes(y = yloc3, x = xloc, label = round(Intensity,1)))
