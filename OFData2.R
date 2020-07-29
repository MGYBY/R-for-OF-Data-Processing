pacman::p_load(pacman, akima, ggplot2, mgcv, MASS, plotly, latex2exp, extrafont,ggpubr,scales)


theory_max_dist <- data.frame(
  x=c(0,40),
  y=c(0.7,1.5)
)

setwd("/run/media/boyuan/DEEPINOS/719FourPeriods_LowFr")
xlist <- list.files(pattern = "*.csv")
maxDepth <- matrix(0,length(xlist),1)
for(i in c(1:length(xlist))) {
  dataset <- read.csv(paste0((i-1),"sur.csv"),header = T)
  maxDepth[i]<- max(dataset$Points.1)
}

maxTimeSeries <- data.frame(
  Time=c(0:216),
  MaxDepth=c(0.70,maxDepth)
)

p1 <- ggplot(maxTimeSeries,aes(Time,MaxDepth)) +
  geom_point(shape=1, size=2) +
  geom_smooth(span=0.5)  +
  theme(text=element_text(family="Latin Modern Roman", size=18)) +
  xlab("Time (s)") +ylab(TeX("\\textit{$h_{max}}$ (m)")) +
  scale_y_continuous(trans = log10_trans(),limits = c(0.60,1.5), breaks = seq(0.6,1.5,by=0.3)) + 
  scale_x_continuous(breaks = seq(0,220,by=40), limits = c(0,220)) +
  theme(aspect.ratio = 1) +
  geom_abline(intercept=0.7,slope=0.029075,size=2, lty=2, color="red") +
  geom_line(data=theory_max_dist,aes(x,y),size=1, lty=2, color="red")


setwd("/run/media/boyuan/DEEPINOS/719EightPeriods_LowFr")
xlist <- list.files(pattern = "*.csv")
maxDepth <- matrix(0,length(xlist),1)
for(i in c(1:length(xlist))) {
  dataset <- read.csv(paste0((i-1),"sur.csv"),header = T)
  maxDepth[i]<- max(dataset$Points.1)
}

maxTimeSeries <- data.frame(
  Time=c(0:162),
  MaxDepth=c(0.70,maxDepth)
)

p2 <- ggplot(maxTimeSeries,aes(Time,MaxDepth)) +
  geom_point(shape=1, size=2) +
  geom_smooth(span=0.5)  +
  theme(text=element_text(family="Latin Modern Roman", size=18)) +
  xlab("Time (s)") +ylab(TeX("\\textit{$h_{max}}$ (m)")) +
  scale_y_continuous(trans = log10_trans(),limits = c(0.60,1.5), breaks = seq(0.6,1.5,by=0.3)) + 
  scale_x_continuous(breaks = seq(0,170,by=40), limits = c(0,170)) +
  theme(aspect.ratio = 1) +
  geom_abline(intercept=0.7,slope=0.029075,size=2, lty=2, color="red") +
  geom_line(data=theory_max_dist,aes(x,y),size=1, lty=2, color="red")

########multiplot#############
margin_val <- c(0.01,0.0,0.5,2)
ggarrange(p1+theme(plot.margin = unit(margin_val, "lines")),p2+theme(plot.margin = unit(margin_val, "lines")) 
          , ncol=2,nrow=1,labels = c("(a) Case VII","(b) Case VIII")
          ,font.label = list(family="Latin Modern Roman",size=16) ,hjust = -0.4, vjust = 8,align = "v"
          )
