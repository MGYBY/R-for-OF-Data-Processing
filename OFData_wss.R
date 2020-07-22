pacman::p_load(pacman, ggplot2, mgcv,dplyr, MASS, plotly,interp, latex2exp, extrafont,ggpubr,scales,akima
               ,colorspace)

################Two Periods Case###################
setwd("/run/media/boyuan/Boyuan2019/Num2MachineOFBackup/615simwithdiss003/U75H_q5withdisext")

#########maximum wss###########
xlist <- list.files(pattern = "*ws.csv")
maxDepth <- matrix(0,length(xlist),1)
for(i in c(1:length(xlist))) {
  dataset <- read.csv(paste0((i-1),"ws.csv"),header = T)
  maxDepth[i]<- max(abs(dataset$wallShearStress.0))
}


maxTimeSeries <- data.frame(
  Time=c(seq(0,100,by=0.5),101:300),
  MaxSS=c(maxDepth)
)

pmaxwss <- ggplot(maxTimeSeries,aes(Time,MaxSS)) +
  geom_point(shape=1, size=2) +
  geom_smooth(span=0.5)  +
  theme(text=element_text(family="Latin Modern Roman", size=18)) +
  xlab("Time (s)") +ylab(TeX("\\textit{$(ω_{w})_{max}}$ (N/m$^2$)")) +
 # scale_y_continuous(trans = log10_trans(),limits = c(0.60,1.5), breaks = seq(0.6,1.5,by=0.3)) + 
  scale_x_continuous(breaks = seq(0,300,by=50), limits = c(0,300)) #+
  #theme(aspect.ratio = 1) + 
#  geom_line(data=theory_max_dist,aes(x,y),size=1, lty=2, color="red")


########function for wss; using dimensional quantities###############
wss_vel_sur <- function(fsur,fws, hn, un){
  surfile <- read.csv(fsur, header = T)
  surfile <- filter(surfile,surfile$Points.2==0.1)
  surfaceData <- data.frame(
    x=surfile$Points.0,
    y=surfile$Points.1
  )
  velData <- data.frame(
    x=surfile$Points.0,
    y=surfile$U.0
  )
  
  wssfile <- read.csv(fws, header = T)
  wssData <- data.frame(
    x=wssfile$Points.0,
    y=wssfile$wallShearStress.0
  )
  
  surface_interp <- as.data.frame(approx(surfaceData$x, surfaceData$y, xout = wssData$x))
  vel_interp <- as.data.frame(approx(velData$x, velData$y, xout = wssData$x))
  
  surf_wss <- cbind(surface_interp,wssData, vel_interp)
  colnames(surf_wss) <- c("x1", "surf", "x2", "wss", "x3", "vel")

  #########non-dimensionlization###########
  surf_wss$surf <- surf_wss$surf/hn
  surf_wss$vel <- surf_wss$vel/un
  
  p1 <- ggplot(surf_wss,aes(x2,abs(wss))) +
    geom_line(aes(color=surf), size=1.5) +
    scale_color_gradient(low="dodgerblue", high="brown1") +
    labs(color=TeX("\\textit{h/h$_n$}")) +theme(legend.title=element_text(family="Latin Modern Roman", size=18), legend.text = element_text(family="Latin Modern Roman", size=14)) +
    theme(axis.title = element_text(family="Latin Modern Roman", size=18), axis.text = element_text(family="Latin Modern Roman", size=16)) +
    xlab(TeX("\\textit{x} (m)")) +ylab(TeX("\\textit{ω$_{w}$} (N/m$^2$)"))+
    ylim(0,NA)
  
  
  
  p11 <- ggplot(surf_wss,aes(surf,abs(wss))) +
    geom_point(shape=1, size=1.5)+
    theme(axis.title = element_text(family="Latin Modern Roman", size=18), axis.text = element_text(family="Latin Modern Roman", size=16)) +
    xlab(TeX("\\textit{h/$h_n$}")) +ylab(TeX("\\textit{ω$_{w}$} (N/m$^2$)")) 
  p12 <- ggplot(surf_wss,aes(vel,abs(wss))) +
    geom_point(shape=1, size=1.5)+
    theme(axis.title = element_text(family="Latin Modern Roman", size=18), axis.text = element_text(family="Latin Modern Roman", size=16)) +
    xlab(TeX("\\textit{U/$U_n$}")) +ylab(TeX("\\textit{ω$_{w}$} (N/m$^2$)")) 
  return_list <- list(p1, p11, p12)
  
}

p <- wss_vel_sur("50sur.csv", "50ws1.csv", 0.667, 7.5)
p1 <- p[1]
p2 <- p[2]
p3 <- p[3]


p <- wss_vel_sur("199sur.csv", "199ws1.csv", 0.667, 7.5)
p4 <- p[1]
p5 <- p[2]
p6 <- p[3]


p <- wss_vel_sur("299sur.csv", "299ws1.csv", 0.667, 7.5)
p7 <- p[1]
p8 <- p[2]
p9 <- p[3]

p <- wss_vel_sur("399sur.csv", "399ws1.csv", 0.667, 7.5)
p10 <- p[1]
p11 <- p[2]
p12 <- p[3]


p <- wss_vel_sur("360sur.csv", "360ws1.csv", 0.667, 7.5)
a <- p[1]
b <- p[2]
c <- p[3]

# p <- wss_vel_sur("350sur.csv", "350ws1.csv", 0.667, 7.5)
# p13 <- p[1]
# p14 <- p[2]
# p15 <- p[3]









#########silly programming; using dimensional quantity#################

#########t=50################
surfile <- read.csv("50sur0.csv", header = T)
surfile <- filter(surfile,surfile$Points.2==0.1)
surfaceData <- data.frame(
  x=surfile$Points.0,
  y=surfile$Points.1
)
velData <- data.frame(
  x=surfile$Points.0,
  y=surfile$U.0
)

wssfile <- read.csv("50ws.csv", header = T)
wssData <- data.frame(
  x=wssfile$Points.0,
  y=wssfile$wallShearStress.0
)

surface_interp <- as.data.frame(approx(surfaceData$x, surfaceData$y, xout = wssData$x))
vel_interp <- as.data.frame(approx(velData$x, velData$y, xout = wssData$x))

surf_wss <- cbind(surface_interp,wssData, vel_interp)
colnames(surf_wss) <- c("x1", "surf", "x2", "wss", "x3", "vel")


p1 <- ggplot(surf_wss,aes(x2,wss)) +
  geom_line(aes(color=surf), size=1.5) +
  scale_color_gradient(low = "blue3", high = "red") +
  labs(color=TeX("\\textit{h}")) +theme(legend.title=element_text(family="Latin Modern Roman", size=18), legend.text = element_text(family="Latin Modern Roman", size=14)) +
  theme(axis.title = element_text(family="Latin Modern Roman", size=18), axis.text = element_text(family="Latin Modern Roman", size=16)) +
  xlab(TeX("\\textit{x} (m)")) +ylab(TeX("\\textit{ω$_{w}$} (N/m$^2$)")) 



p11 <- ggplot(surf_wss,aes(surf,wss)) +
  geom_point(shape=1, size=1.5)+
  theme(axis.title = element_text(family="Latin Modern Roman", size=18), axis.text = element_text(family="Latin Modern Roman", size=16)) +
  xlab(TeX("\\textit{h} (m)")) +ylab(TeX("\\textit{ω$_{w}$} (N/m$^2$)")) 
p12 <- ggplot(surf_wss,aes(vel,wss)) +
  geom_point(shape=1, size=1.5)+
  theme(axis.title = element_text(family="Latin Modern Roman", size=18), axis.text = element_text(family="Latin Modern Roman", size=16)) +
  xlab(TeX("\\textit{U} (m/s)")) +ylab(TeX("\\textit{ω$_{w}$} (N/m$^2$)")) 

#########t=100################
surfile1 <- read.csv("100sur0.csv", header = T)
surfile1 <- filter(surfile1,surfile1$Points.2==0.1)
surfaceData1 <- data.frame(
  x=surfile1$Points.0,
  y=surfile1$Points.1
)
wssfile1 <- read.csv("100ws.csv", header = T)
wssData1 <- data.frame(
  x=wssfile1$Points.0,
  y=wssfile1$wallShearStress.0
)
surface_interp1 <- as.data.frame(approx(surfaceData1$x, surfaceData1$y, xout = wssData1$x))

surf_wss1 <- cbind(surface_interp1,wssData1)
colnames(surf_wss1) <- c("x1", "surf", "x2", "wss")

p2 <- ggplot(surf_wss1,aes(x2,wss)) +
  geom_line(aes(color=surf), size=1.5) +
  scale_color_gradient(low = "blue3", high = "red") +
  labs(color=TeX("\\textit{h}")) +theme(legend.title=element_text(family="Latin Modern Roman", size=18), legend.text = element_text(family="Latin Modern Roman", size=14)) +
  theme(axis.title = element_text(family="Latin Modern Roman", size=18), axis.text = element_text(family="Latin Modern Roman", size=16)) +
  xlab(TeX("\\textit{x} (m)")) +ylab(TeX("\\textit{ω$_{w}$} (m/s)")) 


#########t=200################
surfile3 <- read.csv("200sur0.csv", header = T)
surfile3 <- filter(surfile3,surfile3$Points.2==0.1)
surfaceData3 <- data.frame(
  x=surfile3$Points.0,
  y=surfile3$Points.1
)
wssfile3 <- read.csv("200ws.csv", header = T)
wssData3 <- data.frame(
  x=wssfile3$Points.0,
  y=wssfile3$wallShearStress.0
)
surface_interp3 <- as.data.frame(approx(surfaceData3$x, surfaceData3$y, xout = wssData3$x))

surf_wss3 <- cbind(surface_interp3,wssData3)
colnames(surf_wss3) <- c("x1", "surf", "x2", "wss")

p3 <- ggplot(surf_wss3,aes(x2,wss)) +
  geom_line(aes(color=surf), size=1.5) +
  scale_color_gradient(low = "blue4", high = "red") +
  labs(color=TeX("\\textit{h}")) +theme(legend.title=element_text(family="Latin Modern Roman", size=18), legend.text = element_text(family="Latin Modern Roman", size=14)) +
  theme(axis.title = element_text(family="Latin Modern Roman", size=18), axis.text = element_text(family="Latin Modern Roman", size=16)) +
  xlab(TeX("\\textit{x} (m)")) +ylab(TeX("\\textit{ω$_{w}$} (m/s)")) 

ggplot(surf_wss3,aes(surf,wss)) +
  geom_density_2d_filled()

ggplot(surf_wss1,aes(surf,wss)) +
  geom_density_2d_filled()
