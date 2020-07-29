pacman::p_load(pacman, ggplot2, mgcv,dplyr, MASS, plotly,interp, latex2exp, extrafont,ggpubr,scales,akima)

setwd("/home/boyuan/OpenFOAM/boyuan-7/run/78S005_q8_U1575/100hnOnePeriod")
#######Uncolored paths############
########### t=50 ##############
file1 <- read.csv("t40peak.csv", header = T)
xc <- c(0,file1$Points.1[11])
yc <- c(0,file1$U.0[11])
file1$U.0[1] <- 0
file1$U.0[c(2:10)] <- approx(xc,yc,xout = file1$Points.1[c(2:10)])$y
p <- ggplot() +
  geom_path(data=file1, aes(Points.1, U.0, color = "t=50"),size=1.5) 


########### t=100 ##############
file2 <- read.csv("t100peak.csv", header = T)
xc <- c(0,file2$Points.1[11])
yc <- c(0,file2$U.0[11])
file2$U.0[1] <- 0
file2$U.0[c(2:10)] <- approx(xc,yc,xout = file2$Points.1[c(2:10)])$y
p <- p +
  geom_path(data=file2, aes(Points.1, U.0, color = "t=100"),size=1.5) 

########### t=200 ##############
file3 <- read.csv("t150peak.csv", header = T)
xc <- c(0,file3$Points.1[11])
yc <- c(0,file3$U.0[11])
file3$U.0[1] <- 0
file3$U.0[c(2:10)] <- approx(xc,yc,xout = file3$Points.1[c(2:10)])$y
p <- p +
  geom_path(data=file3, aes(Points.1, U.0, color = "t=200"),size=1.5) 

########### t=300 ##############
file4 <- read.csv("t200peak.csv", header = T)
xc <- c(0,file4$Points.1[11])
yc <- c(0,file4$U.0[11])
file4$U.0[1] <- 0
file4$U.0[c(2:10)] <- approx(xc,yc,xout = file4$Points.1[c(2:10)])$y
p <- p +
  geom_path(data=file4, aes(Points.1, U.0, color = "t=300"),size=1.5) 


p <- p+ theme(axis.title = element_text(family="Latin Modern Roman", size=18), axis.text = element_text(family="Latin Modern Roman", size=16)) +
  xlab(TeX("\\textit{y} (m)")) +ylab(TeX("\\textit{$U_x$} (m/s)")) +
  geom_hline(yintercept = 7.5, size=1.75, colour = "red", lty=2)
  


setwd("/run/media/boyuan/KINGSTON/twoPeriods_highFr")

###################Plan B colored########################
########### t=50 ##############
file1 <- read.csv("t30peak.csv", header = T)
xc <- c(0,file1$Points.1[11])
yc <- c(0,file1$U.0[11])
file1$U.0[1] <- 0
file1$U.0[c(2:10)] <- approx(xc,yc,xout = file1$Points.1[c(2:10)])$y
p <- ggplot() +
  geom_path(data=file1, aes(Points.1, U.0, color = alpha.water),size=1.5) 

########### t=100 ##############
file2 <- read.csv("t100peak.csv", header = T)
xc <- c(0,file2$Points.1[11])
yc <- c(0,file2$U.0[11])
file2$U.0[1] <- 0
file2$U.0[c(2:10)] <- approx(xc,yc,xout = file2$Points.1[c(2:10)])$y
p <- p +
  geom_path(data=file2, aes(Points.1, U.0, color = alpha.water),size=1.5) 

########### t=200 ##############
file3 <- read.csv("t150peak.csv", header = T)
xc <- c(0,file3$Points.1[11])
yc <- c(0,file3$U.0[11])
file3$U.0[1] <- 0
file3$U.0[c(2:10)] <- approx(xc,yc,xout = file3$Points.1[c(2:10)])$y
p <- p +
  geom_path(data=file3, aes(Points.1, U.0, color = alpha.water),size=1.5) 

########### t=300 ##############
file4 <- read.csv("t200peak.csv", header = T)
xc <- c(0,file4$Points.1[11])
yc <- c(0,file4$U.0[11])
file4$U.0[1] <- 0
file4$U.0[c(2:10)] <- approx(xc,yc,xout = file4$Points.1[c(2:10)])$y
p <- p +
  geom_path(data=file4, aes(Points.1, U.0, color = alpha.water),size=1.5) 

###add axis labels and initial vel############
p <- p+ theme(axis.title = element_text(family="Latin Modern Roman", size=18), axis.text = element_text(family="Latin Modern Roman", size=16)) +
  xlab(TeX("\\textit{y} (m)")) +ylab(TeX("\\textit{$U_x$} (m/s)")) +
  geom_hline(yintercept = 7.5, size=1.75, colour = "red", lty=2)
###add colorbar title##########
p <- p+labs(color=TeX("\\textit{α$_{water}$}")) +theme(legend.title=element_text(family="Latin Modern Roman", size=16), legend.text = element_text(family="Latin Modern Roman", size=14))


###########Eight Periods##############
setwd("/run/media/boyuan/KINGSTON/T8_lowFr")

########### t=50 ##############
file1 <- read.csv("t50peak.csv", header = T)
xc <- c(0,file1$Points.1[11])
yc <- c(0,file1$U.0[11])
file1$U.0[1] <- 0
file1$U.0[c(2:10)] <- approx(xc,yc,xout = file1$Points.1[c(2:10)])$y
p <- ggplot() +
  geom_path(data=file1, aes(Points.1, U.0, color = alpha.water),size=1.5) 

########### t=100 ##############
file2 <- read.csv("t100peak.csv", header = T)
xc <- c(0,file2$Points.1[11])
yc <- c(0,file2$U.0[11])
file2$U.0[1] <- 0
file2$U.0[c(2:10)] <- approx(xc,yc,xout = file2$Points.1[c(2:10)])$y
p <- p +
  geom_path(data=file2, aes(Points.1, U.0, color = alpha.water),size=1.5) 
 

########### t=160 ##############
file3 <- read.csv("t160peak.csv", header = T)
xc <- c(0,file3$Points.1[11])
yc <- c(0,file3$U.0[11])
file3$U.0[1] <- 0
file3$U.0[c(2:10)] <- approx(xc,yc,xout = file3$Points.1[c(2:10)])$y
p <- p +
  geom_path(data=file3, aes(Points.1, U.0, color = alpha.water),size=1.5) 

###add axis labels and initial vel############
p <- p+ theme(axis.title = element_text(family="Latin Modern Roman", size=18), axis.text = element_text(family="Latin Modern Roman", size=16)) +
  xlab(TeX("\\textit{y} (m)")) +ylab(TeX("\\textit{$U_x$} (m/s)")) +
  geom_hline(yintercept = 7.5, size=1.75, colour = "red", lty=2)
###add colorbar title##########
p <- p+labs(color=TeX("\\textit{α$_{water}$}")) +theme(legend.title=element_text(family="Latin Modern Roman", size=16), legend.text = element_text(family="Latin Modern Roman", size=14))


###########Four Periods##############
setwd("/run/media/boyuan/KINGSTON/T4_lowFr")

########### t=50 ##############
file1 <- read.csv("t50peak.csv", header = T)
xc <- c(0,file1$Points.1[11])
yc <- c(0,file1$U.0[11])
file1$U.0[1] <- 0
file1$U.0[c(2:10)] <- approx(xc,yc,xout = file1$Points.1[c(2:10)])$y
p <- ggplot() +
  geom_path(data=file1, aes(Points.1, U.0, color = alpha.water),size=1.5) 

########### t=100 ##############
file2 <- read.csv("t100peak.csv", header = T)
xc <- c(0,file2$Points.1[11])
yc <- c(0,file2$U.0[11])
file2$U.0[1] <- 0
file2$U.0[c(2:10)] <- approx(xc,yc,xout = file2$Points.1[c(2:10)])$y
p <- p +
  geom_path(data=file2, aes(Points.1, U.0, color = alpha.water),size=1.5) 


########### t=160 ##############
file3 <- read.csv("t200peak.csv", header = T)
xc <- c(0,file3$Points.1[11])
yc <- c(0,file3$U.0[11])
file3$U.0[1] <- 0
file3$U.0[c(2:10)] <- approx(xc,yc,xout = file3$Points.1[c(2:10)])$y
p <- p +
  geom_path(data=file3, aes(Points.1, U.0, color = alpha.water),size=1.5) 

###add axis labels and initial vel############
p <- p+ theme(axis.title = element_text(family="Latin Modern Roman", size=18), axis.text = element_text(family="Latin Modern Roman", size=16)) +
  xlab(TeX("\\textit{y} (m)")) +ylab(TeX("\\textit{$U_x$} (m/s)")) +
  geom_hline(yintercept = 7.5, size=1.75, colour = "red", lty=2)
###add colorbar title##########
p <- p+labs(color=TeX("\\textit{α$_{water}$}")) +theme(legend.title=element_text(family="Latin Modern Roman", size=16), legend.text = element_text(family="Latin Modern Roman", size=14))
