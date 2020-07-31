pacman::p_load(pacman, ggplot2, mgcv,dplyr, MASS, plotly,interp, latex2exp, extrafont,ggpubr,scales,akima
               ,colorspace,egg)

setwd("/run/media/boyuan/KINGSTON/twoPeriods_highFr/t30VP/")

#######wall shear stress#######
dfwss <- read.csv("../29ws.csv", header = T)
p1 <- ggplot(dfwss,aes(Points.0, abs(wallShearStress.0))) +
  geom_line(size=1.5) +
  theme(axis.title = element_text(family="Latin Modern Roman", size=18), axis.text = element_text(family="Latin Modern Roman", size=17)) +
  xlab(TeX("\\textit{x} (m)")) +ylab(TeX("\\textit{$ω_{w}}$ (N/m$^2$)")) +
  scale_y_continuous(limits = c(0,NA), breaks = seq(0,0.25, by=0.05))

#######mean vel#########
mean_vel <- rep(0,101)
for (i in c(0:100)) {
  df <- read.csv(paste0("data_plot_x",i,".csv"), header = T)
  df1 <- dplyr::filter(df, alpha.water>0.5)
  mean_vel[i+1] <- mean(df1$U.0)
}
hn <- 8.0/15.75
df2 <- data.frame(
  x = seq(0,100*hn,by=hn),
  y = mean_vel
)
p2 <- ggplot(df2,aes(x, y)) +
  geom_line(size=1.5) +
  # geom_point() +
  theme(axis.title = element_text(family="Latin Modern Roman", size=18), axis.text = element_text(family="Latin Modern Roman", size=17)) +
  xlab(TeX("\\textit{x} (m)")) +ylab(TeX("\\textit{$\\bar{U}$ (m/s)")) +
  scale_y_continuous(limits = c(0,NA))

######cf#######
wss_interp <- as.data.frame(approx(dfwss$Points.0, dfwss$wallShearStress.0, xout = seq(0,100*hn,by=hn))) %>% abs
cf <- 2*wss_interp$y/(mean_vel^2) ## for OF data output (incompressible flow)
df3 <- data.frame(
  x = seq(0,100*hn,by=hn),
  y = cf
)
p3 <- ggplot(df3,aes(x, y)) +
  geom_line(size=1.5) +
  # geom_point() +
  theme(axis.title = element_text(family="Latin Modern Roman", size=18), axis.text = element_text(family="Latin Modern Roman", size=17)) +
  xlab(TeX("\\textit{x} (m)")) +ylab(TeX("\\textit{$c_f$")) +
  scale_y_continuous(limits = c(0,NA))

####multi-plot######
margin_val <- c(2.5,0.0,0.5,0.5)
# egg::ggarrange(p1+theme(plot.margin = unit(margin_val, "lines")) ,
#           p2+theme(plot.margin = unit(margin_val, "lines")) ,
#           p3+theme(plot.margin = unit(margin_val, "lines")) ,
#           ncol=1,nrow=3,labels = c("(a) Wall shear stess", "(b) Mean Velocity", "(c) Bed-friction coefficient")
#           ,font.label = list(family="Latin Modern Roman",size=16) ,hjust = -0.5, vjust = 1.5,align = "h"
# )

egg::ggarrange(p1+theme(plot.margin = unit(margin_val, "lines")) ,
               p2+theme(plot.margin = unit(margin_val, "lines")) ,
               p3+theme(plot.margin = unit(margin_val, "lines")) ,
               ncol=1,nrow=3 ,
               labels = c("(a) Wall shear stress", "(b) Mean Velocity", "(c) Bed-friction coefficient") ,
               label.args = list(gp = grid::gpar(fontfamily="Latin Modern Roman", font=2, fontsize=16)) 
               # ,font.label = list(family="Latin Modern Roman",size=16) ,hjust = -0.5, vjust = 1.5,align = "h"
)

