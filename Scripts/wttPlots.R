library(openxlsx)
library(ggplot2)
library(ggpubr)

# Snake River spring summer Chinook adult only SARs ---------------------------------
wttTS.dat <- read.xlsx("Data\\SnakeRiverWTT_1929-2022.xlsx",
                             sheet = 1,
                             colNames = TRUE)

str(wttTS.dat)

mean(subset(wttTS.dat, year>=1975 & year<=max(wttTS.dat$year))[,2])

wttTS.plot<-ggplot() +
  theme_bw()+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title.y = element_text(face = "bold", size = 14,margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 10)),
        axis.title.x = element_text(face = "bold", size = 14,margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.text.x = element_text(face = "bold",color = "black",size = 12),
        axis.text.y = element_text(face = "bold",size = 12,color = "black"),
        plot.title = element_text(hjust = 0.5,size = 18,face = "bold"),
        axis.ticks.length = unit(3,"mm"),
        legend.position = "none")+
  labs(y = "Water Transit Time (days)",x = "Migration Year") +
  geom_line(aes(year,wtt),color = "black",size = 1.2, data = wttTS.dat)+
  geom_bracket(xmin = min(wttTS.dat$year), xmax = 1938, y.position = 8,type = "text",
               label = paste("~ ",round(mean(subset(wttTS.dat, year>=1925 & year<=1938)[,2]),0), " days",sep=""),
               tip.length = c(0.02, 0.02))+
  geom_bracket(xmin = 1938, xmax = 1975, y.position = 30,type = "text",
               label = "dam construction",
               tip.length = c(0.02, 0.02))+
  geom_bracket(xmin = 1975, xmax = max(wttTS.dat$year), y.position = 42,type = "text",
               label = paste("~ ", round(mean(subset(wttTS.dat, year>=1975 & year<=max(wttTS.dat$year))[,2]),0), " days",sep=""),
               tip.length = c(0.02, 0.02))+
  geom_smooth(aes(year,wtt),method = "gam",formula = y~s(x,k=5),color = "#0072B2",data = wttTS.dat)+
  scale_x_discrete(limits=c(seq(min(wttTS.dat$year),max(wttTS.dat$year),10)))+
  scale_y_continuous(limits=c(-1,45),breaks = seq(-1,45,5), expand = c(0,0))

png(filename=paste("Output\\Figures\\wttTSplot_wFit",".png",sep=""),
    type="cairo",
    units="in",
    width=8,
    height=5,
    res=300)

print(wttTS.plot)
dev.off()
print(wttTS.plot)

