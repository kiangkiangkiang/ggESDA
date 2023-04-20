Sweave("ggESDA_Jiang&Wu_20210915.Rnw")
tools::texi2pdf("ggESDA_Jiang&Wu_20210915.tex")
############

#ref????????????~ 
dim(Cardiological)
library(ggESDA)
library(ggplot2)
library(grid)
library(gridExtra)
reinstall <- function(){
  rm(list=ls())
  detach("package:ggESDA", unload=TRUE)
  remove.packages("ggESDA")
  dir<-"C:/Users/user/Desktop/NTPU/PAPER/myStudy/Produce_R_Package/PackageMaking/"
  install.packages(paste0(dir,"ggESDA_0.1.0.zip"), repos = NULL, type="source")
  
  library(ggESDA)
}
reinstall()


#Fig:compare
myDiamonds <- diamonds
set.seed(20211020)
myDiamonds.i  <- classic2sym(myDiamonds)$intervalData
a <- ggplot(myDiamonds, aes(x = carat, y = price))+
  geom_point()
#RColorBrewer::display.brewer.all()
myCol <- rev(RColorBrewer::brewer.pal(5, "Blues"))
myDiamonds.i <- myDiamonds.i[c(1, 2, 3, 5, 4),]

b <- ggInterval_scatter(myDiamonds.i, aes(x = carat, y = price)) + 
  scale_fill_manual(values = myCol, 
                    name="Kmeans-Group",
                    label=1:5);b

grid.arrange(a, b, ncol=2)
marrangeGrob(list(a, b), ncol = 2, nrow = 1)
#plot_grid(a, b, ncol = 2, rel_heights = c(1/4, 1/4, 1/2))
grid.arrange(a, b, ncol=2, widths=c(1.5, 2))
#end Fig:compare

#show_breastData_attr
breastData <- data.table::fread("doc/data.csv")
dim(breastData)
colnames(breastData)
#end show_breastData_attr


#classic2sym_kmeans
breastData <- dplyr::select(breastData, -id)
breastData.sym <- classic2sym(breastData, groupby = "kmeans", k = 5)
breastData.sym.i <- breastData.sym$intervalData
head(breastData.sym.i[, 1:4], 5)
#end classic2sym_kmeans

#classic2sym_hclust
breastData.sym <- classic2sym(breastData, groupby = "hclust")
breastData.sym.i <- breastData.sym$intervalData
#end classic2sym_hclust

#classic2sym_hclust
breastData.sym <- classic2sym(breastData, groupby = "hclust")
breastData.sym.i <- breastData.sym$intervalData
#end classic2sym_hclust

#classic2sym_parVar
breastData.sym <- classic2sym(breastData, groupby = "diagnosis")
breastData.sym.i <- breastData.sym$intervalData
head(breastData.sym.i[, 1:4], 5)
#end classic2sym_parVar



#classic2sym_userDefined
minData <- runif(100, -100, -50)
maxData <- runif(100, 50, 100)
demoData <- data.frame(min = minData, max = maxData)
demoData.sym <- classic2sym(demoData, groupby = "customize", 
                            minData = demoData$min,
                            maxData = demoData$max)

demoData.sym.i <- demoData.sym$intervalData
as.data.frame(head(demoData.sym.i, 5))
#end classic2sym_userDefined



#HistDAWass_getData
library(HistDAWass)

# Get min and max data
blood.min <- get.MatH.stats(BLOOD, stat = "min")
blood.max <- get.MatH.stats(BLOOD, stat = "max")
blood <- data.frame(blood.min, blood.max)

# Reorganized and Build ggESDA obj.
blood.sym <- classic2sym(blood, groupby = "customize",
                     minData = blood[, 2:4],
                     maxData = blood[, 6:8])

# Make names
blood.names <- get.MatH.main.info(BLOOD)$varnames
blood.i <- blood.sym$intervalData
colnames(blood.i) <- blood.names
head(as.data.frame(blood.i), 5)


#end HistDAWass_getData


#MAINT.Data_getData
library(MAINT.Data)
library(tibble)
#get data interval-valued data in AbaloneIdt
Aba.range <- exp(AbaloneIdt@LogR)
Aba.mid <- AbaloneIdt@MidP


#make a necessary transformation for build min max data
Aba <- data.frame(Aba.min = Aba.mid - Aba.range / 2,
                  Aba.max = Aba.mid + Aba.range / 2)


# Reorganized and Build ggESDA obj.
Aba.sym<- classic2sym(Aba, groupby = "customize",
                      minData = Aba[, 1:7],
                      maxData = Aba[, 8:14])


# Make names
colnames(Aba.sym$intervalData) <- AbaloneIdt@VarNames
Aba.i <- Aba.sym$intervalData %>% 
  cbind(Aba.obs = AbaloneIdt@ObsNames) %>% 
  column_to_rownames(var = "Aba.obs")

head(Aba.i[, 1:4], 5)
#end MAINT.Data







#testtttttttttttttttttttttttttt
#symbolicDA ??????????????????example
library(HistDAWass)
#get the second variable and its histogram data
BLOOD[,2]@M[[1]]
abc<- HistDAWass::data2hist(iris[,3])
edf<- HistDAWass::data2hist(iris[,2])
?HistDAWass::register
mydist1 <- new("distributionH", c(1, 2, 3), c(0, 0.4, 1))
class(HistDAWass::plot(abc))

a<- get.MatH.stats(BLOOD, stat = "min")
b<- get.MatH.stats(BLOOD, stat = "max")
d <- data.frame(a, b)
d.sym <- classic2sym(d, groupby = "customize",
            minData = d[,2:4],
            maxData = d[,6:8])
mycolNames <- get.MatH.main.info(BLOOD)$varnames
d.i <- d.sym$intervalData
colnames(d.i) <- mycolNames
d.i



MAINT.Data::AbaloneIdt
str(MAINT.Data::AbaloneIdt)
#???????????? @midp
#???????????????????????????
#???range @LogR
MAINT.Data::AbaloneIdt[,1]@LogR
#MAINT.Data::AbaloneIdt$Shucked_weight


myrange <- exp(MAINT.Data::AbaloneIdt@LogR)
mymid <- MAINT.Data::AbaloneIdt@MidP
mymin <- mymid - myrange/2
mymax <- mymid + myrange/2
d<-data.frame(mymin,mymax)
mynames <- MAINT.Data::AbaloneIdt@VarNames
myRnames <- MAINT.Data::AbaloneIdt@ObsNames
d.sym<- classic2sym(d,groupby = "customize",
            minData = d[,1:7],
            maxData = d[,8:14])
d.i <- d.sym$intervalData
colnames(d.i) <- mynames
rownames(d.i)<-myRnames
iData<-tibble::rownames_to_column(d.i)
iData<-tibble::column_to_rownames(iData, var = "rowname")

#end testttttttttttttt

?get.MatH.stats
a <- classic2sym(mtcars,groupby=c("cyl","vs"))
a$intervalData


#install.packages("BiocManager")
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("EBImage")
#install.packages("EBImage")
library(EBImage) # (Repositories: BioC Software)
a <- readImage("doc/packageStructure.png")
image(a)



a <- HistDAWass::BLOOD
HistDAWass::plot(a)
HistDAWass::WH.plot_multiple_indivs(a)
??HistDAWass::plot
mydist <- distributionH(x = c(7, 8, 10, 15), p = c(0, 0.2, 0.7, 1))
# show the histogram
plot(mydist) # plots mydist
plot(mydist, type = "HISTO", col = "red", border = "blue") # plots mydist
plot(mydist, type = "DENS", col = "red", border = "blue") # plots a density approximation for mydist
plot(mydist, type = "HBOXPLOT") # plots a horizontal boxplot for mydist
plot(mydist, type = "VBOXPLOT") # plots a vertical boxplot for mydist
plot(mydist, type = "CDF") # plots the cumulative distribution function of mydist
plot(mydist, type = "QF") # plots the quantile function of mydist

library(ggESDA)
iris.i<-classic2sym(iris)$intervalData
rownames(facedata)
ggInterval_radar(iris.i,plotPartial = 1)
s <- rep(c("FRA", "HUS", "INC", "ISA", "JPL", "KHA",
           "LOT", "PHI", "ROM"), each = 3)
p <- ggInterval_PCA(facedata, poly = T,
                    concepts_group = s);p
p$ggplotPCA + theme_bw()


install.packages("ISDA.R")



###############################################EDA start
library(ggESDA)
dataSetList <- list(AbaloneIdt = AbaloneIdt, BLOOD = BLOOD,
                    Cardiological = Cardiological,
                    facedata = facedata,
                    oils = oils,
                    mushroom = mushroom)

lapply(dataSetList, FUN = dim)

#1
# Cap.Widths  Stipe.Lengths Stipe.Thicknesses
ggInterval_boxplot(mushroom, aes(Cap.Widths))
reinstall()


ggInterval_2DhistMatrix(Cardiological)
ggInterval_2DhistMatrix(BLOOD)
ggInterval_2DhistMatrix(mushroom)
ggInterval_indexImage(AbaloneIdt, useHeatmap = T,
                      full_strip = T,
                      column_condition = T)
ggInterval_indexImage(mtcars.i, useHeatmap = T,
                      full_strip = T,
                      column_condition = F)
ggInterval_index(Cardiological, aes(Pulse, fill = "blue", alpha=0.5))

a <- ggInterval_scaMatrix(oils)
a + ggplot2::geom_smooth(data = . %>% filter(isPlot),
                         method = "lm",
                         se = T,
                         alpha = 0.3,
                         level = 0.8)+
  theme_bw()

ggInterval_minmax(data = oils, aes(SAP,
                                   size = 3),
                  scaleXY = "global")+
  scale_color_manual(values=c("black","green"))

ggInterval_centerRange(mushroom, aes(Cap.Widths))

ggInterval_scatter(oils, aes(SAP, IOD))


dim(oils)
oils
rownames(oils)

Environment <- a

#usethis::use_data(Environment, Environment)
save(Environment, file = "Environment.rda")
myLibrary <- c("ggESDA", "ggplot2", "ggthemes",
               "data.table", "grid", "gridExtra",
               "tibble", "stringr", "dplyr",
               "extrafont", "ggpubr",
               "RColorBrewer")
lapply(myLibrary, require, character.only = TRUE)

# pdf(file="test_123.pdf")
# 
# dev.off()
#ggsave("aaa.pdf")


a <- oils
a[[2]][1] <- "[-27.00 : -8.00]"
bbb <- min(a[[2]])
bbb[5] <- -21
s <- data.frame(mi = bbb,
           ma = max(a[[2]]))

s
b2 <- classic2sym(s, "customize",
            minData = s$mi,
            maxData = s$ma)$intervalData
a[, 2] <- data.frame(b2)
a

rownames(a) <- c("linseed", "perilla", "cotton", "sesame",
                 "camellia", "olive", "beef", "hog")

rownames(a)
oils <- a
usethis::use_data(oils, oils)

save(oils, file = "oils.rda")
#######################################################EDA end






\begin{figure}[h]
\centering
\subfloat[Column condition]{\includegraphics[scale=0.1]{ggESDA_Jiang&Wu_20210915-imdexImage_col.pdf
}}
\subfloat[Matrix condition]{\includegraphics[scale=0.1]{ggESDA_Jiang&Wu_20210915-imdexImage_mat.pdf
}}
\caption{\label{fig:imdexImage} Index image - heatmap}
\end{figure}


ggInterval_hist(Cardiological, plotAll = T,
                method = "equal-bin",
                bins = 30)

mushroom
ggInterval_centerRange(b$intervalData, plotAll = T)
ggInterval_boxplot(b$intervalData, plotAll = T)

dim(mushroom)
b <- scale_sym_table(mushroom)
ggInterval_2Dhist(Cardiological, aes(x = Syst, y = Diast),
                  xBins = 6,
                  yBins = 4)


colnames(BLOOD)
ggInterval_2Dhist(BLOOD, aes(x = Cholesterol, y = Hemoglobin),
                  xBins = 6,
                  yBins = 5)
a = data.frame(x=rnorm(20000, 10, 1.9), y=rnorm(20000, 10, 1.2) )
b = data.frame(x=rnorm(20000, 14.5, 1.9), y=rnorm(20000, 14.5, 1.9) )
c = data.frame(x=rnorm(20000, 9.5, 1.9), y=rnorm(20000, 15.5, 1.9) )
data = rbind(a,b,c)

#Lines
pp = ggplot(data, aes(x=x, y=y)) +
  geom_hex(bins = 20, size = 0.5, color = "black") +
  scale_fill_viridis_c(option = "C")

(p <- plot_gg(pp, width = 4, height = 4, scale = 300, multicore = TRUE))
#remotes::install_github("tylermorganwall/rayshader")
library(rayshader)
mtplot = ggplot(mtcars) + 
  geom_point(aes(x=mpg,y=disp,color=cyl)) + 
  scale_color_continuous(limits=c(0,8)) 
plot_gg(mtplot, width=3.5, sunangle=225, preview = TRUE)


?RSDA::SODAS.to.RSDA()
RSDA::sodas.ex1


?write.sym.table()
data(example1)
write.sym.table(example1, file = "temp4.csv", sep = "|",
                dec = ".", row.names = F, col.names = TRUE)

a <- read.sym.table("C:\\Users\\user\\Desktop\\NTPU\\PAPER\\myStudy\\doc\\environmentData.csv", sep = "|",
                    dec = ".")
a

aaa<-"$M|URBANICITY|6|4|5|1|3|2|$M|INCOMELEVEL|25|50|75|90|100|$M|EDUCATION||1|3|5|6|$M|REGIONDEVELOPME|4|3|2|1|$I|CONTROL|CONTROL|$I|SATISFY||SATISFY|$I|INDIVIDUAL|INDIVIDUAL|$I|WELFARE|WELFARE|$I|HUMAN|HUMAN|$I|POLITICS|POLITICS|$I|BURDEN|BURDEN|$I|NOISE|NOISE|$I|NATURE|NATURE|$I|SEASETC|SEASETC|$I|MULTI|MULTI|$I|WATERWASTE||WATERWASTE|$I|VEHICLE|VEHICLE"


# 
# 
# In this field, it reveals the trend, distribution, how large the data spread, etc, for a particular variable. Moreover, it's worth mentioning that there will be some advanced graphics implemented which are called the min-max plot, and center-range plot due to the characteristics of interval-valued data. They help researchers to be able to grasp the relationship between center and range, and the difference of range in data. 
# 
# ?ggInterval_minmax()
# ggInterval_boxplot()
# ggInterval_centerRange()
# ?ggInterval_indexImage
# 
# 
# 
# data.frame(matrix(0, nrow = 6, ncol = 4))
# 
# \begin{itemize}
# \item \emph{Mushroom data}: 
#   \item \emph{Face recognition data}: 
#   \item \emph{Oils data}: 
#   \item \emph{Blood pressures data}: 
#   \item \emph{Environment data}: 
#   \end{itemize}

##############################################################
library(ggthemes)
p <- ggInterval_radar(Environment, plotPartial = c(4,6),
                      showLegend = F,
                      base_circle = F,
                      base_lty = 1,
                      addText = F) +
  scale_fill_manual(values = c("darkred", "darkblue")) +
  scale_color_manual(values = c("darkred", "darkblue")) +
  labs(title = "") +
  theme_hc()
p2 <- ggInterval_radar(Environment, plotPartial = c(4,6),
                      showLegend = F,
                      base_circle = F,
                      base_lty = 1,
                      addText = F,
                      type = "rect") +
  scale_fill_manual(values = c("darkred", "darkblue")) +
  scale_color_manual(values = c("darkred", "darkblue")) +
  labs(title = "") +
  theme_hc();p2
gridExtra::marrangeGrob(list(p, p2), nrow = 1, ncol = 2, top = "")
# Roughly, we classify these datasets into three parts by their
#dimension, N > P, N ≃ P, and N < P.

library(ggESDA)
Cardiological
dim(Cardiological)

HistDAWass::BloodBRITO
ggInterval_3Dscatter(facedata, aes(AD, BC, AH),
                     scale = T)

############
this.data.2 <- Environment
min(this.data.2[[5]])
temp <- lapply(5:17, FUN = function(x) round(cbind(min(this.data.2[[x]]),
                                                   max(this.data.2[[x]])), 2))
temp[[1]]
temp2 <- lapply(temp, FUN = function(x) classic2sym(data.frame(x),
                                                    "customize",
                                                    minData = x[, 1],
                                                    maxData = x[, 1])$intervalData )
temp3 <- matrix(0, nrow = 14, ncol = 1)
for(i in 1:13){
  temp3 <- cbind(temp3, temp2[[i]])
}
temp3 <- as.data.frame(temp3[, -1])
colnames(temp3) <- colnames(Environment)[5:17]
class(temp3) <- c("symbolic_tbl", class(temp3))
Environment.n <- temp3
myFill <- c("gray0", "gray10", "gray20",
            "gray30", "gray40", "gray50",
            "gray60", "gray70", "gray80",
            "gray90", "gray100")
pList <- NULL
u <- 1
for(i in useDataSet){
  p <- ggInterval_radar(i,
                        plotPartial = 1,
                        base_circle = F,
                        base_lty = 1,
                        type = "quantile",
                        quantileNum = 10,
                        showLegend = F,
                        Drift = 0)+
    labs(title = names(useDataSet)[u]) +
    scale_fill_manual(values = rev(myFill)) +
    theme_hc()
  pList[[u]] <- p
  print(u)
  u <- u + 1
}

############plot plot plot#################
library(ggESDA)

library(ggpubr)
library(gridExtra)
#set para
this.data <- facedata
p <- dim(this.data)[2]

#### mmplot #####
ggInterval_minmax(this.data, aes(size = 2), plotAll = T)+
  scale_color_manual(values=c("darkblue","darkred"))+
  guides(colour = F)+theme_bw() + coord_fixed(ratio = 1)

plotLocal <- NULL
for(i in 1:p){
  plotLocal[[i]] <- ggInterval_minmax(this.data, aes(this.data[[i]], size = 2))+
    scale_color_manual(values=c("darkblue","darkred"))+
    guides(colour = F)+theme_bw() + coord_fixed(ratio = 1)
}
a <- ggarrange(plotlist = plotLocal, nrow = 1, ncol = p, labels = "") 


plotGlobal <- NULL
for(i in 1:p){
  plotGlobal[[i]] <- ggInterval_minmax(this.data, aes(this.data[[i]], size = 2),
                                       scaleXY = "global")+
    scale_color_manual(values=c("darkblue","darkred"))+
    guides(colour = F)+theme_bw() + coord_fixed(ratio = 1)
}
b <- ggarrange(plotlist = plotGlobal, nrow = 1, ncol = p, labels = "")
marrangeGrob(list(a, b), nrow = 2, ncol = 1, top = "")
#ggarrange(a, b)
#ggarrange(plotlist = c(plotLocal, plotGlobal), nrow = 3, ncol = 4)

#### boxplot centerRange #####
seed(20211216)
a <- ggInterval_boxplot(this.data, plotAll = T) +
  theme_classic()+
  theme(legend.position = "none",
        axis.text.x = element_blank())

b <- ggInterval_centerRange(this.data, aes(size = 1.5),
                           plotAll = T)+
  theme_classic()+ stat_ellipse(geom = "polygon", fill = "blue",
                                alpha = 0.05, col = "grey")
#gridExtra::grid.arrange(a, b, ncol=1, nrow = 2)


this.data.scale <- scale_sym_table(this.data)
c <- ggInterval_boxplot(this.data.scale, plotAll = T) +
  theme_classic()+
  theme(legend.position = "bottom",
        axis.text.x = element_blank())+
  scale_y_continuous(breaks = c(-2:2), 
                     labels = c("-2.0", "-1.0", "0", "1.0", "2.0"));c


d <- ggInterval_centerRange(this.data.scale, aes(size = 1.5),
                           plotAll = T)+
  theme_classic()+ stat_ellipse(geom = "polygon", fill = "blue",
                                alpha = 0.05, col = "grey")
#gridExtra::grid.arrange(c, d, ncol=1, nrow = 2)


gridExtra::grid.arrange(a, c, ncol=1, nrow = 2)
gridExtra::grid.arrange(b, d, ncol=1, nrow = 2)
#end plot
#### histplot #####
ggInterval_hist(this.data, plotAll = T, bins = 30) +
  theme_hc()+
  scale_y_continuous(breaks = c(0, 0.025, 0.05))+
  labs(title = "")

ggInterval_hist(this.data, plotAll = T,
                method="unequal-bin") +
  theme_hc()+
  scale_y_continuous(breaks = c(0, 0.2, 0.4))+
  labs(title = "")

#### scatterplot #####
#temp <- rep(brewer.pal(9, "Set1"), each = 3)
#temp2 <- rep(brewer.pal(9, "Paired"), each = 3)

temp <- rep(brewer.pal(9, "Set1"), each = 3)
ggInterval_scaMatrix(this.data, aes(alpha = 0.4))+
  scale_fill_manual(values = temp)+
  theme_bw()+
  theme(plot.title = element_text(size=14),
        legend.position = "bottom",
        legend.key.size = unit(0.4, 'cm'),
        legend.key.height = unit(0.4, 'cm'),
        legend.key.width = unit(0.4, 'cm'),
        legend.title = element_text(size=8.5),
        legend.text = element_text(size=6.5))+
  guides(fill=guide_legend(nrow=2,byrow=TRUE))

# ggInterval_scaMatrix(this.data)+
#   theme_bw()+
#   theme(legend.position = "none") +
#   geom_smooth(method = "lm",
#               col = "red",
#               se = 0.8,
#               alpha = 0.2,
#               fill = "blue")
#### 2Dhist #####
p <- ggInterval_2Dhist(this.data, aes(this.data[[1]], this.data[[2]], 
                                 col = "grey30"),
                  xBins = 25,
                  yBins = 25)+ theme_light() + labs(fill='Frequency',
                                                    title="")+
  theme(legend.position = "bottom")
p <- p + coord_fixed(ratio=1)
marrangeGrob(list(p), nrow = 1, ncol = 1, top = "")
#### 2DhistMat #####
facedata.scale <- scale_sym_table(facedata)
p_mat <- ggInterval_2DhistMatrix(facedata, aes(col = "grey50"),
                  xBins = 10,
                  yBins = 10,
                  removeZero = T,
                  addFreq = F)

p_mat <- p_mat + theme_light() + 
  labs(title="") +
  theme(legend.position = "bottom")

marrangeGrob(list(p_mat), nrow = 1, ncol = 1, top = "")
#### radar_typecial, with circle, no text, no nominal####

Environment.n <- Environment[, 5:17]
p1 <- ggInterval_radar(Environment.n, 
                 plotPartial = 2,
                 showLegend = F,
                 base_circle = T,
                 base_lty = 2,
                 addText = F
                 ) +
  labs(title = "") +
  theme_hc() +
  scale_fill_manual(values = c("gray50")) +
  scale_color_manual(values = c("red")) 

p2 <- ggInterval_radar(Environment, 
                 plotPartial = 2,
                 showLegend = F,
                 base_circle = F,
                 base_lty = 1,
                 addText = T
) +
  labs(title = "") +
  theme_hc() +
  scale_fill_manual(values = c("gray50")) +
  scale_color_manual(values = c("gray50")) 

gridExtra::marrangeGrob(list(p1, p2), nrow = 1, ncol = 2, top = "")


#### radar_3obs, no circle, add text, no nominal####
#showN <- dim(Environment)[1]
showN <- 3
p <- NULL

for(i in 1:showN){
  p[[i]] <- ggInterval_radar(Environment, 
                       plotPartial = i,
                       showLegend = F,
                       base_circle = T,
                       base_lty = 2,
                       addText = F
      ) +
        labs(title = "") +
        theme_hc() +
        scale_fill_manual(values = c("gray50")) +
        scale_color_manual(values = c("gray30")) 
}
marrangeGrob(p, nrow = 1, ncol = 3, top = "")

#### radar 2 kind #####
p <- ggInterval_radar(Environment, plotPartial = c(4,6),
                      showLegend = F,
                      base_circle = F,
                      base_lty = 1,
                      addText = F) +
  scale_fill_manual(values = c("darkred", "darkblue")) +
  scale_color_manual(values = c("darkred", "darkblue")) +
  labs(title = "") +
  theme_hc()
p2 <- ggInterval_radar(Environment, plotPartial = c(4,6),
                       showLegend = F,
                       base_circle = F,
                       base_lty = 1,
                       addText = F,
                       type = "rect") +
  scale_fill_manual(values = c("darkred", "darkblue")) +
  scale_color_manual(values = c("darkred", "darkblue")) +
  labs(title = "") +
  theme_hc();p2
gridExtra::marrangeGrob(list(p, p2), nrow = 1, ncol = 2, top = "")


#### not put in ggESDA radar 3 kind #####
ggInterval_radar(Environment, plotPartial = c(4,6,14),
                 showLegend = F,
                 base_circle = F,
                 base_lty = 1,
                 addText = F) +
  scale_fill_manual(values = c("gray25", "gray50", "gray75")) +
  scale_color_manual(values = c("gray25", "gray50", "gray75")) +
  labs(title = "") +
  theme_hc()
#### radar quantile #####

myFill <- c("white", "gray20",
            "gray30", "gray40", "gray50",
            "gray60", "gray70", "white",
            "white", "white")
# ggInterval_radar(Environment,
#                  base_circle = F,
#                  base_lty = 1,
#                  type = "quantile",
#                  quantileNum = 9,
#                  showLegend = F,
#                  Drift = 0) +
#   scale_fill_manual(values = rev(myFill)) +
#   scale_colour_manual(values = rev(myFill))+ 
#   theme_hc()


useDataSet <- list(facedata = facedata,
                   Environment = Environment)
pList <- NULL
u <- 1
for(i in useDataSet){
  p <- ggInterval_radar(i,
                        plotPartial = 1,
                        base_circle = F,
                        base_lty = 1,
                        type = "quantile",
                        quantileNum = 9,
                        showLegend = F,
                        Drift = 0)+
    labs(title = names(useDataSet)[u]) +
    scale_fill_manual(values = rev(myFill)) +
    scale_colour_manual(values = rev(myFill))+ 
    theme_hc()
  pList[[u]] <- p
  print(u)
  u <- u + 1
}
marrangeGrob(pList, nrow = 1, ncol = 2, top = "")
#### Example datasets 5 #####
\begin{table}[htbp]
\centering
\caption{Example Symbolic Datasets}
\setlength{\extrarowheight}{6pt}
\begin{tabular}{llll}
\toprule

& \multicolumn{1}{l}{N} & \multicolumn{1}{l}{P} & Description \\

\specialrule{.1em}{.05em}{.05em} 
Face data & 27    & 6     & Six face measures of nine-man (three observations each). \\
Mushroom & 23    & 3     & A range of widths, lengths, and thicknesses of species of mushroom. \\
Blood pressure & 11    & 3     & A range of pulse, systolic, and diastolic during a time unit. \\
Oils  & 8     & 5     & Physicochemical properties of distinct oils. \\
Environment & 14    & 17    & Environmental questionnaire summary. \\
\bottomrule

\end{tabular}%
\label{tab:datasets}%
\end{table}%

Mushroom, Blood pressure \cite{billard:2006}, Oils \cite{ichino:1994}
\ref{tab:datasets}

On the whole, we classify these datasets into three parts by their dimension, $N > P$, $N \simeq P$, and $N < P$, roughly.
Use these properties to observe the performance of the  \pkg{ggESDA} package. 

In table , we may see 


the first three datasets, whose dimension are commonly observed in practice, 

#### Compare  dimonds #####
myCol <- rev(RColorBrewer::brewer.pal(5, "Blues"))
myDiamonds.i <- myDiamonds.i[c(1, 2, 3, 5, 4),]

a <- ggplot(myDiamonds, aes(x = carat, y = price))+
  geom_point() + theme_bw()
b <- ggInterval_scatter(myDiamonds.i, aes(x = carat, y = price)) + 
  scale_fill_manual(values = myCol, 
                    name="Kmeans-Group",
                    label=1:5) + theme_bw()

grid.arrange(a, b, ncol=2, widths=c(1.5, 2))

#### index four #####
m <- mean(facedata$AD)
ggInterval_index(facedata, aes(AD)) +
  theme_bw()+
  scale_y_continuous(breaks = 1:27,
                     labels = rownames(facedata))+
  labs(title = "", y = "")+
  theme(axis.text.y = element_text(size = 6))

ggInterval_index(facedata, aes(x = AD, fill = Concepts))+
  theme_bw() +
  scale_fill_brewer(palette = "Set1")+
  geom_segment(x = m, xend = m, y = 0, yend = 27,
               lty = 2, col = "red", lwd = 1) +
  geom_text(aes(x = m, y = 28), label = "Mean")+
  scale_y_continuous(breaks = 1:27,
                     labels = rownames(facedata))+
  labs(title = "", y = "")+
  theme(axis.text.y = element_text(size = 6),
        legend.key.size = unit(0.5, 'cm'),
        legend.key.height = unit(0.5, 'cm'),
        legend.key.width = unit(0.5, 'cm'),
        legend.title = element_text(size=10),
        legend.text = element_text(size=10))
ggInterval_indexImage(facedata, aes(x = AD))+
  coord_flip()+
  theme_bw()+
  labs(title = "")+
  theme(axis.text.y = element_text(size = 6),
        legend.key.size = unit(0.5, 'cm'),
        legend.key.height = unit(0.5, 'cm'),
        legend.key.width = unit(0.5, 'cm'),
        legend.title = element_text(size=10),
        legend.text = element_text(size=10))+
  guides(col=F)
ggInterval_indexImage(facedata, aes(x = AD),
                      full_strip  = T)+
  coord_flip()+
  theme_bw()+
  labs(title = "")+
  theme(axis.text.y = element_text(size = 6),
        legend.key.size = unit(0.5, 'cm'),
        legend.key.height = unit(0.5, 'cm'),
        legend.key.width = unit(0.5, 'cm'),
        legend.title = element_text(size=10),
        legend.text = element_text(size=10))
#############################

a <- ggInterval_hist(Environment, aes(WELFARE), bins = 10);a

b <- cumsum(a$data$frequency)
d <- data.frame(s=a$data$start,e= a$data$end, f = b)
ggplot(d)+
  geom_rect(aes(xmin = s, ymin = 0, xmax = e, ymax = f))
a
library(ggplot2)
dim(ggplot2::diamonds)
stats::kmeans()
citation("RSDA")
