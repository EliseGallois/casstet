###############   S E T   U P   W O R K S P A C E   #################


rm(list=ls(all=TRUE))

#set working directory
setwd("/Volumes/Seagate Backup Plus Drive")

#download necessary packages
library(graphics)
library(stats)
library(utils)
library(tidyverse)
library(ggplot2)
library(ggmap)
library(esquisse)

#upload all ring width and residual chronologies 
chrono<-read.csv("pca5.csv", header=T)
chronoC<-read.csv("pcaC2.csv", header=T)

#set rownames
chrono2 <- chronoC[,-1]
rownames(chrono2) <- chronoC[,1]
#set active (numeric) columns
chrono.active <- chrono2[1:18, 1:10]


#RES PCA#
library("FactoMineR")
library("factoextra")
res.pca <- PCA(chrono.active, graph = TRUE)
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))
var <- get_pca_var(res.pca)
# Coordinates
head(var$coord)
# Cos2: quality on the factore map
head(var$cos2)
# Contributions to the principal components
head(var$contrib)
# Coordinates of variables
head(var$coord, 4)
fviz_pca_var(res.pca, col.var = "black")
#correlation plot
library("corrplot")
corrplot(var$cos2, is.corr=TRUE)
# Total cos2 of variables on Dim.1 and Dim.2
fviz_cos2(res.pca, choice = "var", axes = 1:2)
# Color by cos2 values: quality on the factor map
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)
# Change the transparency by cos2 values
fviz_pca_var(res.pca, alpha.var = "cos2")
head(var$contrib, 4)
# Create a grouping variable using kmeans
# Create 3 groups of variables (centers = 3)
set.seed(123)
res.km <- kmeans(var$coord, centers = 3, nstart = 25)
grp <- as.factor(res.km$cluster)
# Color variables by groups
fviz_pca_var(res.pca, col.var = grp, 
             palette = c("#0073C2FF", "#EFC000FF", "#868686FF"),
             legend.title = "Cluster",
             repel = TRUE)
#look at individuals
ind <- get_pca_ind(res.pca)
ind
# Coordinates of individuals
head(ind$coord)
# Quality of individuals
head(ind$cos2)
# Contributions of individuals
head(ind$contrib)
fviz_pca_ind(res.pca)
fviz_pca_ind(res.pca, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)
fviz_pca_ind(res.pca, pointsize = "cos2", 
             pointshape = 21, fill = "#E7B800",
             repel = TRUE # Avoid text overlapping (slow if many points)
)
#contribution of individuals
fviz_cos2(res.pca, choice = "ind")
# Total contribution on PC1 and PC2
fviz_contrib(res.pca, choice = "ind", axes = 1:2)
# Create a random continuous variable of length 23,
# Same length as the number of active individuals in the PCA
set.seed(123)
my.cont.var <- rnorm(18)
# Color individuals by the continuous variable
fviz_pca_ind(res.pca, col.ind = my.cont.var,
             gradient.cols = c("blue", "yellow", "red"),
             legend.title = "Cont.Var")
#colour by groups
head(chrono2, 3)

# The variable Species (index = 5) is removed
# before PCA analysis
rwi.pca <- PCA(chrono2[,-17:-26], graph = FALSE)
fviz_pca_ind(res.pca,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = chrono2$country , # color by groups
             palettepalette = "pal",
             addEllipses = TRUE, ellipse.type = "confidence",# Concentration ellipses,
             legend.title = "Groups")

# Add confidence ellipses
fviz_pca_ind(res.pca, geom.ind = "point", col.ind = chrono2$country, 
             palette = "pal",
             addEllipses = TRUE, 
            
             legend.title = "Groups"
)
install.packages("viridis")
library(viridis)

chronoC$country <- factor(chronoC$country, levels = c("Ellesmere",  "Svalbard","Alaska","Greenland","Northern Scandinavia"))
fviz_pca_biplot(res.pca, 
             # Individuals
               geom.ind = "point",
               fill.ind = chronoC$country, col.ind = "black",
               pointshape = 21, pointsize = 2,
               palette = palette(c("blue1","deepskyblue1","gold1","orange1","red1")),
               addEllipses = TRUE, ellipse.type = "confidence",
             ellipse.alpha = 0.5,
               # Variables
               alpha.var ="contrib", col.var = "contrib", 
               repel = TRUE,
              
             legend.title = list(fill = "Location", color = "Contrib",
                                   alpha = "Contrib")
)

pal <- viridisLite::magma(5)

ind.p <- fviz_pca_biplot(res.pca, geom = "point", col.ind = chrono2$country)
ggpubr::ggpar(ind.p,
              title = "Principal Component Analysis",
              fill.ind = chronoC$country, col.ind = "black",
              subtitle = "Cassiope tetragona Chronology Characteristics",
              addEllipses = TRUE, ellipse.type = "confidence",
              xlab = "PC1", ylab = "PC2", 
              legend.title = "Location", legend.position = "top",
              ggtheme = theme_linedraw(), palette = "ucscgb"
)

#bootstrap pca
res.desc <- dimdesc(res.pca, axes = c(1,2), proba = 0.05)
# Description of dimension 1
pc1 <- res.desc$Dim.1
# Description of dimension 2
pc2 <- res.desc$Dim.2

library(stargazer)
stargazer(pc1,pc2,type = "html",out="bootstrap.html",out.header = TRUE,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")
stargazer(dog, type = "html",out="test.html",out.header = TRUE,
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")


data.frame((table(pc1, pc2)))
setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)

setSessionTimeLimit(cpu = Inf, elapsed = Inf)

