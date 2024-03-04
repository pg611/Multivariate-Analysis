library(readr)
library(lattice)
library(ggplot2)
library(ggridges)
library(ggvis)
library(ggthemes)
library(cowplot)
library(gapminder)
library(gganimate)
library(dplyr)
library(tidyverse)
library(grid)
library(gridExtra)
library(RColorBrewer)
layoff <- read_csv("/Users/parul/OneDrive/LAYOFF1.csv")
str(layoff[,7:10])
attach(layoff[,7:10])
#Get the Correlations between the measurements
cor(layoff[,7:10][-1])
# Using prcomp to compute the principal components (eigenvalues and eigenvectors). With scale=TRUE, variable means are set to zero, and variances set to one
layoff_pca <- prcomp(layoff[,7:10][,-1],scale=TRUE)
layoff_pca
summary(layoff_pca)


# Eigenvalues are sdev^2
(eigen_layoff <- layoff_pca$sdev^2)
names(eigen_layoff) <- paste("PC",1:3,sep="")
eigen_layoff
sumlambdas <- sum(eigen_layoff)
sumlambdas
propvar <- eigen_layoff/sumlambdas
propvar
cumvar_layoff <- cumsum(propvar)
cumvar_layoff
matlambdas <- rbind(eigen_layoff,propvar,cumvar_layoff)
rownames(matlambdas) <- c("Eigenvalues","Prop. variance","Cum. prop. variance")
round(matlambdas,4)
summary(layoff_pca)
layoff_pca$rotation
print(layoff_pca)
## Sample scores stored in sparrow_pca$x
layoff_pca$x
# Identifying the scores by their survival status
layofftyp_pca <- cbind(data.frame(layoff$Laid_Off_Yes_No),layoff_pca$x)
layofftyp_pca



# Means of scores for all the PC's classified by Survival status
tabmeansPC <- aggregate(layofftyp_pca[,2:4],by=list(Laid_Off_Yes_No=layoff$Laid_Off_Yes_No),mean)
tabmeansPC
tabmeansPC <- tabmeansPC[rev(order(tabmeansPC$Laid_Off_Yes_No)),]
tabmeansPC
tabfmeans <- t(tabmeansPC[,-1])
tabfmeans
colnames(tabfmeans) <- t(as.vector(tabmeansPC[1]$Laid_Off_Yes_No))
tabfmeans
# Standard deviations of scores for all the PC's classified by Survival status
tabsdsPC <- aggregate(layofftyp_pca[,2:4],by=list(Laid_Off_Yes_No=layoff$Laid_Off_Yes_No),sd)
tabfsds <- t(tabsdsPC[,-1])
colnames(tabfsds) <- t(as.vector(tabsdsPC[1]$Laid_Off_Yes_No))
tabfsds
t.test(PC1~layoff$Laid_Off_Yes_No,data=layofftyp_pca)
t.test(PC2~layoff$Laid_Off_Yes_No,data=layofftyp_pca)
t.test(PC3~layoff$Laid_Off_Yes_No,data=layofftyp_pca)

## F ratio tests
var.test(PC1~layoff$Laid_Off_Yes_No,data=layofftyp_pca)
var.test(PC2~layoff$Laid_Off_Yes_No,data=layofftyp_pca)
var.test(PC3~layoff$Laid_Off_Yes_No,data=layofftyp_pca)

# Levene's tests (one-sided)
library(car)
(LTPC1 <- leveneTest(PC1~layoff$Laid_Off_Yes_No,data=layofftyp_pca))
(p_PC1_1sided <- LTPC1[[3]][1]/2)
(LTPC2 <- leveneTest(PC2~layoff$Laid_Off_Yes_No,data=layofftyp_pca))
(p_PC2_1sided=LTPC2[[3]][1]/2)
(LTPC3 <- leveneTest(PC3~layoff$Laid_Off_Yes_No,data=layofftyp_pca))
(p_PC3_1sided <- LTPC3[[3]][1]/2)

# Plotting the scores for the first and second components
plot(layofftyp_pca$PC1, layofftyp_pca$PC2,pch=ifelse(layofftyp_pca$Laid_Off_Yes_No == "Yes",1,16),xlab="PC1", ylab="PC2", main="Layoff against values for PC1 & PC2")
abline(h=0)
abline(v=0)
legend("bottomleft", legend=c("Layoff","No Layoff"), pch=c(1,16))
plot(eigen_layoff, xlab = "Component number", ylab = "Component variance", type = "l", main = "Scree diagram")
plot(log(eigen_layoff), xlab = "Component number",ylab = "log(Component variance)", type="l",main = "Log(eigenvalue) diagram")
print(summary(layoff_pca))
diag(cov(layoff_pca$x))
xlim <- range(layoff_pca$x[,1])
layoff_pca$x[,1]
layoff_pca$x
plot(layoff_pca$x,xlim=xlim,ylim=xlim)
layoff_pca$rotation[,1]
layoff_pca$rotation
plot(layoff[,-1])
layoff_pca$x
plot(layoff_pca)
#get the original value of the data based on PCA
center <- layoff_pca$center
scale <- layoff_pca$scale
new_layoff <- as.matrix(layoff[,7:10][,-1])
new_layoff











drop(scale(new_layoff,center=center, scale=scale)%*%layoff_pca$rotation[,1])
predict(layoff_pca)[,1]
#The aboved two gives us the same thing. predict is a good function to know.
layoff$Laid_Off_Yes_No <- as.factor(layoff$Laid_Off_Yes_No)
out <- sapply(1:3, function(i){plot(layoff$Laid_Off_Yes_No,layoff_pca$x[,i],xlab=paste("PC",i,sep=""),ylab="layoff")})
pairs(layoff_pca$x[,1:3], ylim = c(-6,4),xlim = c(-6,4),panel=function(x,y,...){text(x,y,layoff$Laid_Off_Yes_No)})

# Better Ways to Visualize

library(factoextra)
library(FactoMineR)
library(ggfortify)
library(psych)
library(corrplot)
library(devtools)


# Correlation
pairs.panels(layoff[,-1],
             gap = 0,
             bg = c("red", "blue")[layoff$Laid_Off_Yes_No],
             pch=21)

pairs.panels(layoff_pca$x,
             gap=0,
             bg = c("red", "blue")[layoff$Laid_Off_Yes_No],
             pch=21)


install.packages("vctrs", lib = "C:/Users/parul/Documents/R/win-library/4.3")

library(factoextra)
fviz_eig(layoff_pca, addlabels = TRUE)



fviz_eig(layoff_pca, addlabels = TRUE)
fviz_pca_var(layoff_pca,col.var = "cos2",
             gradient.cols = c("#FFCC00", "#CC9933", "#660033", "#330033"),
             repel = TRUE)
fviz_pca_ind(layoff_pca, col.ind = "cos2", 
             gradient.cols = c("#FFCC00", "#CC9933", "#660033", "#330033"), 
             repel = TRUE)
biplot(layoff_pca)
autoplot(layoff_pca,
         data = layoff[,-1],
         loadings = TRUE,
         labels = layoff$Laid_Off_Yes_No)

# Different PCA Method. 
res.pca <- PCA(layoff[,7:10][,-1], graph = FALSE)
print(res.pca)

# Visualize and Interpret PCA using these functions 

#get_eigenvalue(res.pca): Extract the eigenvalues/variances of principal components
#fviz_eig(res.pca): Visualize the eigenvalues
#get_pca_ind(res.pca), get_pca_var(res.pca): Extract the results for individuals and variables, respectively.
#fviz_pca_ind(res.pca), fviz_pca_var(res.pca): Visualize the results individuals and variables, respectively.
#fviz_pca_biplot(res.pca): Make a biplot of individuals and variables.

eig.val <- get_eigenvalue(res.pca)
eig.val

fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))

var <- get_pca_var(res.pca)
#var$coord: coordinates of variables to create a scatter plot
#var$cos2: represents the quality of representation for variables on the factor map. It’s calculated as the squared coordinates: var.cos2 = var.coord * var.coord.
#var$contrib: contains the contributions (in percentage) of the variables to the principal components. 
#The contribution of a variable (var) to a given principal component is (in percentage) : (var.cos2 * 100) / (total cos2 of the component).
var

# Coordinates
head(var$coord)
# Cos2: quality on the factore map
head(var$cos2)
# Contributions to the principal components
head(var$contrib)

#The plot Below is also known as variable correlation plots. It shows the relationships between all variables. It can be interpreted as follow:

#Positively correlated variables are grouped together.
#Negatively correlated variables are positioned on opposite sides of the plot origin (opposed quadrants).
#The distance between variables and the origin measures the quality of the variables on the factor map. 
#Variables that are away from the origin are well represented on the factor map.

# Correlation circle
fviz_pca_var(res.pca, col.var = "black")

# Quality of representation


corrplot(var$cos2, is.corr=FALSE)
# Total cos2 of variables on Dim.1 and Dim.2
#A high cos2 indicates a good representation of the variable on the principal component. 
#In this case the variable is positioned close to the circumference of the correlation circle.
#A low cos2 indicates that the variable is not perfectly represented by the PCs. 
#In this case the variable is close to the center of the circle.

fviz_cos2(res.pca, choice = "var", axes = 1:2)
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)
# Change the transparency by cos2 values
fviz_pca_var(res.pca, alpha.var = "cos2")
corrplot(var$contrib, is.corr=FALSE)
# Contributions of variables to PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)
fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)
fviz_pca_var(res.pca, alpha.var = "contrib")

fviz_pca_ind(res.pca,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = layoff$Laid_Off_Yes_No, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Groups"
)


# Description of PC

res.desc <- dimdesc(res.pca, axes = c(1,2,3,4,5), proba = 0.05)
# Description of dimension 1
res.desc$Dim.1
res.desc$Dim.2
res.desc$Dim.3
res.desc$Dim.4
res.desc$Dim.5

# Graph of Indiviuals
ind <- get_pca_ind(res.pca)
ind

## Principal Component Analysis Results for individuals
##  ===================================================
##   Name       Description                       
## 1 "$coord"   "Coordinates for the individuals" 
## 2 "$cos2"    "Cos2 for the individuals"        
## 3 "$contrib" "contributions of the individuals"
#To get access to the different components, use this:

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

fviz_pca_ind(res.pca, col.ind = "cos2", pointsize = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)
fviz_cos2(res.pca, choice = "ind")
# Total contribution on PC1 and PC2
fviz_contrib(res.pca, choice = "ind", axes = 1:2)

# Create a random continuous variable of length 23,
# Same length as the number of active individuals in the PCA
set.seed(123)
my.cont.var <- rnorm(49)
# Color individuals by the continuous variable
fviz_pca_ind(res.pca, col.ind = my.cont.var,
             gradient.cols = c("blue", "yellow", "red"),
             legend.title = "Cont.Var")

fviz_pca_ind(res.pca,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = layoff$Laid_Off_Yes_No, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Groups"
)

fviz_pca_ind(res.pca, geom.ind = "point", col.ind = layoff$Laid_Off_Yes_No, 
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, ellipse.type = "confidence",
             legend.title = "Groups"
)
fviz_pca_ind(res.pca,
             label = "none", # hide individual labels
             habillage = layoff$Laid_Off_Yes_No, # color by groups
             addEllipses = TRUE, # Concentration ellipses
             palette = "jco"
)
fviz_pca_var(res.pca, geom.var = c("point", "text"))
# Show individuals text labels only
fviz_pca_ind(res.pca, geom.ind =  "text")
# Change the size of arrows an labels
fviz_pca_var(res.pca, arrowsize = 1, labelsize = 5, 
             repel = TRUE)
# Change points size, shape and fill color
# Change labelsize
fviz_pca_ind(res.pca, 
             pointsize = 3, pointshape = 21, fill = "lightblue",
             labelsize = 5, repel = TRUE)

fviz_pca_ind(res.pca,
             geom.ind = "point", # show points only (but not "text")
             group.ind = layoff$Laid_Off_Yes_No, # color by groups
             legend.title = "Groups",
             mean.point = FALSE)
fviz_pca_ind(res.pca,
             geom.ind = "point", # show points only (but not "text")
             group.ind = layoff$Laid_Off_Yes_No, # color by groups
             legend.title = "Groups",
             mean.point = TRUE)
fviz_pca_var(res.pca, axes.linetype = "blank")



ind.p <- fviz_pca_ind(res.pca, geom = "point", col.ind = layoff$Laid_Off_Yes_No)
ggpubr::ggpar(ind.p,
              title = "Principal Component Analysis",
              subtitle = "layoff data set",
              caption = "Source: factoextra",
              xlab = "PC1", ylab = "PC2",
              legend.title = "Laid Off", legend.position = "top",
              ggtheme = theme_gray(), palette = "jco"
)

fviz_pca_biplot(res.pca, repel = TRUE,col.ind = layoff$Laid_Off_Yes_No,
                col.var = "#2E9FDF", # Variables color
)

fviz_pca_biplot(res.pca, 
                col.ind = layoff$Laid_Off_Yes_No, palette = "jco", 
                addEllipses = TRUE, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "Laid Off") 

fviz_pca_biplot(res.pca, 
                # Fill individuals by groups
                geom.ind = "point",
                pointshape = 21,
                pointsize = 2.5,
                fill.ind = layoff$Laid_Off_Yes_No,
                col.ind = "black",
                # Color variable by groups
                legend.title = list(fill = "Laid Off", color = "Clusters"),
                repel = TRUE        # Avoid label overplotting
)+
  ggpubr::fill_palette("jco")+      # Indiviual fill color
  ggpubr::color_palette("npg")      # Variable colors

fviz_pca_biplot(res.pca, 
                # Individuals
                geom.ind = "point",
                fill.ind = layoff$Laid_Off_Yes_No, col.ind = "black",
                pointshape = 21, pointsize = 2,
                palette = "jco",
                addEllipses = TRUE,
                # Variables
                alpha.var ="contrib", col.var = "contrib",
                gradient.cols = "RdYlBu",
                
                legend.title = list(fill = "Laid Off", color = "Contrib",
                                    alpha = "Contrib")
)

