#libraries
library(ggpubr)
library(car)
library(pheatmap)
library(gplots)
library(tidyverse)
library(leaps)
library(caret)
library(MASS)
library("rpart")
library("rpart.plot")
library(party)
library("partykit")
library(ranger)
library(dplyr)
library(factoextra)
library(NbClust)
library(pheatmap)
library(plm)
library(tree)
library(ISLR)
library(corrplot)
library(factoextra)
#load dataset
serie_a <- read.csv("Documents/StatLearningProject/serie_a_stats_21_22.csv")

#test if response variable (player market value) is normal 
summary(serie_a$player_market_value_euro)
qqnorm(serie_a$player_market_value_euro)
shapiro.test(serie_a$player_market_value_euro)
ggqqplot(serie_a$player_market_value_euro)
ggdensity(serie_a, x = "player_market_value_euro", fill = "lightgray", title = "Player Value") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

summary(aov(serie_a$logvalue~serie_a$single_pos))
out <- boxplot.stats(log(serie_a$player_market_value_euro))$out
serie_a <- subset(serie_a, serie_a$logvalue>out[1]) 

#response variable is not normal we reject null hypotesis of shapiro test, try to use it with logarithm
serie_a$logvalue=log(serie_a$player_market_value_euro)
#remove -inf rows
serie_a <- subset(serie_a, logvalue!=-Inf)
shapiro.test(serie_a$logvalue)
ggqqplot(serie_a$logvalue)
ggdensity(serie_a, x = "logvalue", fill = "lightgray", title = "Player Value (log)") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")
serie_a$X <- NULL

#removing id variables
serie_a_no_ids <- serie_a
serie_a_no_ids$Player <- NULL
serie_a_no_ids$player_market_value_euro <- NULL
serie_a_no_ids$Squad <- NULL
serie_a_no_ids$single_pos <- NULL
serie_a_no_ids$position <- NULL
#compute correlations
correlations <- round(cor(serie_a_no_ids),2)
symnum(correlations, abbr.colnames = FALSE)
pheatmap(correlations, cluster_rows=FALSE, cluster_cols=FALSE)

#market value differences in positions
numeric_positions <- c()
for (player in serie_a$single_pos){
  if(player == "GK")
    numeric_positions <- c(numeric_positions,0)
  if(player == "DF")
    numeric_positions <- c(numeric_positions,1)
  if(player == "MF")
    numeric_positions <- c(numeric_positions,2)
  if(player == "FW")
    numeric_positions <- c(numeric_positions,3)
}
serie_a_no_ids$position <- factor(numeric_positions,levels = c(0:3),labels = c("GK","DF","MF","FW"))
boxplot(logvalue ~ position, data=serie_a_no_ids, names=c("Goalkeeper", "Defender", "Midfielder", "Forward"))

t.test(logvalue ~ keeper, alternative = "two.sided", conf.level=.95, var.equal=FALSE, data=serie_a_no_ids)
t.test(logvalue ~ defender, alternative = "two.sided", conf.level=.95, var.equal=FALSE, data=serie_a_no_ids)
t.test(logvalue ~ midfield, alternative = "two.sided", conf.level=.95, var.equal=FALSE, data=serie_a_no_ids)
t.test(logvalue ~ forward, alternative = "two.sided", conf.level=.95, var.equal=FALSE, data=serie_a_no_ids)

#reg
#diagnostics
linreg <- lm(logvalue~.,data=serie_a_no_ids)
summary(linreg)
plot(linreg)
summary(linreg$residuals)
shapiro.test(linreg$residuals)
ggqqplot(linreg$residuals)

#Trees

#split train and test 
set.seed(101)
tree.seriea=rpart(logvalue ~., data=serie_a_no_ids, control=rpart.control(cp=-1))
summary(tree.seriea)
rpart.plot(tree.seriea)
plotcp(tree.seriea)
op <- par(mfrow = c(1, 2))
tree.seriea
rpart
rsq.rpart(tree.seriea)
par(op)
#from cp table
bestfit <-tree.seriea$cptable[which.min(tree.seriea$cptable[,"xerror"]),"CP"]

pruned_tree <- prune(tree.seriea, cp=bestfit)
prp(pruned_tree,
    faclen=1, #use full names for factor labels
    extra=1, #display number of obs. for each terminal node
    roundint=F, #don't round to integers in output
    digits=2) 
plot(pruned_tree)
summary(pruned_tree)
rsq.rpart(pruned_tree)
plotcp(pruned_tree)

dotchart(pruned_tree$variable.importance[1:8], las=2, horiz = T, cex.axis = 0.5)
tree.pred = predict(pruned_tree,serie_a_no_ids[-train,])
print(tree.pred)

#Unsupervised section
#compute the within sum of squares as a function of n cluster to determine K
fviz_nbclust(serie_a_stats, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2) + # add line for better visualisation
  labs(subtitle = "Elbow method") # add subtitle

value_labels <- c()
for (value in serie_a$logvalue){
  print(value)
  if(value > round(quantile(serie_a_no_ids$logvalue, 0.75)))
    value_labels <- c(value_labels,3)
  else
    if(value > round(quantile(serie_a_no_ids$logvalue, 0.50)))
      value_labels <- c(value_labels,2)
    else
      if(value > round(quantile(serie_a_no_ids$logvalue, 0.25)))
        value_labels <- c(value_labels,1)
      else 
        value_labels <- c(value_labels,0)
}
serie_a_no_ids$value_labels<- factor(value_labels,levels = c(0:3),labels = c("Low Cost","Mid-low Cost","Stepping stone","Top Player"))

#remove response variable and other clustering features (position, squad)
serie_a_stats <- serie_a
serie_a_stats$player_market_value_euro <- NULL
serie_a_stats$Squad <- NULL
serie_a_stats$logvalue <- NULL
serie_a_stats$pred_values <- NULL
serie_a_stats$X <- NULL

serie_a_stats$Pos <- NULL
serie_a_stats$single_pos <- NULL

serie_a_stats$keeper <- NULL
serie_a_stats$midfield <- NULL
serie_a_stats$defender <- NULL
serie_a_stats$forward <- NULL
serie_a_stats$Player <- NULL

rownames(serie_a_stats) <- serie_a_stats$Player
pheatmap(corr, cluster_rows=FALSE, cluster_cols=FALSE)
pca <- prcomp(serie_a_stats,  center = TRUE,scale = TRUE)
pca$rotation <- -1*pca$rotation
pheatmap(res.var$cos2, is.corr=FALSE)
var_explained <- pca$sdev^2 / sum(pca$sdev^2)

#create scree plot
fviz_screeplot(pca)

data_pca <- pca$x[,1:4]
#components
res.var <- get_pca_var(pca)
res.var$cor
col1<- res.var$cor[,1]
pos_cor1<- col1[col1>0.8]
neg_cor1<- col1[col1<(-0.8)]
pos_cor1
neg_cor1

col2<- res.var$cor[,2]
pos_cor<- col2[col2>0.6]
neg_cor<- col2[col2<(-0.4)]
pos_cor
neg_cor

col3<- res.var$cor[,3]
pos_cor<- col3[col3>0.6]
neg_cor<- col3[col3<(-0.6)]
pos_cor
neg_cor

col4<- res.var$cor[,4]
pos_cor<- col4[col4>0.5]
neg_cor<- col4[col4<(-0.5)]
pos_cor
neg_cor


#K-means
set.seed(123)
k.means.fit <- kmeans(data_pca, 4) # k = 4
str(k.means.fit)
km_cluster <- k.means.fit$cluster
names(km_cluster) <- NULL
fviz_cluster(k.means.fit, data=data_pca)

g4 <- names(km_cluster[km_cluster==4])
g4_stat <- subset(serie_a, serie_a$Player %in% g4)
summary(g4_stat)

g3 <- names(km_cluster[km_cluster==3])
g3_stat <- subset(serie_a, serie_a$Player %in% g3)
summary(g3_stat)


g2 <- names(km_cluster[km_cluster==2])
g2_stat <- subset(serie_a, serie_a$Player %in% g2)
summary(g2_stat)

g1 <- names(km_cluster[km_cluster==1])
g1_stat <- subset(serie_a, serie_a$Player %in% g1)
summary(g1_stat)


#hierarchical
d <- dist(data_pca, method="euclidean")
h.fit <- hclust(d, method="ward.D2")
plot(h.fit)
#abline(h=15, col="red")

#k =3

groups_hier <- cutree(h.fit, k=4)
groups_hier[groups==1]
groups[groups==2]
groups[groups==3]
groups[groups==4]

agglo <- function(hc){
  data.frame(row.names=paste0("Cluster",seq_along(hc$height)),
             height=hc$height,
             components=ifelse(hc$merge<0, 
                               hc$labels[abs(hc$merge)], paste0("Cluster",hc$merge)),
             stringsAsFactors=FALSE) }


# agglo 
agglo(h.fit)

g4 <- names(groups[groups==4])
g4_stat <- subset(serie_a, serie_a$Player %in% g4)
summary(g4_stat)

g3 <- names(groups[groups==3])
g3_stat <- subset(serie_a, serie_a$Player %in% g3)
summary(g3_stat)

g2 <- names(groups[groups==2])
g2_stat <- subset(serie_a, serie_a$Player %in% g2)
summary(g2_stat)

g1 <- names(groups[groups==1])
g1_stat <- subset(serie_a, serie_a$Player %in% g1)
summary(g1_stat)

length(groups_hier[groups_hier==1])
length(k.means.fit$cluster[k.means.fit$cluster==1])

length(groups_hier[groups_hier==2])
length(k.means.fit$cluster[k.means.fit$cluster==2])

length(groups_hier[groups_hier==3])
length(k.means.fit$cluster[k.means.fit$cluster==3])

length(groups_hier[groups_hier==4])
length(k.means.fit$cluster[k.means.fit$cluster==4])
groups_hier[groups_hier==1] != k.means.fit$cluster[k.means.fit$cluster==1]
groups_hier[groups_hier==2] != k.means.fit$cluster[k.means.fit$cluster==2]
groups_hier[groups_hier==3] != k.means.fit$cluster[k.means.fit$cluster==3]
groups_hier[groups_hier==4] != k.means.fit$cluster[k.means.fit$cluster==4]

d