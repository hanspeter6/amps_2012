# loading packages
library(stringr)
library(tidyverse)
library(corrplot)
library(rpart)
library(rpart.plot)
library(scatterplot3d)
library(rgl)
library(kohonen)
library(caret)
library(randomForest)
library(MASS)
library(CCA)
library(nFactors)
library(FactoMineR)
library(factoextra)
library(gridExtra)

# read datafiles
magazines_engagement_12 <- readRDS("magazines_engagement_12.rds")
magazines_engagement_12_simple <- readRDS("magazines_engagement_12_simple.rds")
newspapers_engagement_12 <- readRDS("newspapers_engagement_12.rds")
newspapers_engagement_12_simple <- readRDS("newspapers_engagement_12_simple.rds")
radio_engagement_12 <- readRDS("radio_engagement_12.rds")
tv_engagement_12 <- readRDS("tv_engagement_12.rds")
internet_engagement_12 <- readRDS("internet_engagement_12.rds")
internet_engagement_12_simple <- readRDS("internet_engagement_12_simple.rds")

media_type_12 <- readRDS("media_type_12.rds")
media_vehicles_12 <- readRDS("media_vehicles_12.rds")
media_type_12_simple <- readRDS("media_type_12_simple.rds")
media_vehicles_12_simple <- readRDS("media_vehicles_12_simple.rds")

demographics_12 <- readRDS("demographics_12.rds")

#reducing levels of categorical variables and setting factor types for demographics:

# age:
demographics_12$age <- ifelse(demographics_12$age %in% c(1,2), 1, demographics_12$age)
demographics_12$age <- ifelse(demographics_12$age %in% c(3,4), 2, demographics_12$age)
demographics_12$age <- ifelse(demographics_12$age %in% c(5,6), 3, demographics_12$age)
demographics_12$age <- ifelse(demographics_12$age %in% c(7,8), 4, demographics_12$age)
demographics_12$age <- factor(demographics_12$age, ordered = TRUE)

# sex:
demographics_12$sex <- factor(demographics_12$sex, ordered = FALSE)

#edu:
demographics_12$edu <- ifelse(demographics_12$edu %in% c(1,2,3,4), 1, demographics_12$edu)
demographics_12$edu <- ifelse(demographics_12$edu %in% c(5), 2, demographics_12$edu)
demographics_12$edu <- ifelse(demographics_12$edu %in% c(6,7,8), 3, demographics_12$edu)
demographics_12$edu <- factor(demographics_12$edu, ordered = TRUE)

#hh_inc
demographics_12$hh_inc <- ifelse(demographics_12$hh_inc %in% c(1,2,3,4), 1, demographics_12$hh_inc)
demographics_12$hh_inc <- ifelse(demographics_12$hh_inc %in% c(5,6), 2, demographics_12$hh_inc)
demographics_12$hh_inc <- ifelse(demographics_12$hh_inc %in% c(7), 3, demographics_12$hh_inc)
demographics_12$hh_inc <- ifelse(demographics_12$hh_inc %in% c(8), 4, demographics_12$hh_inc)
demographics_12$hh_inc <- factor(demographics_12$hh_inc, ordered = TRUE)

demographics_12$race <- factor(demographics_12$race, ordered = FALSE)
demographics_12$province <- factor(demographics_12$province, ordered = FALSE)
demographics_12$metro <- factor(demographics_12$metro, ordered = FALSE)
demographics_12$lang <- factor(demographics_12$lang, ordered = FALSE)
demographics_12$lifestages <- factor(demographics_12$lifestages, ordered = FALSE)
demographics_12$mar_status <- factor(demographics_12$mar_status, ordered = FALSE)

# lsm
demographics_12$lsm <- ifelse(demographics_12$lsm %in% c(1,2), 1, demographics_12$lsm)
demographics_12$lsm <- ifelse(demographics_12$lsm %in% c(3,4), 2, demographics_12$lsm)
demographics_12$lsm <- ifelse(demographics_12$lsm %in% c(5,6), 3, demographics_12$lsm)
demographics_12$lsm <- ifelse(demographics_12$lsm %in% c(7,8), 4, demographics_12$lsm)
demographics_12$lsm <- ifelse(demographics_12$lsm %in% c(9,10), 5, demographics_12$lsm)
demographics_12$lsm <- factor(demographics_12$lsm, ordered = TRUE)

demographics_12$lifestyle <- factor(demographics_12$lifestyle, ordered = FALSE)
demographics_12$attitudes <- factor(demographics_12$attitudes, ordered = FALSE)

# #create single dataset minus non metropolitans
set12 <- demographics_12 %>%
        left_join(media_type_12) %>%
        left_join(media_vehicles_12) %>%
        filter(metro != 0)

set12_simple <- demographics_12 %>%
        left_join(media_type_12_simple) %>%
        left_join(media_vehicles_12_simple) %>%
        filter(metro != 0)

# consider some correlations

png('corTypePlot2012.png')
corrplot(cor(set12[,c("newspapers","magazines","radio", "tv", "internet")]),
         method = "pie",
         order = "hclust",
         hclust.method = "complete",
         tl.col = 'black',
         mar = c(1,1,1,1),
         addCoefasPercent = TRUE,
         tl.pos = TRUE)
dev.off()

## consider kmeans
wss <- vector()
set.seed(5)
for(k in c(1,2,3,4,5,6)) {
        temp <- kmeans(set12[,c("newspapers","magazines","radio", "tv", "internet", "all")],
                       centers = k,
                       nstart = 10,
                       iter.max = 20)
        wss <- append(wss,temp$tot.withinss)
}

png('kmeansTypePlot2012.png')
plot(c(1,2,3,4,5,6), wss, type = "b", xlab = "k-values", ylab = "total within sum of squares" )
dev.off()

set.seed(56)
kmeans12 <- kmeans(set12[,c("newspapers","magazines","radio", "tv", "internet","all")],
                   centers = 4,
                   nstart = 20)

set.seed(1)
kmeans12_simple <- kmeans(set12_simple[,c("newspapers","magazines","radio", "tv", "internet", "all")],
                   centers = 5,
                   nstart = 20)

# add cluster labels to the dataset
set12 <- set12 %>%
        mutate(cluster = factor(kmeans12$cluster))

set12_simple <- set12_simple %>%
        mutate(cluster = factor(kmeans12_simple$cluster))

saveRDS(set12, "set12.rds")
saveRDS(set12_simple, "set12_simple.rds")

set12 <- readRDS("set12.rds")
set12_simple <- readRDS("set12_simple.rds")

# consider multidimensional scaling and self organising maps on the clusters :

# 1st create a subset to ensure easier running
set.seed(56)
sub12 <- set12[sample(nrow(set12), size = 1000),]

# distance matrix and MDS
sub12_dist <- dist(sub12[,c("newspapers","magazines","radio", "tv", "internet", "all")])
mds12 <- cmdscale(sub12_dist)
plot(mds12, col = as.numeric(sub12$cluster) + 1, pch = 19, ylab = "", xlab = "")

# 3D scaling
mds3 <- cmdscale(dist(sub12[,c("newspapers", "magazines", "radio", "tv", "internet", "all")]), k = 3)
mds3 <- as.data.frame(mds3)

# 2D Scatterplots of 4 cente

# setting colours
cols <- as.numeric(sub12$cluster) + 1
cols <- ifelse(cols == 5, 6, cols)

jpeg('kmeans2DPlot2012.jpeg')
plot(mds12, col = cols, ylab = "", xlab = "", pch = 19)
dev.off()
# 

# consider for some predictions:
# create training and test sets:

set.seed(56)
ind_train <- createDataPartition(set12$cluster, p = 0.7, list = FALSE)
training <- set12[ind_train,]
testing <- set12[-ind_train,]

# # using random forest:
forest12_type <- randomForest(cluster ~ newspapers
                              + tv
                              + radio
                              + magazines
                              + internet,
                              data = training )

pred_forest12_type <- predict(forest12_type, newdata = testing)

confusionMatrix(pred_forest12_type, testing$cluster) 

# with lda. Although given accuracy of forest,  no real need.
set.seed(56)
lda12 <- lda(cluster ~ newspapers
             + tv
             + radio
             + magazines
             + internet,
             data = training)
summary(lda12)

pred_lda12 <- predict(lda12, newdata = testing)
confusionMatrix(pred_lda12$class, testing$cluster) # collinearity meant took out 

# using only demographic information
forest12_demogr <- randomForest(cluster ~ age
                                + sex
                                + edu
                                + hh_inc
                                + race
                                + lang
                                + lifestages
                                + mar_status
                                + lsm
                                + lifestyle
                                + attitudes,
                                data = training)

pred_forest12_demogr <- predict(forest12_demogr, newdata = testing)

confusionMatrix(pred_forest12_demogr, testing$cluster)

# with lda
set.seed(56)
lda12_demogr <- lda(cluster ~ age
                    + sex
                    + edu
                    + hh_inc
                    + race
                    + lang
                    + lifestages
                    + mar_status
                    + lsm
                    + lifestyle
                    + attitudes,
                    data = training)

pred_lda12_demogr <- predict(lda12_demogr, newdata = testing)
confusionMatrix(pred_lda12_demogr$class, testing$cluster)

##  some qualitative consideration of the four types:

# consider a single tree partitioning to try to add meaning to the four clusters
control <- rpart.control(maxdepth = 3, cp = 0.001)
tree12 <- rpart(cluster ~ newspapers + tv + radio + magazines + internet + all, 
                data = set12,
                control = control) # weights = pwgt
par(mfrow = c(1,1))
plot(tree12, uniform = TRUE, margin = 0.2)
text(tree12, pretty = 0, cex = 0.8)

# for more detail
rpart.plot(tree12, type = 4, extra = 1, cex = 0.5)

percentile <- ecdf(set12$internet)
percentile(1.4)

# some plots
jpeg('typeBoxPlots_12.jpeg', quality = 100, type = "cairo")
par(mfrow = c(2,3))
plot(set12$radio ~ set12$cluster, col = c(2,3,4,6), main = "radio", xlab = "cluster", ylab = '')
plot(set12$tv ~ set12$cluster, col = c(2,3,4,6), main = "tv", xlab = "cluster", ylab = '')
plot(set12$newspapers ~ set12$cluster, col = c(2,3,4,6), main = "newspapers", xlab = "cluster", ylab = '')
plot(set12$magazines ~ set12$cluster, col = c(2,3,4,6), main = "magazines", xlab = "cluster", ylab = '')
plot(set12$internet ~ set12$cluster, col = c(2,3,4,6), main = "internet", xlab = "cluster", ylab = '')
plot(set12$all ~ set12$cluster, col = c(2,3,4,6), main = "all", xlab = "cluster", ylab = '')
dev.off()

# try to make sense of demographics
jpeg('typeDemogPlots1_12.jpeg', quality = 100, type = "cairo")
par(mfrow = c(2,2))
plot(set12$cluster ~ factor(set12$race,labels = c("black", "coloured", "indian", "white")), col = c(2,3,4,6), main = "race", xlab = "", ylab = "")
plot(set12$cluster ~ factor(set12$edu, labels = c("<matric", "matric",">matric" )), col = c(2,3,4,6), main = "education", xlab = "", ylab = "")
plot(set12$cluster ~ factor(set12$age, labels = c("15-24","25-44", "45-54","55+")), col = c(2,3,4,6), main = "age", xlab = "", ylab = "")
plot(set12$cluster ~ factor(set12$lsm, labels = c("1-2", "3-4", "5-6", "7-8", "9-10")), col = c(2,3,4,6), main = "LSM", xlab = "", ylab = "")
dev.off()

jpeg('typeDemogPlots2_12.jpeg', quality = 100, type = "cairo")
par(mfrow = c(2,2))
plot(set12$cluster ~ factor(set12$sex, labels = c("male", "female")), col = c(2,3,4,6), main = "sex", xlab = "", ylab = "")
plot(set12$cluster ~ factor(set12$hh_inc, labels = c("<5000","5000-10999","11000-19999",">=20000")), col = c(2,3,4,6), main = "hh_inc", xlab = "", ylab = "")
plot(set12$cluster ~ set12$lifestages, col = c(2,3,4,6), main = "lifestages", xlab = "", ylab = "")
plot(set12$cluster ~ set12$lifestyle, col = c(2,3,4,6), main = "lifestyle", xlab = "", ylab = "")
dev.off()
