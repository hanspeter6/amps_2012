# # loading packages
# library(stringr)
library(tidyverse)
library(corrplot)
library(rpart)
library(rpart.plot)
# library(scatterplot3d)
# library(rgl)
# library(kohonen)
library(caret)
library(randomForest)
library(MASS)
# library(CCA)
# library(nFactors)
# library(FactoMineR)
# library(factoextra)
library(gridExtra)
library(ggplot2)

#  read in datasets
set12 <- readRDS("set12.rds")

# consider some correlations
jpeg('corTypePlot2012.jpeg')
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
set.seed(123)
for(k in c(1,2,3,4,5,6)) {
        temp <- kmeans(set12[,c("newspapers","magazines","radio", "tv", "internet", "all")],
                       centers = k,
                       nstart = 5,
                       iter.max = 30)
        wss <- append(wss,temp$tot.withinss)
}

jpeg('kmeansTypePlot2012.jpeg')
plot(c(1,2,3,4,5,6), wss, type = "b", xlab = "k-values", ylab = "total within sum of squares" )
dev.off()

set.seed(123)
kmeans12 <- kmeans(set12[,c("newspapers","magazines","radio", "tv", "internet","all")],
                   centers = 4,
                   nstart = 5,
                   iter.max = 100)

table(kmeans12$cluster)

# add cluster labels to the dataset
set12c <- set12 %>%
        mutate(cluster = factor(kmeans12$cluster)) %>%
        dplyr::select(qn, pwgt, cluster, everything())

# save them
saveRDS(set12c, "set12c.rds")
# read back
set12c <- readRDS("set12c.rds")

## some plots for simple version to use in longitudinal stuff later...
# boxplots of clusters and media types
p1 <- ggplot(set12c, aes(cluster, all, fill = cluster)) +
        geom_boxplot() +
        guides(fill = FALSE) +
        labs(title = "all")
p2 <- ggplot(set12c, aes(cluster, newspapers, fill = cluster)) +
        geom_boxplot() +
        guides(fill = FALSE) +
        labs(title = "newspapers")
p3 <- ggplot(set12c, aes(cluster, magazines, fill = cluster)) +
        geom_boxplot() +
        guides(fill = FALSE) +
        labs(title = "magazines")
p4 <- ggplot(set12c, aes(cluster, radio, fill = cluster)) +
        geom_boxplot() +
        guides(fill = FALSE) +
        labs(title = "radio")
p5 <- ggplot(set12c, aes(cluster, tv, fill = cluster)) +
        geom_boxplot() +
        guides(fill = FALSE) +
        labs(title = "tv")
p6 <- ggplot(set12c, aes(cluster, internet, fill = cluster)) +
        geom_boxplot() +
        guides(fill = FALSE) +
        labs(title = "internet")

jpeg('typeBoxPlots_12.jpeg', quality = 100, type = "cairo")
grid.arrange(p1, p2, p3, p4, p5, p6,  ncol=3, nrow = 2)
dev.off()

# try to make sense of demographics

# size of each cluster
ggplot(data = set12c, aes(x = cluster, fill = cluster)) +
        geom_bar(stat = "count") +
        guides(fill = FALSE)

# demographics by cluster
d1 <- ggplot(data = set12c, aes(x = cluster, fill = race)) +
        geom_bar(stat = "count", position = position_dodge()) +
        scale_fill_discrete(labels=c("black", "coloured", "indian", "white")) +
        labs(title = "Population Group 2012")

d2 <- ggplot(data = set12c, aes(x = cluster, fill = edu)) +
        geom_bar(stat = "count", position = position_dodge()) +
        scale_fill_discrete(labels=c("<matric", "matric",">matric")) +
        labs(title = "Education Level 2012")

d3 <- ggplot(data = set12c, aes(x = cluster, fill = age)) +
        geom_bar(stat = "count", position = position_dodge()) +
        scale_fill_discrete(labels=c("15-24","25-44", "45-54","55+")) +
        labs(title = "Age Group 2012")

d4 <- ggplot(data = set12c, aes(x = cluster, fill = lsm)) +
        geom_bar(stat = "count", position = position_dodge()) +
        scale_fill_discrete(labels=c("1-2", "3-4", "5-6", "7-8", "9-10")) +
        labs(title = "LSM 2012")

d5 <- ggplot(data = set12c, aes(x = cluster, fill = sex)) +
        geom_bar(stat = "count", position = position_dodge()) +
        scale_fill_discrete(labels=c("male", "female")) +
        labs(title = "Gender 2012")

d6 <- ggplot(data = set12c, aes(x = cluster, fill = hh_inc)) +
        geom_bar(stat = "count", position = position_dodge()) +
        scale_fill_discrete(labels=c("<5000","5000-10999","11000-19999",">=20000")) +
        labs(title = "Household Income 2012")

jpeg('typeDemogPlots_12.jpeg', quality = 100, type = "cairo")
grid.arrange(d1, d2, d3, d4, d5, d6, ncol=2, nrow = 3)
dev.off()

# consider multidimensional scaling and self organising maps on the clusters :

# 1st create a subset to ensure easier running
set.seed(56)
sub12 <- set12c[sample(nrow(set12c), size = 1000),]

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
ind_train <- createDataPartition(set12c$cluster, p = 0.7, list = FALSE)
training <- set12c[ind_train,]
testing <- set12c[-ind_train,]

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
                                + lsm,
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
                    + lsm,
                    data = training)

pred_lda12_demogr <- predict(lda12_demogr, newdata = testing)
confusionMatrix(pred_lda12_demogr$class, testing$cluster)

##  some qualitative consideration of the four types:

# consider a single tree partitioning to try to add meaning to the four clusters
control <- rpart.control(maxdepth = 3, cp = 0.001)
tree12 <- rpart(cluster ~ newspapers + tv + radio + magazines + internet, 
                data = set12c,
                control = control) # weights = pwgt
par(mfrow = c(1,1))
plot(tree12, uniform = TRUE, margin = 0.2)
text(tree12, pretty = 0, cex = 0.8)

# for more detail
rpart.plot(tree12, type = 4, extra = 1, cex = 0.5)




