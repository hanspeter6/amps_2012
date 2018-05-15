# # loading packages
library(tidyverse)
library(corrplot)
library(rpart)
library(rpart.plot)
# library(scatterplot3d)
# library(rgl)
library(caret)
library(randomForest)
library(MASS)
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
boxplot <- function(set,type) {
        ggplot(set, aes_string("cluster", type, fill = "cluster")) +
                geom_boxplot() +
                guides(fill = FALSE) +
                labs(title = type)
}

jpeg('typeBoxPlots_12.jpeg', quality = 100, type = "cairo")
grid.arrange(boxplot(set12c, type = "all"),
             boxplot(set12c, type = "newspapers"),
             boxplot(set12c, type = "magazines"),
             boxplot(set12c, type = "radio"),
             boxplot(set12c, type = "tv"),
             boxplot(set12c, type = "internet"),
             ncol=3, nrow = 2)
dev.off()

# try to make sense of demographics

# size of each cluster
ggplot(data = set12c, aes(x = cluster, fill = cluster)) +
        geom_bar(stat = "count") +
        guides(fill = FALSE)

# demographics by cluster
bars_by_cluster <- function(set, category) { # category:one of race, edu, age, lsm, sex, hh_inc
        if(category == "race") {
                level = c("black", "coloured", "indian", "white")
                title = "Population Group 2012"
        }
        if(category == "edu") {
                level = c(c("<matric", "matric",">matric"))
                title = "Education Level 2012"
        }
        if(category == "age") {
                level = c(c("15-24","25-44", "45-54","55+"))
                title = "Age Group 2012"
        }
        if(category == "lsm") {
                level = c("1-2", "3-4", "5-6", "7-8", "9-10")
                title = "LSM 2012"
        }
        if(category == "sex") {
                level = c("male", "female")
                title = "Gender 2012"
        }
        if(category == "hh_inc") {
                level = c("<5000","5000-10999","11000-19999",">=20000")
                title = "Household Income 2012"
        }
        
        ggplot(data = set, aes_string(x = "cluster", fill = category)) +
                geom_bar(stat = "count", position = position_dodge()) +
                scale_fill_discrete(labels=level) +
                labs(title = title) +
                guides(fill=guide_legend(title=NULL)) 
}

jpeg('typeDemogPlots_12.jpeg', quality = 100, type = "cairo")
grid.arrange(bars_by_cluster(set12c, "sex"),
             bars_by_cluster(set12c, "age"),
             bars_by_cluster(set12c, "race"),
             bars_by_cluster(set12c, "edu"),
             bars_by_cluster(set12c, "hh_inc"),
             bars_by_cluster(set12c, "lsm"),
             ncol=2, nrow = 3)
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




