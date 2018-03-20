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
library(ggplot2)

#  read in datasets
set12 <- readRDS("set12.rds")
set12_simple <- readRDS("set12_simple.rds")

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

set.seed(56)
kmeans12_simple <- kmeans(set12_simple[,c("newspapers","magazines","radio", "tv", "internet", "all")],
                   centers = 4,
                   nstart = 20)

# add cluster labels to the dataset
set12c <- set12 %>%
        mutate(cluster = factor(kmeans12$cluster)) %>%
        dplyr::select(qn, pwgt, cluster, everything())
# 
set12c_simple <- set12_simple %>% ### sort out bloody internet thingy
        mutate(cluster = factor(kmeans12_simple$cluster)) %>%
        dplyr::select(qn, pwgt, cluster, everything())

saveRDS(set12c, "set12c.rds")
saveRDS(set12c_simple, "set12c_simple.rds")

set12c <- readRDS("set12c.rds")
set12c_simple <- readRDS("set12c_simple.rds")

# some plots
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
d1 <- ggplot(set12c, aes(race, cluster, fill = cluster)) +
        geom_col() +
        labs(title = "race", y = "", x = "") +
        scale_x_discrete(labels=c("black", "coloured", "indian", "white"))
d2 <- ggplot(set12c, aes(edu, cluster, fill = cluster)) +
        geom_col() +
        labs(title = "education", y = "", x = "") +
        scale_x_discrete(labels=c("<matric", "matric",">matric"))
d3 <- ggplot(set12c, aes(age, cluster, fill = cluster)) +
        geom_col() +
        labs(title = "age", y = "", x = "") +
        scale_x_discrete(labels=c("15-24","25-44", "45-54","55+"))
d4 <- ggplot(set12c, aes(lsm, cluster, fill = cluster)) +
        geom_col() +
        labs(title = "lsm", y = "", x = "") +
        scale_x_discrete(labels=c("1-2", "3-4", "5-6", "7-8", "9-10"))

jpeg('typeDemogPlots1_12.jpeg', quality = 100, type = "cairo")
grid.arrange(d1, d2, d3, d4, ncol=2, nrow = 2)
dev.off()

d5 <- ggplot(set12c, aes(sex, cluster, fill = cluster)) +
        geom_col() +
        labs(title = "gender", y = "", x = "") +
        scale_x_discrete(labels=c("male", "female"))
d6 <- ggplot(set12c, aes(hh_inc, cluster, fill = cluster)) +
        geom_col() +
        labs(title = "household income", y = "", x = "") +
        scale_x_discrete(labels=c("<5000","5000-10999","11000-19999",">=20000"))
d7 <- ggplot(set12c, aes(lifestages, cluster, fill = cluster)) +
        geom_col() +
        labs(title = "lifestages", y = "", x = "")# +
# scale_x_discrete(labels=c("<5000","5000-10999","11000-19999",">=20000"))
d8 <- ggplot(set12c, aes(lifestyle, cluster, fill = cluster)) +
        geom_col() +
        labs(title = "lifestyle", y = "", x = "")# +
# scale_x_discrete(labels=c("<5000","5000-10999","11000-19999",">=20000"))
jpeg('typeDemogPlots2_12.jpeg', quality = 100, type = "cairo")
grid.arrange(d5, d6, d7, d8, ncol=2, nrow = 2)
dev.off()


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



