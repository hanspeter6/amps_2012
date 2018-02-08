# libraries
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
# 

# read datafiles
magazines_engagement_12 <- readRDS("magazines_engagement_12.rds")
newspapers_engagement_12 <- readRDS("newspapers_engagement_12.rds")
radio_engagement_12 <- readRDS("radio_engagement_12.rds")
tv_engagement_12 <- readRDS("tv_engagement_12.rds")
internet_engagement_12 <- readRDS("internet_engagement_12.rds")

media_type_12 <- readRDS("media_type_12.rds")
media_vehicles_12 <- readRDS("media_vehicles_12.rds")

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
demographics_12$pers_inc <- factor(demographics_12$pers_inc, ordered = TRUE)

# lsm
demographics_12$lsm <- ifelse(demographics_12$lsm %in% c(1,2), 1, demographics_12$lsm)
demographics_12$lsm <- ifelse(demographics_12$lsm %in% c(3,4), 2, demographics_12$lsm)
demographics_12$lsm <- ifelse(demographics_12$lsm %in% c(5,6), 3, demographics_12$lsm)
demographics_12$lsm <- ifelse(demographics_12$lsm %in% c(7,8), 4, demographics_12$lsm)
demographics_12$lsm <- ifelse(demographics_12$lsm %in% c(9,10), 5, demographics_12$lsm)
demographics_12$lsm <- factor(demographics_12$lsm, ordered = FALSE)

demographics_12$lifestyle <- factor(demographics_12$lifestyle, ordered = FALSE)
demographics_12$attitudes <- factor(demographics_12$attitudes, ordered = FALSE)

# #create single dataset minus non metropolitans
set12 <- demographics_12 %>%
        left_join(media_type_12) %>%
        left_join(media_vehicles_12) %>%
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

# # consider some clustering
# # construct distance matrix for newspapers, magazines, radio, tv and internet engagement:
# 
# dist12 <- dist(set12[,c("newspapers","magazines","radio", "tv", "internet")])
# clust12 <- hclust(dist12, method = "complete")
# plot(clust12) # messy, unhelpful

## consider kmeans
wss <- vector()
for(k in c(3,4,5,6,7,8,9,10,11,12)) {
        temp <- kmeans(set12[,c("newspapers","magazines","radio", "tv", "internet")],
                       centers = k,
                       nstart = 3,
                       iter.max = 20)
        wss <- append(wss,temp$tot.withinss)
}

png('kmeansTypePlot2012.png')
plot(c(3,4,5,6,7,8,9,10,11,12), wss, type = "b", xlab = "k-values", ylab = "total within sum of squares" )
dev.off()

kmeans12 <- kmeans(set12[,c("newspapers","magazines","radio", "tv", "internet")], centers = 5)
table(kmeans12$cluster) #

# add cluster labels to the dataset
set12 <- set12 %>%
        mutate(cluster = factor(kmeans12$cluster))

saveRDS(set12, "set12.rds")

set12 <- readRDS("set12.rds")

# consider multidimensional scaling and self organising maps on the clusters :

# 1st create a subset to ensure easier running
set.seed(56)
sub12 <- set12[sample(nrow(set12), size = 1000),]

# distance matrix and MDS
sub12_dist <- dist(sub12[,c("newspapers","magazines","radio", "tv", "internet")])
mds12 <- cmdscale(sub12_dist)
plot(mds12, col = as.numeric(sub12$cluster) + 1, pch = 19, ylab = "", xlab = "")

# 3D scaling
mds3 <- cmdscale(dist(sub12[,c("newspapers", "magazines", "radio", "tv", "internet")]), k = 3)
mds3 <- as.data.frame(mds3)

# 2D & 3D Scatterplots of 5 centers
jpeg('kmeans2DPlot2012.jpeg')
plot(mds12, col = as.numeric(sub12$cluster) + 1, ylab = "", xlab = "", pch = 19)
dev.off()

jpeg('kmeans3DPlot2012.jpeg')
scatterplot3d(mds3, color = as.numeric(sub12$cluster) + 1, xlab = '', ylab = '', zlab = '')
dev.off()

# Spinning 3D for 5 classes
jpeg('kmeansSpinningPlot2012.png')
plot3d(jitter(mds3$V1), jitter(mds3$V2), jitter(mds3$V3), col= as.numeric(sub12$cluster) + 1, size=5, xlab = '', ylab = '', zlab = '', pch = 19)
dev.off()

# try some Self Organising Maps.... try to explain the differences....

# set up somgrid
grid <- somgrid(xdim = 10, ydim = 10, topo = "hexagonal")

# run som
# set up as data matrix
mat_sub <- as.matrix(sub12[,c('newspapers', 'magazines', 'radio', 'tv','internet')])
som_sub <- som(mat_sub, grid = grid, rlen = 10000) 

par(mfrow = c(1,1))
plot(som_sub, type = "codes")
plot(som_sub, type = "changes")
plot(som_sub, type = "counts")
plot(som_sub, type = "dist.neighbours")
plot(som_sub, type = "quality")

par(mfrow = c(3,2))
plot(som_sub, type = "property", property = as.data.frame(som_sub$codes)[,1], main = names(sub12['newspapers']))
plot(som_sub, type = "property", property = as.data.frame(som_sub$codes)[,2], main = names(sub12['magazines']))
plot(som_sub, type = "property", property = as.data.frame(som_sub$codes)[,3], main = names(sub12['radio']))
plot(som_sub, type = "property", property = as.data.frame(som_sub$codes)[,4], main = names(sub12['tv']))
plot(som_sub, type = "property", property = as.data.frame(som_sub$codes)[,5], main = names(sub12['internet']))

par(mfrow = c(1,1))
plot(som_sub, type = "mapping", bgcol = sub12$cluster ) # not very good organising??

# Try pca to get sense of relative use of media type... not very helpful since in most cases require many components to reflect variation in the data.

mags_pca <- princomp(scale(magazines_engagement_12))
screeplot(mags_pca, type = "lines")
newsp_pca <- princomp(scale(newspapers_engagement_12))
screeplot(newsp_pca, type = "lines")
tv_pca <- princomp(scale(tv_engagement_12))
screeplot(tv_pca, type = "lines")
rad_pca <- princomp(scale(radio_engagement_12[,-60])) # cant divide by zero
screeplot(rad_pca, type = "lines")
int_pca <- princomp(scale(internet_engagement_12))
screeplot(int_pca, type = "lines")

all_pca <- princomp(set12[,c('newspapers','magazines', 'tv', 'radio', 'internet')])
screeplot(all_pca, type = "lines")
summary(all_pca) # first component could be useful (@~40% of variation) to give relative multimedia scores

# try kmeans on the first pca and compare with cluster values...
test <- kmeans(all_pca$scores[,1], centers = 6)
test$cluster
set12$cluster
cor(test$cluster, as.numeric(set12$cluster))

# consider for some predictions:
# create training and test sets:

set.seed(56)
ind_train <- createDataPartition(set12$cluster, p = 0.7, list = FALSE)
training <- set12[ind_train,]
testing <- set12[-ind_train,]

# using only media type as set12_CT_factors
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
confusionMatrix(pred_lda12$class, testing$cluster) # 

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

# consider a single tree partitioning to try to add meaning to the six clusters
control <- rpart.control(maxdepth = 3, cp = 0.001)
tree12 <- rpart(cluster ~ newspapers + tv + radio + magazines + internet, 
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
jpeg('typeBoxPlots.jpeg', quality = 100, type = "cairo")
par(mfrow = c(2,3))
plot(set12$radio ~ set12$cluster, col = c(2,3,4,5,6), main = "radio", xlab = "cluster", ylab = '')
plot(set12$tv ~ set12$cluster, col = c(2,3,4,5,6), main = "tv", xlab = "cluster", ylab = '')
plot(set12$newspapers ~ set12$cluster, col = c(2,3,4,5,6), main = "newspapers", xlab = "cluster", ylab = '')
plot(set12$magazines ~ set12$cluster, col = c(2,3,4,5,6), main = "magazines", xlab = "cluster", ylab = '')
plot(set12$internet ~ set12$cluster, col = c(2,3,4,5,6), main = "internet", xlab = "cluster", ylab = '')
dev.off()

# try to make sense of demographics
jpeg('typeDemogPlots1.jpeg', quality = 100, type = "cairo")
par(mfrow = c(2,2))
plot(set12$cluster ~ factor(set12$race,labels = c("black", "coloured", "indian", "white")), col = c(2,3,4,5,6), main = "race", xlab = "", ylab = "")
plot(set12$cluster ~ factor(set12$edu, labels = c("<matric", "matric",">matric" )), col = c(2,3,4,5,6), main = "education", xlab = "", ylab = "")
plot(set12$cluster ~ factor(set12$age, labels = c("15-24","25-44", "45-54","55+")), col = c(2,3,4,5,6), main = "age", xlab = "", ylab = "")
plot(set12$cluster ~ factor(set12$lsm, labels = c("1-2", "3-4", "5-6", "7-8", "9-10")), col = c(2,3,4,5,6), main = "LSM", xlab = "", ylab = "")
dev.off()

jpeg('typeDemogPlots2.jpeg', quality = 100, type = "cairo")
par(mfrow = c(2,2))
plot(set12$cluster ~ factor(set12$sex, labels = c("male", "female")), col = c(2,3,4,5,6), main = "sex", xlab = "", ylab = "")
plot(set12$cluster ~ factor(set12$hh_inc, labels = c("<2500","2500-6999","7000-11999",">=12000")), col = c(2,3,4,5,6), main = "hh_inc", xlab = "", ylab = "")
plot(set12$cluster ~ set12$lifestages, col = c(2,3,4,5,6), main = "lifestages", xlab = "", ylab = "")
plot(set12$cluster ~ set12$lifestyle, col = c(2,3,4,5,6), main = "lifestyle", xlab = "", ylab = "")
dev.off()

# then a need to consider media vehicles and metropoles as in '95
# LEVEL 2

# subset by metropoles (find out two / two largest Cape, Gauteng)?
# focus on media vehicles cape town and jhb
# cape town and cape town fringe = metro == 1 | 2
# greater jhb = metro == 7

# isolate cape town
set12_CT <- set12 %>% filter(metro == 1) # first did 1 and 2 but found marginal factor messes up...

# isolate johannesburg
set12_JHB <- set12 %>% filter(metro == 7)

# actually want to consider only those variables with a reasonable number (consider 10%)
# cap town
tempVec <- vector()
for(i in 1:ncol(set12_CT)) {
        tempVec[i] <- sum(set12_CT[,i] != 0, na.rm = TRUE)
}

set12_CT <- set12_CT[,which(tempVec > 0.1*nrow(set12_CT))]

# johannesburg
tempVec_ct <- vector()
for(i in 1:ncol(set12_JHB)) {
        tempVec_ct[i] <- sum(set12_JHB[,i] != 0, na.rm = TRUE)
}

set12_JHB <- set12_JHB[,which(tempVec_ct > 0.1*nrow(set12_JHB))]

# also want to consider only main languages in Cape Town (jhb seems more divided in language)
table(set12_CT$lang)
table(set12_JHB$lang)

# in Cape Town (English, Afrikaans, Xhosa)
# ie Same: 1 = Afrikaans; 2 = English
#  Change 4 to 3 = Xhosa.

set12_CT$lang <- ifelse(set12_CT$lang == 3, 12, set12_CT$lang)
set12_CT$lang <- ifelse(set12_CT$lang == 4, 3, set12_CT$lang)
set12_CT$lang <- ifelse(!set12_CT$lang %in% c(1,2,3), 4, set12_CT$lang)
set12_CT$lang <- factor(set12_CT$lang)

# scale the media vehicle columns (note, exclude the clusters for type columns)
set12_CT[,22:59] <- scale(set12_CT[,22:59])
set12_JHB[,22:64] <- scale(set12_JHB[,22:64])

## first pca to get sense of latent structure dimensions
## Cape Town
pca12_CT <- princomp(set12_CT[,22:59])
summary(pca12_CT)

jpeg("pca_screeplot_CT.jpeg")
par(mfrow = c(1,1))
screeplot(pca12_CT, type = "lines", npcs = 15, main = "Cape Town") # looks like elbow at 7. Strange drop for second PC??
abline(v = 7, lty = 2)
dev.off()

## Johannesburg
pca12_JHB <- princomp(set12_JHB[,22:64])
summary(pca12_JHB)

jpeg("pca_screeplot_JHB.jpeg")
par(mfrow = c(1,1))
screeplot(pca12_JHB, type = "lines", npcs = 15, main = "Johannesburg") # looks like elbow at 6 or 8. will stick with 7 to be aligned with CT (is that right??)
abline(v = 8, lty = 2)
dev.off()

# factor analysis on seven factors:
# for cape town
fa12_CT <- factanal(set12_CT[,22:59], factors = 7, scores = "regression")
fa12_CT # 35% variance explained

# for johannesburg
fa12_JHB <- factanal(set12_JHB[,22:64], factors = 8, scores = "regression")
fa12_JHB # 32% variance explained

## want to consider media vehicle loadings of the seven factors:
## cape town (note: want to try to ensure factor 1 for cape town is similar to factor 1 jhb, see below (will use Cape Town as basis))

# first create loadings dataframe
loadings_CT <- fa12_CT$loadings
vehicles_CT = rownames(loadings_CT)
loadings_CT <- data.frame(vehicles_CT, loadings_CT[,1:7])

# factor 1
one_ct <- loadings_CT %>% arrange(desc(Factor1)) %>% head(10) # SABC3, etv, SABC2, SABC1, DailyVoice, Son: (Afrikaans, Coloured/Indian, mature singles/mature couples, midlevelLSM, lower education)
write.table(data.frame(vehicle = one_ct[,1], loading = round(one_ct[,2],2)), file = "one_ct.csv")

# factor 2
two_ct <- loadings_CT %>% arrange(desc(Factor2)) %>% head(10) # umhlobo wene, drum, metro fm, daily sun, kickoff, sabc1, jetclub: black, lower age,, lower income, xhosa, singles/young indp/sing fam, lower lsm, lifestyle 7,8,10
write.table(data.frame(vehicle = two_ct[,1], loading = round(two_ct[,3],2)), file = "two_ct.csv")

# factor 3
three_ct <- loadings_CT %>% arrange(desc(Factor3)) %>% head(10) # int_search, int_social, int_news, 5FM, int_print, kfm, goodhope: higer edu, higher hhinc, athomesingles/young fam/ younger, higher lsm, cell sophisticates/bars&betters
write.table(data.frame(vehicle = three_ct[,1], loading = round(three_ct[,4],2)), file = "three_ct.csv")

# factor 4
four_ct <- loadings_CT %>% arrange(desc(Factor4)) %>% head(10) # you, cape argus, sunday times, cape times, dstv, premiumCD, fairlady, mnet main, weargus,people, menshealth, goodhope: older, higherincome, seperated, higherLSM
write.table(data.frame(vehicle = four_ct[,1], loading = round(four_ct[,5],2)), file = "four_ct.csv")

# factor 5
five_ct <- loadings_CT %>% arrange(desc(Factor5)) %>% head(10) # huisgenoot, die burger, rapportsun, son, radio tygerberg: older, lower edu, afrikaans, mature family, avid readers
write.table(data.frame(vehicle = five_ct[,1], loading = round(five_ct[,6],2)), file = "five_ct.csv")

# factor 6
six_ct <- loadings_CT %>% arrange(desc(Factor6)) %>% head(10) # intPrint, int_news, (lower loadings on: int-social, int search, metrofm, mnet main, 5fm): higher edu,higher income, youngind/youngcouples,higher lsm, ages (20-40)
write.table(data.frame(vehicle = six_ct[,1], loading = round(six_ct[,7],2)), file = "six_ct.csv")

# factor 7
seven_ct <- loadings_CT %>% arrange(desc(Factor7)) %>% head(10) # intPrint, int_news, (lower loadings on: int-social, int search, metrofm, mnet main, 5fm): higher edu,higher income, youngind/youngcouples,higher lsm, ages (20-40)
write.table(data.frame(vehicle = seven_ct[,1], loading = round(seven_ct[,8],2)), file = "seven_ct.csv")

## for johannesburg

# first create loadings dataframe for jhb
loadings_JHB<- fa12_JHB$loadings
vehicles_JHB = rownames(loadings_JHB)
loadings_JHB <- data.frame(vehicles_JHB, loadings_JHB[,1:8])

# factor 1 jhb
one_jhb <- loadings_JHB %>% arrange(desc(Factor1)) %>% head(10) # ct Factor 3 ?
write.table(data.frame(vehicle = one_jhb[,1], loading = round(one_jhb[,2],2)), file = "one_jhb.csv")

# factor 2 jhb
two_jhb <- loadings_JHB %>% arrange(desc(Factor2)) %>% head(10) # ct Factor 1 ?
write.table(data.frame(vehicle = two_jhb[,1], loading = round(two_jhb[,3],2)), file = "two_jhb.csv")

# factor 3 jhb
three_jhb <- loadings_JHB %>% arrange(desc(Factor3)) %>% head(10) # ct Factor 4 ?
write.table(data.frame(vehicle = three_jhb[,1], loading = round(three_jhb[,4],2)), file = "three_jhb.csv")

# factor 4 jhb
four_jhb <- loadings_JHB %>% arrange(desc(Factor4)) %>% head(10) # ct Factor 2 ?
write.table(data.frame(vehicle = four_jhb[,1], loading = round(four_jhb[,5],2)), file = "four_jhb.csv")

# factor 5 jhb
five_jhb <- loadings_JHB %>% arrange(desc(Factor5)) %>% head(10) # ct Factor 3/7 ?
write.table(data.frame(vehicle = five_jhb[,1], loading = round(five_jhb[,6],2)), file = "five_jhb.csv")

# factor 6 jhb
six_jhb <- loadings_JHB %>% arrange(desc(Factor6)) %>% head(10)  # ct Factor 6 ?
write.table(data.frame(vehicle = six_jhb[,1], loading = round(six_jhb[,7],2)), file = "six_jhb.csv")

# factor 7 jhb
seven_jhb <- loadings_JHB %>% arrange(desc(Factor7)) %>% head(10) # ct Factor 4 ?
write.table(data.frame(vehicle = seven_jhb[,1], loading = round(seven_jhb[,8],2)), file = "seven_jhb.csv")

# factor 8 jhb
eight_jhb <- loadings_JHB %>% arrange(desc(Factor8)) %>% head(10) # c?
write.table(data.frame(vehicle = eight_jhb[,1], loading = round(eight_jhb[,9],2)), file = "eight_jhb.csv")

## kmeans to determine categories based on proximity to all seven factor scores 

# first determine sensible number of centers:
# for cape town
wss_ct <- vector()
for(k in 3:20) {
        temp <- kmeans(fa12_CT$scores,
                       centers = k,
                       nstart = 3,
                       iter.max = 20)
        wss_ct <- append(wss_ct,temp$tot.withinss)
}

jpeg('kmeansPlot_12_CT.jpeg')
plot(3:20, wss_ct, type = "b", xlab = "k-values", ylab = "total within sum of squares", main = "Cape Town" )
abline(v = 8, lty = 2)
dev.off()

# for johannesburg
wss_jhb <- vector()
for(k in 3:20) {
        temp <- kmeans(fa12_JHB$scores,
                       centers = k,
                       nstart = 3,
                       iter.max = 20)
        wss_jhb <- append(wss_jhb,temp$tot.withinss)
}

jpeg('kmeansPlot_12_JHB.jpeg')
plot(3:20, wss_jhb, type = "b", xlab = "k-values", ylab = "total within sum of squares", main = "Johannesburg" )
abline(v = 7, lty = 2)
dev.off()

# eight for Cape Town and seven for Johannesburg
# cape town
set.seed(56)
kmeans12_CT <- kmeans(fa12_CT$scores, centers = 8, nstart = 5, iter.max = 30)
table(kmeans12_CT$cluster) # reasonable distribution

# johannesburg
set.seed(56)
kmeans12_JHB <- kmeans(fa12_JHB$scores, centers = 7, nstart = 5, iter.max = 30)
table(kmeans12_JHB$cluster) # reasonable distribution

# checking centers 
kmeans12_CT$centers
kmeans12_JHB$centers

## add kmeans groups as factors to dataset
## cape town
set12_CT <- set12_CT %>%
        mutate(group = factor(kmeans12_CT$cluster))

## johannesburg
set12_JHB <- set12_JHB %>%
        mutate(group = factor(kmeans12_JHB$cluster))

## save
## save the cape town set:
saveRDS(set12_CT, "set12_CT.rds")
set12_CT <- readRDS("set12_CT.rds")

## save the johannesburg set:
saveRDS(set12_JHB, "set12_JHB.rds")
set12_JHB <- readRDS("set12_JHB.rds")

## want to profile each media group by media type and demographics

## cape town & jhb
## 
# consider media-type engagement values and media groups
jpeg("mediaTypeVsGroup_12_CT")
par(mfrow = c(2,3), oma = c(0,0,2,0))
plot(set12_CT$radio ~ set12_CT$group, col = c(1,2,3,4,5,6,7,8), main = "radio", xlab = "group", ylab = '')
plot(set12_CT$tv ~ set12_CT$group, col = c(1,2,3,4,5,6,7,8), main = "tv", xlab = "group", ylab = '')
plot(set12_CT$newspapers ~ set12_CT$group, col = c(1,2,3,4,5,6,7,8), main = "newspapers", xlab = "group", ylab = '')
plot(set12_CT$magazines ~ set12_CT$group, col = c(1,2,3,4,5,6,7,8), main = "magazines", xlab = "group", ylab = '')
title("Cape Town", outer = TRUE, cex.main = 2.5)
dev.off()

jpeg("mediaTypeVsGroup_12_JHB")
par(mfrow = c(2,3), oma = c(0,0,2,0))
plot(set12_JHB$radio ~ set12_JHB$group, col = c(2,3,4,5,6,7,8), main = "radio", xlab = "group", ylab = '')
plot(set12_JHB$tv ~ set12_JHB$group, col = c(2,3,4,5,6,7,8), main = "tv", xlab = "group", ylab = '')
plot(set12_JHB$newspapers ~ set12_JHB$group, col = c(2,3,4,5,6,7,8), main = "newspapers", xlab = "group", ylab = '')
plot(set12_JHB$magazines ~ set12_JHB$group, col = c(2,3,4,5,6,7,8), main = "magazines", xlab = "group", ylab = '')
plot(set12_JHB$internet ~ set12_JHB$group, col = c(2,3,4,5,6,7,8), main = "internet", xlab = "group", ylab = '')
title("Johannesburg", outer = TRUE, cex.main = 2.5)
dev.off()

## consider demographics and media groups

#cape town
jpeg("groupVsDemog1_12_CT")
par(mfrow = c(2,3), oma = c(0,0,2,0))
plot(set12_CT$group ~ factor(set12_CT$age, labels = c("15-24","25-44", "45-54","55+")), col = c(1,2,3,4,5,6,7,8), ylab = 'group', xlab = 'age')
plot(set12_CT$group ~ factor(set12_CT$sex, labels = c("male", "female")), col = c(1,2,3,4,5,6,7,8), ylab = 'group', xlab = 'sex')
plot(set12_CT$group ~ factor(set12_CT$edu, labels = c("<matric", "matric",">matric" )), col = c(1,2,3,4,5,6,7,8), ylab = 'group', xlab = 'education')
plot(set12_CT$group ~ factor(set12_CT$hh_inc, labels = c("<2500","2500-6999","7000-11999",">=12000")), col = c(1,2,3,4,5,6,7,8), ylab = 'group', xlab = 'hh_income')
title("Cape Town", outer = TRUE, cex.main = 2.5)
dev.off()

# johannesburg
jpeg("groupVsDemog1_12_JHB")
par(mfrow = c(2,3), oma = c(0,0,2,0))
plot(set12_JHB$group ~ factor(set12_JHB$age, labels = c("15-24","25-44", "45-54","55+")), col = c(2,3,4,5,6,7,8), ylab = 'group', xlab = 'age')
plot(set12_JHB$group ~ factor(set12_JHB$sex, labels = c("male", "female")), col = c(2,3,4,5,6,7,8), ylab = 'group', xlab = 'sex')
plot(set12_JHB$group ~ factor(set12_JHB$edu, labels = c("<matric", "matric",">matric" )), col = c(2,3,4,5,6,7,8), ylab = 'group', xlab = 'education')
plot(set12_JHB$group ~ factor(set12_JHB$hh_inc, labels = c("<2500","2500-6999","7000-11999",">=12000")), col = c(2,3,4,5,6,7,8), ylab = 'group', xlab = 'hh_income')
title("Johannesburg", outer = TRUE, cex.main = 2.5)
dev.off()
# 
# ##
# cape town
jpeg("groupVsDemog2_12_CT")
par(mfrow = c(2,3), oma = c(0,0,2,0))
plot(set12_CT$group ~ factor(set12_CT$race,labels = c("black", "coloured", "indian", "white")), col = c(1,2,3,4,5,6,7,8), ylab = 'group', xlab = 'race')
plot(set12_CT$group ~ factor(set12_CT$lang, labels = c("Afrikaans", "English", "Xhosa", "Other")), col = c(1,2,3,4,5,6,7,8), ylab = 'group', xlab = 'language')
plot(set12_CT$group ~ factor(set12_CT$lsm, labels = c("1-2", "3-4", "5-6", "7-8", "9-10")), col = c(1,2,3,4,5,6,7,8), ylab = 'group', xlab = 'LSM')
plot(set12_CT$group ~ set12_CT$cluster, col = c(1,2,3,4,5,6,7,8), ylab = 'group', xlab = 'cluster')
title("Cape Town", outer = TRUE, cex.main = 2.5)
dev.off()

# johannesburg
jpeg("groupVsDemog2_12_JHB")
par(mfrow = c(2,3), oma = c(0,0,2,0))
plot(set12_JHB$group ~ factor(set12_JHB$race,labels = c("black", "coloured", "indian", "white")), col = c(2,3,4,5,6,7,8), ylab = 'group', xlab = 'race')
plot(set12_JHB$group ~ set12_JHB$lang, col = c(2,3,4,5,6,7,8), ylab = 'group', xlab = 'language')
plot(set12_JHB$group ~ factor(set12_JHB$lsm, labels = c("1-2", "3-4", "5-6", "7-8", "9-10")), col = c(2,3,4,5,6,7,8), ylab = 'group', xlab = 'LSM')
plot(set12_JHB$group ~ set12_JHB$cluster, col = c(2,3,4,5,6,7,8), ylab = 'group', xlab = 'cluster')
title("Johannesburg", outer = TRUE, cex.main = 2.5)
dev.off()

# cape town
jpeg("groupVsDemog3_12_CT")
par(mfrow = c(2,3), oma = c(0,0,4,0))
plot(set12_CT$group ~ set12_CT$lifestyle, col = c(1,2,3,4,5,6,7,8), ylab = 'group', xlab = 'lifestyle')
plot(set12_CT$group ~ set12_CT$attitudes, col = c(1,2,3,4,5,6,7,8), ylab = 'group', xlab = 'attitudes')
plot(set12_CT$group ~ set12_CT$lifestages, col = c(1,2,3,4,5,6,7,8), ylab = 'group', xlab = 'lifestages')
plot(set12_CT$group ~ set12_CT$mar_status, col = c(1,2,3,4,5,6,7,8), ylab = 'group', xlab = 'marital status')
title("Cape Town", outer = TRUE, cex.main = 2.5)
dev.off()

# johannesburg
jpeg("groupVsDemog3_12_JHB")
par(mfrow = c(2,3), oma = c(0,0,4,0))
plot(set12_JHB$group ~ set12_JHB$lifestyle, col = c(2,3,4,5,6,7,8), ylab = 'group', xlab = 'lifestyle')
plot(set12_JHB$group ~ set12_JHB$attitudes, col = c(2,3,4,5,6,7,8), ylab = 'group', xlab = 'attitudes')
plot(set12_JHB$group ~ set12_JHB$lifestages, col = c(2,3,4,5,6,7,8), ylab = 'group', xlab = 'lifestages')
plot(set12_JHB$group ~ set12_JHB$mar_status, col = c(2,3,4,5,6,7,8), ylab = 'group', xlab = 'marital status')
title("Johannesburg", outer = TRUE, cex.main = 2.5)
dev.off()

## want to interpret media groups according to kmeans centroid coefficients:

# look at them and print to csv files
kmeans12_CT$centers
write.table(data.frame(round(kmeans12_CT$centers, 4)), file = "centroids_12_CT.csv")

kmeans12_JHB$centers
write.table(data.frame(round(kmeans12_JHB$centers, 4)), file = "centroids_12_JHB.csv")

## RETURN to try this...maybe good ..
# try consider tree for explanatory reasons:
control3 <- rpart.control(maxdepth = 4, cp = 0.0001)
tree12_CT <- rpart(factor_ct ~ age + 
                           sex + 
                           edu + 
                           hh_inc + 
                           race + 
                           lang +
                           lifestages + 
                           mar_status +
                           # pers_inc +
                           lsm +
                           lifestyle +
                           attitudes,
                           # cluster,
                   control = control3,
                   data = set12_CT)

plot(tree12_CT)
text(tree12_CT, pretty = 0)
rpart.plot(tree12_CT, type = 4, extra = 1)





## consider Prediction to get sense of value of media groups

# cape town
# separate into test and training sets for this round
set.seed(56)
ind_rf_ct <- createDataPartition(set12_CT$group, p = 0.7, list = FALSE)
training_rf_ct <- set12_CT[ind_rf_ct,]
testing_rf_ct <- set12_CT[-ind_rf_ct,]
forest12_CT <- randomForest(group ~ age + 
                                    sex + 
                                    edu + 
                                    hh_inc +
                                    race +
                                    lang + 
                                    lifestages +
                                    mar_status +
                                    lsm +
                                    lifestyle +
                                    attitudes +
                                    cluster,
                                    # newspapers +
                                    # magazines +
                                    # radio +
                                    # tv +
                                    # internet,
                            data = training_rf_ct)

pred12_rf_CT <- predict(forest12_CT, newdata = testing_rf_ct)
confusionMatrix(pred12_rf_CT, testing_rf_ct$group) # ~ 55 accuracy on test set.

## for johannesburg
# separate into test and training sets for this round
set.seed(56)
ind_rf_jhb <- createDataPartition(set12_JHB$group, p = 0.7, list = FALSE)
training_rf_jhb <- set12_CT[ind_rf_jhb,]
testing_rf_jhb <- set12_CT[-ind_rf_jhb,]
forest12_JHB <- randomForest(group ~ age + 
                                    sex + 
                                    edu + 
                                    hh_inc +
                                    race +
                                    lang + 
                                    lifestages +
                                    mar_status +
                                    lsm +
                                    lifestyle +
                                    attitudes +
                                    cluster,
                            # newspapers +
                            # magazines +
                            # radio +
                            # tv +
                            # internet,
                            data = training_rf_jhb)

pred12_rf_JHB <- predict(forest12_JHB, newdata = testing_rf_jhb)
confusionMatrix(pred12_rf_JHB, testing_rf_jhb$group) # ~ 55 accuracy on test set.

## try some linear regression 

## cape town
factor_scores_12_ct <- as.data.frame(fa12_CT$scores) # scaled or not??
set12_CT_factors <- cbind(set12_CT, factor_scores_12_ct)

# creating scaled numeric variables for the ordered factors:
set12_CT_factors$age <- scale(as.numeric(set12_CT_factors$age))
set12_CT_factors$edu <- scale(as.numeric(set12_CT_factors$edu))
set12_CT_factors$hh_inc <- scale(as.numeric(set12_CT_factors$hh_inc))
set12_CT_factors$lsm <- scale(as.numeric(set12_CT_factors$lsm))

# separate into test and training sets for this round
set.seed(56)
ind_lm_12_ct <- createDataPartition(set12_CT_factors$qn, p = 0.7, list = FALSE)
training_lm_12_ct <- set12_CT_factors[ind_lm_12_ct,]
testing_lm_12_ct <- set12_CT_factors[-ind_lm_12_ct,]

# regression on each factor score to ascertain most important predictors
lm1_ct <- lm(Factor1 ~ age + sex + edu + hh_inc + race  + lang + lifestages + mar_status + lsm + lifestyle + attitudes + cluster, data = training_lm_12_ct)
lm2_ct <- lm(Factor2 ~ age + sex + edu + hh_inc + race + lang + lifestages + mar_status + lsm + lifestyle + attitudes + cluster, data = training_lm_12_ct)
lm3_ct <- lm(Factor3 ~ age + sex + edu + hh_inc + race + lang + lifestages + mar_status + lsm + lifestyle + attitudes + cluster, data = training_lm_12_ct)
lm4_ct <- lm(Factor4 ~ age + sex + edu + hh_inc + race + lang + lifestages + mar_status + lsm + lifestyle + attitudes + cluster, data = training_lm_12_ct)
lm5_ct <- lm(Factor5 ~ age + sex + edu + hh_inc + race + lang + lifestages + mar_status + lsm + lifestyle + attitudes + cluster, data = training_lm_12_ct)
lm6_ct <- lm(Factor6 ~ age + sex + edu + hh_inc + race + lang + lifestages + mar_status + lsm + lifestyle + attitudes + cluster, data = training_lm_12_ct)
lm7_ct <- lm(Factor7 ~ age + sex + edu + hh_inc + race + lang + lifestages + mar_status + lsm + lifestyle + attitudes + cluster, data = training_lm_12_ct)

# to examine and print linear regression object

lm_summary_example <- summary(lm2_ct)
write.table(data.frame(round(lm_summary_example$coefficients,4)), file = "lm_summary_example.csv")

sqrt(mean((predict(lm2_ct, newdata = testing_lm_12_ct) - testing_lm_12_ct$Factor1)^2))
range(training_lm_12_ct$Factor2)[2] - range(training_lm_12_ct$Factor2)[1]

## and canonical collerations...
# Some canonical trials on using selected predictor (demographic) variables
# 
# creating model matrix for dummy variables of selected unordered factors:
set12_CT_factors_dummies <- as.data.frame(model.matrix(~ age + sex + edu + hh_inc + race + lsm + cluster + Factor1 + Factor2 + Factor3 + Factor4 + Factor5 + Factor6 + Factor7, data = set12_CT_factors))

# get sense of correlations of the factors and the demographic variables
cor(set12_CT_factors[,c("Factor1", "Factor2", "Factor3", "Factor4", "Factor5", "Factor6", "Factor7")])
cor(set12_CT_factors_dummies)

cannonical_cor <- cancor(x = set12_CT_factors_dummies[,2:13], y = set12_CT_factors_dummies[,14:20])
cannonical_cor

# just making sure I understand what's going on "under the hood"...
v_1 <- rep(0, 1960)
for(i in 1:12) {
        vector_v1 <- cannonical_cor$xcoef[i,1] * (set12_CT_factors_dummies[,i + 1] - mean(set12_CT_factors_dummies[,i + 1]))
        v_1 <- v_1 + vector_v1
}


# v_1 <- cannonical_cor$xcoef[1,1] * (set12_CT_factors_test[,1] - mean(set12_CT_factors_test[,1])) +
#         cannonical_cor$xcoef[2,1] * (set12_CT_factors_test[,2] - mean(set12_CT_factors_test[,2])) +
#         cannonical_cor$xcoef[3,1] * (set12_CT_factors_test[,3] - mean(set12_CT_factors_test[,3])) +
#         cannonical_cor$xcoef[4,1] * (set12_CT_factors_test[,4] - mean(set12_CT_factors_test[,4])) +
#         cannonical_cor$xcoef[5,1] * (set12_CT_factors_test[,5] - mean(set12_CT_factors_test[,5])) +
#         cannonical_cor$xcoef[6,1] * (set12_CT_factors_test[,6] - mean(set12_CT_factors_test[,6])) +
#         cannonical_cor$xcoef[7,1] * (set12_CT_factors_test[,7] - mean(set12_CT_factors_test[,7]))

u_1 <- cannonical_cor$ycoef[1,1] * set12_CT_factors_dummies[,14] + # since means = 0, no need to subtract them
        cannonical_cor$ycoef[2,1] * set12_CT_factors_dummies[,15] +
        cannonical_cor$ycoef[3,1] * set12_CT_factors_dummies[,16] +
        cannonical_cor$ycoef[4,1] * set12_CT_factors_dummies[,17] +
        cannonical_cor$ycoef[5,1] * set12_CT_factors_dummies[,18] +
        cannonical_cor$ycoef[6,1] * set12_CT_factors_dummies[,19] +
        cannonical_cor$ycoef[7,1] * set12_CT_factors_dummies[,20]

cor(v_1,u_1) # matches...coool

# follow up on this... what do I do with standardised coefficients???
u1_standardised <- cannonical_cor$xcoef[,1] * apply(set12_CT_factors_dummies[,2:13], 2, sd)
v1_standardised <- cannonical_cor$ycoef[,1] * apply(set12_CT_factors_dummies[,14:20], 2, sd)
write.table(data.frame(round(u1_standardised, 5)), file = "u1_std_12_CT.csv")
write.table(data.frame(round(v1_standardised, 5)), file = "v1_std_12_CT.csv")

# just making sure I understand what's going on "under the hood"...
v_2 <- rep(0, 1960)
for(i in 1:12) {
        vector_v2 <- cannonical_cor$xcoef[i,2] * (set12_CT_factors_dummies[,i + 1] - mean(set12_CT_factors_dummies[,i + 1]))
        v_2 <- v_2 + vector_v2
}

u_2 <- cannonical_cor$ycoef[1,2] * set12_CT_factors_dummies[,14] + # since means = 0, no need to subtract them
        cannonical_cor$ycoef[2,2] * set12_CT_factors_dummies[,15] +
        cannonical_cor$ycoef[3,2] * set12_CT_factors_dummies[,16] +
        cannonical_cor$ycoef[4,2] * set12_CT_factors_dummies[,17] +
        cannonical_cor$ycoef[5,2] * set12_CT_factors_dummies[,18] +
        cannonical_cor$ycoef[6,2] * set12_CT_factors_dummies[,19] +
        cannonical_cor$ycoef[7,2] * set12_CT_factors_dummies[,20]
cor(v_2, u_2) # right

u2_standardised <- cannonical_cor$xcoef[,2] * apply(set12_CT_factors_dummies[,2:13], 2, sd)
v2_standardised <- cannonical_cor$ycoef[,2] * apply(set12_CT_factors_dummies[,14:20], 2, sd)
write.table(data.frame(round(u2_standardised, 5)), file = "u2_std_12_CT.csv")
write.table(data.frame(round(v2_standardised, 5)), file = "v2_std_12_CT.csv")

jpeg("ccPlotsExperimental.jpeg")
par(mfrow = c(1,2))
plot(v_1, u_1) # good elliptical pattern... so can have some confidence...
plot(v_2, u_2)
dev.off()

# try to understand what causes what seems to be very clear clustering (not too sure what to do about it though):
try_ind2 <- v_2 > 0.01

tops <- set12_CT[try_ind2,]
bottoms <- set12_CT[!try_ind2,]

table(tops$race)
table(bottoms$race) # mainly....??

