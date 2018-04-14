#https://stackoverflow.com/questions/17273949/clustered-bar-plot-in-r-using-ggplot2

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
library(CCA)
library(nFactors)
library(FactoMineR)
library(factoextra)
library(gridExtra)
library(ggplot2)

# load datafiles 
set12c <- readRDS("set12c.rds")
set12c_simple <- readRDS("set12c_simple.rds")

# LEVEL 2

# subset by metropoles
# focus on media vehicles cape town and jhb
# cape town: metro == 1 | 2
# greater jhb: metro == 7

# isolate cape town
set12_CT <- set12c %>% filter(metro == 1) 

# isolate johannesburg
set12_JHB <- set12c %>% filter(metro == 7)

# get rid of near zero variances:
ind_ct <- nearZeroVar(set12_CT[,23:ncol(set12_CT)], saveMetrics = TRUE)
ind_jhb <- nearZeroVar(set12_JHB[,23:ncol(set12_JHB)], saveMetrics = TRUE)

good_ct <- set12_CT[,23:ncol(set12_CT)][,!ind_ct$nzv]
good_jhb <- set12_JHB[,23:ncol(set12_JHB)][,!ind_jhb$nzv]

catSet12_CT <- data.frame(cbind(set12_CT[,1:22], good_ct))
catSet12_JHB <- data.frame(cbind(set12_JHB[,1:22], good_jhb))

saveRDS(catSet12_CT, "catSet12_CT.rds")
saveRDS(catSet12_JHB, "catSet12_JHB.rds")

nuSet12_CT <- data.frame(cbind(set12_CT[,1:22], good_ct))
nuSet12_JHB <- data.frame(cbind(set12_JHB[,1:22], good_jhb))

#setting the ordered variables as scaled numerical:
nuSet12_CT$age <- scale(as.numeric(nuSet12_CT$age))
nuSet12_CT$edu <- scale(as.numeric(nuSet12_CT$edu))
nuSet12_CT$hh_inc <- scale(as.numeric(nuSet12_CT$hh_inc))
nuSet12_CT$lsm <- scale(as.numeric(nuSet12_CT$lsm))

nuSet12_JHB$age <- scale(as.numeric(nuSet12_JHB$age))
nuSet12_JHB$edu <- scale(as.numeric(nuSet12_JHB$edu))
nuSet12_JHB$hh_inc <- scale(as.numeric(nuSet12_JHB$hh_inc))
nuSet12_JHB$lsm <- scale(as.numeric(nuSet12_JHB$lsm))

# naming the factors

# Cape Town
nuSet12_CT$cluster <- factor(nuSet12_CT$cluster,
                                        levels = c(1,2,3,4),
                                        labels = c("heavy all media", "internet lead", "light all media", "internet lag"))
nuSet12_CT$sex <- factor(nuSet12_CT$sex,
                                 levels = c(1,2),
                                 labels = c("male", "female"))
nuSet12_CT$race <- factor(nuSet12_CT$race,
                                  levels = c(1,2,3,4),
                                  labels = c("black", "coloured", "indian", "white"))
nuSet12_CT$lifestages <- factor(nuSet12_CT$lifestages,
                                        levels = c(1,2,3,4,5,6,7,8),
                                        labels = c("at home singles", "young independent singles", "mature singles", "young couples", "mature couples", "young family", "single parent family", "mature family"))
nuSet12_CT$mar_status <- factor(nuSet12_CT$mar_status,
                                        levels = c(1,2,3,4,5),
                                        labels = c("single", "married or living together", "widowed", "divorced", "separated"))
nuSet12_CT$lifestyle <- factor(nuSet12_CT$lifestyle,
                                       levels = c(1,2,3,4,5,6,7,8,9,10,11,12),
                                       labels = c("none", "cell sophisticates", "sports", "gamers", "outdoors", "good living", "avid readers", "traditionalists","cell fundamentals", "homebodies", "studious", "showgoers"))
nuSet12_CT$attitudes <- factor(nuSet12_CT$attitudes,
                                       levels = c(1,2,3,4,5,6,7),
                                       labels = c("none", "now generation", "nation builders", "distants survivors", "distants established", "rooted", "global citizens"))

# Johannesburg
# 
nuSet12_JHB$cluster <- factor(nuSet12_JHB$cluster,
                             levels = c(1,2,3,4),
                             labels = c("heavy all media", "internet lead", "light all media", "internet lag"))
nuSet12_JHB$sex <- factor(nuSet12_JHB$sex,
                                  levels = c(1,2),
                                  labels = c("male", "female"))
nuSet12_JHB$race <- factor(nuSet12_JHB$race,
                                   levels = c(1,2,3,4),
                                   labels = c("black", "coloured", "indian", "white"))
nuSet12_JHB$lifestages <- factor(nuSet12_JHB$lifestages,
                                         levels = c(1,2,3,4,5,6,7,8),
                                         labels = c("at home singles", "young independent singles", "mature singles", "young couples", "mature couples", "young family", "single parent family", "mature family"))
nuSet12_JHB$mar_status <- factor(nuSet12_JHB$mar_status,
                                         levels = c(1,2,3,4,5),
                                         labels = c("single", "married or living together", "widowed", "divorced", "separated"))
nuSet12_JHB$lifestyle <- factor(nuSet12_JHB$lifestyle,
                                        levels = c(1,2,3,4,5,6,7,8,9,10,11,12),
                                        labels = c("none", "cell sophisticates", "sports", "gamers", "outdoors", "good living", "avid readers", "traditionalists","cell fundamentals", "homebodies", "studious", "showgoers"))
nuSet12_JHB$attitudes <- factor(nuSet12_JHB$attitudes,
                                        levels = c(1,2,3,4,5,6,7),
                                        labels = c("none", "now generation", "nation builders", "distants survivors", "distants established", "rooted", "global citizens"))

# focussing only on the variable I intend to use in this section:
nuSet12_CT <- nuSet12_CT[,-c(1:2,9:11, 17:22)]
nuSet12_JHB <- nuSet12_JHB[,-c(1:2,9:11, 17:22)]

# saving these objects:
saveRDS(nuSet12_CT, "nuSet12_CT.rds")
saveRDS(nuSet12_JHB, "nuSet12_JHB.rds")

nuSet12_CT <- readRDS("nuSet12_CT.rds")
nuSet12_JHB <- readRDS("nuSet12_JHB.rds")
# 
# ## Determine Number of Factors to Extract
# ev_ct <- eigen(cor(nuSet12_CT[,12:ncol(nuSet12_CT)]))
# ap_ct <- parallel(subject=nrow(nuSet12_CT[,12:ncol(nuSet12_CT)]),var=ncol(nuSet12_CT[,12:ncol(nuSet12_CT)]),
#                rep=100,cent=.05)
# nS_ct <- nScree(x=ev_ct$values, aparallel=ap_ct$eigen$qevpea)
# jpeg("nScree_12_ct")
# plotnScree(nS_ct, main = "Cape Town") # optimal = 7
# dev.off()
# 
# ev_jhb <- eigen(cor(nuSet12_JHB[,12:ncol(nuSet12_JHB)]))
# ap_jhb <- parallel(subject=nrow(nuSet12_JHB[,12:ncol(nuSet12_JHB)]),var=ncol(nuSet12_JHB[,12:ncol(nuSet12_JHB)]),
#                rep=100,cent=.05)
# nS_jhb <- nScree(x=ev_jhb$values, aparallel=ap_jhb$eigen$qevpea)
# jpeg("nScree_12_jhb")
# plotnScree(nS_jhb, main = "Johannesburg") # 
# dev.off()
# 
# npc_ct <- nS_ct$Components$noc
# npc_jhb <- nS_jhb$Components$noc

# will set them at six for both Jhb and CT for now
npc_ct <- 6
npc_jhb <- 6

# creating objects with supplementary variables (qualitative and quantitative) and active one defined:
set.seed(56)
pca_12_ct <- PCA(nuSet12_CT,
                 quanti.sup = c(2,4,5,9),
                 quali.sup = c(1,3,6,7,8,10,11),
                 ncp = npc_ct,
                 graph = FALSE)
set.seed(56)
pca_12_jhb <- PCA(nuSet12_JHB,
                  quanti.sup = c(2,4,5,9),
                  quali.sup = c(1,3,6,7,8,10,11),
                  ncp = npc_jhb,
                  graph = FALSE)

# save for later use:

saveRDS(pca_12_ct, "pca_12_ct.rds")
saveRDS(pca_12_jhb, "pca_12_jhb.rds")


# # try FactoInvestigate
# library(FactoInvestigate)
# Investigate(pca_12_ct)
# Investigate(pca_12_jhb)

# cape town contributions plots
jpeg("contributions12_ct_1n2.jpeg")
fviz_pca_var(pca_12_ct,
             axes = c(1,2),
             col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             title = "Cape Town",
             repel = TRUE # Avoid text overlapping
)
dev.off()

jpeg("contributions12_ct_3n4.jpeg")
fviz_pca_var(pca_12_ct,
             axes = c(3,4),
             col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             title = "Cape Town",
             repel = TRUE # Avoid text overlapping
)
dev.off()

jpeg("contributions12_ct_5n6.jpeg")
fviz_pca_var(pca_12_ct,
             axes = c(5,6),
             col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             title = "Cape Town",
             repel = TRUE # Avoid text overlapping
)
dev.off()

# jpeg("contributions12_ct_6n7.jpeg")
# fviz_pca_var(pca_12_ct,
#              axes = c(6,7),
#              col.var="contrib",
#              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#              title = "Cape Town",
#              repel = TRUE # Avoid text overlapping
# )
# dev.off()

# for the six jhb dimensions
jpeg("contributions12_jhb_1m2.jpeg")
fviz_pca_var(pca_12_jhb,
             axes = c(1,2),
             col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             title = "Johannesburg",
             repel = TRUE # Avoid text overlapping
)
dev.off()

jpeg("contributions12_jhb_3m4.jpeg")
fviz_pca_var(pca_12_jhb,
             axes = c(3,4),
             col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             title = "Johannesburg",
             repel = TRUE # Avoid text overlapping
)
dev.off()

jpeg("contributions12_jhb_5m6.jpeg")
fviz_pca_var(pca_12_jhb,
             axes = c(5,6),
             col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             title = "Johannesburg",
             repel = TRUE # Avoid text overlapping
)
dev.off()

# can identify most important
# # high absolute values describe variables that best describe a particular dimension.
# and a negatively correlated value (eg Kickoff) means that individuals who have high coordinates on the given dimension would have a low value for engagement with kickoff... (see FactoMineR document p 8)

# create dataframe based on correlations > plus or minus 0.3
# create dataframe of values:

# for cape town
vehicle_ct <- rownames(pca_12_ct$var$cor)
corr_ct <- as.data.frame(pca_12_ct$var$cor)
contrib_ct <- as.data.frame(pca_12_ct$var$contrib)
cos2_ct <- as.data.frame(pca_12_ct$var$cos2)

dims_ct <- list()
for(i in 1:npc_ct) {
        temp <- data.frame(vehicle_ct, corr = corr_ct[,i], contrib = contrib_ct[,i], cos2 = cos2_ct[,i]) %>%
                filter(corr > 0.3 | corr < -0.3) %>%
                arrange(desc(corr))
        rownames(temp) <- temp$vehicle
        dims_ct[[i]] <- temp[,-1]
}

# for johannesburg
vehicle_jhb <- rownames(pca_12_jhb$var$cor)
corr_jhb <- as.data.frame(pca_12_jhb$var$cor, row.names = '')
contrib_jhb <- as.data.frame(pca_12_jhb$var$contrib, row.names = '')
cos2_jhb <- as.data.frame(pca_12_jhb$var$cos2, row.names = '')

dims_jhb <- list()
for(i in 1:npc_jhb) {
        temp <- data.frame(vehicle_jhb, corr = corr_jhb[,i], contrib = contrib_jhb[,i], cos2 = cos2_jhb[,i]) %>%
                filter(corr > 0.3 | corr < -0.3) %>%
                arrange(desc(corr))
        rownames(temp) <- temp$vehicle
        dims_jhb[[i]] <- temp[,-1]
}

# Dimension Tables Information Cape Town 1 & 2:
# for dimension 1
tab_ct_1 <- tableGrob(round(dims_ct[[1]], 2), theme = ttheme_minimal(base_size = 10)) # table

grid.newpage()
h_ct_1 <- grobHeight(tab_ct_1)
w_ct_1 <- grobWidth(tab_ct_1)
title_ct_1 <- textGrob("Dimension 1", y=unit(0.5,"npc") + 0.5*h_ct_1, 
                  vjust=-6, hjust = 0.2, gp=gpar(fontsize=14)) # title
gt_ct_1 <- gTree(children = gList(tab_ct_1, title_ct_1)) #,footnote
# grid.draw(gt_ct_1) # check

# for dimension 2
tab_ct_2 <- tableGrob(round(dims_ct[[2]], 2), theme = ttheme_minimal(base_size = 10)) # table

grid.newpage()
h_ct_2 <- grobHeight(tab_ct_1)
w_ct_2 <- grobWidth(tab_ct_1)
title_ct_2 <- textGrob("Dimension 2", y=unit(0.5,"npc") + 0.5*h_ct_2, 
                       vjust=-6, hjust = 0.2, gp=gpar(fontsize=14)) # title
gt_ct_2 <- gTree(children = gList(tab_ct_2, title_ct_2)) #,footnote
# grid.draw(gt_ct_2) # check

# arrange two Dimensions on one plot and print to interactive graphic with latex
ml_ct_1n2 <- marrangeGrob(list(gt_ct_1,gt_ct_2), nrow=1, ncol=2, top = '\n\n\n\nCape Town')

# print to graphic
jpeg("dims12_ct_1n2.jpeg")
ml_ct_1n2
dev.off()

# JHB 1 & 2:
# for dimension 1
# given so many, will cut it off at 0.4:

tab_jhb_1 <- tableGrob(round(dims_jhb[[1]], 2), theme = ttheme_minimal(base_size = 10)) # table

grid.newpage()
h_jhb_1 <- grobHeight(tab_jhb_1)
w_jhb_1 <- grobWidth(tab_jhb_1)
title_jhb_1 <- textGrob("Dimension 1", y=unit(0.5,"npc") + 0.5*h_jhb_1, 
                       vjust=-8, hjust = 0.3, gp=gpar(fontsize=14)) # title
gt_jhb_1 <- gTree(children = gList(tab_jhb_1, title_jhb_1))
# grid.draw(gt_jhb_1) # check

# for dimension 2
tab_jhb_2 <- tableGrob(round(dims_jhb[[2]], 2), theme = ttheme_minimal(base_size = 10)) # table

grid.newpage()
h_jhb_2 <- grobHeight(tab_jhb_2)
w_jhb_2 <- grobWidth(tab_jhb_2)
title_jhb_2 <- textGrob("Dimension 2", y=unit(0.5,"npc") + 0.5*h_jhb_2, 
                       vjust=-6, hjust = 0.2, gp=gpar(fontsize=14)) # title
gt_jhb_2 <- gTree(children = gList(tab_jhb_2, title_jhb_2))
# grid.draw(gt_jhb_2) # check

# arrange two Dimensions
ml_jhb_1n2 <- marrangeGrob(list(gt_jhb_1,gt_jhb_2), nrow=1, ncol=2, top = '\n\nJohannesburg')

# print to graphic
jpeg("dims12_jhb_1n2.jpeg")
ml_jhb_1n2
dev.off()

# CAPE TOWN 3 & 4:
# for dimension 3
tab_ct_3 <- tableGrob(round(dims_ct[[3]], 2), theme = ttheme_minimal(base_size = 10)) # table

grid.newpage()
h_ct_3 <- grobHeight(tab_ct_3)
w_ct_3 <- grobWidth(tab_ct_3)
title_ct_3 <- textGrob("Dimension 3", y=unit(0.5,"npc") + 0.5*h_ct_3, 
                       vjust=-6, hjust = 0.2, gp=gpar(fontsize=14)) # title
gt_ct_3 <- gTree(children = gList(tab_ct_3, title_ct_3)) #,footnote
# grid.draw(gt_ct_3) # check

# for dimension 4
tab_ct_4 <- tableGrob(round(dims_ct[[4]], 2), theme = ttheme_minimal(base_size = 10)) # table

grid.newpage()
h_ct_4 <- grobHeight(tab_ct_4)
w_ct_4 <- grobWidth(tab_ct_4)
title_ct_4 <- textGrob("Dimension 4", y=unit(0.5,"npc") + 0.5*h_ct_4, 
                       vjust=-5, hjust = 0.2, gp=gpar(fontsize=14)) # title
gt_ct_4 <- gTree(children = gList(tab_ct_4, title_ct_4)) #,footnote
# grid.draw(gt_ct_2) # check

# arrange two Dimensions on one plot and print to interactive graphic with latex
ml_ct_3n4 <- marrangeGrob(list(gt_ct_3,gt_ct_4), nrow=1, ncol=2, top = '\n\n\n\nCape Town')

# print to graphic
jpeg("dims12_ct_3n4.jpeg")
ml_ct_3n4
dev.off()

# JHB 3 & 4:

tab_jhb_3 <- tableGrob(round(dims_jhb[[3]], 2), theme = ttheme_minimal(base_size = 10)) # table

grid.newpage()
h_jhb_3 <- grobHeight(tab_jhb_3)
w_jhb_3 <- grobWidth(tab_jhb_3)
title_jhb_3 <- textGrob("Dimension 3", y=unit(0.5,"npc") + 0.5*h_jhb_3, 
                        vjust=-7, hjust = 0.3, gp=gpar(fontsize=14)) # title
gt_jhb_3 <- gTree(children = gList(tab_jhb_3, title_jhb_3)) #,footnote
# grid.draw(gt_jhb_3) # check

# for dimension 4
tab_jhb_4 <- tableGrob(round(dims_jhb[[4]], 2), theme = ttheme_minimal(base_size = 10)) # table

grid.newpage()
h_jhb_4 <- grobHeight(tab_jhb_4)
w_jhb_4 <- grobWidth(tab_jhb_4)
title_jhb_4 <- textGrob("Dimension 4", y=unit(0.5,"npc") + 0.5*h_jhb_4, 
                        vjust=-6.5, hjust = 0.2, gp=gpar(fontsize=14)) # title
gt_jhb_4 <- gTree(children = gList(tab_jhb_4, title_jhb_4)) #,footnote
# grid.draw(gt_jhb_4) # check

# arrange two Dimensions
ml_jhb_3n4 <- marrangeGrob(list(gt_jhb_3,gt_jhb_4), nrow=1, ncol=2, top = '\n\nJohannesburg')

# print to graphic
jpeg("dims12_jhb_3n4.jpeg")
ml_jhb_3n4
dev.off()



# CAPE TOWN 5 & 6 & 7:
# for dimension 5
tab_ct_5 <- tableGrob(round(dims_ct[[5]], 2), theme = ttheme_minimal(base_size = 10)) # table

grid.newpage()
h_ct_5 <- grobHeight(tab_ct_5)
w_ct_5 <- grobWidth(tab_ct_5)
title_ct_5 <- textGrob("Dimension 5", y=unit(0.5,"npc") + 0.5*h_ct_5, 
                       vjust=-6, hjust = 0.2, gp=gpar(fontsize=14)) # title
gt_ct_5 <- gTree(children = gList(tab_ct_5, title_ct_5)) #,footnote
# grid.draw(gt_ct_5) # check

# for dimension 6
tab_ct_6 <- tableGrob(round(dims_ct[[6]], 2), theme = ttheme_minimal(base_size = 10)) # table

grid.newpage()
h_ct_6 <- grobHeight(tab_ct_6)
w_ct_6 <- grobWidth(tab_ct_6)
title_ct_6 <- textGrob("Dimension 6", y=unit(0.5,"npc") + 0.5*h_ct_6, 
                       vjust=-5, hjust = 0.2, gp=gpar(fontsize=14)) # title
gt_ct_6 <- gTree(children = gList(tab_ct_6, title_ct_6)) #,footnote
# grid.draw(gt_ct_6) # check

# # for dimension 7
# tab_ct_7 <- tableGrob(round(dims_ct[[7]], 2), theme = ttheme_minimal(base_size = 10)) # table
# 
# grid.newpage()
# h_ct_7 <- grobHeight(tab_ct_7)
# w_ct_7 <- grobWidth(tab_ct_7)
# title_ct_7 <- textGrob("Dimension 7", y=unit(0.5,"npc") + 0.5*h_ct_7, 
#                        vjust=-5, hjust = 0.2, gp=gpar(fontsize=14)) # title
# gt_ct_7 <- gTree(children = gList(tab_ct_7, title_ct_7)) #,footnote
# # grid.draw(gt_ct_7) # check

# arrange two Dimensions on one plot and print to interactive graphic with latex
ml_ct_5n6 <- marrangeGrob(list(gt_ct_5,gt_ct_6), nrow=1, ncol=2, top = '\nCape Town')

# print to graphic
jpeg("dims12_ct_5n6.jpeg")
ml_ct_5n6
dev.off()

# JHB 5 and 6:
# for dimension 5

tab_jhb_5 <- tableGrob(round(dims_jhb[[5]], 2), theme = ttheme_minimal(base_size = 10)) # table

grid.newpage()
h_jhb_5 <- grobHeight(tab_jhb_5)
w_jhb_5 <- grobWidth(tab_jhb_5)
title_jhb_5 <- textGrob("Dimension 5", y=unit(0.5,"npc") + 0.5*h_jhb_3, 
                        vjust=-7, hjust = 0.3, gp=gpar(fontsize=14)) # title
gt_jhb_5 <- gTree(children = gList(tab_jhb_5, title_jhb_5)) #,footnote
# grid.draw(gt_jhb_5) # check

# for dimension 6

tab_jhb_6 <- tableGrob(round(dims_jhb[[6]], 2), theme = ttheme_minimal(base_size = 10)) # table

grid.newpage()
h_jhb_6 <- grobHeight(tab_jhb_6)
w_jhb_6 <- grobWidth(tab_jhb_6)
title_jhb_6 <- textGrob("Dimension 6", y=unit(0.5,"npc") + 0.5*h_jhb_3, 
                        vjust=-7, hjust = 0.3, gp=gpar(fontsize=14)) # title
gt_jhb_6 <- gTree(children = gList(tab_jhb_6, title_jhb_6)) #,footnote
# grid.draw(gt_jhb_6) # check

# arrange two Dimensions
ml_jhb_5n65 <- marrangeGrob(list(gt_jhb_5,gt_jhb_6), nrow=1, ncol=2,
                              top = '\n\n\n\nJohannesburg')

# print to graphic
jpeg("dims12_jhb_5n6.jpeg")
ml_jhb_5n65
dev.off()


## CAPE TOWN
# getting all the dimension descriptions
dimdesc_12_ct <- dimdesc(pca_12_ct, c(1:npc_ct), proba = 1)

# categorical supplementaries per dimension ... need to explain and interpret "Estimate"
cat_coord_12_ct <- list()
for(i in 1:npc_ct) {
        temp1 <- dimdesc_12_ct[[i]]$category[order(dimdesc_12_ct[[i]]$category[,1], decreasing = TRUE),]
        temp2 <- temp1[c(1:5, (nrow(temp1) - 4): nrow(temp1)),1]
        cat_coord_12_ct[[i]] <- data.frame(Est = round(temp2, 2))
        # write.table(cat_coord_12_ct[[i]], file = paste0("cat_dim", i, "_ct.csv")) # if needed
        
}

# getting tables 
# Dim 1 CT
tab_ct_cat1 <- tableGrob(cat_coord_12_ct[[1]], theme = ttheme_minimal(base_size = 12)) # table

grid.newpage()
h_cat_ct1 <- grobHeight(tab_ct_cat1)
w_cat_ct1 <- grobWidth(tab_ct_cat1)
title_tab_cat_ct1 <- textGrob('Dimension 1', y=unit(0.5,"npc") + 0.5*h_cat_ct1, 
                              vjust=-7, hjust = 0.3, gp=gpar(fontsize=14)) # title
gt_cat_ct1 <- gTree(children = gList(tab_ct_cat1, title_tab_cat_ct1))

# grid.draw(gt_cat_ct1) # check

# Dim 2 CT
tab_ct_cat2 <- tableGrob(cat_coord_12_ct[[2]], theme = ttheme_minimal(base_size = 12)) # table

grid.newpage()
h_cat_ct2 <- grobHeight(tab_ct_cat2)
w_cat_ct2 <- grobWidth(tab_ct_cat2)
title_tab_cat_ct2 <- textGrob('Dimension 2', y=unit(0.5,"npc") + 0.5*h_cat_ct2, 
                             vjust=-7, hjust = 0.3, gp=gpar(fontsize=14)) # title
gt_cat_ct2 <- gTree(children = gList(tab_ct_cat2, title_tab_cat_ct2))

# grid.draw(gt_cat_ct2) # check

# Dim 3 CT
tab_ct_cat3 <- tableGrob(cat_coord_12_ct[[3]], theme = ttheme_minimal(base_size = 12)) # table

grid.newpage()
h_cat_ct3 <- grobHeight(tab_ct_cat3)
w_cat_ct3 <- grobWidth(tab_ct_cat3)
title_tab_cat_ct3 <- textGrob('Dimension 3', y=unit(0.5,"npc") + 0.5*h_cat_ct3, 
                              vjust=-7, hjust = 0.3, gp=gpar(fontsize=14)) # title
gt_cat_ct3 <- gTree(children = gList(tab_ct_cat3, title_tab_cat_ct3))

# grid.draw(gt_cat_ct3) # check        

# Dim 4 CT
tab_ct_cat4 <- tableGrob(cat_coord_12_ct[[4]], theme = ttheme_minimal(base_size = 12)) # table

grid.newpage()
h_cat_ct4 <- grobHeight(tab_ct_cat4)
w_cat_ct4 <- grobWidth(tab_ct_cat4)
title_tab_cat_ct4 <- textGrob('Dimension 4', y=unit(0.5,"npc") + 0.5*h_cat_ct4, 
                              vjust=-7, hjust = 0.3, gp=gpar(fontsize=14)) # title
gt_cat_ct4 <- gTree(children = gList(tab_ct_cat4, title_tab_cat_ct4))

# grid.draw(gt_cat_ct4) # check

# Dim 5 CT
tab_ct_cat5 <- tableGrob(cat_coord_12_ct[[5]], theme = ttheme_minimal(base_size = 12)) # table

grid.newpage()
h_cat_ct5 <- grobHeight(tab_ct_cat5)
w_cat_ct5 <- grobWidth(tab_ct_cat5)
title_tab_cat_ct5 <- textGrob('Dimension 5', y=unit(0.5,"npc") + 0.5*h_cat_ct5, 
                              vjust=-7, hjust = 0.3, gp=gpar(fontsize=14)) # title
gt_cat_ct5 <- gTree(children = gList(tab_ct_cat5, title_tab_cat_ct5))

# grid.draw(gt_cat_ct5) # check  

# Dim 6 CT
tab_ct_cat6 <- tableGrob(cat_coord_12_ct[[6]], theme = ttheme_minimal(base_size = 12)) # table

grid.newpage()
h_cat_ct6 <- grobHeight(tab_ct_cat6)
w_cat_ct6 <- grobWidth(tab_ct_cat6)
title_tab_cat_ct6 <- textGrob('Dimension 6', y=unit(0.5,"npc") + 0.5*h_cat_ct6, 
                              vjust=-7, hjust = 0.3, gp=gpar(fontsize=14)) # title
gt_cat_ct6 <- gTree(children = gList(tab_ct_cat6, title_tab_cat_ct6))

# grid.draw(gt_cat_ct6) # check  

# # Dim 7 CT
# tab_ct_cat7 <- tableGrob(cat_coord_12_ct[[7]], theme = ttheme_minimal(base_size = 12)) # table
# 
# grid.newpage()
# h_cat_ct7 <- grobHeight(tab_ct_cat7)
# w_cat_ct7 <- grobWidth(tab_ct_cat7)
# title_tab_cat_ct7 <- textGrob('Dimension 7', y=unit(0.5,"npc") + 0.5*h_cat_ct7, 
#                               vjust=-7, hjust = 0.3, gp=gpar(fontsize=14)) # title
# gt_cat_ct7 <- gTree(children = gList(tab_ct_cat7, title_tab_cat_ct7))
# 
# # grid.draw(gt_cat_ct7) # check

# creating two files for these 1st 4 and then 3 more
# arrange first four
ml_ct_1n2 <- marrangeGrob(list(gt_cat_ct1,
                                gt_cat_ct2), nrow=1, ncol=2,
                            top = '\n\n\n\nCape Town')

# print to graphic
jpeg("cats12_ct_1n2.jpeg")
ml_ct_1n2
dev.off()

ml_ct_3n4 <- marrangeGrob(list(gt_cat_ct3,
                                gt_cat_ct4), nrow=1, ncol=2,
                           top = '\n\n\n\nCape Town')

# print to graphic
jpeg("cats12_ct_3n4.jpeg")
ml_ct_3n4
dev.off()

ml_ct_5n6 <- marrangeGrob(list(gt_cat_ct5,
                               gt_cat_ct6), nrow=1, ncol=2,
                          top = '\n\n\n\nCape Town')

# print to graphic
jpeg("cats12_ct_5n6.jpeg")
ml_ct_5n6
dev.off()

# ml_ct_7 <- marrangeGrob(list(gt_cat_ct7), nrow=1, ncol=1,
#                           top = '\n\n\n\nCape Town')
# 
# # print to graphic
# jpeg("cats12_ct_7.jpeg")
# ml_ct_7
# dev.off()

## JOHANNESBURG
# getting all the dimension descriptions
dimdesc_12_jhb <- dimdesc(pca_12_jhb, c(1:npc_jhb), proba = 1)

# categorical supplementaries per dimension ... need to explain and interpret "Estimate"
cat_coord_12_jhb <- list()
for(i in 1:npc_jhb) {
        temp1 <- dimdesc_12_jhb[[i]]$category[order(dimdesc_12_jhb[[i]]$category[,1], decreasing = TRUE),]
        temp2 <- temp1[c(1:5, (nrow(temp1) - 4): nrow(temp1)),1]
        cat_coord_12_jhb[[i]] <- data.frame(Est = round(temp2, 2))
        # write.table(cat_coord_12_ct[[i]], file = paste0("cat_dim", i, "_ct.csv")) # if needed
        
}

# getting tables 
# Dim 1 JHB
tab_jhb_cat1 <- tableGrob(cat_coord_12_jhb[[1]], theme = ttheme_minimal(base_size = 12)) # table

grid.newpage()
h_cat_jhb1 <- grobHeight(tab_jhb_cat1)
w_cat_jhb1 <- grobWidth(tab_jhb_cat1)
title_tab_cat_jhb1 <- textGrob('Dimension 1', y=unit(0.5,"npc") + 0.5*h_cat_jhb1, 
                              vjust=-7, hjust = 0.3, gp=gpar(fontsize=14)) # title
gt_cat_jhb1 <- gTree(children = gList(tab_jhb_cat1, title_tab_cat_jhb1))

grid.draw(gt_cat_jhb1) # check

# Dim 2 jhb
tab_jhb_cat2 <- tableGrob(cat_coord_12_jhb[[2]], theme = ttheme_minimal(base_size = 12)) # table

grid.newpage()
h_cat_jhb2 <- grobHeight(tab_jhb_cat2)
w_cat_jhb2 <- grobWidth(tab_jhb_cat2)
title_tab_cat_jhb2 <- textGrob('Dimension 2', y=unit(0.5,"npc") + 0.5*h_cat_jhb2, 
                              vjust=-7, hjust = 0.3, gp=gpar(fontsize=14)) # title
gt_cat_jhb2 <- gTree(children = gList(tab_jhb_cat2, title_tab_cat_jhb2))

# grid.draw(gt_cat_jhb2) # check

# Dim 3 jhb
tab_jhb_cat3 <- tableGrob(cat_coord_12_jhb[[3]], theme = ttheme_minimal(base_size = 12)) # table

grid.newpage()
h_cat_jhb3 <- grobHeight(tab_jhb_cat3)
w_cat_jhb3 <- grobWidth(tab_jhb_cat3)
title_tab_cat_jhb3 <- textGrob('Dimension 3', y=unit(0.5,"npc") + 0.5*h_cat_jhb3, 
                              vjust=-7, hjust = 0.3, gp=gpar(fontsize=14)) # title
gt_cat_jhb3 <- gTree(children = gList(tab_jhb_cat3, title_tab_cat_jhb3))

# grid.draw(gt_cat_jhb3) # check        

# Dim 4 jhb
tab_jhb_cat4 <- tableGrob(cat_coord_12_jhb[[4]], theme = ttheme_minimal(base_size = 12)) # table

grid.newpage()
h_cat_jhb4 <- grobHeight(tab_jhb_cat4)
w_cat_jhb4 <- grobWidth(tab_jhb_cat4)
title_tab_cat_jhb4 <- textGrob('Dimension 4', y=unit(0.5,"npc") + 0.5*h_cat_jhb4, 
                              vjust=-7, hjust = 0.3, gp=gpar(fontsize=14)) # title
gt_cat_jhb4 <- gTree(children = gList(tab_jhb_cat4, title_tab_cat_jhb4))

# grid.draw(gt_cat_jhb4) # check

# Dim 5 jhb
tab_jhb_cat5 <- tableGrob(cat_coord_12_jhb[[5]], theme = ttheme_minimal(base_size = 12)) # table

grid.newpage()
h_cat_jhb5 <- grobHeight(tab_jhb_cat5)
w_cat_jhb5 <- grobWidth(tab_jhb_cat5)
title_tab_cat_jhb5 <- textGrob('Dimension 5', y=unit(0.5,"npc") + 0.5*h_cat_jhb5, 
                              vjust=-7, hjust = 0.3, gp=gpar(fontsize=14)) # title
gt_cat_jhb5 <- gTree(children = gList(tab_jhb_cat5, title_tab_cat_jhb5))

# grid.draw(gt_cat_jhb5) # check  

# Dim 6 jhb
tab_jhb_cat6 <- tableGrob(cat_coord_12_jhb[[6]], theme = ttheme_minimal(base_size = 12)) # table

grid.newpage()
h_cat_jhb6 <- grobHeight(tab_jhb_cat6)
w_cat_jhb6 <- grobWidth(tab_jhb_cat6)
title_tab_cat_jhb6 <- textGrob('Dimension 6', y=unit(0.5,"npc") + 0.5*h_cat_jhb6, 
                              vjust=-7, hjust = 0.3, gp=gpar(fontsize=14)) # title
gt_cat_jhb6 <- gTree(children = gList(tab_jhb_cat6, title_tab_cat_jhb6))

# grid.draw(gt_cat_jhb6) # check  


# creating two files for these 1st 4 and then 3 more
# arrange first four
ml_jhb_1n2 <- marrangeGrob(list(gt_cat_jhb1,
                               gt_cat_jhb2), nrow=1, ncol=2,
                          top = '\n\n\n\nJohannesburg')

# print to graphic
jpeg("cats12_jhb_1n2.jpeg")
ml_jhb_1n2
dev.off()

ml_jhb_3n4 <- marrangeGrob(list(gt_cat_jhb3,
                               gt_cat_jhb4), nrow=1, ncol=2,
                          top = '\n\n\n\nJohannesburg')

# print to graphic
jpeg("cats12_jhb_3n4.jpeg")
ml_jhb_3n4
dev.off()

ml_jhb_5n6 <- marrangeGrob(list(gt_cat_jhb5,
                               gt_cat_jhb6), nrow=1, ncol=2,
                          top = '\n\n\n\nJohannesburg')

# print to graphic
jpeg("cats12_jhb_5n6.jpeg")
ml_jhb_5n6
dev.off()

# continuous supplementaries per dimension...explain difference... blah blah
# cape town:

cont_corrs_12_ct <- matrix(0, nrow = 4, ncol = 0)
for(i in 1:npc_ct) {
        temp1 <- as.matrix(dimdesc_12_ct[[i]]$quanti)
        temp2 <- temp1[which(rownames(temp1) %in% c("age", "edu", "hh_inc", "lsm")),]
        cont_corrs_12_ct <- as.data.frame(round(cbind(cont_corrs_12_ct, temp2), 2))
}
names(cont_corrs_12_ct) <- c("Dim1", "pVal","Dim2", "pVal", "Dim3", "pVal", "Dim4", "pVal", "Dim5", "pVal", "Dim6", "pVal" )

# print to file for graphic:

tab_ct_cont <- tableGrob(cont_corrs_12_ct, theme = ttheme_minimal(base_size = 8)) # table

grid.newpage()
h_cont_ct <- grobHeight(tab_ct_cont)
w_cont_ct <- grobWidth(tab_ct_cont)
title_tab_cont_ct <- textGrob('Cape Town', y=unit(0.5,"npc") + 0.3*h_cont_ct, 
                        vjust=-4, hjust = 0.5, gp=gpar(fontsize=14)) # title
gt_cont_ct<- gTree(children = gList(tab_ct_cont, title_tab_cont_ct)) #
# grid.draw(gt_cont_ct) # check

# Johannesburg:
dimdesc_12_jhb <- dimdesc(pca_12_jhb, c(1:npc_jhb), proba = 1)
cont_corrs_12_jhb <- matrix(0, nrow = 4, ncol = 0)
for(i in 1:npc_jhb) {
        temp1 <- as.matrix(dimdesc_12_jhb[[i]]$quanti)
        temp2 <- temp1[which(rownames(temp1) %in% c("age", "edu", "hh_inc", "lsm")),]
        cont_corrs_12_jhb <- as.data.frame(round(cbind(cont_corrs_12_jhb, temp2), 2))
}
names(cont_corrs_12_jhb) <- c("Dim1", "pVal","Dim2", "pVal", "Dim3", "pVal", "Dim4", "pVal", "Dim5", "pVal", "Dim6", "pVal" )

# print to file for graphic:
tab_jhb_cont <- tableGrob(cont_corrs_12_jhb, theme = ttheme_minimal(base_size = 8)) # table

grid.newpage()
h_cont_jhb <- grobHeight(tab_jhb_cont)
w_cont_jhb <- grobWidth(tab_jhb_cont)
title_tab_cont_jhb <- textGrob('Johannesburg', y=unit(0.5,"npc") + 0.5*h_cont_jhb, 
                              vjust=-4, hjust = 0.5, gp=gpar(fontsize=14)) # title
gt_cont_jhb  <- gTree(children = gList(tab_jhb_cont, title_tab_cont_jhb))
# grid.draw(gt_cont_jhb) # check

# arrange single output
ml_cont_ct_jhb <- marrangeGrob(list(gt_cont_ct,gt_cont_jhb), nrow=2, ncol=1,
                              top = '')

# print to graphic
jpeg("cont_12_ct_jhb.jpeg")
ml_cont_ct_jhb
dev.off()


# cant figure out why the image looks so very different to the simple PCA version above...??? expected the same..
# visualising all the quantitative (including supplementary vars)
fviz_pca_var(pca_12_ct_supp,
             axes = c(1,2),
             col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             title = "Cape Town",
             repel = TRUE # Avoid text overlapping
             )


# R2 = square correlation ratio between the coordinates of individuals (scores) on the dimension and the categorical variables. Used in one-way anovas to see
# if there is a link btw continuous( dim) and a categorical.
# Estimate: eg...0.45: a coefficient or coordinate value. Sum of all estimates for a dimension = 0.
#         : 

# checking out this function... helpful (for some reason problems in cape town set with vars 47:49 in ct set). Running it on jhb set to help me with interpretation
library(FactoInvestigate)
Investigate(pca_12_ct_supp)
Investigate(pca_12_jhb_supp)


# Contributions of variables to PC1
fviz_contrib(pca_12_ct_supp, choice = "var", axes = 1, top = 15)
# This reference line corresponds to the expected value if the contribution where uniform
#Contributions of variables to PC2
fviz_contrib(pca_12_ct, choice = "var", axes = 2, top = 10)

# try ellipses again:
plotellipses(pca_12_ct_supp) # pretty but messy

# Onto SCORES!!! kmeans.... media groups...

# Extract the scores for individuals
scores_ct <- get_pca_ind(pca_12_ct)$coord
scores_jhb <- get_pca_ind(pca_12_jhb)$coord



# consider kmeans for clustering:

# first consider number of clusters:
# for cape town
wss_12_ct <- vector()
for(k in 3:20) {
        temp <- kmeans(scores_ct,
                       centers = k,
                       nstart = 20,
                       iter.max = 20)
        wss_12_ct <- append(wss_12_ct,temp$tot.withinss)
}

plot(3:20, wss_12_ct, type = "b")

# for johannesburg
wss_12_jhb <- vector()
for(k in 3:20) {
        temp <- kmeans(scores_jhb,
                       centers = k,
                       nstart = 20,
                       iter.max = 20)
        wss_12_jhb <- append(wss_12_jhb,temp$tot.withinss)
}

plot(3:20, wss_12_jhb, type = "b")

# will go with seven for cape town and 5 for johannesburg (pretty arb, but based on optimal factors...)
kmeans_12_ct <- kmeans(scores_ct, centers = 7, nstart = 20)
kmeans_12_jhb <- kmeans(scores_jhb, centers = 5, nstart = 20)

# get cluster centers
centers_12_ct <- kmeans_12_ct$centers
centers_12_jhb <- kmeans_12_jhb$centers

# append cluster centers to dataframe:
categoricals_12_CT_mg <- data.frame(categoricals_12_CT, mediaGroup = as.factor(kmeans_12_ct$cluster))
categoricals_12_JHB_mg <- data.frame(categoricals_12_JHB, mediaGroup = as.factor(kmeans_12_jhb$cluster))

# consider relationship between clusters and dimensions (Q: is it worth doing this?? or should I stick with pca only)
# use adjusted...for media groups
pca_12_ct_supp_mg <- PCA(categoricals_12_CT_mg, quanti.sup = c(1,3,4,9), quali.sup = c(2,5:8,10:12,51), ncp = 7, graph = FALSE)
pca_12_jhb_supp_mg <- PCA(categoricals_12_JHB_mg, quanti.sup = c(1,3,4,9), quali.sup = c(2,5:8,10:12,56 ), ncp = 5, graph = FALSE)

# consider the relationship between dimension and media groups:

test <- dimdesc(pca_12_ct_supp_mg, axes = c(1,2))
test$Dim.2$category

test_ct$Dim.3

# consider some pictures as in previous...

heatmap(centers_12_ct)
heatmap(centers_12_jhb) # seems media groups 4 & 5 are both essentially Dim1..


#cape town
par(mfrow = c(2,3), oma = c(0,0,2,0))
plot(categoricals_12_CT_mg$age ~ categoricals_12_CT_mg$mediaGroup, col = c(1,2,3,4,5,6,7))
plot(categoricals_12_CT_mg$edu ~ categoricals_12_CT_mg$mediaGroup, col = c(1,2,3,4,5,6,7))
plot(categoricals_12_CT_mg$hh_inc ~ categoricals_12_CT_mg$mediaGroup, col = c(1,2,3,4,5,6,7))
plot(categoricals_12_CT_mg$lsm ~ categoricals_12_CT_mg$mediaGroup, col = c(1,2,3,4,5,6,7))
title("Cape Town", outer = TRUE, cex.main = 2.5)
dev.off()


# johannesburg
par(mfrow = c(2,3), oma = c(0,0,2,0))
plot(categoricals_12_JHB_mg$age ~ categoricals_12_JHB_mg$mediaGroup, col = c(1,2,3,4,5,6,7))
plot(categoricals_12_JHB_mg$edu ~ categoricals_12_JHB_mg$mediaGroup, col = c(1,2,3,4,5,6,7))
plot(categoricals_12_JHB_mg$hh_inc ~ categoricals_12_JHB_mg$mediaGroup, col = c(1,2,3,4,5,6,7))
plot(categoricals_12_JHB_mg$lsm ~ categoricals_12_JHB_mg$mediaGroup, col = c(1,2,3,4,5,6,7))
title("JHB", outer = TRUE, cex.main = 2.5)
dev.off()
# 
# 
# 
# 
# 
# 
# 
# 
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




# 3. Visualize
library("factoextra")
fviz_cluster(kmeans_ct, data = scale(scores_ct),
             axes = c(1,2),
             labelsize = 0,
             palette = c("#00AFBB","#2E9FDF", "#E7B800", "#FC4E07", "#2E9FDF"),
             ggtheme = ttheme_minimal(),
             main = "Partitioning Clustering Plot",
             repel = FALSE # TRUE means you have to wait long time....
)











# try factominer considering mixed data
# trying out FAMD:
pca_12_ct_famd <- FAMD(categoricals_12_CT, ncp = 7, graph = FALSE)
summary(pca_12_ct_famd)
plot(pca_12_ct_famd, habillage = 5 , axes = 3:4)
test_famd <- pca_12_ct_famd$quali.var$contrib
rownames(test_famd) <- c("age1", "age2", "age3", "age4",
                         "sex1", "sex2",
                         "edu1", "edu2", "edu3",
                         "race1", "race2", "race3", "race4", rownames(test_famd)[14:47])
test_famd

# confidence intervals around categories
plotellipses(pca_12_ct_famd, keepvar = c("cluster") , axes = 3:4, pch = 3, keepnames = FALSE, label = "none", level = 0.99)


# OK... so I can go some way toward describing main media vehicles and also main demographics per dimension.

# next want to work with the scores:
# recall, getting scores from factominer:

# these are the scores: 
scores_12_ct <- pca_12_ct$ind$coord
scores_12_jhb <- pca_12_jhb$ind$coord

# doing kmeans on the scores:

# try using pamk()
library(fpc)
kmean_ct <- pamk(pca_12_ct$ind$coord, krange = 4:12, usepam = FALSE, criterion = "multiasw") # gives 5
kmean_jhb <- pamk(pca_12_jhb$ind$coord, krange = 4:12, usepam = FALSE, criterion = "multiasw") # gives 4
table(kmean_ct$pamobject$clustering)
table(kmean_jhb$pamobject$clustering)
kmean_ct$pamobject$medoids
kmean_jhb$pamobject$medoids


# or

test <- kmeans(pca_12_ct$ind$coord, centers = 5)
sum(test$withinss)
test$betweenss

# for cape town
wss_12_ct <- vector()
for(k in 3:20) {
        temp <- kmeans(score,
                       centers = k,
                       nstart = 3,
                       iter.max = 20)
        wss_12_ct <- append(wss_12_ct,temp$tot.withinss)
}

plot(3:20, wss_12_ct)

aggregate(kmean_ct$pamobject, by=list(fit$cluster),FUN=mean) ## to get mean... want to check how this compares with medoids...




jpeg('kmeansPlot_12_CT.jpeg')
plot(3:20, wss_12_ct, type = "b", xlab = "k-values", ylab = "total within sum of squares", main = "Cape Town" )
abline(v = 8, lty = 2)
dev.off()

# for johannesburg
wss_12_jhb <- vector()
for(k in 3:20) {
        temp <- kmeans(pca_12_jhb$ind$coord,
                       centers = k,
                       nstart = 3,
                       iter.max = 20)
        wss_12_jhb <- append(wss_12_jhb,temp$tot.withinss)
}

jpeg('kmeansPlot_12_JHB.jpeg')
plot(3:20, wss_12_jhb, type = "b", xlab = "k-values", ylab = "total within sum of squares", main = "Johannesburg" )
abline(v = 7, lty = 2)
dev.off()

# eight for Cape Town and seven for Johannesburg
# cape town
set.seed(56)
kmeans_12_CT <- kmeans(fa12_CT$scores, centers = 8, nstart = 5, iter.max = 30)
table(kmeans12_CT$cluster) # reasonable distribution

# johannesburg
set.seed(56)
kmeans_12_JHB <- kmeans(fa12_JHB$scores, centers = 7, nstart = 5, iter.max = 30)
table(kmeans12_JHB$cluster) # reasonable distribution

# checking centers 
kmeans12_CT$centers
kmeans12_JHB$centers

kmeans_12_ct <- kmeans(pca_12_ct$ind$coord, 5)
                             

#### PREVIOUS APPROACH FROM HERE:
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

# save for later use:
saveRDS(fa12_CT, "fa12_CT.rds")
saveRDS(fa12_JHB, "fa12_JHB.rds")

fa12_CT <- readRDS("fa12_CT.rds")
fa12_JHB <- readRDS("fa12_JHB.rds")



#### SOME EXPERIMENTATION WITH FACTOR ANALYSES SINCE THIS STEP VERY IMPORTANT!!!

# consider also the number of eigenvalues:
eigen_ct <- eigen(cor(set12_CT[,22:59]))$values # 11
eigen_jhb <- eigen(cor(set12_JHB[,22:64]))$values # 12

# trying factanal on these numbers:
# for cape town
fa12_CT_eigen <- factanal(set12_CT[,22:59], factors = 11, scores = "regression")
fa12_CT_eigen # 42% variance explained
# for johannesburg
fa12_JHB_eigen <- factanal(set12_JHB[,22:64], factors = 12, scores = "regression")
fa12_JHB_eigen # 40% variance explained

# then for the sake of simplicity also consider reducing factors for both:
# for cape town
fa12_CT_ten <- factanal(set12_CT[,22:59], factors = 10, scores = "regression")
fa12_CT_ten # 41% variance explained _ acceptable drop
# for johannesburg
fa12_JHB_twelve <- factanal(set12_JHB[,22:64], factors = 12, scores = "regression")
fa12_JHB_twelve # 40% explained

# adding promax...
fa12_CT_F10_promax <- factanal(set12_CT[,22:59], factors = 10, scores = "regression", rotation = "promax")
fa12_JHB_F12_promax <- factanal(set12_JHB[,22:64], factors = 12, scores = "regression", rotation = "promax")

# create loadings dataframe
loadings_CT_F10_promax <- data.frame(vehicles_CT = rownames(fa12_CT_F10_promax$loadings), fa12_CT_F10_promax$loadings[,1:10])
loadings_JHB_F12_promax <- data.frame(vehicles_JHB = rownames(fa12_JHB_F12_promax$loadings), fa12_JHB_F12_promax$loadings[,1:12])

# write loadings > 0.2 to table file for use in Lyx:
# cape town
for(i in 1:10) {
        temp <- loadings_CT_F10_promax %>%
                select_("vehicles_CT", paste("Factor", i, sep = '')) %>%
                filter_(paste("Factor", i, " > ", "0.2", sep = '')) %>%
                arrange_(paste("desc(Factor", i, ")", sep = ''))
        
        write.table(data.frame(vehicles = temp[,1], loading = round(temp[,2],2)), file = "loadings_ct.csv", row.names = FALSE, append = TRUE)
}
# johannesburg
for(i in 1:12) {
        temp <- loadings_JHB_F12_promax %>%
                select_("vehicles_JHB", paste("Factor", i, sep = '')) %>%
                filter_(paste("Factor", i, " > ", "0.2", sep = '')) %>%
                arrange_(paste("desc(Factor", i, ")", sep = ''))
        
        write.table(data.frame(vehicles = temp[,1], loading = round(temp[,2],2)), file = "loadings_jhb.csv", row.names = FALSE, append = TRUE)
}


# getting loadings for the larger factors and for the oblique rotation:
# first create loadings dataframe
loadings_obl_CT <- obl_ct$loadings
vehicles_obl_CT = rownames(loadings_obl_CT)
loadings_obl_CT <- data.frame(vehicles_obl_CT, loadings_obl_CT[,1:10])

# look at each factor
loadings_obl_CT %>% arrange(desc(Factor1)) %>% head(10) #
loadings_obl_CT %>% arrange(desc(Factor2)) %>% head(10) 
loadings_obl_CT %>% arrange(desc(Factor3)) %>% head(10) 
loadings_obl_CT %>% arrange(desc(Factor4)) %>% head(10) 
loadings_obl_CT %>% arrange(desc(Factor5)) %>% head(10) 
loadings_obl_CT %>% arrange(desc(Factor6)) %>% head(10) 
loadings_obl_CT %>% arrange(desc(Factor7)) %>% head(10) 
loadings_obl_CT %>% arrange(desc(Factor8)) %>% head(10) 
loadings_obl_CT %>% arrange(desc(Factor9)) %>% head(10)
loadings_obl_CT %>% arrange(desc(Factor10)) %>% head(10) 

# want to consider scores and kmeans stuff on this basis:

# try principal components fact analysis
# Principal Axis Factor Analysis
library(psych)
fit <- fa(set12_CT[,22:59], nfactors = 7, fm = "pa", rotate = "varimax")
fit # print results

fit$r.scores
fit$e.values
fit$fit
fit$objective
fit$PVAL
fit$scores

# Determine Number of Factors to Extract
library(nFactors)
ev <- eigen(cor(set12_CT[,22:59])) # get eigenvalues
ap <- parallel(subject=nrow(set12_CT[,22:59]),var=ncol(set12_CT[,22:59]),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

# PCA Variable Factor Map 
library(FactoMineR)
fit_PCA <- PCA(set12_CT[,22:59], ncp = 7) # graphs generated automatically
fit_princomp <- princomp(set12_CT[,22:59])
fit_prcomp <- prcomp(set12_CT[,22:59])
fit_principal <- principal(set12_CT[,22:59], nfactors = 7)
fit_fa_pa <- fa(set12_CT[,22:59], nfactors = 7, fm = "pa", rotate = "none")


fit_fa_pa$loadings[1,]
fit_fa_pa$weights[1,]
fit_fa_pa$Structure[1,]
fit_fa_pa$TLI


fit2$var$coord[1,1:7]/sqrt(fit2$eig[1:7,1]) # same as loadings....?? what's going on here..
fit2$var$contrib

print.psych(fit_fa_pa, sort = TRUE)
fit_principal$loadings[1,]


fit_prcomp$rotation[1,1:7]
loadings(fit_princomp)[1,1:7]

fit_prcomp$x[1,1:7]
fit_princomp$scores[1,1:7]





# some experimentation to make sure I fully grasp PCA first:
trial_pca <- princomp(set12_CT[,22:59])
trial_pca_cormat <- princomp(set12_CT[,22:59], cor = TRUE)

biplot(trial_pca)

# compare the two loadings and std dev to show its the same... so standardised input or cormatrix results in corrleations.
loadings(trial_pca)[1:5,1:5]
loadings(trial_pca_cormat)[1:5,1:5]
trial_pca$sdev[1:5]
trial_pca_cormat$sdev[1:5]
var(trial_pca$scores[,1])

trial_pca$scores

var(trial_pca$scores[,1]) # equal to the first eigen value of the covariance matrix
eigen(cov(set12_CT[,22:59]))$values[1]

sqrt(eigen(cor(set12_CT[,22:59]))$values)[1] # this is equal to the std dev of the components (length of the spread along that axis)
trial_pca$sdev[1]

trial_pca$loadings[1:5,1:5]
eigen(cor(set12_CT[,22:59]))$vectors[1:5,1:5] # loadings are coefficients of components = eigen vectors

# try a cor between first component and first variable (NB since already standarised):
(trial_pca$loadings[1,1] * trial_pca$sdev[1]) # not sure how the sign should be treated.

# OK.. now for Factor Analysis
?fa # note... variables should be standardised.

#check that they are standardised:
apply(set12_CT[,22:59], 2, mean)
apply(set12_CT[,22:59], 2, sd)

trial_fa <- fa(set12_CT[,22:59], nfactors = 7, fm = "pa")
trial_fa$e.values
trial_fa$values
trial_fa$communality
trial_fa$loadings[1:5,1:5]
trial_fa$Structure[1:5,1:5]
trial_fa$fit

# will try to work from principal components and compare values...
factors <- scale(trial_pca$scores[,1:7], center = FALSE, scale = TRUE)

trial_fa$loadings[1:7,1:7]
trial_fa$uniquenesses[1:7]

#try replicate Die Burger values:
dieBurger2 <- trial_fa$loadings[1,1]*factors[,1] +
        trial_fa$loadings[1,2]*factors[,2] +
        trial_fa$loadings[1,3]*factors[,3] +
        trial_fa$loadings[1,4]*factors[,4] +
        trial_fa$loadings[1,5]*factors[,5] +
        trial_fa$loadings[1,6]*factors[,6] +
        trial_fa$loadings[1,7]*factors[,7] +
        trial_fa$uniquenesses[1]

dieBurger2[1:5]
set12_CT$Die.Burger[1:5]  #doesnt work... not sure why??

# try again and understand FactoMineR's PCA:
library(FactoMineR)
trial_PCA <- PCA(set12_CT[,22:59], ncp = 7, scale = FALSE)
trial_PCA$var$cor[1:5, 1:5]
trial_PCA$var$coord[1:5,1:5]
trial_PCA$var$contrib[1:5, 1:5]
sd(trial_PCA$var$cor[,1])

trial_PCA$var$cor[1:5,1]/trial_pca$sdev[1]
trial_pca$loadings[1:5,1:5]

#compare factominer with princomp:
loadings_PCA <- sweep(trial_PCA$var$coord,2,sqrt(trial_PCA$eig[1:ncol(trial_PCA$var$coord),1]),FUN="/")
scores_PCA <- trial_PCA$ind$coord

# check loadings and scores...yay..it works!!
loadings_PCA[1:5,1:5]
trial_pca$loadings[1:5,1:5]

scores_PCA[1:5,1:5]
trial_pca$scores[1:5,1:5]

# tried to compare factominer with fa's principal component factor analysis (not managed):
loadings_PCA[1:5,1:5]
trial_fa$loadings[1:5,1:5]





## want to consider media vehicle loadings of the seven factors:
## cape town (note: want to try to ensure factor 1 for cape town is similar to factor 1 jhb, see below (will use Cape Town as basis))

# first create loadings dataframe
loadings_CT <- data.frame(vehicles_CT = rownames(fa12_CT$loadings), fa12_CT$loadings[,1:7])

# write loadings > 0.2 to table file for use in Lyx:
for(i in 1:7) {
        temp <- loadings_CT %>%
                select_("vehicles_CT", paste("Factor", i, sep = '')) %>%
                filter_(paste("Factor", i, " > ", "0.2", sep = '')) %>%
                arrange_(paste("desc(Factor", i, ")", sep = ''))
        
        write.table(data.frame(vehicles = temp[,1], loading = round(temp[,2],2)), file = "test2_ct.csv", row.names = FALSE, append = TRUE)
}

# factor 1
one_ct <- loadings_CT %>% arrange(desc(Factor1)) %>% filter(Factor1 > 0.2) # SABC3, etv, SABC2, SABC1, DailyVoice, Son: (Afrikaans, Coloured/Indian, mature singles/mature couples, midlevelLSM, lower education)
write.table(data.frame(Factor1 = one_ct[,1], loading = round(one_ct[,2],2)), file = "one_ct.csv", row.names = FALSE)

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

heatmap(kmeans12_CT$centers)





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
library(RColorBrewer)
# coul = colorRampPalette(brewer.pal(8, "PiYG"))(25)
heatmap(kmeans12_CT$centers, Colv = NA, revC = TRUE, Rowv = NA, cexRow=1.5, col= colorRampPalette(brewer.pal(8, "Blues"))(25), legend=c("none", "col"))

kmeans12_JHB$centers
write.table(data.frame(round(kmeans12_JHB$centers, 4)), file = "centroids_12_JHB.csv")
heatmap(kmeans12_JHB$centers, Colv = NA, revC = TRUE, Rowv = NA, cexRow=1.5, col= colorRampPalette(brewer.pal(8, "Blues"))(25), legend=c("none", "col"))

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


## Some canonical trials on using selected predictor (demographic) variables
# 
# creating model matrix for dummy variables of selected unordered factors:
set12_CT_factors_dummies <- as.data.frame(model.matrix(~ age + sex + edu + hh_inc + race + lsm + cluster + Factor1 + Factor2 + Factor3 + Factor4 + Factor5 + Factor6 + Factor7, data = set12_CT_factors))

# consider means and sd of the set

means_sd12_ct <-  round(data.frame(cbind(mean = apply(set12_CT_factors_dummies, 2, mean),
      stdDev = apply(set12_CT_factors_dummies, 2, sd))), 2)

# get sense of correlations of the factors and the demographic variables
corrs12_ct <- round(cor(set12_CT_factors_dummies[,-1]),2)
corrplot(cor(set12_CT_factors_dummies[,-1]),
         method = "pie",
         order = "original",
         hclust.method = "complete",
         type = "lower",
         tl.col = 'black',
         mar = c(1,1,1,1),
         addCoefasPercent = TRUE,
         tl.pos = TRUE,
         las = 0)


library("CCA")
X = set12_CT_factors_dummies[,2:21]
Y = set12_CT_factors_dummies[,22:28]
cannonical_cor <- cc(X,Y)
plot(cannonical_cor$cor,type="b")
plt.cc(cannonical_cor)
res.regul = estim.regul(X,Y) # leave one out criterion 2D grid to determine optimum values for regularisation
img.estim.regul(res.regul)



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

