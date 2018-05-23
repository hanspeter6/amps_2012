# libraries
library(nFactors)
library(psych)
library(FactoMineR)

# load datafiles 
set12_min <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_nationals/set12_min.rds")

# LEVEL 2

# Subsetting only on the variable I intend to use in this section:
set12_min <- set12_min[,-c(1:2,8:12,14:21)]

## Determine Number of Factors to Extract
ev <- eigen(cor(set12_min[,7:ncol(set12_min)]))
ap <- parallel(subject=nrow(set12_min[,7:ncol(set12_min)]),var=ncol(set12_min[,7:ncol(set12_min)]),
               rep=100,cent=.02)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
jpeg("nScree_12_min")
plotnScree(nS, main = "National") # optimal = 6
dev.off()

# # will set them at six for both Jhb and CT for now
# npc <- 6
# 
# # creating objects with supplementary variables (qualitative and quantitative) and active one defined:
# set.seed(56)
# pca_12_min <- PCA(set12_min,
#                   quanti.sup = c(1,3,4,6),
#                   quali.sup = c(2,5),
#                   ncp = npc,
#                   graph = FALSE)
# saveRDS(pca_12_min, "pca_12_min.rds")

# pa method of factor analysis with oblimin rotation allowed....to try and get better estimation
set.seed(123)
fact_12 <- fa(set12_min[7:ncol(set12_min)], nfactors = 7, fm = "ml") # default rotation oblimin, so does allow correlation between factors
fact_12_loadings <- fact_12$loadings
fact_12_scores <- fact_12$scores

# save model
saveRDS(fact_12, "fact_12.rds")

# save loadings:
saveRDS(fact_12_loadings, "fact_12_loadings.rds")

# save scores:
saveRDS(fact_12_scores, "fact_12_scores.rds")

write.csv(round(loadings(fact_12, sort = TRUE), 2), file = "loadings_min_12.csv")
