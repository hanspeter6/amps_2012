# libraries
library(nFactors)
library(psych)
library(FactoMineR)

# load datafiles 
set12_min_simple <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_nationals/set12_min_simple.rds")

# LEVEL 2

# Subsetting only on the variable I intend to use in this section:
set12_min_simple <- set12_min_simple[,-c(1:2,8:12,14:21)]

# ## Determine Number of Factors to Extract
# ev <- eigen(cor(set12_min[,7:ncol(set12_min)]))
# ap <- parallel(subject=nrow(set12_min[,7:ncol(set12_min)]),var=ncol(set12_min[,7:ncol(set12_min)]),
#                rep=100,cent=.02)
# nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
# jpeg("nScree_12_min")
# plotnScree(nS, main = "National") # optimal = 6
# dev.off()
# 
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
fact_12_simple <- fa(set12_min_simple[7:ncol(set12_min_simple)], nfactors = 6, fm = "pa") # default rotation oblimin, so does allow correlation between factors
fact_12_loadings_simple <- fact_12_simple$loadings
fact_12_scores_simple <- fact_12_simple$scores

# save model
saveRDS(fact_12_simple, "fact_12_simple.rds")

# save loadings:
saveRDS(fact_12_loadings_simple, "fact_12_loadings_simple.rds")

# save scores:
saveRDS(fact_12_scores_simple, "fact_12_scores_simple.rds")

