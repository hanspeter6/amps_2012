# libraries
library(nFactors)
library(psych)
library(FactoMineR)

# load datafiles 
set12_min <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_nationals/set12_min.rds")
set12_min2 <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_nationals/set12_min2.rds") # Daily Sun also

# LEVEL 2

# focussing only on the variable I intend to use in this section:
set12_min <- set12_min[,-c(1:2,8:12,14:21)]
set12_min2 <- set12_min2[,-c(1:2,8:12,14:21)]

# ## Determine Number of Factors to Extract
# ev <- eigen(cor(set12_min[,7:ncol(set12_min)]))
# ap <- parallel(subject=nrow(set12_min[,7:ncol(set12_min)]),var=ncol(set12_min[,7:ncol(set12_min)]),
#                rep=100,cent=.10)
# nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
# jpeg("nScree_12_min")
# plotnScree(nS, main = "National") # optimal = 7
# dev.off()
# 
# # will set them at six for now
# npc <- 6
# 
# # creating objects with supplementary variables (qualitative and quantitative) and active one defined:
# set.seed(56)
# pca_12_min <- PCA(set12_min,
#                   quanti.sup = c(1,3,4,6),
#                   quali.sup = c(2,5),
#                   ncp = npc,
#                   graph = FALSE)
# 
# saveRDS(pca_12_min, "pca_12_min.rds")

# try pa method of factor analysis with oblimin rotation allowed....to try and get better estimation
set.seed(123)
fact_12 <- fa(set12_min[7:ncol(set12_min)], nfactors = 6, fm = "pa") # default rotation oblimin, so does allow correlation between factors
fact_12_loadings <- fact_12$loadings
fact_12_scores <- fact_12$scores

# save model
saveRDS(fact_12, "fact_12.rds")

# save loadings:
saveRDS(fact_12_loadings, "fact_12_loadings.rds")

# save scores:
saveRDS(fact_12_scores, "fact_12_scores.rds")













# # try FactoInvestigate
# library(FactoInvestigate)
# Investigate(pca_12_nat, "factoInvestigate_12_nat.Rmd", c("pdf_document"))

# contributions plots
jpeg("contributions12_nat_1n2.jpeg")
fviz_pca_var(pca_12_nat,
             axes = c(1,2),
             col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             title = "National",
             repel = TRUE # Avoid text overlapping
)
dev.off()

jpeg("contributions12_nat_3n4.jpeg")
fviz_pca_var(pca_12_nat,
             axes = c(3,4),
             col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             title = "National",
             repel = TRUE # Avoid text overlapping
)
dev.off()

jpeg("contributions12_nat_5n6.jpeg")
fviz_pca_var(pca_12_nat,
             axes = c(5,6),
             col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             title = "National",
             repel = TRUE # Avoid text overlapping
)
dev.off()

# can identify most important
# # high absolute values describe variables that best describe a particular dimension.
# and a negatively correlated value (eg Kickoff) means that individuals who have high coordinates on the given dimension would have a low value for engagement with kickoff... (see FactoMineR document p 8)

# create dataframe based on correlations > plus or minus 0.3
# create dataframe of values:

# for National
vehicle_nat <- rownames(pca_12_nat$var$cor)
corr_nat <- as.data.frame(pca_12_nat$var$cor)
contrib_nat <- as.data.frame(pca_12_nat$var$contrib)
cos2_nat <- as.data.frame(pca_12_nat$var$cos2)

dims_nat <- list()
for(i in 1:npc) {
        temp <- data.frame(vehicle_nat, corr = corr_nat[,i], contrib = contrib_nat[,i], cos2 = cos2_nat[,i]) %>%
                filter(corr > 0.3 | corr < -0.3) %>%
                arrange(desc(corr))
        rownames(temp) <- temp$vehicle
        dims_nat[[i]] <- temp[,-1]
}

# Dimension Tables Information National 1 & 2:
# for dimension 1
tab_nat_1 <- tableGrob(round(dims_nat[[1]], 2), theme = ttheme_minimal(base_size = 10)) # table

grid.newpage()
h_nat_1 <- grobHeight(tab_nat_1)
w_nat_1 <- grobWidth(tab_nat_1)
title_nat_1 <- textGrob("Dimension 1", y=unit(0.5,"npc") + 0.5*h_nat_1, 
                        vjust=-6, hjust = 0.2, gp=gpar(fontsize=14)) # title
gt_nat_1 <- gTree(children = gList(tab_nat_1, title_nat_1)) #,footnote
# grid.draw(gt_nat_1) # check

# for dimension 2
tab_nat_2 <- tableGrob(round(dims_nat[[2]], 2), theme = ttheme_minimal(base_size = 10)) # table

grid.newpage()
h_nat_2 <- grobHeight(tab_nat_1)
w_nat_2 <- grobWidth(tab_nat_1)
title_nat_2 <- textGrob("Dimension 2", y=unit(0.5,"npc") + 0.5*h_nat_2, 
                        vjust=-6, hjust = 0.2, gp=gpar(fontsize=14)) # title
gt_nat_2 <- gTree(children = gList(tab_nat_2, title_nat_2)) #,footnote
# grid.draw(gt_nat_2) # check

# arrange two Dimensions on one plot and print to interactive graphic with latex
ml_nat_1n2 <- marrangeGrob(list(gt_nat_1,gt_nat_2), nrow=1, ncol=2, top = '\n\n\n\nNational')

# print to graphic
jpeg("dims12_nat_1n2.jpeg")
ml_nat_1n2
dev.off()



# National 3 & 4:
# for dimension 3
tab_nat_3 <- tableGrob(round(dims_nat[[3]], 2), theme = ttheme_minimal(base_size = 10)) # table

grid.newpage()
h_nat_3 <- grobHeight(tab_nat_3)
w_nat_3 <- grobWidth(tab_nat_3)
title_nat_3 <- textGrob("Dimension 3", y=unit(0.5,"npc") + 0.5*h_nat_3, 
                        vjust=-6, hjust = 0.2, gp=gpar(fontsize=14)) # title
gt_nat_3 <- gTree(children = gList(tab_nat_3, title_nat_3)) #,footnote
# grid.draw(gt_nat_3) # check

# for dimension 4
tab_nat_4 <- tableGrob(round(dims_nat[[4]], 2), theme = ttheme_minimal(base_size = 10)) # table

grid.newpage()
h_nat_4 <- grobHeight(tab_nat_4)
w_nat_4 <- grobWidth(tab_nat_4)
title_nat_4 <- textGrob("Dimension 4", y=unit(0.5,"npc") + 0.5*h_nat_4, 
                        vjust=-5, hjust = 0.2, gp=gpar(fontsize=14)) # title
gt_nat_4 <- gTree(children = gList(tab_nat_4, title_nat_4)) #,footnote
# grid.draw(gt_nat_2) # check

# arrange two Dimensions on one plot and print to interactive graphic with latex
ml_nat_3n4 <- marrangeGrob(list(gt_nat_3,gt_nat_4), nrow=1, ncol=2, top = '\n\n\n\nNational')

# print to graphic
jpeg("dims12_nat_3n4.jpeg")
ml_nat_3n4
dev.off()



# National 5 & 6 & 7:
# for dimension 5
tab_nat_5 <- tableGrob(round(dims_nat[[5]], 2), theme = ttheme_minimal(base_size = 10)) # table

grid.newpage()
h_nat_5 <- grobHeight(tab_nat_5)
w_nat_5 <- grobWidth(tab_nat_5)
title_nat_5 <- textGrob("Dimension 5", y=unit(0.5,"npc") + 0.5*h_nat_5, 
                        vjust=-6, hjust = 0.2, gp=gpar(fontsize=14)) # title
gt_nat_5 <- gTree(children = gList(tab_nat_5, title_nat_5)) #,footnote
# grid.draw(gt_nat_5) # check

# for dimension 6
tab_nat_6 <- tableGrob(round(dims_nat[[6]], 2), theme = ttheme_minimal(base_size = 10)) # table

grid.newpage()
h_nat_6 <- grobHeight(tab_nat_6)
w_nat_6 <- grobWidth(tab_nat_6)
title_nat_6 <- textGrob("Dimension 6", y=unit(0.5,"npc") + 0.5*h_nat_6, 
                        vjust=-5, hjust = 0.2, gp=gpar(fontsize=14)) # title
gt_nat_6 <- gTree(children = gList(tab_nat_6, title_nat_6)) #,footnote
# grid.draw(gt_nat_6) # check


# arrange two Dimensions on one plot and print to interactive graphic with latex
ml_nat_5n6 <- marrangeGrob(list(gt_nat_5,gt_nat_6), nrow=1, ncol=2, top = '\nNational')

# print to graphic
jpeg("dims12_nat_5n6.jpeg")
ml_nat_5n6
dev.off()


## Nationals
# getting all the dimension descriptions
dimdesc_12_nat <- dimdesc(pca_12_nat, c(1:npc), proba = 1)

# categorical supplementaries per dimension ... need to explain and interpret "Estimate"
cat_coord_12_nat <- list()
for(i in 1:npc) {
        temp1 <- dimdesc_12_nat[[i]]$category[order(dimdesc_12_nat[[i]]$category[,1], decreasing = TRUE),]
        temp2 <- temp1[c(1:5, (nrow(temp1) - 4): nrow(temp1)),1]
        cat_coord_12_nat[[i]] <- data.frame(Est = round(temp2, 2))
        # write.table(cat_coord_12_nat[[i]], file = paste0("cat_dim", i, "_nat.csv")) # if needed
        
}

# getting tables 
# Dim 1 CT
tab_nat_cat1 <- tableGrob(cat_coord_12_nat[[1]], theme = ttheme_minimal(base_size = 12)) # table

grid.newpage()
h_cat_nat1 <- grobHeight(tab_nat_cat1)
w_cat_nat1 <- grobWidth(tab_nat_cat1)
title_tab_cat_nat1 <- textGrob('Dimension 1', y=unit(0.5,"npc") + 0.5*h_cat_nat1, 
                               vjust=-7, hjust = 0.3, gp=gpar(fontsize=14)) # title
gt_cat_nat1 <- gTree(children = gList(tab_nat_cat1, title_tab_cat_nat1))

# grid.draw(gt_cat_nat1) # check

# Dim 2 CT
tab_nat_cat2 <- tableGrob(cat_coord_12_nat[[2]], theme = ttheme_minimal(base_size = 12)) # table

grid.newpage()
h_cat_nat2 <- grobHeight(tab_nat_cat2)
w_cat_nat2 <- grobWidth(tab_nat_cat2)
title_tab_cat_nat2 <- textGrob('Dimension 2', y=unit(0.5,"npc") + 0.5*h_cat_nat2, 
                               vjust=-7, hjust = 0.3, gp=gpar(fontsize=14)) # title
gt_cat_nat2 <- gTree(children = gList(tab_nat_cat2, title_tab_cat_nat2))

# grid.draw(gt_cat_nat2) # check

# Dim 3 CT
tab_nat_cat3 <- tableGrob(cat_coord_12_nat[[3]], theme = ttheme_minimal(base_size = 12)) # table

grid.newpage()
h_cat_nat3 <- grobHeight(tab_nat_cat3)
w_cat_nat3 <- grobWidth(tab_nat_cat3)
title_tab_cat_nat3 <- textGrob('Dimension 3', y=unit(0.5,"npc") + 0.5*h_cat_nat3, 
                               vjust=-7, hjust = 0.3, gp=gpar(fontsize=14)) # title
gt_cat_nat3 <- gTree(children = gList(tab_nat_cat3, title_tab_cat_nat3))

# grid.draw(gt_cat_nat3) # check        

# Dim 4 CT
tab_nat_cat4 <- tableGrob(cat_coord_12_nat[[4]], theme = ttheme_minimal(base_size = 12)) # table

grid.newpage()
h_cat_nat4 <- grobHeight(tab_nat_cat4)
w_cat_nat4 <- grobWidth(tab_nat_cat4)
title_tab_cat_nat4 <- textGrob('Dimension 4', y=unit(0.5,"npc") + 0.5*h_cat_nat4, 
                               vjust=-7, hjust = 0.3, gp=gpar(fontsize=14)) # title
gt_cat_nat4 <- gTree(children = gList(tab_nat_cat4, title_tab_cat_nat4))

# grid.draw(gt_cat_nat4) # check

# Dim 5 CT
tab_nat_cat5 <- tableGrob(cat_coord_12_nat[[5]], theme = ttheme_minimal(base_size = 12)) # table

grid.newpage()
h_cat_nat5 <- grobHeight(tab_nat_cat5)
w_cat_nat5 <- grobWidth(tab_nat_cat5)
title_tab_cat_nat5 <- textGrob('Dimension 5', y=unit(0.5,"npc") + 0.5*h_cat_nat5, 
                               vjust=-7, hjust = 0.3, gp=gpar(fontsize=14)) # title
gt_cat_nat5 <- gTree(children = gList(tab_nat_cat5, title_tab_cat_nat5))

# grid.draw(gt_cat_nat5) # check  

# Dim 6 CT
tab_nat_cat6 <- tableGrob(cat_coord_12_nat[[6]], theme = ttheme_minimal(base_size = 12)) # table

grid.newpage()
h_cat_nat6 <- grobHeight(tab_nat_cat6)
w_cat_nat6 <- grobWidth(tab_nat_cat6)
title_tab_cat_nat6 <- textGrob('Dimension 6', y=unit(0.5,"npc") + 0.5*h_cat_nat6, 
                               vjust=-7, hjust = 0.3, gp=gpar(fontsize=14)) # title
gt_cat_nat6 <- gTree(children = gList(tab_nat_cat6, title_tab_cat_nat6))

# grid.draw(gt_cat_nat6) # check  



# creating two files for these 1st 4 and then 3 more
# arrange first four
ml_nat_1n2 <- marrangeGrob(list(gt_cat_nat1,
                                gt_cat_nat2), nrow=1, ncol=2,
                           top = '\n\n\n\nNational')

# print to graphic
jpeg("cats12_nat_1n2.jpeg")
ml_nat_1n2
dev.off()

ml_nat_3n4 <- marrangeGrob(list(gt_cat_nat3,
                                gt_cat_nat4), nrow=1, ncol=2,
                           top = '\n\n\n\nNational')

# print to graphic
jpeg("cats12_nat_3n4.jpeg")
ml_nat_3n4
dev.off()

ml_nat_5n6 <- marrangeGrob(list(gt_cat_nat5,
                                gt_cat_nat6), nrow=1, ncol=2,
                           top = '\n\n\n\nNational')

# print to graphic
jpeg("cats12_nat_5n6.jpeg")
ml_nat_5n6
dev.off()

# continuous supplementaries per dimension...explain difference... blah blah
# National:

cont_corrs_12_nat <- matrix(0, nrow = 4, ncol = 0)
for(i in 1:npc) {
        temp1 <- as.matrix(dimdesc_12_nat[[i]]$quanti)
        temp2 <- temp1[which(rownames(temp1) %in% c("age", "edu", "hh_inc", "lsm")),]
        cont_corrs_12_nat <- as.data.frame(round(cbind(cont_corrs_12_nat, temp2), 2))
}
names(cont_corrs_12_nat) <- c("Dim1", "pVal","Dim2", "pVal", "Dim3", "pVal", "Dim4", "pVal", "Dim5", "pVal", "Dim6", "pVal" )

# print to file for graphic:

tab_nat_cont <- tableGrob(cont_corrs_12_nat, theme = ttheme_minimal(base_size = 8)) # table

grid.newpage()
h_cont_nat <- grobHeight(tab_nat_cont)
w_cont_nat <- grobWidth(tab_nat_cont)
title_tab_cont_nat <- textGrob('National', y=unit(0.5,"npc") + 0.3*h_cont_nat, 
                               vjust=-4, hjust = 0.5, gp=gpar(fontsize=14)) # title
gt_cont_nat<- gTree(children = gList(tab_nat_cont, title_tab_cont_nat)) #
# grid.draw(gt_cont_nat) # check
