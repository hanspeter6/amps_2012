# libraries
library(stringr)
library(tidyverse)
library(caret)

print_12 <- read.csv("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/AMPS_2012/csv/amps-2012-newspaper-magazine-readership-v1.1.csv")
electr_12 <- read.csv("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/AMPS_2012/csv/amps-2012-electronic-media-v1.1.csv")
internet_12 <- read.csv("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/AMPS_2012/csv/amps-2012-cellphone-and-internet-v1.1.csv")
demogrs_12 <- read.csv("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/AMPS_2012/csv/amps-2012-demographics-v1.1.csv")
personal_12 <- read.csv("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/AMPS_2012/csv/amps-2012-personal-v1.2.csv")
lsm_12 <- read.csv("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/AMPS_2012/csv/amps-2012-lsm-saarf-segmentations-v1.1.csv")
lifestage_12 <- read.csv("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/AMPS_2012/csv/amps-2012-lifestage-v1.1.csv")
attitudes_12 <- read.csv("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/AMPS_2012/csv/amps-2012-attitudes-v1.1.csv")
# 
save(print_12, electr_12, internet_12, demogrs_12, personal_12, lsm_12, lifestage_12, attitudes_12, file = "input_12.RData")

load("input_12.RData")
# 
print_12_labels <- readLines("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/AMPS_2012/csv/metadata/variable_labels/amps-2012-newspaper-magazine-readership-v1.1_variable_labels.txt")
electr_12_labels <- readLines("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/AMPS_2012/csv/metadata/variable_labels/amps-2012-electronic-media-v1.1_variable_labels.txt")
internet_12_labels <- readLines("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/AMPS_2012/csv/metadata/variable_labels/amps-2012-cellphone-and-internet-v1.1_variable_labels.txt")
demogrs_12_labels <- readLines("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/AMPS_2012/csv/metadata/variable_labels/amps-2012-demographics-v1.1_variable_labels.txt")
personal_12_labels <- readLines("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/AMPS_2012/csv/metadata/variable_labels/amps-2012-personal-v1.2_variable_labels.txt")
lsm_12_labels <- readLines("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/AMPS_2012/csv/metadata/variable_labels/amps-2012-lsm-saarf-segmentations-v1.1_variable_labels.txt")
lifestage_12_labels <- readLines("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/AMPS_2012/csv/metadata/variable_labels/amps-2012-lifestage-v1.1_variable_labels.txt")
attitudes_12_labels <- readLines("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/AMPS_2012/csv/metadata/variable_labels/amps-2012-attitudes-v1.1_variable_labels.txt")
# 
save(print_12_labels, electr_12_labels, internet_12_labels, demogrs_12_labels, personal_12_labels, lsm_12_labels, lifestage_12_labels, attitudes_12_labels, file = "labels_12.RData")

load("labels_12.RData")

## 1st Print (newspapers and magazines) Media Set

names_issues_print_12 <- str_subset(print_12_labels, 'Number of different issues usually read or page through') %>%
        str_replace('.+\\s-', '') %>%
        str_trim()
vars_issues_print_12 <- str_subset(print_12_labels, 'Number of different issues usually read or page through') %>%
        str_replace('Number\\sof\\sdifferent.+', '') %>%
        str_trim()

##Newspapers
# fix names and get rid of some and save
# names_newspapers_12_issues <- names_issues_print_12[c(1:52)]
# fix(names_newspapers_12_issues)
# saveRDS(names_newspapers_12_issues, "names_newspapers_12_issues.rds")
names_newspapers_12_issues <- readRDS("names_newspapers_12_issues.rds")

# vector of variables
vars_newspapers_12_issues <- vars_issues_print_12[c(1:52)]
issues_newspapers_12 <- print_12[,vars_newspapers_12_issues]

# Magazines
# fix names and get rid of some (including MNet guides and save
# names_magazines_12_issues <- names_issues_print_12[c(54:66,68:78,80:91,93:98,100:114,117:139,141:148,151:162,165:168)]
# fix(names_magazines_12_issues)
# saveRDS(names_magazines_12_issues, "names_magazines_12_issues.rds")
names_magazines_12_issues <- readRDS("names_magazines_12_issues.rds")

# vector of variables
vars_magazines_12_issues <- vars_issues_print_12[c(54:66,68:78,80:91,93:98,100:114,117:139,141:148,151:162,165:168)]
issues_magazines_12 <- print_12[,vars_magazines_12_issues]

## THOUROUGHLY
names_thorough_print_12 <- str_subset(print_12_labels, 'How thoroughly respondent usually read') %>%
        str_replace('.+\\s-', '') %>%
        str_replace("\\'",'') %>%
        str_trim()
vars_thorough_print_12 <- str_subset(print_12_labels, 'How thoroughly respondent usually read') %>%
        str_replace('How\\sthoroughly.+', '') %>%
        str_trim()

##Newspapers
# get names and get rid of some and save (already sorted above)
# names_newspapers_12_thorough <- names_thorough_print_12[c(1:39,77)]
# fix(names_newspapers_12_thorough)
# saveRDS(names_newspapers_12_issues, "names_newspapers_12_issues.rds")

# vector of variables
vars_newspapers_12_thorough <- vars_thorough_print_12[c(1:52)]
thorough_newspapers_12 <- print_12[,vars_newspapers_12_thorough]
thorough_newspapers_12 <- 7 - thorough_newspapers_12

# Magazines
# fix names and get rid of some and save
# names_magazines_12_thorough <- names_thorough_print_12[c(77:99,103:107,109:157)]
# fix(names_magazines_12_issues)
# saveRDS(names_magazines_12_issues, "names_magazines_12_issues.rds")

# vector of variables
vars_magazines_12_thorough <- vars_thorough_print_12[c(54:66,68:78,80:91,93:98,100:114,117:139,141:148,151:162,165:168)]
thorough_magazines_12 <- print_12[,vars_magazines_12_thorough]

# # need to reverse numbering to serve as weights (see value_lables text file):
thorough_magazines_12 <- 7 - thorough_magazines_12

# create datasets ...for newspapers and magazines:
newspapers_engagement_12_all <- issues_newspapers_12 * thorough_newspapers_12
names(newspapers_engagement_12_all) <- names_newspapers_12_issues
magazines_engagement_12_all <- issues_magazines_12 * thorough_magazines_12
names(magazines_engagement_12_all) <- names_magazines_12_issues

newspapers_engagement_12_simple_all <- issues_newspapers_12
names(newspapers_engagement_12_simple_all) <- names_newspapers_12_issues
magazines_engagement_12_simple_all <- issues_magazines_12
names(magazines_engagement_12_simple_all) <- names_magazines_12_issues

# # # replace NAs with zeros
newspapers_engagement_12_all[is.na(newspapers_engagement_12_all)] <- 0
magazines_engagement_12_all[is.na(magazines_engagement_12_all)] <- 0

newspapers_engagement_12_simple_all[is.na(newspapers_engagement_12_simple_all)] <- 0
magazines_engagement_12_simple_all[is.na(magazines_engagement_12_simple_all)] <- 0

# save (alls)
saveRDS(newspapers_engagement_12_all, "newspapers_engagement_12_all.rds")
saveRDS(magazines_engagement_12_all, "magazines_engagement_12_all.rds")
saveRDS(newspapers_engagement_12_simple_all, "newspapers_engagement_12_simple_all.rds")
saveRDS(magazines_engagement_12_simple_all, "magazines_engagement_12_simple_all.rds")

## CLEAN UP and reduce variables

# for newspapers: include mean of "Sondag" and "The Zimbabwean" as "other.news"
other.news <- as.vector(apply(newspapers_engagement_12_all[,c(38,51)], 1, mean))
newspapers_engagement_12 <- newspapers_engagement_12_all %>%
        mutate(other.news = other.news)
newspapers_engagement_12 <- newspapers_engagement_12[,-c(38,51)]

other.news_simple <- as.vector(apply(newspapers_engagement_12_simple_all[,c(38,51)], 1, mean))
newspapers_engagement_12_simple <- newspapers_engagement_12_simple_all %>%
        mutate(other.news = other.news_simple)
newspapers_engagement_12_simple <- newspapers_engagement_12_simple[,-c(38,51)]

# for magazines - dealt with it in vehicle_cleaning project
magazines_engagement_12 <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/vehicle_cleaning/magazines_engagement_12.rds")
magazines_engagement_12_simple <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/vehicle_cleaning/magazines_engagement_12_simple.rds")

# save them in this project
saveRDS(newspapers_engagement_12, "newspapers_engagement_12.rds")
saveRDS(magazines_engagement_12, "magazines_engagement_12.rds")
saveRDS(newspapers_engagement_12_simple, "newspapers_engagement_12_simple.rds")
saveRDS(magazines_engagement_12_simple, "magazines_engagement_12_simple.rds")

magazines_engagement_12 <- readRDS("magazines_engagement_12.rds")
newspapers_engagement_12 <- readRDS("newspapers_engagement_12.rds")
magazines_engagement_12_simple <- readRDS("magazines_engagement_12_simple.rds")
newspapers_engagement_12_simple <- readRDS("newspapers_engagement_12_simple.rds")

## 2nd Electronic Media Set ( no "other")
# RADIO
# 
# names_radio_12_4w <- electr_12_labels %>%
#         str_subset('ca64co\\d{2}_\\d') %>%
#         str_replace('.+listened.+4\\sweeks\\s-\\s','')
# names_radio_12_4w <- names_radio_12_4w[-c(98,99)] # get rid of "unsure" and "none"
# 
# names_radio_12_7 <- electr_12_labels %>%
#         str_subset('ca65co\\d{2}_\\d') %>%
#         str_replace('.+listened.+7\\sdays\\s-\\s','')
# names_radio_12_7 <- names_radio_12_7[-c(81, 87,88)] # get rid of "unsure" and "none" & empty one 
# 
# names_radio_12_y <- electr_12_labels %>%
#         str_subset('ca66co\\d{2}_\\d') %>%
#         str_replace('.+listened\\sto\\syesterday\\s-\\s','')
# names_radio_12_y <- names_radio_12_y[-c(64,65)] # get rid of "unsure" and "none"

# # most radio stations in 4 weeks, so use that to create names list
# # names_radio_12 <- names_radio_12_4w
# fix(names_radio_12)
# saveRDS(names_radio_12, "names_radio_12.rds")
names_radio_12 <- readRDS('names_radio_12.rds')

# get data...
radio4weeks_12 <- electr_12[,str_detect(names(electr_12), 'ca64co\\d{2}_\\d')]
radio4weeks_12 <- radio4weeks_12[,-c(98,99)] # get rid of "unsure" and "none"

radio7days_12 <- electr_12[,str_detect(names(electr_12), 'ca65co\\d{2}_\\d')]
radio7days_12 <- radio7days_12[,-c(81, 87,88)]  # get rid of "unsure" and "none" & empty one 

radioYesterday_12 <- electr_12[,str_detect(names(electr_12), 'ca66co\\d{2}_\\d')]
radioYesterday_12 <- radioYesterday_12[,-c(64,65)]  # get rid of "unsure" and "none"

# identifying missing stations by changing all to "64"
a <- names(radio4weeks_12)
b <- names(radio7days_12)
c <- names(radioYesterday_12)
b_adj <- b %>%
        str_replace("65", "64")
c_adj <- c %>%
        str_replace("66", "64")

names(radio7days_12) <- b_adj
names(radioYesterday_12) <- c_adj

ind_7 <- which(names(radio4weeks_12) %in% names(radio7days_12))
ind_y <- which(names(radio4weeks_12) %in% names(radioYesterday_12))

# adding up
radio4weeks_12[,ind_7] <- radio4weeks_12[,ind_7] + radio7days_12
radio4weeks_12[,ind_y] <- radio4weeks_12[,ind_y] + radioYesterday_12

# creating engagement set:
radio_engagement_12_all <- radio4weeks_12
names(radio_engagement_12_all) <- names_radio_12

saveRDS(radio_engagement_12_all, "radio_engagement_12_all.rds")
radio_engagement_12_all <- readRDS("radio_engagement_12_all.rds")


# AFTER CLEANING (see vehicle cleaning project)
radio_engagement_12 <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/vehicle_cleaning/radio_engagement_12.rds")


## TV (this year, included specific dstv and toptv channels (will include them))
names_tv_12 <- c("e tv",
                 "SABC 1",
                 "SABC 2",
                 "SABC 3",
                 "IKZN TV",
                 "Bay TV",
                 "Cape Town TV",
                 "Soweto TV",
                 "Top TV",
                 "DSTV",
                 "Other TV")

saveRDS(names_tv_12, "names_tv_12.rds")
names_tv_12 <- readRDS("names_tv_12.rds")
# fix(names_tv_12)
# want to isolate only past 4 weeks and get rid of ("UNSURE", and "no TV")
tv4weeks_12 <- electr_12[,c('ca45co30_1', #e tv
                            'ca45co30_4', # sabc 1
                            'ca45co30_5', # sabc 2
                            'ca45co30_6', # sabc 3
                            'ca45co30_7', # IKZN
                            'ca45co30_8', # Bay
                            'ca45co30_9', # Cape Town
                            'ca45co31_0', # Soweto
                            'ca45co72_3', # top tv
                            'ca45co72_8', # dstv
                            'ca45co31_4' # other tv
                            )] 

# want to isolate only past 7 days...
tv7days_12 <- electr_12[,c('ca45co32_1',
                           'ca45co32_4',
                           'ca45co32_5',
                           'ca45co32_6',
                           'ca45co32_7',
                           'ca45co32_8',
                           'ca45co32_9',
                           'ca45co33_0',
                           'ca45co74_3',
                           'ca45co74_8',
                           'ca45co33_4'
                           )] 

# want to isolate only yesterday...(indexes w.r.t 4weeks that are missing here: 5, 8, 11 (other))
tvYesterday_12 <- electr_12[,c('ca45co34_1',
                               'ca45co34_4',
                               'ca45co34_5',
                               'ca45co34_6',
                               'ca45co34_8',
                               'ca45co34_9',
                               'ca45co76_3',
                               'ca45co76_8'
                               )]

# combining into a tv engagement dataset (using tv4weeks_12 as basis):

tv_engagement_12 <- tv4weeks_12 + tv7days_12
tv_engagement_12[,-c(5,8,11)] <- tv_engagement_12[,-c(5,8,11)] + tvYesterday_12
names(tv_engagement_12) <- names_tv_12

saveRDS(tv_engagement_12, "tv_engagement_12.rds")

tv_engagement_12 <- readRDS("tv_engagement_12.rds")

## 3rd Internet Media Set

## accessed: sum of 12 months, 4weeks, 7days and yesterday
internet_level1 <- internet_12[,str_detect(names(internet_12), 'ca49co(45)|(46)|(47)|(48)')]

#change all 2 = "No" and NA's' to 0
internet_level1 <- data.frame(ifelse(is.na(internet_level1) | internet_level1 == 2, 0, 1))

internet_level1 <- rowSums(internet_level1)

# what internet was accessed for...
##  (maybe could use similar to vehicles?? as well as add up and multiply with first eng):

internet_level2 <- internet_12[,str_detect(names(internet_12), 'ca49co(55)|(58)|(63)|(64)|(69)|(71)')]

# change NA and 3 = 0; 1,2,4 = 1
internet_level2[is.na(internet_level2)] <- 0
internet_level2 <- data.frame(apply(internet_level2, c(1,2), function(x) ifelse(x %in% c(0,3), 0, 1)))

names(internet_level2) <- c('int_search',
                          'int_social',
                          'int_print',
                          'int_news',
                          'int_tv',
                          'int_radio')

## create single dataframe for internet multiplying internet_level1 with sum of internet_level2:
internet_engagement_12 <- internet_level2  * internet_level1
internet_engagement_12_simple <- internet_level1

saveRDS(internet_engagement_12, "internet_engagement_12.rds")
saveRDS(internet_engagement_12_simple, "internet_engagement_12_simple.rds")

internet_engagement_12 <- readRDS("internet_engagement_12.rds")
internet_engagement_12_simple <- readRDS("internet_engagement_12_simple.rds")

## create single dataframe for media12, including total_engagement columns (consider using media groupings .. follow up on this!)
# Level 1: Type
media_type_12 <- data.frame(cbind(qn = print_12$qn,
                                  rowSums(newspapers_engagement_12),
                                  rowSums(magazines_engagement_12),
                                  rowSums(radio_engagement_12),
                                  rowSums(tv_engagement_12),
                                  rowSums(internet_engagement_12)))
names(media_type_12) <- c("qn",
                          "newspapers",
                          "magazines",
                          "radio",
                          "tv",
                          "internet")

media_type_12 <- media_type_12 %>%
        mutate(all = as.vector(newspapers + magazines + radio + tv + internet))

media_type_12_simple <- data.frame(cbind(qn = print_12$qn,
                                  rowSums(newspapers_engagement_12_simple),
                                  rowSums(magazines_engagement_12_simple),
                                  rowSums(radio_engagement_12),
                                  rowSums(tv_engagement_12),
                                  internet_engagement_12_simple))
names(media_type_12_simple) <- c("qn",
                          "newspapers",
                          "magazines",
                          "radio",
                          "tv",
                          "internet")

media_type_12_simple <- media_type_12_simple %>%
        mutate(all = as.vector(newspapers + magazines + radio + tv + internet))

# Level 2: Vehicles
media_vehicles_12 <- data.frame(cbind(qn = print_12$qn,
                                      newspapers_engagement_12,
                                      magazines_engagement_12,
                                      radio_engagement_12,
                                      tv_engagement_12,
                                      internet_engagement_12))
media_vehicles_12_simple <- data.frame(cbind(qn = print_12$qn,
                                      newspapers_engagement_12_simple,
                                      magazines_engagement_12_simple,
                                      radio_engagement_12,
                                      tv_engagement_12,
                                      internet_eng = internet_engagement_12_simple))

saveRDS(media_type_12, 'media_type_12.rds')
saveRDS(media_vehicles_12, 'media_vehicles_12.rds')
saveRDS(media_type_12_simple, 'media_type_12_simple.rds')
saveRDS(media_vehicles_12_simple, 'media_vehicles_12_simple.rds')

media_type_12 <- readRDS('media_type_12.rds')
media_vehicles_12 <- readRDS('media_vehicles_12.rds')
media_type_12_simple <- readRDS('media_type_12_simple.rds')
media_vehicles_12_simple <- readRDS('media_vehicles_12_simple.rds')

## 4th Demographics Set (see notes for descriptions)

age <- personal_12[,'ca56co34']
sex <- demogrs_12[,'ca91co51a']
edu <- demogrs_12[,'ca91co48']
for(i in 1: length(edu)) {
        if(edu[i] %in% c(6,7)) {
                edu[i] <- edu[i] + 1
        }
        else if(edu[i] == 8) {
                edu[i] <- 6
        }
}
hh_inc <- demogrs_12[,'ca91co50']
race <- demogrs_12[,'ca91co51b']
province <- demogrs_12[,'ca91co56']
metro1 <- demogrs_12[,'ca91co57']
metro2 <- demogrs_12[,'ca91co58'] + 9


metro <- rowSums(cbind(metro1,
                       metro2), na.rm = TRUE)

# collect and code into single metro set:
#0 = no metro
#1 Cape Town
#2 Cape Town Fringe Area
#3 Port Elizabeth/Uitenhage
#4 East London
#5 Durban
#6 Bloemfontein
#7 Greater Johannesburg
#8 Reef
#9 Pretoria
#10 Kimberley
##11 Pietermaritzburg
##12 Vaal

metro <- ifelse(metro == 19, 7, metro) # add soweto back to greater jhb
metro <- ifelse(metro == 13, 12, metro) # change code of Vaal

lang <- demogrs_12[,'ca91co75'] + 1 # change 0 to 1, so add one to all
lifestages <- demogrs_12[,'ca91co77']
mar_status <- personal_12[,'ca56co09']

lsm <- lsm_12[,'ca91co64']
lsm <- ifelse(lsm == 0,10,lsm)

lifestyle <- lsm_12[,'ca58co39'] + 1 # to get rid of zero

attitudesA <- lsm_12[,'ca67co10'] + 1 # to get rid of zeros
attitudesB <- lsm_12[,'ca67co10_lsm']
attitudesA <- ifelse(is.na(attitudesA), 0, attitudesA)
attitudesB <- ifelse(is.na(attitudesB), 0, attitudesB)
attitudes <- attitudesA + attitudesB
attitudes <- ifelse(attitudes == 8, 4, attitudes) # distant rooted
attitudes <- ifelse(attitudes == 5 | attitudes == 6, attitudes + 1, attitudes)
attitudes <- ifelse(attitudes == 9, 5, attitudes) # distant ...
table(attitudes) # check


demographics_12 <- data.frame(qn = print_12$qn,
                              pwgt = print_12$pwgt,
                              age,
                              sex,
                              edu,
                              hh_inc,
                              race,
                              province,
                              metro,
                              lang,
                              lifestages,
                              mar_status,
                              lsm,
                              lifestyle,
                              attitudes)


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

saveRDS(demographics_12, "demographics_12.rds")
demographics_12 <- readRDS("demographics_12.rds")

# read datafiles again (if necessary)
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

# #create single dataset minus non metropolitans
set12 <- demographics_12 %>%
        left_join(media_type_12) %>%
        left_join(media_vehicles_12) %>%
        filter(metro != 0)

set12_simple <- demographics_12 %>%
        left_join(media_type_12_simple) %>%
        left_join(media_vehicles_12_simple) %>%
        filter(metro != 0)

# get rid of zero variances:
ind_12 <- nearZeroVar(set12[,16:ncol(set12)], saveMetrics = TRUE)
good_set <- set12[,16:ncol(set12)][,!ind_12$zeroVar]
set12 <- data.frame(cbind(set12[,1:15], good_set))

ind_12_simple <- nearZeroVar(set12_simple[,16:ncol(set12_simple)], saveMetrics = TRUE)
good_set_simple <- set12_simple[,16:ncol(set12_simple)][,!ind_12_simple$zeroVar]
set12_simple <- data.frame(cbind(set12_simple[,1:15], good_set_simple))


# scale media type and media vehicles
set12[,16:ncol(set12)] <- scale(set12[,16:ncol(set12)])
set12_simple[,16:ncol(set12_simple)] <- scale(set12_simple[,16:ncol(set12_simple)])

# save them:
saveRDS(set12, "set12.rds")
saveRDS(set12_simple, "set12_simple.rds")
