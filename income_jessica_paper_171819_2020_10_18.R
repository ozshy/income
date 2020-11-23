# income_Jessica_paper_171819_2020_10_18.R: Finding card adoption by race (for policy hub blog with Larry)
# income_Jessica_paper_171819_2020_9_25.R: Revising Tables for the Financial Inclusion paper, using 2017-218-19 data
# income171819_200703.R start working on merged 2017-18-19 diary data
# income_17_18_200629.R Last version containing Appendix (regressing with demographics)
# income_17_18_200626.R Giving up on 2019 data, back to 2017_18 data
# Shifting to median assessments (similar to the cashless paper)
# income_200405.R start revision after Mgmt Sci rejection: Adding subsection on constant terms and demongraphics. 
# income_200207.R Remove monthly val and vol from Table 1. And,
# Line 450 restricting regressions of none to PI cash and prepaid only (305+51 payments)
# income_200203.R complete version
# income_200125.R switch from mlogit built-in logsum function to computing CS directly from utilities of cash, prepaid, and debit (when added). 
# income_200124.R typed into income-1.tex, Uses mlogit built-in logsum function (SAVE)
# income_191223.R start new paper: "Household Income and Consumer Payment Choice

### The following packages are used:
library(formattable)# has percent function
#library(plotrix)# weighted histograms
library(dplyr)
library(xtable)# for LaTeX tables
#library(rpart)
#library(rpart.plot)
#library(partykit)# modifies rpart tree plot
#library(ggplot2)
library(spatstat) # for weighted.median
#library(mfx)# marginal effects logit regression
#library(regclass) # for confusion_matrix
#library(nnet) # for multinomial logit
#library(AER) # for p-values of nnet multinom logit coeftest(regression)
#library(mlogit)
#library(mnlogit)# Where I replicate the existing mlogit regressions and experiment with individual effects. 
#library(tibble)
setwd("~/Papers/income/income_coding/Jessica's paper coding")# NOTE, your working directory is different. Adjust accordingly!!! 
dir()

### Reading RDS datasets (merged data: merged by a separate R-file)
d1 = readRDS("diary171819_200704.rds")
objects()
names(d1)
length(unique(d1$uasid)) # num of unique respondents
dim(d1) # num trans times num variables

### Unlike the cashless paper, (d1, d2,...), here I analyze adoption (and spending) for all respondents (bill payments included) ==> data frame labeled as h1, h2, etc.

### Start preparing for Figure and Tables
length(unique(d1$uasid))# total num unique resp in 3 years
#
d1_2017.df = subset(d1, year == 2017)
resp_2017.vec = d1_2017.df$uasid[!duplicated(d1_2017.df$uasid)]# list of unique resp
head(resp_2017.vec)# 
length(resp_2017.vec)# num 2017 resp
#
d1_2018.df = subset(d1, year == 2018)
resp_2018.vec = d1_2018.df$uasid[!duplicated(d1_2018.df$uasid)]# list of unique resp
head(resp_2018.vec)# 
length(resp_2018.vec)# num 2018 resp
#
d1_2019.df = subset(d1, year == 2019)
resp_2019.vec = d1_2019.df$uasid[!duplicated(d1_2019.df$uasid)]# list of unique resp
head(resp_2019.vec)# 
length(resp_2019.vec)# num 2019 resp
#
# num resp who participated in 2017 and 2018
length(intersect(resp_2017.vec, resp_2018.vec))
# num resp who participated in 2017 and 2019
length(intersect(resp_2017.vec, resp_2019.vec))
# num resp who participated in 2018 and 2019
length(intersect(resp_2018.vec, resp_2019.vec))
# num resp who participated in 2017, 2018 and 2019
length(intersect(resp_2017.vec, intersect(resp_2018.vec, resp_2019.vec)))

##
h1 = d1
table(h1$pi)
nrow(h1[is.na(h1$pi), ])# num missing pi
h2 = h1[!is.na(h1$pi), ]# removing pi missing
table(h2$pi)
#
h3 = h2
table(h3$type)# The removal of the above NA pi also removed type==income, so there no need to remove type==income. All remaining obs are payments

# Shorten PI names
str(h3$pi)
levels(h3$pi)
#
#levels(h3$pi)[levels(h3$pi)=="1"] = "cash"
#levels(h3$pi)[levels(h3$pi)=="2"] = "check"
levels(h3$pi)[levels(h3$pi)=="credit_card"] = "credit"
levels(h3$pi)[levels(h3$pi)=="debit_card"] = "debit"
levels(h3$pi)[levels(h3$pi)=="prepaid/gift/EBT_card"] = "prepaid"
table(h3$pi)

# removing NA from card and bank adoption profiles
h4 = h3
sum(is.na(h4$cc_adopt))
sum(is.na(h4$dc_adopt))
sum(is.na(h4$bnk_acnt_adopt))
# remove NAs from the above 3 var
nrow(h4)
h4 = h4[!is.na(h4$cc_adopt), ]
h4 = h4[!is.na(h4$dc_adopt), ]
h4 = h4[!is.na(h4$bnk_acnt_adopt), ]
nrow(h4)

## adding var with respondent's adoption profile: Both_cards, DC_only, CC_only, None, both_UB. 
h4$adopt = NA
h4$adopt[h4$dc_adopt==1 & h4$cc_adopt==1] = "Both_cards"
h4$adopt[h4$cc_adopt==0 & h4$pi != "credit"] = "No_cc"
h4$adopt[h4$dc_adopt==0 & h4$pi != "debit"] = "No_dc"
h4$adopt[h4$dc_adopt==0 & h4$cc_adopt==0 & h4$pi != "credit" & h4$pi != "debit"] = "None"
table(h4$adopt)
str(h4$adopt)
h4$adopt = as.factor(h4$adopt)
sum(is.na(h4$adopt))# num trans with missing adopt =0

## create some duplicates demographics (upper and lower case)
names(dplyr::select(h4, contains("age")))
h4$Age = h4$age # have it both ways
summary(h4$Age)
names(dplyr::select(h4, contains("income")))
h4$HH_income = h4$hh_income
summary(h4$HH_income)
names(dplyr::select(h4, contains("size")))
h4$HH_size = h4$hh_size
summary(h4$HH_size)
h4$Work = h4$work
table(h4$Work)
h4$Marital = h4$marital
table(h4$Marital)
names(dplyr::select(h4, contains("gender")))
table(h4$gender)
h4$Gender = h4$gender
table(h4$Gender)
#
names(dplyr::select(h4, contains("educ")))
table(h4$education)
h4$Education = h4$education
table(h4$Education)
#

# removing payments not made in October (otherwise, the per-respondent monthly averages become complicated)
dim(h4)
h5 = h4 %>% filter((date > "2017-09-30" & date < "2017-11-01") | (date > "2018-09-30" & date < "2018-11-01") | (date > "2019-09-30" & date < "2019-11-01"))
table(h5[h5$year==2017, ]$date)
table(h5[h5$year==2018, ]$date)
table(h5[h5$year==2019, ]$date)
dim(h5)

# remove NA from HH_income
h6 = h5
summary(h6$HH_income)
dim(h6)
h6 = h6[!is.na(h5$HH_income),]
dim(h6)
summary(h6$HH_income)

h6_unique = h6[!duplicated(h6$uasid), ] # data set containing each resp only once (not to be used for trans stats, only adoption stats)
dim(h6_unique)
# check missing weights for h6_unique$weight_171819_1
sum(is.na(h6_unique$weight_171819_1))
dim(h6_unique)
h6_unique = h6_unique[!is.na(h6_unique$weight_171819_1), ]
dim(h6_unique)

### Start constructing FIgure 1: PI adoption vs. income buckets
#
#Percent of None by income
(none_perc_10k = nrow(h6_unique[h6_unique$HH_income<=10000 & h6_unique$adopt=="None", ])/nrow(h6_unique[h6_unique$HH_income<=10000, ]))
#
(none_perc_20k = nrow(h6_unique[h6_unique$HH_income > 10000 & h6_unique$HH_income <= 20000 & h6_unique$adopt=="None", ])/nrow(h6_unique[h6_unique$HH_income > 10000 & h6_unique$HH_income <= 20000, ]))
#
(none_perc_30k = nrow(h6_unique[h6_unique$HH_income > 20000 & h6_unique$HH_income <= 30000 & h6_unique$adopt=="None", ])/nrow(h6_unique[h6_unique$HH_income > 20000 & h6_unique$HH_income <= 30000, ]))
#
(none_perc_40k = nrow(h6_unique[h6_unique$HH_income > 30000 & h6_unique$HH_income <= 40000 & h6_unique$adopt=="None", ])/nrow(h6_unique[h6_unique$HH_income > 30000 & h6_unique$HH_income <= 40000, ]))
#
(none_perc_50k = nrow(h6_unique[h6_unique$HH_income > 40000 & h6_unique$HH_income <= 50000 & h6_unique$adopt=="None", ])/nrow(h6_unique[h6_unique$HH_income > 40000 & h6_unique$HH_income <= 50000, ]))
#
(none_perc_60k = nrow(h6_unique[h6_unique$HH_income > 50000 & h6_unique$HH_income <= 60000 & h6_unique$adopt=="None", ])/nrow(h6_unique[h6_unique$HH_income > 50000 & h6_unique$HH_income <= 60000, ]))
#
(none_perc_70k = nrow(h6_unique[h6_unique$HH_income > 60000 & h6_unique$HH_income <= 70000 & h6_unique$adopt=="None", ])/nrow(h6_unique[h6_unique$HH_income > 60000 & h6_unique$HH_income <= 70000, ]))
#

#jpeg("jess_fig1.jpeg", res = 300 )
par(mar = c(2.5,2,1,2))
plot(adopt~HH_income, data = subset(h6_unique, HH_income <= 120000), col = c("magenta", "blue", "green", "red"), ylab = (""), xlab = "Household income (USD)", breaks = seq(0,120000, 10000), axes = T)
legend("bottomright", c("Both cards", "No credit card", "No debit card", "Neither card"), fill=c("magenta", "blue", "green", "red"))
text(0.04, 0.85, labels = percent(none_perc_10k, digits = 1), cex = 0.8)
text(0.13, 0.9, labels = percent(none_perc_20k, digits = 1), cex = 0.8)
text(0.22, 0.97, labels = percent(none_perc_30k, digits = 1), cex = 0.8)
text(0.33, 0.98, labels = percent(none_perc_40k, digits = 1), cex = 0.8)
text(0.45, 0.99, labels = percent(none_perc_50k, digits = 1), cex = 0.8)
text(0.55, 0.99, labels = percent(none_perc_60k, digits = 1), cex = 0.8)
text(0.65, 0.99, labels = percent(none_perc_70k, digits = 1), cex = 0.8)
#dev.off()


# Caption of Fig 1: frac resp with hh income < 120k
length(unique(h6$uasid))
length(unique(h6[h6$HH_income <= 120000, ]$uasid))
percent(length(unique(h6[h6$HH_income <= 120000, ]$uasid))/length(unique(h6$uasid)))

(none_perc_median = percent(nrow(h6_unique[h6_unique$HH_income <= 61372 & h6_unique$adopt=="None", ])/nrow(h6_unique[h6_unique$HH_income <= 61372, ])))# none adoption below 2017 HH med income
nrow(h6_unique)
nrow(h6_unique[h6_unique$year==2017, ])
nrow(h6_unique[h6_unique$year==2018, ])# low b/c it includes only new resp
nrow(h6_unique[h6_unique$year==2019, ])# low b/c it includes only new resp
nrow(h6_unique[h6_unique$year==2017, ])+nrow(h6_unique[h6_unique$year==2018, ])+nrow(h6_unique[h6_unique$year==2019, ])# verify = sum of unique resp
# End of Fig 1 

# For Policy Hub blog: Percent of none by race
names(dplyr::select(h4, contains("race")))
#Percent of None by race
table(h6_unique$race_white)
str(h6_unique$race_white)
(num_white = sum(h6_unique$race_white, na.rm = T))
#
table(h6_unique$race_black)
str(h6_unique$race_black)
(num_black = sum(h6_unique$race_black, na.rm = T))
#
table(h6_unique$race_asian)
str(h6_unique$race_asian)
(num_asian = sum(h6_unique$race_asian, na.rm = T))
#
table(h6_unique$race_other)
str(h6_unique$race_other)
(num_other = sum(h6_unique$race_other, na.rm = T))
#
(none_white = 100*round(nrow(h6_unique[h6_unique$race_white==1 & h6_unique$adopt=="None", ])/num_white, digits = 3))
#
(none_black = 100*round(nrow(h6_unique[h6_unique$race_black==1 & h6_unique$adopt=="None", ])/num_black, digits=3))
#
(none_asian = 100*round(nrow(h6_unique[h6_unique$race_asian==1 & h6_unique$adopt=="None", ])/num_asian, digits=3))

### Start Table 2 (Jessica's paper): Adoption vs. income
# Table 1 (Jessica's paper) follows
# define adopt2 which separates banked from unbaned
h7 = h6
# below, construct 2 additional "adopt" variables (for different tables)
h7$adopt2=NA
h7$adopt3=NA# needed for Any_card and No_card
h7$adopt4=NA# needed to divide the above into banked and unbanked
h7$adopt5 = NA# needed for Table 1 (following Table 2 below)
h7$adopt2[h7$dc_adopt==1 & h7$cc_adopt==1] = "Both_cards"
h7$adopt2[h7$cc_adopt==0 & h7$pi != "credit"] = "No_cc"
h7$adopt2[h7$dc_adopt==0 & h7$pi != "debit"] = "No_dc"
#h7$adopt2[h7$dc_adopt==0 & h7$cc_adopt==0 & h7$bnk_acnt_adopt==1 & h7$pi != "credit" & h7$pi != "debit"] = "None_banked"# modified for Jess paper to match No_card new variable
h7$adopt2[h7$dc_adopt==0 & h7$cc_adopt==0 & h7$bnk_acnt_adopt==1] = "None_banked"
#h7$adopt2[h7$dc_adopt==0 & h7$cc_adopt==0 & h7$bnk_acnt_adopt==0 & h7$pi != "credit" & h7$pi != "debit"] = "None_unbanked"# modified for Jess paper to match No_card new variable
h7$adopt2[h7$dc_adopt==0 & h7$cc_adopt==0 & h7$bnk_acnt_adopt==0] = "None_unbanked"
h7$adopt3[h7$dc_adopt==1 || h7$cc_adopt==1] = "Any_card"
h7$adopt3[h7$dc_adopt==0 & h7$cc_adopt==0] = "No_card"
# verify no card banks + no card unbanked = no card:
nrow(subset(h7, adopt2=="None_unbanked")) + nrow(subset(h7, adopt2=="None_banked"))
nrow(subset(h7, adopt3=="No_card"))
# modifying no card bank and unbanked to equal no card
h7$adopt4[h7$adopt3=="No_card"& h7$bnk_acnt_adopt==0]="None_unbanked"
h7$adopt4[h7$adopt3=="No_card"& h7$bnk_acnt_adopt==1]="None_banked"
#verify sum
nrow(subset(h7, adopt4=="None_unbanked")) + nrow(subset(h7, adopt4=="None_banked"))
# for Table 1
h7$adopt5[h7$dc_adopt==1 & h7$cc_adopt==0] = "Yes_dc_No_cc"
h7$adopt5[h7$cc_adopt==1 & h7$dc_adopt==0] = "Yes_cc_No_dc"


#h7$adopt3[h7$dc_adopt==0 & h7$cc_adopt==0 & h7$pi != "credit" & h7$pi != "debit"] = "No_card"
table(h7$adopt)
table(h7$adopt2)# 
table(h7$adopt3)#
table(h7$adopt4)# 

# for the table, construct avg monthly exp and avg. monthly bill spending
table(h7$type)# verify payments only
h8 = h7 
dim(h8)
table(h8$bill)
h8_unique = h8[!duplicated(h8$uasid), ] # all 

# rescaling weight constructing weight_2_unique
names(dplyr::select(h8_unique, contains("weight")))
h8_unique$weight_2_unique = nrow(h8_unique)*h8_unique$weight_171819_1 / sum(h8_unique$weight_171819_1, na.rm = T)
dim(h8_unique)
sum(h8_unique$weight_2_unique, na.rm = T)# = nrow = num resp

## subseting h8 (trans obs) into income groups
h8_0_10k_unique = subset(h8_unique, HH_income >= 0     & HH_income < 10000)
h8_10_20k_unique = subset(h8_unique, HH_income >= 10000 & HH_income < 20000)
h8_20_30k_unique = subset(h8_unique, HH_income >= 20000 & HH_income < 30000)
h8_30_40k_unique = subset(h8_unique, HH_income >= 30000 & HH_income < 40000)
h8_40_60k_unique = subset(h8_unique, HH_income >= 40000 & HH_income < 60000)
h8_60_80k_unique = subset(h8_unique, HH_income >= 60000 & HH_income < 80000)
h8_80_120k_unique = subset(h8_unique, HH_income >= 80000 & HH_income < 120000)
h8_120_180k_unique = subset(h8_unique, HH_income >= 120000 & HH_income < 180000)
h8_180_inf_unique  = subset(h8_unique, HH_income >= 180000)

table(h8$adopt)
table(h8$adopt2)
table(h8$adopt3)
#
# frac resp having both CC and DC by income group
(h8_0_10k_cc_dc = nrow(subset(h8_0_10k_unique, adopt=="Both_cards"))/nrow(h8_0_10k_unique))
(h8_10_20k_cc_dc = nrow(subset(h8_10_20k_unique, adopt=="Both_cards"))/nrow(h8_10_20k_unique))
(h8_20_30k_cc_dc = nrow(subset(h8_20_30k_unique, adopt=="Both_cards"))/nrow(h8_20_30k_unique))
(h8_30_40k_cc_dc = nrow(subset(h8_30_40k_unique, adopt=="Both_cards"))/nrow(h8_30_40k_unique))
(h8_40_60k_cc_dc = nrow(subset(h8_40_60k_unique, adopt=="Both_cards"))/nrow(h8_40_60k_unique))
(h8_60_80k_cc_dc = nrow(subset(h8_60_80k_unique, adopt=="Both_cards"))/nrow(h8_60_80k_unique))
(h8_80_120k_cc_dc = nrow(subset(h8_80_120k_unique, adopt=="Both_cards"))/nrow(h8_80_120k_unique))
(h8_120_180k_cc_dc = nrow(subset(h8_120_180k_unique, adopt=="Both_cards"))/nrow(h8_120_180k_unique))
(h8_180_inf_cc_dc = nrow(subset(h8_180_inf_unique, adopt=="Both_cards"))/nrow(h8_180_inf_unique))
(h8_all_cc_dc = nrow(subset(h8_unique, adopt=="Both_cards"))/nrow(h8_unique))
#(both = c(h8_0_10k_cc_dc, h8_10_20k_cc_dc, h8_20_30k_cc_dc, h8_40_60k_cc_dc, h8_60_80k_cc_dc, h8_80_120k_cc_dc, h8_120_180k_cc_dc, h8_180_inf_cc_dc))# column in Table 1
#
# frac resp No CC by income group
(h8_0_10k_no_cc = nrow(subset(h8_0_10k_unique, adopt=="No_cc"))/nrow(h8_0_10k_unique))
(h8_10_20k_no_cc = nrow(subset(h8_10_20k_unique, adopt=="No_cc"))/nrow(h8_10_20k_unique))
(h8_20_30k_no_cc = nrow(subset(h8_20_30k_unique, adopt=="No_cc"))/nrow(h8_20_30k_unique))
(h8_30_40k_no_cc = nrow(subset(h8_30_40k_unique, adopt=="No_cc"))/nrow(h8_30_40k_unique))
(h8_40_60k_no_cc = nrow(subset(h8_40_60k_unique, adopt=="No_cc"))/nrow(h8_40_60k_unique))
(h8_60_80k_no_cc = nrow(subset(h8_60_80k_unique, adopt=="No_cc"))/nrow(h8_60_80k_unique))
(h8_80_120k_no_cc = nrow(subset(h8_80_120k_unique, adopt=="No_cc"))/nrow(h8_80_120k_unique))
(h8_120_180k_no_cc = nrow(subset(h8_120_180k_unique, adopt=="No_cc"))/nrow(h8_120_180k_unique))
(h8_180_inf_no_cc = nrow(subset(h8_180_inf_unique, adopt=="No_cc"))/nrow(h8_180_inf_unique))
(h8_all_no_cc = nrow(subset(h8_unique, adopt=="No_cc"))/nrow(h8_unique))
#(no_cc = c(h8_0_10k_no_cc, h8_10_20k_no_cc, h8_20_30k_no_cc, h8_40_60k_no_cc, h8_60_80k_no_cc, h8_80_120k_no_cc, h8_120_180k_no_cc, h8_180_inf_no_cc))# column in Table 1
#
# frac resp No DC by income group
(h8_0_10k_no_dc = nrow(subset(h8_0_10k_unique, adopt=="No_dc"))/nrow(h8_0_10k_unique))
(h8_10_20k_no_dc = nrow(subset(h8_10_20k_unique, adopt=="No_dc"))/nrow(h8_10_20k_unique))
(h8_20_30k_no_dc = nrow(subset(h8_20_30k_unique, adopt=="No_dc"))/nrow(h8_20_30k_unique))
(h8_30_40k_no_dc = nrow(subset(h8_30_40k_unique, adopt=="No_dc"))/nrow(h8_30_40k_unique))
(h8_40_60k_no_dc = nrow(subset(h8_40_60k_unique, adopt=="No_dc"))/nrow(h8_40_60k_unique))
(h8_60_80k_no_dc = nrow(subset(h8_60_80k_unique, adopt=="No_dc"))/nrow(h8_60_80k_unique))
(h8_80_120k_no_dc = nrow(subset(h8_80_120k_unique, adopt=="No_dc"))/nrow(h8_80_120k_unique))
(h8_120_180k_no_dc = nrow(subset(h8_120_180k_unique, adopt=="No_dc"))/nrow(h8_120_180k_unique))
(h8_180_inf_no_dc = nrow(subset(h8_180_inf_unique, adopt=="No_dc"))/nrow(h8_180_inf_unique))
(h8_all_no_dc = nrow(subset(h8_unique, adopt=="No_dc"))/nrow(h8_unique))
#(no_dc = c(h8_0_10k_no_dc, h8_10_20k_no_dc, h8_20_30k_no_dc, h8_40_60k_no_dc, h8_60_80k_no_dc, h8_80_120k_no_dc, h8_120_180k_no_dc, h8_180_inf_no_dc))# column in Table 1
#
table(h8$adopt)
table(h8$adopt2)
table(h8$adopt3)
# frac resp having Any_card by income group
(h8_0_10k_any_card = nrow(subset(h8_0_10k_unique, adopt3=="Any_card"))/nrow(h8_0_10k_unique))
(h8_10_20k_any_card = nrow(subset(h8_10_20k_unique, adopt3=="Any_card"))/nrow(h8_10_20k_unique))
(h8_20_30k_any_card = nrow(subset(h8_20_30k_unique, adopt3=="Any_card"))/nrow(h8_20_30k_unique))
(h8_30_40k_any_card = nrow(subset(h8_30_40k_unique, adopt3=="Any_card"))/nrow(h8_30_40k_unique))
(h8_40_60k_any_card = nrow(subset(h8_40_60k_unique, adopt3=="Any_card"))/nrow(h8_40_60k_unique))
(h8_60_80k_any_card = nrow(subset(h8_60_80k_unique, adopt3=="Any_card"))/nrow(h8_60_80k_unique))
(h8_80_120k_any_card = nrow(subset(h8_80_120k_unique, adopt3=="Any_card"))/nrow(h8_80_120k_unique))
(h8_120_180k_any_card = nrow(subset(h8_120_180k_unique, adopt3=="Any_card"))/nrow(h8_120_180k_unique))
(h8_180_inf_any_card = nrow(subset(h8_180_inf_unique, adopt3=="Any_card"))/nrow(h8_180_inf_unique))
(h8_all_any_card = nrow(subset(h8_unique, adopt3=="Any_card"))/nrow(h8_unique))

# frac resp having No_card by income group
(h8_0_10k_no_card = nrow(subset(h8_0_10k_unique, adopt3=="No_card"))/nrow(h8_0_10k_unique))
(h8_10_20k_no_card = nrow(subset(h8_10_20k_unique, adopt3=="No_card"))/nrow(h8_10_20k_unique))
(h8_20_30k_no_card = nrow(subset(h8_20_30k_unique, adopt3=="No_card"))/nrow(h8_20_30k_unique))
(h8_30_40k_no_card = nrow(subset(h8_30_40k_unique, adopt3=="No_card"))/nrow(h8_30_40k_unique))
(h8_40_60k_no_card = nrow(subset(h8_40_60k_unique, adopt3=="No_card"))/nrow(h8_40_60k_unique))
(h8_60_80k_no_card = nrow(subset(h8_60_80k_unique, adopt3=="No_card"))/nrow(h8_60_80k_unique))
(h8_80_120k_no_card = nrow(subset(h8_80_120k_unique, adopt3=="No_card"))/nrow(h8_80_120k_unique))
(h8_120_180k_no_card = nrow(subset(h8_120_180k_unique, adopt3=="No_card"))/nrow(h8_120_180k_unique))
(h8_180_inf_no_card = nrow(subset(h8_180_inf_unique, adopt3=="No_card"))/nrow(h8_180_inf_unique))
(h8_all_no_card = nrow(subset(h8_unique, adopt3=="No_card"))/nrow(h8_unique))

# frac resp None_banked by income group
(h8_0_10k_none_banked = nrow(subset(h8_0_10k_unique, adopt4=="None_banked"))/nrow(h8_0_10k_unique))
(h8_10_20k_none_banked = nrow(subset(h8_10_20k_unique, adopt4=="None_banked"))/nrow(h8_10_20k_unique))
(h8_20_30k_none_banked = nrow(subset(h8_20_30k_unique, adopt4=="None_banked"))/nrow(h8_20_30k_unique))
(h8_30_40k_none_banked = nrow(subset(h8_30_40k_unique, adopt4=="None_banked"))/nrow(h8_30_40k_unique))
(h8_40_60k_none_banked = nrow(subset(h8_40_60k_unique, adopt4=="None_banked"))/nrow(h8_40_60k_unique))
(h8_60_80k_none_banked = nrow(subset(h8_60_80k_unique, adopt4=="None_banked"))/nrow(h8_60_80k_unique))
(h8_80_120k_none_banked = nrow(subset(h8_80_120k_unique, adopt4=="None_banked"))/nrow(h8_80_120k_unique))
(h8_120_180k_none_banked = nrow(subset(h8_120_180k_unique, adopt4=="None_banked"))/nrow(h8_120_180k_unique))
(h8_180_inf_none_banked = nrow(subset(h8_180_inf_unique, adopt4=="None_banked"))/nrow(h8_180_inf_unique))
(h8_all_none_banked = nrow(subset(h8_unique, adopt4=="None_banked"))/nrow(h8_unique))
#(none_banked = c(h8_0_10k_none_banked, h8_10_20k_none_banked, h8_20_30k_none_banked, h8_40_60k_none_banked, h8_60_80k_none_banked, h8_80_120k_none_banked, h8_120_180k_none_banked, h8_180_inf_none_banked))# column in Table 1
#
# frac resp None_unbanked by income group
(h8_0_10k_none_unbanked = nrow(subset(h8_0_10k_unique, adopt4=="None_unbanked"))/nrow(h8_0_10k_unique))
(h8_10_20k_none_unbanked = nrow(subset(h8_10_20k_unique, adopt4=="None_unbanked"))/nrow(h8_10_20k_unique))
(h8_20_30k_none_unbanked = nrow(subset(h8_20_30k_unique, adopt4=="None_unbanked"))/nrow(h8_20_30k_unique))
(h8_30_40k_none_unbanked = nrow(subset(h8_30_40k_unique, adopt4=="None_unbanked"))/nrow(h8_30_40k_unique))
(h8_40_60k_none_unbanked = nrow(subset(h8_40_60k_unique, adopt4=="None_unbanked"))/nrow(h8_40_60k_unique))
(h8_60_80k_none_unbanked = nrow(subset(h8_60_80k_unique, adopt4=="None_unbanked"))/nrow(h8_60_80k_unique))
(h8_80_120k_none_unbanked = nrow(subset(h8_80_120k_unique, adopt4=="None_unbanked"))/nrow(h8_80_120k_unique))
(h8_120_180k_none_unbanked = nrow(subset(h8_120_180k_unique, adopt4=="None_unbanked"))/nrow(h8_120_180k_unique))
(h8_180_inf_none_unbanked = nrow(subset(h8_180_inf_unique, adopt4=="None_unbanked"))/nrow(h8_180_inf_unique))
(h8_all_none_unbanked = nrow(subset(h8_unique, adopt4=="None_unbanked"))/nrow(h8_unique))

# frac resp share of no card who are unbanked
(h8_0_10k_none_unbanked_share = h8_0_10k_none_unbanked/(h8_0_10k_none_unbanked + h8_0_10k_none_banked))
(h8_10_20k_none_unbanked_share = h8_10_20k_none_unbanked/(h8_10_20k_none_unbanked + h8_10_20k_none_banked))
(h8_20_30k_none_unbanked_share = h8_20_30k_none_unbanked/(h8_20_30k_none_unbanked + h8_20_30k_none_banked))
(h8_30_40k_none_unbanked_share = h8_30_40k_none_unbanked/(h8_30_40k_none_unbanked + h8_20_30k_none_banked))
(h8_40_60k_none_unbanked_share = h8_40_60k_none_unbanked/(h8_40_60k_none_unbanked + h8_40_60k_none_banked))
(h8_60_80k_none_unbanked_share = h8_60_80k_none_unbanked/(h8_60_80k_none_unbanked + h8_60_80k_none_banked))
(h8_80_120k_none_unbanked_share = h8_80_120k_none_unbanked/(h8_80_120k_none_unbanked + h8_80_120k_none_banked))
(h8_120_180k_none_unbanked_share = h8_120_180k_none_unbanked/(h8_120_180k_none_unbanked + h8_120_180k_none_banked))
(h8_180_inf_none_unbanked_share = h8_180_inf_none_unbanked/(h8_180_inf_none_unbanked + h8_180_inf_none_banked))
(h8_all_none_unbanked_share = h8_all_none_unbanked/(h8_all_none_unbanked + h8_all_none_banked))

# for Table 1 (see after Table 2 ends)
table(h8$adopt5)
(h8_all_yes_dc_no_cc = nrow(subset(h8_unique, adopt5=="Yes_dc_No_cc"))/nrow(h8_unique))
(h8_all_yes_cc_no_dc = nrow(subset(h8_unique, adopt5=="Yes_cc_No_dc"))/nrow(h8_unique))


# total frac resp having No_card and anycard by income group
(h8_0_10k_total = h8_0_10k_any_card + h8_0_10k_no_card)
(h8_10_20k_total = h8_10_20k_any_card + h8_10_20k_no_card)
(h8_20_30k_total = h8_20_30k_any_card + h8_20_30k_no_card)
(h8_20_30k_total = h8_20_30k_any_card + h8_20_30k_no_card)
(h8_30_40k_total = h8_30_40k_any_card + h8_30_40k_no_card)
(h8_40_60k_total = h8_40_60k_any_card + h8_40_60k_no_card)
(h8_60_80k_total = h8_60_80k_any_card + h8_60_80k_no_card)
(h8_80_120k_total = h8_80_120k_any_card + h8_80_120k_no_card)
(h8_120_180k_total = h8_120_180k_any_card + h8_120_180k_no_card)
(h8_180_inf_total = h8_180_inf_any_card + h8_180_inf_no_card)
(h8_all_total = h8_all_any_card + h8_all_no_card)

## weighted percentage respondents in each income group (row in Table 2)
# payments
nrow(h8)# num payments
nrow(h8_unique)# num resp
sum(h8_unique$weight_2_unique, na.rm = T)# verify nrow = num resp
#
(frac_h8_0_10k_w = sum(h8_0_10k_unique$weight_2_unique, na.rm = T)/sum(h8_unique$weight_2_unique, na.rm = T))
(frac_h8_10_20k_w = sum(h8_10_20k_unique$weight_2_unique, na.rm = T)/sum(h8_unique$weight_2_unique, na.rm = T))
(frac_h8_20_30k_w = sum(h8_20_30k_unique$weight_2_unique, na.rm = T)/sum(h8_unique$weight_2_unique, na.rm = T))
(frac_h8_30_40k_w = sum(h8_30_40k_unique$weight_2_unique, na.rm = T)/sum(h8_unique$weight_2_unique, na.rm = T))
(frac_h8_40_60k_w = sum(h8_40_60k_unique$weight_2_unique, na.rm = T)/sum(h8_unique$weight_2_unique, na.rm = T))
(frac_h8_60_80k_w = sum(h8_60_80k_unique$weight_2_unique, na.rm = T)/sum(h8_unique$weight_2_unique, na.rm = T))
(frac_h8_80_120k_w = sum(h8_80_120k_unique$weight_2_unique, na.rm = T)/sum(h8_unique$weight_2_unique, na.rm = T))
(frac_h8_120_180k_w = sum(h8_120_180k_unique$weight_2_unique, na.rm = T)/sum(h8_unique$weight_2_unique, na.rm = T))
(frac_h8_180_inf_w = sum(h8_180_inf_unique$weight_2_unique, na.rm = T)/sum(h8_unique$weight_2_unique, na.rm = T))
(frac_h8_all_w = sum(h8_unique$weight_2_unique, na.rm = T)/sum(h8_unique$weight_2_unique, na.rm = T))
#
#(frac_income_w = c(frac_h8_0_10k_w, frac_h8_10_20k_w, frac_h8_20_30k_w, frac_h8_30_40k_w, frac_h8_40_60k_w, frac_h8_60_80k_w, frac_h8_80_120k_w, frac_h8_120_180k_w, frac_h8_180_inf_w))# column in Table 1

## UNweighted percentage respondents in each income group (row in Table 1) 
(frac_h8_0_10k = nrow(h8_0_10k_unique)/nrow(h8_unique))
(frac_h8_10_20k = nrow(h8_10_20k_unique)/nrow(h8_unique))
(frac_h8_20_30k = nrow(h8_20_30k_unique)/nrow(h8_unique))
(frac_h8_30_40k = nrow(h8_30_40k_unique)/nrow(h8_unique))
(frac_h8_40_60k = nrow(h8_40_60k_unique)/nrow(h8_unique))
(frac_h8_60_80k = nrow(h8_60_80k_unique)/nrow(h8_unique))
(frac_h8_80_120k = nrow(h8_80_120k_unique)/nrow(h8_unique))
(frac_h8_120_180k = nrow(h8_120_180k_unique)/nrow(h8_unique))
(frac_h8_180_inf = nrow(h8_180_inf_unique)/nrow(h8_unique))
(frac_h8_all = nrow(h8_unique)/nrow(h8_unique))
#
## Sample number respondents in each income group (row in Table 2) 
(num_h8_0_10k = nrow(h8_0_10k_unique))
(num_h8_10_20k = nrow(h8_10_20k_unique))
(num_h8_20_30k = nrow(h8_20_30k_unique))
(num_h8_30_40k = nrow(h8_30_40k_unique))
(num_h8_40_60k = nrow(h8_40_60k_unique))
(num_h8_60_80k = nrow(h8_60_80k_unique))
(num_h8_80_120k = nrow(h8_80_120k_unique))
(num_h8_120_180k = nrow(h8_120_180k_unique))
(num_h8_180_inf = nrow(h8_180_inf_unique))
(num_h8_all = nrow(h8_unique))
#

## Finalizing Table 2 (Jessica's paper): Variable column
(variable = c("Any card", "No card", "Total", "No card/banked", "No card/unbanked", "No card/unbanked share", "Num respondents", "Population share"))
length(variable)
# 0-10k column
(income_0_10k = c(100*h8_0_10k_any_card, 100*h8_0_10k_no_card,  100*h8_0_10k_total, 100*h8_0_10k_none_banked, 100*h8_0_10k_none_unbanked, 100*h8_0_10k_none_unbanked_share, num_h8_0_10k, 100*frac_h8_0_10k_w))
# 10_20k column
(income_10_20k = c(100*h8_10_20k_any_card, 100*h8_10_20k_no_card,  100*h8_10_20k_total, 100*h8_10_20k_none_banked, 100*h8_10_20k_none_unbanked, 100*h8_10_20k_none_unbanked_share, num_h8_10_20k, 100*frac_h8_10_20k_w))
# 20_30k column
(income_20_30k = c(100*h8_20_30k_any_card, 100*h8_20_30k_no_card,  100*h8_20_30k_total, 100*h8_20_30k_none_banked, 100*h8_20_30k_none_unbanked, 100*h8_20_30k_none_unbanked_share, num_h8_20_30k, 100*frac_h8_20_30k_w))
# 30_40k column
(income_30_40k = c(100*h8_30_40k_any_card, 100*h8_30_40k_no_card,  100*h8_30_40k_total, 100*h8_30_40k_none_banked, 100*h8_30_40k_none_unbanked, 100*h8_30_40k_none_unbanked_share, num_h8_30_40k, 100*frac_h8_30_40k_w))
# 40_60k column
(income_40_60k = c(100*h8_40_60k_any_card, 100*h8_40_60k_no_card,  100*h8_40_60k_total, 100*h8_40_60k_none_banked, 100*h8_40_60k_none_unbanked, 100*h8_40_60k_none_unbanked_share, num_h8_40_60k, 100*frac_h8_40_60k_w))
# 60_80k column
(income_60_80k = c(100*h8_60_80k_any_card, 100*h8_60_80k_no_card,  100*h8_60_80k_total, 100*h8_60_80k_none_banked, 100*h8_60_80k_none_unbanked, 100*h8_60_80k_none_unbanked_share, num_h8_60_80k, 100*frac_h8_60_80k_w))
# 80_120k column
(income_80_120k = c(100*h8_80_120k_any_card, 100*h8_80_120k_no_card,  100*h8_80_120k_total, 100*h8_80_120k_none_banked, 100*h8_80_120k_none_unbanked, 100*h8_80_120k_none_unbanked_share, num_h8_80_120k, 100*frac_h8_80_120k_w))
# 120_180k column
(income_120_180k = c(100*h8_120_180k_any_card, 100*h8_120_180k_no_card,  100*h8_120_180k_total, 100*h8_120_180k_none_banked, 100*h8_120_180k_none_unbanked, 100*h8_120_180k_none_unbanked_share, num_h8_120_180k, 100*frac_h8_120_180k_w))
# 180_inf column
(income_180_inf = c(100*h8_180_inf_any_card, 100*h8_180_inf_no_card,  100*h8_180_inf_total, 100*h8_180_inf_none_banked, 100*h8_180_inf_none_unbanked, 100*h8_180_inf_none_unbanked_share, num_h8_180_inf, 100*frac_h8_180_inf_w))
# all column
(income_all = c(100*h8_all_any_card, 100*h8_all_no_card,  100*h8_all_total, 100*h8_all_none_banked, 100*h8_all_none_unbanked, 100*h8_all_none_unbanked_share, num_h8_all, 100*frac_h8_all_w))
#

# Constructing Table 2 data frame
(income.df = data.frame(variable, income_0_10k, income_10_20k, income_20_30k, income_30_40k, income_40_60k, income_60_80k, income_80_120k, income_120_180k, income_180_inf, income_all))
(colnames(income.df) =  c("Variable", "0--10k", "10k--20k", "20k--30k", "30k--40k", "40k--60k", "60k--80k", "80k--120k", "120k--180k", "180k+", "All"))
income.df
dim(income.df)
# 
write.csv(income.df, "jess_table_2.csv")

### Start Table 1: Jessica's paper
h8_all_cc_dc# both cards
h8_all_yes_dc_no_cc
h8_all_yes_cc_no_dc
h8_all_no_card

#Table 1 var
(table_1_var = c("Both credit and debit card", "Debit card, no credit card", "Credit card no debit card", "No card", "Total", "Number of respondents"))
(table_1_share = c(100*h8_all_cc_dc, 100*h8_all_yes_dc_no_cc, 100*h8_all_yes_cc_no_dc, 100*h8_all_no_card, 100, length(unique(h8$uasid))))
#
(table_1.df = data.frame("Category" = table_1_var, "Share" = table_1_share))
write.csv(table_1.df, "jess_table_1.csv")

#################
### End of income code ###

