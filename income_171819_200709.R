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
library(rpart)
library(rpart.plot)
library(partykit)# modifies rpart tree plot
#library(ggplot2)
library(spatstat) # for weighted.median
library(mfx)# marginal effects logit regression
#library(regclass) # for confusion_matrix
library(nnet) # for multinomial logit
#library(AER) # for p-values of nnet multinom logit coeftest(regression)
library(mlogit)
#library(mnlogit)# Where I replicate the existing mlogit regressions and experiment with individual effects. 
#library(tibble)
setwd("~/Papers/Accepted/income/income_coding")# NOTE, your working directory is different. Adjust accordingly!!! 
dir()

### Reading RDS datasets (merged data: merged by a separate R-file)
d1 = readRDS("diary171819_200704.rds")
objects()
names(d1)
length(unique(d1$uasid)) # num of unique respondents
dim(d1) # num trans times num variables

### Unlike the cashless paper, (d1, d2,...), here I analyze adoption (and spending) for all respondents (bill payments included) ==> data frame labeled as h1, h2, etc.

### Section 2 in paper (some sample statistics)
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

## adding var with respondent's adoption profile: Both_cards, DC_only, CC_only, None, both_UB. 
h4= h3
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

par(mar = c(2.5,4,1,2))
plot(adopt~HH_income, data = subset(h6_unique, HH_income <= 120000), col = c("magenta", "blue", "green", "red"), ylab = "Respondents' card adoption profile", xlab = "Household income (USD)", breaks = seq(0,120000, 10000), axes = T)
legend("bottomright", c("Both cards", "No credit card", "No debit card", "Neither card"), fill=c("magenta", "blue", "green", "red"))
text(0.04, 0.85, labels = percent(none_perc_10k, digits = 1), cex = 0.8)
text(0.13, 0.9, labels = percent(none_perc_20k, digits = 1), cex = 0.8)
text(0.22, 0.97, labels = percent(none_perc_30k, digits = 1), cex = 0.8)
text(0.33, 0.98, labels = percent(none_perc_40k, digits = 1), cex = 0.8)
text(0.45, 0.99, labels = percent(none_perc_50k, digits = 1), cex = 0.8)
text(0.55, 0.99, labels = percent(none_perc_60k, digits = 1), cex = 0.8)
text(0.65, 0.99, labels = percent(none_perc_70k, digits = 1), cex = 0.8)

# Caption of Fig 1: frac resp with hh income < 120k
length(unique(h6[h6$HH_income <= 120000, ]$uasid))
percent(length(unique(h6[h6$HH_income <= 120000, ]$uasid))/length(unique(h6$uasid)))

(none_perc_median = percent(nrow(h6_unique[h6_unique$HH_income <= 61372 & h6_unique$adopt=="None", ])/nrow(h6_unique[h6_unique$HH_income <= 61372, ])))# none adoption below 2017 HH med income
nrow(h6_unique)
nrow(h6_unique[h6_unique$year==2017, ])
nrow(h6_unique[h6_unique$year==2018, ])# low b/c it includes only new resp
nrow(h6_unique[h6_unique$year==2019, ])# low b/c it includes only new resp
nrow(h6_unique[h6_unique$year==2017, ])+nrow(h6_unique[h6_unique$year==2018, ])+nrow(h6_unique[h6_unique$year==2019, ])# verify = sum of unique resp
# End of Fig 1 

### Start Table 1: Adoption vs. income
# define adopt2 which separates banked from unbaned
h7 = h6
# below, split (may be used for table)
h7$adopt2=NA
h7$adopt2[h7$dc_adopt==1 & h7$cc_adopt==1] = "Both_cards"
h7$adopt2[h7$cc_adopt==0 & h7$pi != "credit"] = "No_cc"
h7$adopt2[h7$dc_adopt==0 & h7$pi != "debit"] = "No_dc"
h7$adopt2[h7$dc_adopt==0 & h7$cc_adopt==0 & h7$bnk_acnt_adopt==1 & h7$pi != "credit" & h7$pi != "debit"] = "None_banked"
h7$adopt2[h7$dc_adopt==0 & h7$cc_adopt==0 & h7$bnk_acnt_adopt==0 & h7$pi != "credit" & h7$pi != "debit"] = "None_unbanked"
table(h7$adopt)
table(h7$adopt2)# 

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
## the above subsets restricted to unique respondents
# h8_0_10k_unique = h8_0_10k[!duplicated(h8_0_10k$uasid), ] 
# h8_10_20k_unique = h8_10_20k[!duplicated(h8_10_20k$uasid), ] 
# h8_20_30k_unique = h8_20_30k[!duplicated(h8_20_30k$uasid), ] 
# h8_30_40k_unique = h8_30_40k[!duplicated(h8_30_40k$uasid), ] 
# h8_40_60k_unique = h8_40_60k[!duplicated(h8_40_60k$uasid), ] 
# h8_60_80k_unique = h8_60_80k[!duplicated(h8_60_80k$uasid), ] 
# h8_80_120k_unique = h8_80_120k[!duplicated(h8_80_120k$uasid), ] 
# h8_120_180k_unique = h8_120_180k[!duplicated(h8_120_180k$uasid), ] 
# h8_180_inf_unique = h8_180_inf[!duplicated(h8_180_inf$uasid), ] 

table(h8$adopt)
table(h8$adopt2)
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
# frac resp None_banked by income group
(h8_0_10k_none_banked = nrow(subset(h8_0_10k_unique, adopt2=="None_banked"))/nrow(h8_0_10k_unique))
(h8_10_20k_none_banked = nrow(subset(h8_10_20k_unique, adopt2=="None_banked"))/nrow(h8_10_20k_unique))
(h8_20_30k_none_banked = nrow(subset(h8_20_30k_unique, adopt2=="None_banked"))/nrow(h8_20_30k_unique))
(h8_30_40k_none_banked = nrow(subset(h8_30_40k_unique, adopt2=="None_banked"))/nrow(h8_30_40k_unique))
(h8_40_60k_none_banked = nrow(subset(h8_40_60k_unique, adopt2=="None_banked"))/nrow(h8_40_60k_unique))
(h8_60_80k_none_banked = nrow(subset(h8_60_80k_unique, adopt2=="None_banked"))/nrow(h8_60_80k_unique))
(h8_80_120k_none_banked = nrow(subset(h8_80_120k_unique, adopt2=="None_banked"))/nrow(h8_80_120k_unique))
(h8_120_180k_none_banked = nrow(subset(h8_120_180k_unique, adopt2=="None_banked"))/nrow(h8_120_180k_unique))
(h8_180_inf_none_banked = nrow(subset(h8_180_inf_unique, adopt2=="None_banked"))/nrow(h8_180_inf_unique))
(h8_all_none_banked = nrow(subset(h8_unique, adopt2=="None_banked"))/nrow(h8_unique))
#(none_banked = c(h8_0_10k_none_banked, h8_10_20k_none_banked, h8_20_30k_none_banked, h8_40_60k_none_banked, h8_60_80k_none_banked, h8_80_120k_none_banked, h8_120_180k_none_banked, h8_180_inf_none_banked))# column in Table 1
#
# frac resp None_unbanked by income group
(h8_0_10k_none_unbanked = nrow(subset(h8_0_10k_unique, adopt2=="None_unbanked"))/nrow(h8_0_10k_unique))
(h8_10_20k_none_unbanked = nrow(subset(h8_10_20k_unique, adopt2=="None_unbanked"))/nrow(h8_10_20k_unique))
(h8_20_30k_none_unbanked = nrow(subset(h8_20_30k_unique, adopt2=="None_unbanked"))/nrow(h8_20_30k_unique))
(h8_30_40k_none_unbanked = nrow(subset(h8_30_40k_unique, adopt2=="None_unbanked"))/nrow(h8_30_40k_unique))
(h8_40_60k_none_unbanked = nrow(subset(h8_40_60k_unique, adopt2=="None_unbanked"))/nrow(h8_40_60k_unique))
(h8_60_80k_none_unbanked = nrow(subset(h8_60_80k_unique, adopt2=="None_unbanked"))/nrow(h8_60_80k_unique))
(h8_80_120k_none_unbanked = nrow(subset(h8_80_120k_unique, adopt2=="None_unbanked"))/nrow(h8_80_120k_unique))
(h8_120_180k_none_unbanked = nrow(subset(h8_120_180k_unique, adopt2=="None_unbanked"))/nrow(h8_120_180k_unique))
(h8_180_inf_none_unbanked = nrow(subset(h8_180_inf_unique, adopt2=="None_unbanked"))/nrow(h8_180_inf_unique))
(h8_all_none_unbanked = nrow(subset(h8_unique, adopt2=="None_unbanked"))/nrow(h8_unique))

#(none_unbanked = c(h8_0_10k_none_unbanked, h8_10_20k_none_unbanked, h8_20_30k_none_unbanked, h8_40_60k_none_unbanked, h8_60_80k_none_unbanked, h8_80_120k_none_unbanked, h8_120_180k_none_unbanked, h8_180_inf_none_unbanked))# column in Table 1

## weighted percentage respondents in each income group (row in Table 1)
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
## Sample number respondents in each income group (row in Table 1) 
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

## Monthly avg dollar non-bill expenditure per respondent
table(h8$bill)
table(h8$year)
length(unique(h8[h8$year==2017,]$uasid))# num resp in 2017
length(unique(h8[h8$year==2018,]$uasid))# num resp in 2018
length(unique(h8[h8$year==2019,]$uasid))# num resp in 2018
h8_2017 = subset(h8, year==2017)# 2017 payments
h8_2018 = subset(h8, year==2018)# 2018 payments
h8_2019 = subset(h8, year==2019)# 2019 payments
#
# 2017 payments within this income group
h8_2017_0_10k = subset(h8_2017, uasid %in% h8_0_10k_unique$uasid)
h8_2017_10_20k = subset(h8_2017, uasid %in% h8_10_20k_unique$uasid)
h8_2017_20_30k = subset(h8_2017, uasid %in% h8_20_30k_unique$uasid)
h8_2017_30_40k = subset(h8_2017, uasid %in% h8_30_40k_unique$uasid)
h8_2017_40_60k = subset(h8_2017, uasid %in% h8_40_60k_unique$uasid)
h8_2017_60_80k = subset(h8_2017, uasid %in% h8_60_80k_unique$uasid)
h8_2017_80_120k = subset(h8_2017, uasid %in% h8_80_120k_unique$uasid)
h8_2017_120_180k = subset(h8_2017, uasid %in% h8_120_180k_unique$uasid)
h8_2017_180_inf = subset(h8_2017, uasid %in% h8_180_inf_unique$uasid)
h8_2017_all = subset(h8_2017, uasid %in% h8_unique$uasid)
# 2018 payments within this income group
h8_2018_0_10k = subset(h8_2018, uasid %in% h8_0_10k_unique$uasid)
h8_2018_10_20k = subset(h8_2018, uasid %in% h8_10_20k_unique$uasid)
h8_2018_20_30k = subset(h8_2018, uasid %in% h8_20_30k_unique$uasid)
h8_2018_30_40k = subset(h8_2018, uasid %in% h8_30_40k_unique$uasid)
h8_2018_40_60k = subset(h8_2018, uasid %in% h8_40_60k_unique$uasid)
h8_2018_60_80k = subset(h8_2018, uasid %in% h8_60_80k_unique$uasid)
h8_2018_80_120k = subset(h8_2018, uasid %in% h8_80_120k_unique$uasid)
h8_2018_120_180k = subset(h8_2018, uasid %in% h8_120_180k_unique$uasid)
h8_2018_180_inf = subset(h8_2018, uasid %in% h8_180_inf_unique$uasid)
h8_2018_all = subset(h8_2018, uasid %in% h8_unique$uasid)
# 2019 payments within this income group
h8_2019_0_10k = subset(h8_2019, uasid %in% h8_0_10k_unique$uasid)
h8_2019_10_20k = subset(h8_2019, uasid %in% h8_10_20k_unique$uasid)
h8_2019_20_30k = subset(h8_2019, uasid %in% h8_20_30k_unique$uasid)
h8_2019_30_40k = subset(h8_2019, uasid %in% h8_30_40k_unique$uasid)
h8_2019_40_60k = subset(h8_2019, uasid %in% h8_40_60k_unique$uasid)
h8_2019_60_80k = subset(h8_2019, uasid %in% h8_60_80k_unique$uasid)
h8_2019_80_120k = subset(h8_2019, uasid %in% h8_80_120k_unique$uasid)
h8_2019_120_180k = subset(h8_2019, uasid %in% h8_120_180k_unique$uasid)
h8_2019_180_inf = subset(h8_2019, uasid %in% h8_180_inf_unique$uasid)
h8_2019_all = subset(h8_2019, uasid %in% h8_unique$uasid)

# 2017 monthly avg num payments per responding by income group
(h8_2017_0_10k_num = (31/3)*nrow(h8_2017_0_10k)/length(unique(h8_2017_0_10k$uasid)))
(h8_2017_10_20k_num = (31/3)*nrow(h8_2017_10_20k)/length(unique(h8_2017_10_20k$uasid)))
(h8_2017_20_30k_num = (31/3)*nrow(h8_2017_20_30k)/length(unique(h8_2017_20_30k$uasid)))
(h8_2017_30_40k_num = (31/3)*nrow(h8_2017_30_40k)/length(unique(h8_2017_30_40k$uasid)))
(h8_2017_40_60k_num = (31/3)*nrow(h8_2017_40_60k)/length(unique(h8_2017_40_60k$uasid)))
(h8_2017_60_80k_num = (31/3)*nrow(h8_2017_60_80k)/length(unique(h8_2017_60_80k$uasid)))
(h8_2017_80_120k_num = (31/3)*nrow(h8_2017_80_120k)/length(unique(h8_2017_80_120k$uasid)))
(h8_2017_120_180k_num = (31/3)*nrow(h8_2017_120_180k)/length(unique(h8_2017_120_180k$uasid)))
(h8_2017_180_inf_num = (31/3)*nrow(h8_2017_180_inf)/length(unique(h8_2017_180_inf$uasid)))
(h8_2017_all_num = (31/3)*nrow(h8_2017)/length(unique(h8_2017$uasid)))
#
# 2018 monthly avg num payments per responding by income group
(h8_2018_0_10k_num = (31/3)*nrow(h8_2018_0_10k)/length(unique(h8_2018_0_10k$uasid)))
(h8_2018_10_20k_num = (31/3)*nrow(h8_2018_10_20k)/length(unique(h8_2018_10_20k$uasid)))
(h8_2018_20_30k_num = (31/3)*nrow(h8_2018_20_30k)/length(unique(h8_2018_20_30k$uasid)))
(h8_2018_30_40k_num = (31/3)*nrow(h8_2018_30_40k)/length(unique(h8_2018_30_40k$uasid)))
(h8_2018_40_60k_num = (31/3)*nrow(h8_2018_40_60k)/length(unique(h8_2018_40_60k$uasid)))
(h8_2018_60_80k_num = (31/3)*nrow(h8_2018_60_80k)/length(unique(h8_2018_60_80k$uasid)))
(h8_2018_80_120k_num = (31/3)*nrow(h8_2018_80_120k)/length(unique(h8_2018_80_120k$uasid)))
(h8_2018_120_180k_num = (31/3)*nrow(h8_2018_120_180k)/length(unique(h8_2018_120_180k$uasid)))
(h8_2018_180_inf_num = (31/3)*nrow(h8_2018_180_inf)/length(unique(h8_2018_180_inf$uasid)))
(h8_2018_all_num = (31/3)*nrow(h8_2018)/length(unique(h8_2018$uasid)))
#
# 2019 monthly avg num payments per responding by income group
(h8_2019_0_10k_num = (31/3)*nrow(h8_2019_0_10k)/length(unique(h8_2019_0_10k$uasid)))
(h8_2019_10_20k_num = (31/3)*nrow(h8_2019_10_20k)/length(unique(h8_2019_10_20k$uasid)))
(h8_2019_20_30k_num = (31/3)*nrow(h8_2019_20_30k)/length(unique(h8_2019_20_30k$uasid)))
(h8_2019_30_40k_num = (31/3)*nrow(h8_2019_30_40k)/length(unique(h8_2019_30_40k$uasid)))
(h8_2019_40_60k_num = (31/3)*nrow(h8_2019_40_60k)/length(unique(h8_2019_40_60k$uasid)))
(h8_2019_60_80k_num = (31/3)*nrow(h8_2019_60_80k)/length(unique(h8_2019_60_80k$uasid)))
(h8_2019_80_120k_num = (31/3)*nrow(h8_2019_80_120k)/length(unique(h8_2019_80_120k$uasid)))
(h8_2019_120_180k_num = (31/3)*nrow(h8_2019_120_180k)/length(unique(h8_2019_120_180k$uasid)))
(h8_2019_180_inf_num = (31/3)*nrow(h8_2019_180_inf)/length(unique(h8_2019_180_inf$uasid)))
(h8_2019_all_num = (31/3)*nrow(h8_2019)/length(unique(h8_2018$uasid)))

# 2017-2018-2019 avg monthly avg num payments per resp (averaging 2017-18)
(h8_0_10k_num = (h8_2017_0_10k_num + h8_2018_0_10k_num + h8_2019_0_10k_num)/3)
(h8_10_20k_num = (h8_2017_10_20k_num + h8_2018_10_20k_num + h8_2019_10_20k_num)/3)
(h8_20_30k_num = (h8_2017_20_30k_num + h8_2018_20_30k_num + h8_2019_20_30k_num)/3)
(h8_30_40k_num = (h8_2017_30_40k_num + h8_2018_30_40k_num + h8_2019_30_40k_num)/3)
(h8_40_60k_num = (h8_2017_40_60k_num + h8_2018_40_60k_num + h8_2019_40_60k_num)/3)
(h8_60_80k_num = (h8_2017_60_80k_num + h8_2018_60_80k_num + h8_2019_60_80k_num)/3)
(h8_80_120k_num = (h8_2017_80_120k_num + h8_2018_80_120k_num + h8_2019_80_120k_num)/3)
(h8_120_180k_num = (h8_2017_120_180k_num + h8_2018_120_180k_num + h8_2019_120_180k_num)/3)
(h8_180_inf_num = (h8_2017_180_inf_num + h8_2018_180_inf_num + h8_2019_180_inf_num)/3)
(h8_all_num = (h8_2017_all_num + h8_2018_all_num + h8_2019_all_num)/3)

# 2017 monthly avg spending per responding by income group
(h8_2017_0_10k_amnt = (31/3)*sum(h8_2017_0_10k$amnt, na.rm = T)/length(unique(h8_2017_0_10k$uasid)))
(h8_2017_10_20k_amnt = (31/3)*sum(h8_2017_10_20k$amnt, na.rm = T)/length(unique(h8_2017_10_20k$uasid)))
(h8_2017_20_30k_amnt = (31/3)*sum(h8_2017_20_30k$amnt, na.rm = T)/length(unique(h8_2017_20_30k$uasid)))
(h8_2017_30_40k_amnt = (31/3)*sum(h8_2017_30_40k$amnt, na.rm = T)/length(unique(h8_2017_30_40k$uasid)))
(h8_2017_40_60k_amnt = (31/3)*sum(h8_2017_40_60k$amnt, na.rm = T)/length(unique(h8_2017_40_60k$uasid)))
(h8_2017_60_80k_amnt = (31/3)*sum(h8_2017_60_80k$amnt, na.rm = T)/length(unique(h8_2017_60_80k$uasid)))
(h8_2017_80_120k_amnt = (31/3)*sum(h8_2017_80_120k$amnt, na.rm = T)/length(unique(h8_2017_80_120k$uasid)))
(h8_2017_120_180k_amnt = (31/3)*sum(h8_2017_120_180k$amnt, na.rm = T)/length(unique(h8_2017_120_180k$uasid)))
(h8_2017_180_inf_amnt = (31/3)*sum(h8_2017_180_inf$amnt, na.rm = T)/length(unique(h8_2017_180_inf$uasid)))
(h8_2017_all_amnt = (31/3)*sum(h8_2017$amnt, na.rm = T)/length(unique(h8_2017$uasid)))
# 2018 monthly avg spending per responding by income group
(h8_2018_0_10k_amnt = (31/3)*sum(h8_2018_0_10k$amnt, na.rm = T)/length(unique(h8_2018_0_10k$uasid)))
(h8_2018_10_20k_amnt = (31/3)*sum(h8_2018_10_20k$amnt, na.rm = T)/length(unique(h8_2018_10_20k$uasid)))
(h8_2018_20_30k_amnt = (31/3)*sum(h8_2018_20_30k$amnt, na.rm = T)/length(unique(h8_2018_20_30k$uasid)))
(h8_2018_30_40k_amnt = (31/3)*sum(h8_2018_30_40k$amnt, na.rm = T)/length(unique(h8_2018_30_40k$uasid)))
(h8_2018_40_60k_amnt = (31/3)*sum(h8_2018_40_60k$amnt, na.rm = T)/length(unique(h8_2018_40_60k$uasid)))
(h8_2018_60_80k_amnt = (31/3)*sum(h8_2018_60_80k$amnt, na.rm = T)/length(unique(h8_2018_60_80k$uasid)))
(h8_2018_80_120k_amnt = (31/3)*sum(h8_2018_80_120k$amnt, na.rm = T)/length(unique(h8_2018_80_120k$uasid)))
(h8_2018_120_180k_amnt = (31/3)*sum(h8_2018_120_180k$amnt, na.rm = T)/length(unique(h8_2018_120_180k$uasid)))
(h8_2018_180_inf_amnt = (31/3)*sum(h8_2018_180_inf$amnt, na.rm = T)/length(unique(h8_2018_180_inf$uasid)))
(h8_2018_all_amnt = (31/3)*sum(h8_2018$amnt, na.rm = T)/length(unique(h8_2018$uasid)))
# 2019 monthly avg spending per responding by income group
(h8_2019_0_10k_amnt = (31/3)*sum(h8_2019_0_10k$amnt, na.rm = T)/length(unique(h8_2019_0_10k$uasid)))
(h8_2019_10_20k_amnt = (31/3)*sum(h8_2019_10_20k$amnt, na.rm = T)/length(unique(h8_2019_10_20k$uasid)))
(h8_2019_20_30k_amnt = (31/3)*sum(h8_2019_20_30k$amnt, na.rm = T)/length(unique(h8_2019_20_30k$uasid)))
(h8_2019_30_40k_amnt = (31/3)*sum(h8_2019_30_40k$amnt, na.rm = T)/length(unique(h8_2019_30_40k$uasid)))
(h8_2019_40_60k_amnt = (31/3)*sum(h8_2019_40_60k$amnt, na.rm = T)/length(unique(h8_2019_40_60k$uasid)))
(h8_2019_60_80k_amnt = (31/3)*sum(h8_2019_60_80k$amnt, na.rm = T)/length(unique(h8_2019_60_80k$uasid)))
(h8_2019_80_120k_amnt = (31/3)*sum(h8_2019_80_120k$amnt, na.rm = T)/length(unique(h8_2019_80_120k$uasid)))
(h8_2019_120_180k_amnt = (31/3)*sum(h8_2019_120_180k$amnt, na.rm = T)/length(unique(h8_2019_120_180k$uasid)))
(h8_2019_180_inf_amnt = (31/3)*sum(h8_2019_180_inf$amnt, na.rm = T)/length(unique(h8_2019_180_inf$uasid)))
(h8_2019_all_amnt = (31/3)*sum(h8_2019$amnt, na.rm = T)/length(unique(h8_2019$uasid)))
#
# 2017-2018-2019 avg monthly avg num payments per resp (averaging 2017-18)
(h8_0_10k_amnt = (h8_2017_0_10k_amnt + h8_2018_0_10k_amnt + h8_2019_0_10k_amnt)/3)
(h8_10_20k_amnt = (h8_2017_10_20k_amnt + h8_2018_10_20k_amnt + h8_2019_10_20k_amnt)/3)
(h8_20_30k_amnt = (h8_2017_20_30k_amnt + h8_2018_20_30k_amnt + h8_2019_20_30k_amnt)/3)
(h8_30_40k_amnt = (h8_2017_30_40k_amnt + h8_2018_30_40k_amnt + h8_2019_30_40k_amnt)/3)
(h8_40_60k_amnt = (h8_2017_40_60k_amnt + h8_2018_40_60k_amnt + h8_2019_40_60k_amnt)/3)
(h8_60_80k_amnt = (h8_2017_60_80k_amnt + h8_2018_60_80k_amnt + h8_2019_60_80k_amnt)/3)
(h8_80_120k_amnt = (h8_2017_80_120k_amnt + h8_2018_80_120k_amnt + h8_2019_80_120k_amnt)/3)
(h8_120_180k_amnt = (h8_2017_120_180k_amnt + h8_2018_120_180k_amnt + h8_2019_120_180k_amnt)/3)
(h8_180_inf_amnt = (h8_2017_180_inf_amnt + h8_2018_180_inf_amnt + h8_2019_180_inf_amnt)/3)
(h8_all_amnt = (h8_2017_all_amnt + h8_2018_all_amnt + h8_2019_all_amnt)/3)

## Finalizing Table 1: Variable column
(variable = c("Both cards (%)", "No credit (%)", "No debit (%)", "None banked (%)", "None unbanked (%)", "Num respondents", "Percentage (%)", "Percentage (%)(w)", "Payments/month", "Spending/month ($)"))
length(variable)
# 0-10k column
(income_0_10k = c(100*h8_0_10k_cc_dc, 100*h8_0_10k_no_cc, 100*h8_0_10k_no_dc, 100*h8_0_10k_none_banked, 100*h8_0_10k_none_unbanked, num_h8_0_10k,100*frac_h8_0_10k,100*frac_h8_0_10k_w, h8_0_10k_num, h8_0_10k_amnt))
(income_10_20k = c(100*h8_10_20k_cc_dc, 100*h8_10_20k_no_cc, 100*h8_10_20k_no_dc, 100*h8_10_20k_none_banked, 100*h8_10_20k_none_unbanked, num_h8_10_20k, 100*frac_h8_10_20k, 100*frac_h8_10_20k_w, h8_10_20k_num, h8_10_20k_amnt))
(income_20_30k = c(100*h8_20_30k_cc_dc, 100*h8_20_30k_no_cc, 100*h8_20_30k_no_dc, 100*h8_20_30k_none_banked, 100*h8_20_30k_none_unbanked, num_h8_20_30k, 100*frac_h8_20_30k, 100*frac_h8_20_30k_w, h8_20_30k_num, h8_20_30k_amnt))
(income_30_40k = c(100*h8_30_40k_cc_dc, 100*h8_30_40k_no_cc, 100*h8_30_40k_no_dc, 100*h8_30_40k_none_banked, 100*h8_30_40k_none_unbanked, num_h8_30_40k, 100*frac_h8_30_40k, 100*frac_h8_30_40k_w, h8_30_40k_num, h8_30_40k_amnt))
(income_40_60k = c(100*h8_40_60k_cc_dc, 100*h8_40_60k_no_cc, 100*h8_40_60k_no_dc, 100*h8_40_60k_none_banked, 100*h8_40_60k_none_unbanked, num_h8_40_60k, 100*frac_h8_40_60k, 100*frac_h8_40_60k_w, h8_40_60k_num, h8_40_60k_amnt))
(income_60_80k = c(100*h8_60_80k_cc_dc, 100*h8_60_80k_no_cc, 100*h8_60_80k_no_dc, 100*h8_60_80k_none_banked, 100*h8_60_80k_none_unbanked, num_h8_60_80k, 100*frac_h8_60_80k, 100*frac_h8_60_80k_w, h8_60_80k_num, h8_60_80k_amnt))
(income_80_120k = c(100*h8_80_120k_cc_dc, 100*h8_80_120k_no_cc, 100*h8_80_120k_no_dc, 100*h8_80_120k_none_banked, 100*h8_80_120k_none_unbanked, num_h8_80_120k, 100*frac_h8_80_120k, 100*frac_h8_80_120k_w, h8_80_120k_num, h8_80_120k_amnt))
(income_120_180k = c(100*h8_120_180k_cc_dc, 100*h8_120_180k_no_cc, 100*h8_120_180k_no_dc, 100*h8_120_180k_none_banked, 100*h8_120_180k_none_unbanked, num_h8_120_180k, 100*frac_h8_120_180k, 100*frac_h8_120_180k_w, h8_120_180k_num, h8_120_180k_amnt))
(income_180_inf = c(100*h8_180_inf_cc_dc, 100*h8_180_inf_no_cc, 100*h8_180_inf_no_dc, 100*h8_180_inf_none_banked, 100*h8_180_inf_none_unbanked, num_h8_180_inf, 100*frac_h8_180_inf, 100*frac_h8_180_inf_w, h8_180_inf_num, h8_180_inf_amnt))
(income_all = c(100*h8_all_cc_dc, 100*h8_all_no_cc, 100*h8_all_no_dc, 100*h8_all_none_banked, 100*h8_all_none_unbanked, num_h8_all, 100*frac_h8_all, 100*frac_h8_all_w, h8_all_num, h8_all_amnt))

## Constructing Table 1 data frame
# (income.df = data.frame(variable, income_0_10k, income_10_20k, income_20_30k, income_30_40k, income_40_60k, income_60_80k, income_80_120k, income_120_180k, income_180_inf, income_all))
# (colnames(income.df) =  c("Variable", "0--10k", "10k--20k", "20k--30k", "30k--40k", "40k--60k", "60k--80k", "80k--120k", "120k--180k", "180k+", "All"))
# income.df
# dim(income.df)
# 
# (digitm = matrix(c(rep(1,11+1), rep(1,11+1), rep(1,11+1), rep(1,11+1), rep(1,11+1) , rep(0,11+1) , rep(1,11+1) , rep(1,11+1), rep(1,11+1), rep(0,11+1)), nrow = 10, ncol = 11+1, byrow = T))

# income_200207.R displaying only rows 1:8 (not reporting monthly vol and val)
(income.df = data.frame(variable[1:8], income_0_10k[1:8], income_10_20k[1:8], income_20_30k[1:8], income_30_40k[1:8], income_40_60k[1:8], income_60_80k[1:8], income_80_120k[1:8], income_120_180k[1:8], income_180_inf[1:8], income_all[1:8]))
(colnames(income.df) =  c("Variable", "0--10k", "10k--20k", "20k--30k", "30k--40k", "40k--60k", "60k--80k", "80k--120k", "120k--180k", "180k+", "All"))
income.df
dim(income.df)

(digitm = matrix(c(rep(1,11+1), rep(1,11+1), rep(1,11+1), rep(1,11+1), rep(1,11+1) , rep(0,11+1) , rep(1,11+1) , rep(1,11+1)), nrow = 8, ncol = 11+1, byrow = T))

print(xtable(income.df, digits = digitm), include.rownames = F, hline.after = c(0,5,8))# Table 1

# testing whether frac rows sum up to 100%
sum(c(frac_h8_0_10k_w, frac_h8_10_20k_w, frac_h8_20_30k_w, frac_h8_30_40k_w, frac_h8_40_60k_w, frac_h8_60_80k_w, frac_h8_80_120k_w, frac_h8_120_180k_w, frac_h8_180_inf_w))# 
sum(c(frac_h8_0_10k, frac_h8_10_20k, frac_h8_20_30k, frac_h8_30_40k, frac_h8_40_60k, frac_h8_60_80k, frac_h8_80_120k, frac_h8_120_180k, frac_h8_180_inf))

# For the conclusion in the paper
(conclusion_0_20k_frac =  (nrow(subset(h8_0_10k_unique, adopt2=="None_unbanked")) + nrow(subset(h8_10_20k_unique, adopt2=="None_unbanked")))/(nrow(h8_0_10k_unique)+ nrow(h8_10_20k_unique))) # frac of unbanked in this income group
#
(conclusion_0_30k_frac =  (nrow(subset(h8_0_10k_unique, adopt2=="None_unbanked")) + nrow(subset(h8_10_20k_unique, adopt2=="None_unbanked")) + nrow(subset(h8_20_30k_unique, adopt2=="None_unbanked")))/(nrow(h8_0_10k_unique)+ nrow(h8_10_20k_unique) + nrow(h8_20_30k_unique))) # frac of unbanked in this income group
#
(conclusion_0_40k_frac =  (nrow(subset(h8_0_10k_unique, adopt2=="None_unbanked")) + nrow(subset(h8_10_20k_unique, adopt2=="None_unbanked")) + nrow(subset(h8_20_30k_unique, adopt2=="None_unbanked")) + nrow(subset(h8_30_40k_unique, adopt2=="None_unbanked")))/(nrow(h8_0_10k_unique)+ nrow(h8_10_20k_unique) + nrow(h8_20_30k_unique) + nrow(h8_30_40k_unique))) # frac of unbanked in this income group

# caption of Table 1
length(unique(h8$uasid))

### Start Table 2: three assessments: cost, security, and convenience
h9 = h8
names(h9)
# Reversing reverse) COST rating: 
# Before the change 1=highest cost 5=lowest cost (highest rating)
# After the change 1=lowest cost 5=highest cost (lowest rating). Now consistent with Figure 4
h9$assess_cost_cash = 6 - h8$assess_cost_cash
summary(h9$assess_cost_cash)
h9$assess_cost_check = 6 - h8$assess_cost_check
summary(h9$assess_cost_check)
h9$assess_cost_debit = 6 - h8$assess_cost_debit
summary(h9$assess_cost_debit)
h9$assess_cost_credit = 6 - h8$assess_cost_credit
summary(h9$assess_cost_credit)
h9$assess_cost_prepaid = 6 - h8$assess_cost_prepaid
summary(h9$assess_cost_prepaid)
#
table(h9$assess_cost_cash)# rank dist
table(h9$assess_cost_check)
table(h9$assess_cost_debit)
table(h9$assess_cost_credit)
table(h9$assess_cost_prepaid)

# population weighted medians and avg of assessments
# Compute weighted median assessments for all
# scale weights first
length(unique(h9$uasid))# num resp
h9_resp = h9[!duplicated(h9$uasid), ]# Remove duplicate payments by the same resp
dim(h9_resp)
# construct weight_3 (weights by respondent)
names(dplyr::select(h9_resp, starts_with("w")))
nrow(h9_resp)# num resp
sum(h9_resp$weight_171819_1, na.rm = T)
h9_resp$weight_3 = nrow(h9_resp)*h9_resp$weight_171819_1/sum(h9_resp$weight_171819_1, na.rm = T)
sum(h9_resp$weight_3, na.rm = T)# new rescaled weights

names(dplyr::select(h9_resp, starts_with("assess")))
#
(assess_convenience_cash_med = weighted.median(h9_resp$assess_convenience_cash, h9_resp$weight_3, na.rm = T))
(assess_convenience_prepaid_med = weighted.median(h9_resp$assess_convenience_prepaid, h9_resp$weight_3, na.rm = T))
(assess_convenience_debit_med = weighted.median(h9_resp$assess_convenience_debit, h9_resp$weight_3, na.rm = T))
(assess_convenience_credit_med = weighted.median(h9_resp$assess_convenience_credit, h9_resp$weight_3, na.rm = T))

## start assessment weighted median
(assess_cost_cash_med = weighted.median(h9_resp$assess_cost_cash, h9_resp$weight_3, na.rm = T))
(assess_cost_check_med = weighted.median(h9_resp$assess_cost_check, h9_resp$weight_3, na.rm = T))
(assess_cost_credit_med = weighted.median(h9_resp$assess_cost_credit, h9_resp$weight_3, na.rm = T))
(assess_cost_debit_med = weighted.median(h9_resp$assess_cost_debit, h9_resp$weight_3, na.rm = T))
(assess_cost_prepaid_med = weighted.median(h9_resp$assess_cost_prepaid, h9_resp$weight_3, na.rm = T))
#
(assess_security_cash_med = weighted.median(h9_resp$assess_security_cash, h9_resp$weight_3, na.rm = T))
(assess_security_check_med = weighted.median(h9_resp$assess_security_check, h9_resp$weight_3, na.rm = T))
(assess_security_credit_med = weighted.median(h9_resp$assess_security_credit, h9_resp$weight_3, na.rm = T))
(assess_security_debit_med = weighted.median(h9_resp$assess_security_debit, h9_resp$weight_3, na.rm = T))
(assess_security_prepaid_med = weighted.median(h9_resp$assess_security_prepaid, h9_resp$weight_3, na.rm = T))
#
(assess_convenience_cash_med = weighted.median(h9_resp$assess_convenience_cash, h9_resp$weight_3, na.rm = T))
(assess_convenience_check_med = weighted.median(h9_resp$assess_convenience_check, h9_resp$weight_3, na.rm = T))
(assess_convenience_credit_med = weighted.median(h9_resp$assess_convenience_credit, h9_resp$weight_3, na.rm = T))
(assess_convenience_debit_med = weighted.median(h9_resp$assess_convenience_debit, h9_resp$weight_3, na.rm = T))
(assess_convenience_prepaid_med = weighted.median(h9_resp$assess_convenience_prepaid, h9_resp$weight_3, na.rm = T))

#
(assess_records_cash_med = weighted.median(h9_resp$assess_records_cash, h9_resp$weight_3, na.rm = T))
(assess_records_check_med = weighted.median(h9_resp$assess_records_check, h9_resp$weight_3, na.rm = T))
(assess_records_credit_med = weighted.median(h9_resp$assess_records_credit, h9_resp$weight_3, na.rm = T))
(assess_records_debit_med = weighted.median(h9_resp$assess_records_debit, h9_resp$weight_3, na.rm = T))
(assess_records_prepaid_med = weighted.median(h9_resp$assess_records_prepaid, h9_resp$weight_3, na.rm = T))

## start assessment weighted average
(assess_cost_cash_avg = weighted.mean(h9_resp$assess_cost_cash, h9_resp$weight_3, na.rm = T))
(assess_cost_check_avg = weighted.mean(h9_resp$assess_cost_check, h9_resp$weight_3, na.rm = T))
(assess_cost_credit_avg = weighted.mean(h9_resp$assess_cost_credit, h9_resp$weight_3, na.rm = T))
(assess_cost_debit_avg = weighted.mean(h9_resp$assess_cost_debit, h9_resp$weight_3, na.rm = T))
(assess_cost_prepaid_avg = weighted.mean(h9_resp$assess_cost_prepaid, h9_resp$weight_3, na.rm = T))
#
(assess_security_cash_avg = weighted.mean(h9_resp$assess_security_cash, h9_resp$weight_3, na.rm = T))
(assess_security_check_avg = weighted.mean(h9_resp$assess_security_check, h9_resp$weight_3, na.rm = T))
(assess_security_credit_avg = weighted.mean(h9_resp$assess_security_credit, h9_resp$weight_3, na.rm = T))
(assess_security_debit_avg = weighted.mean(h9_resp$assess_security_debit, h9_resp$weight_3, na.rm = T))
(assess_security_prepaid_avg = weighted.mean(h9_resp$assess_security_prepaid, h9_resp$weight_3, na.rm = T))

#
(assess_convenience_cash_avg = weighted.mean(h9_resp$assess_convenience_cash, h9_resp$weight_3, na.rm = T))
(assess_convenience_check_avg = weighted.mean(h9_resp$assess_convenience_check, h9_resp$weight_3, na.rm = T))
(assess_convenience_credit_avg = weighted.mean(h9_resp$assess_convenience_credit, h9_resp$weight_3, na.rm = T))
(assess_convenience_debit_avg = weighted.mean(h9_resp$assess_convenience_debit, h9_resp$weight_3, na.rm = T))
(assess_convenience_prepaid_avg = weighted.mean(h9_resp$assess_convenience_prepaid, h9_resp$weight_3, na.rm = T))
#
(assess_records_cash_avg = weighted.mean(h9_resp$assess_records_cash, h9_resp$weight_3, na.rm = T))
(assess_records_check_avg = weighted.mean(h9_resp$assess_records_check, h9_resp$weight_3, na.rm = T))
(assess_records_credit_avg = weighted.mean(h9_resp$assess_records_credit, h9_resp$weight_3, na.rm = T))
(assess_records_debit_avg = weighted.mean(h9_resp$assess_records_debit, h9_resp$weight_3, na.rm = T))
(assess_records_prepaid_avg = weighted.mean(h9_resp$assess_records_prepaid, h9_resp$weight_3, na.rm = T))
#

## assessments into vectors
## Table of assessments by these resp: cash, prepaid, debit, credit
table(h9_resp$assess_cost_cash)
table(h9_resp$assess_cost_check)
table(h9_resp$assess_cost_credit)
table(h9_resp$assess_cost_debit)
table(h9_resp$assess_cost_prepaid)

#
table(h9_resp$assess_security_cash)
table(h9_resp$assess_security_check)
table(h9_resp$assess_security_credit)
table(h9_resp$assess_security_debit)
table(h9_resp$assess_security_prepaid)
#
table(h9_resp$assess_convenience_cash)
table(h9_resp$assess_convenience_check)
table(h9_resp$assess_convenience_credit)
table(h9_resp$assess_convenience_debit)
table(h9_resp$assess_convenience_prepaid)
#
table(h9_resp$assess_records_cash)
table(h9_resp$assess_records_check)
table(h9_resp$assess_records_credit)
table(h9_resp$assess_records_debit)
table(h9_resp$assess_records_prepaid)

# make the above into vectors of percentages
(assess_cost_cash.vec = as.vector(prop.table(table(h9_resp$assess_cost_cash))))
(assess_cost_check.vec = as.vector(prop.table(table(h9_resp$assess_cost_check))))
(assess_cost_credit.vec = as.vector(prop.table(table(h9_resp$assess_cost_credit))))
(assess_cost_debit.vec = as.vector(prop.table(table(h9_resp$assess_cost_debit))))
(assess_cost_prepaid.vec = as.vector(prop.table(table(h9_resp$assess_cost_prepaid))))
#
(assess_security_cash.vec = as.vector(prop.table(table(h9_resp$assess_security_cash))))
(assess_security_check.vec = as.vector(prop.table(table(h9_resp$assess_security_check))))
(assess_security_credit.vec = as.vector(prop.table(table(h9_resp$assess_security_credit))))
(assess_security_debit.vec = as.vector(prop.table(table(h9_resp$assess_security_debit))))
(assess_security_prepaid.vec = as.vector(prop.table(table(h9_resp$assess_security_prepaid))))
#
(assess_convenience_cash.vec = as.vector(prop.table(table(h9_resp$assess_convenience_cash))))
(assess_convenience_check.vec = as.vector(prop.table(table(h9_resp$assess_convenience_check))))
(assess_convenience_credit.vec = as.vector(prop.table(table(h9_resp$assess_convenience_credit))))
(assess_convenience_debit.vec = as.vector(prop.table(table(h9_resp$assess_convenience_debit))))
(assess_convenience_prepaid.vec = as.vector(prop.table(table(h9_resp$assess_convenience_prepaid))))
#
(assess_records_cash.vec = as.vector(prop.table(table(h9_resp$assess_records_cash))))
(assess_records_check.vec = as.vector(prop.table(table(h9_resp$assess_records_check))))
(assess_records_credit.vec = as.vector(prop.table(table(h9_resp$assess_records_credit))))
(assess_records_debit.vec = as.vector(prop.table(table(h9_resp$assess_records_debit))))
(assess_records_prepaid.vec = as.vector(prop.table(table(h9_resp$assess_records_prepaid))))

# adding median and average assessment level to each vector
(assess_cost_cash2.vec = c(100*assess_cost_cash.vec, assess_cost_cash_med, assess_cost_cash_avg))
(assess_cost_check2.vec = c(100*assess_cost_check.vec, assess_cost_check_med, assess_cost_check_avg))
(assess_cost_credit2.vec = c(100*assess_cost_credit.vec, assess_cost_credit_med, assess_cost_credit_avg))
(assess_cost_debit2.vec = c(100*assess_cost_debit.vec, assess_cost_debit_med, assess_cost_debit_avg))
(assess_cost_prepaid2.vec = c(100*assess_cost_prepaid.vec, assess_cost_prepaid_med, assess_cost_prepaid_avg))

#
(assess_security_cash2.vec = c(100*assess_security_cash.vec, assess_security_cash_med, assess_security_cash_avg))
(assess_security_check2.vec = c(100*assess_security_check.vec, assess_security_check_med, assess_security_check_avg))
(assess_security_credit2.vec = c(100*assess_security_credit.vec, assess_security_credit_med, assess_security_credit_avg))
(assess_security_debit2.vec = c(100*assess_security_debit.vec, assess_security_debit_med, assess_security_debit_avg))
(assess_security_prepaid2.vec = c(100*assess_security_prepaid.vec, assess_security_prepaid_med, assess_security_prepaid_avg))

#
(assess_convenience_cash2.vec = c(100*assess_convenience_cash.vec, assess_convenience_cash_med, assess_convenience_cash_avg))
(assess_convenience_check2.vec = c(100*assess_convenience_check.vec, assess_convenience_check_med, assess_convenience_check_avg))
(assess_convenience_credit2.vec = c(100*assess_convenience_credit.vec, assess_convenience_credit_med, assess_convenience_credit_avg))
(assess_convenience_debit2.vec = c(100*assess_convenience_debit.vec, assess_convenience_debit_med, assess_convenience_debit_avg))
(assess_convenience_prepaid2.vec = c(100*assess_convenience_prepaid.vec, assess_convenience_prepaid_med, assess_convenience_prepaid_avg))
#
(assess_records_cash2.vec = c(100*assess_records_cash.vec, assess_records_cash_med, assess_records_cash_avg))
(assess_records_check2.vec = c(100*assess_records_check.vec, assess_records_check_med, assess_records_check_avg))
(assess_records_credit2.vec = c(100*assess_records_credit.vec, assess_records_credit_med, assess_records_credit_avg))
(assess_records_debit2.vec = c(100*assess_records_debit.vec, assess_records_debit_med, assess_records_debit_avg))
(assess_records_prepaid2.vec = c(100*assess_records_prepaid.vec, assess_records_prepaid_med, assess_records_prepaid_avg))
#

# Finalizing Table 2 (assessments)
(assess.df = rbind(assess_cost_cash2.vec, assess_cost_check2.vec, assess_cost_credit2.vec, assess_cost_debit2.vec, assess_cost_prepaid2.vec))
dim(assess.df)
(assess2.df = rbind(assess.df, assess_security_cash2.vec, assess_security_check2.vec, assess_security_credit2.vec, assess_security_debit2.vec, assess_security_prepaid2.vec))
dim(assess2.df)
(assess3.df = rbind(assess2.df, assess_convenience_cash2.vec, assess_convenience_check2.vec, assess_convenience_credit2.vec, assess_convenience_debit2.vec,  assess_convenience_prepaid2.vec))
(assess4.df = rbind(assess3.df, assess_records_cash2.vec, assess_records_check2.vec, assess_records_credit2.vec, assess_records_debit2.vec, assess_records_prepaid2.vec))
assess4.df = assess3.df
(colnames(assess4.df) = c("1 (%)", "2 (%)", "3 (%)", "4 (%)", "5 (%)", "Median", "Average"))
assess4.df
# rename rows
(assess5.df = as.data.frame(assess4.df))
#(rownames(assess4.df) = c("Cash cost assessment", "Prepaid cost assessment", "Debit cost assessment", "Credit cost assessment", "Cash security assessment", "Prepaid security assessment", "Debit security assessment", "Credit security assessment", "Cash convenience assessment", "Prepaid convenience assessment", "Debit convenience assessment", "Credit convenience assessment", "Cash record assessment", "Prepaid record assessment", "Debit record assessment", "Credit record assessment"))# with record
(rownames(assess5.df) = c("Cash cost assessment", "Check cost assessment",  "Credit cost assessment", "Debit cost assessment", "Prepaid cost assessment",  "Cash security assessment", "Check security assessment", "Credit security assessment", "Debit security assessment", "Prepaid security assessment",  "Cash convenience assessment", "Check convenience assessment", "Credit convenience assessment", "Debit convenience assessment", "Prepaid convenience assessment"))# without record

assess5.df
str(assess5.df)
(colnames(assess5.df) = c("1 (%)", "2 (%)", "3 (%)", "4 (%)", "5 (%)", "Median(w)", "Average(w)"))
dim(assess5.df)
assess5.df

#(digitm = matrix(rep(c(0, rep(2,7+1)),12), nrow = 12, ncol = 8+1, byrow = T))
#
#print(xtable(assess4.df, digits = 2), include.rownames = T, hline.after = c(0,4,8,12,16))# with record
print(xtable(assess5.df, digits = 1), include.rownames = T, hline.after = c(0,5,10,15))# Table 2

#caption of Table 2, num of respondents
nrow(h9)# num payments
length(unique(h9$uasid))# num respo
#
# for paper discussion of Table 2 (assessments):
round(assess5.df[1,1] + assess5.df[1,2], 1)# frac who assess cost as low
round(assess5.df[4,1] + assess5.df[4,2], 1)# frac who assess debit as low
sum(assess5.df[1,1:5])# check sum to 100%
sum(assess5.df[3,1:5])

### For the random utility estimation: Restricting the payments data to respondents who have BOTH cards
table(h9$adopt)
table(h9$adopt2)
h9_both = subset(h9, adopt2 == "Both_cards")
table(h9_both$pi)
# Restricting to 5 payment instruments
h9_both = subset(h9_both, pi %in% c("cash", "check", "credit", "debit", "prepaid"))
h9_both$pi = droplevels(h9_both$pi)
table(h9_both$pi)
nrow(h9_both)# num payments
length(unique(h9_both$uasid))# num respondents
h9_both_unique = h9_both[!duplicated(h9_both$uasid), ]# each obs is 1 respondent
dim(h9_both_unique)

### Start random utility estimation 
#income_200125.R switch from mlogit logsum function to explicit compuation ###
names(h9_both)
h9_both2 = h9_both
table(h9_both2$pi)
str(h9_both2$pi)
h9_both2$pi = factor(h9_both2$pi)
levels(h9_both2$pi)
table(h9_both2$pi)
percent(prop.table(table(h9_both2$pi)))# percentage paid with each method (not final, look for h9_both4 to quote in the paper)
nrow(h9_both2)# num payments (don't quote here, look below for h9_both4 for actual payments used in the estimation)

dim(h9_both2)
# remove unused variables (demographics etc) for random utility analysis 
#h9_both3 = subset(h9_both2, select = c(uasid, pi, assess_cost_cash, assess_cost_credit, assess_cost_debit, assess_cost_prepaid, assess_security_cash, assess_security_credit, assess_security_debit, assess_security_prepaid, assess_convenience_cash, assess_convenience_credit, assess_convenience_debit, assess_convenience_prepaid, assess_records_cash, assess_records_credit, assess_records_debit, assess_records_prepaid))# record not used
# check of NAs
h9_both3 = subset(h9_both2, select = c(uasid, pi))
dim(h9_both3)
h9_both3[which(is.na(h9_both3)),]
dim(h9_both3)
h9_both4 = na.omit(h9_both3)
dim(h9_both4) 
nrow(h9_both4)# num payments
length(unique(h9_both4$uasid))# num resp
nrow(h9_both4)/length(unique(h9_both4$uasid)) # payments per resp

# turn data into mlogit data
names(h9_both4)
table(h9_both4$pi)# reported in Section 3.2
dim(h9_both4)
both_data = mlogit.data(h9_both4, choice = "pi", shape = "wide", id.var = "uasid", drop.index = F)
names(both_data)
head(both_data)

# add a column "cost" corresponding to respondent's assessment of each choice (including NOT chosen PI) (both)
both_data$cost = NA
both_data[both_data$alt=="cash", ]$cost = assess_cost_cash_med
both_data[both_data$alt=="check", ]$cost = assess_cost_check_med
both_data[both_data$alt=="credit", ]$cost = assess_cost_credit_med
both_data[both_data$alt=="debit", ]$cost = assess_cost_debit_med
both_data[both_data$alt=="prepaid", ]$cost = assess_cost_prepaid_med
# add a column "security" corresponding to respondent's assessment of each choice (including NOT chosen PI)
both_data$security = NA
both_data[both_data$alt=="cash", ]$security = assess_security_cash_med
both_data[both_data$alt=="check", ]$security = assess_security_check_med
both_data[both_data$alt=="credit", ]$security = assess_security_credit_med
both_data[both_data$alt=="debit", ]$security = assess_security_debit_med
both_data[both_data$alt=="prepaid", ]$security = assess_security_prepaid_med
# add a column "convenience" corresponding to respondent's assessment of each choice (including NOT chosen PI)
both_data$convenience = NA
both_data[both_data$alt=="cash", ]$convenience = assess_convenience_cash_med
both_data[both_data$alt=="check", ]$convenience = assess_convenience_check_med
both_data[both_data$alt=="credit", ]$convenience = assess_convenience_credit_med
both_data[both_data$alt=="debit", ]$convenience = assess_convenience_debit_med
both_data[both_data$alt=="prepaid", ]$convenience = assess_convenience_prepaid_med
#
# add a column "record" corresponding to respondent's assessment of each choice (including NOT chosen PI) (both) [Not used, produces negative coefficient]
 both_data$record = NA
both_data[both_data$alt=="cash", ]$record = assess_records_cash_med
both_data[both_data$alt=="check", ]$record = assess_records_check_med
both_data[both_data$alt=="credit", ]$record = assess_records_credit_med
both_data[both_data$alt=="debit", ]$record = assess_records_debit_med
both_data[both_data$alt=="prepaid", ]$record = assess_records_prepaid_med

names(both_data)
tail(both_data)
head(both_data)
# model to be estimated (none UNbanked)
#both_model = mFormula( pi ~ cost + security + convenience + record | -1 ) # with record. Not used b/c it generates negative coefficient
both_model = mFormula( pi ~ cost + security + convenience | -1 ) # without record
#both_model = mFormula( pi ~ cost + security + convenience + record | -1 ) # with record => Singularity
#both_ml = mlogit(both_model, reflevel = "cash", alt.subset = c("cash", "prepaid"),  data = both_data)
both_ml = mlogit(both_model,  data = both_data)
summary(both_ml)# Report in equation (3) in paper
(both_ml_coef = as.vector(both_ml$coefficients[1:3])) # cost, security, convenience
(both_ml_pvalue = coef(summary(both_ml))[,4]) # extract p-value
(both_ml_pvalue = as.vector(both_ml_pvalue)) # p-values vector
(both_ml_sig = as.vector(symnum(both_ml_pvalue, corr = FALSE, na = FALSE, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", " ")))) # sig vector

# Discussion below equation (3): estimated equation
nrow(h9_both4)# num payments
length(unique(h9_both4$uasid))
table(h9_both4$pi)# num payments with each PI
percent(prop.table(table(h9_both4$pi)))# percentage paid with each method
sum(percent(prop.table(table(h9_both4$pi))))# verify 100%

## Computing manually CS (just like in the cashless paper)
## Computations of Consumer Surplus (CS) (None Unbanked) for the FE regression (see below ME)
# computing utility of cash, debit, and prepaid: Equation 4 
# Note: Utility computations apply to all respondents (both and none), because the assessments are medians of entire sample, and estimation is on both (b/c they have both cards). However, they will be applied to consumers with access to cash and prepaid first, before adding a debit card.
dim(h9_both4) 
nrow(h9_both4)# num payments
length(unique(h9_both4$uasid))# num resp
nrow(h9_both4)/length(unique(h9_both4$uasid)) # payments per resp

h9_both4$v_cash = NA # # estimated utility from paying cash
h9_both4$v_cash = both_ml_coef[1]*assess_cost_cash_med + both_ml_coef[2]*assess_security_cash_med + both_ml_coef[3]*assess_convenience_cash_med 
# since all consumers get the same utilty, define
(v_cash = h9_both4$v_cash[1])# equation (4) in paper
#
h9_both4$v_debit = NA # # estimated utility from paying unsbusidized debit
h9_both4$v_debit = both_ml_coef[1]*assess_cost_debit_med + both_ml_coef[2]*assess_security_debit_med + both_ml_coef[3]*assess_convenience_debit_med 
# since all consumers get the same utilty, define
(v_debit = h9_both4$v_debit[1])# equation (4) in paper
#
h9_both4$v_prepaid = NA # # estimated utility from paying v_prepaid 
h9_both4$v_prepaid = both_ml_coef[1]*assess_cost_prepaid_med + both_ml_coef[2]*assess_security_prepaid_med + both_ml_coef[3]*assess_convenience_prepaid_med 
# since all consumers get the same utilty, define
(v_prepaid = h9_both4$v_prepaid[1])# equation (4) in paper
#
head(h9_both4[, c("uasid", "pi", "v_cash", "v_debit", "v_prepaid")], 20)

## Simulations 1: section 4.3
# Any consumer: compute CS *before* adding debit based equation (3.10) on page 55 Train's book 
(mui = 1) # marginal utility of income = 1 (no use for that b/c only percentage change in CS is computed), same as logsum
# CS before debit is added:
h9_both4$cs = mui*log(exp(h9_both4$v_cash) + 0 + exp(h9_both4$v_prepaid)) # cash is ref pi here, 0 is for missing debit
#
# CS *after* debit option is added to none consumers 
h9_both4$cs_debit =  mui*log(exp(h9_both4$v_cash) + exp(h9_both4$v_debit) + exp(h9_both4$v_prepaid))
#
# Difference in CS (with debit - before with no debit)
h9_both4$cs_diff = h9_both4$cs_debit - h9_both4$cs
# rate of change
h9_both4$cs_rate = h9_both4$cs_diff/h9_both4$cs
#
length(h9_both4$cs)# by payments
summary(h9_both4$cs) # CS before the change
summary(h9_both4$cs_debit) # CS after stores become cashless
summary(h9_both4$cs_diff)
summary(h9_both4$cs_rate)# equation (6)
head(h9_both4[, c("pi", "cs", "cs_debit", "cs_diff", "cs_rate")])

## Now, compute cs assuming that cost debit =1 (subsidized) (call it "cost1" instead of "after". Basically, subsidized debit card is added)
# cost of debit before the subsidy
assess_cost_debit_med
h9_both4$v_debit_cost1 = both_ml_coef[1]*1 + both_ml_coef[2]*assess_security_debit_med + both_ml_coef[3]*assess_convenience_debit_med 
# since all consumers gain the same utility, define
(v_debit_cost1 = h9_both4$v_debit_cost1[1])
#
# CS *after* debit option is added to none consumers and cost debit =1
h9_both4$cs_debit_cost1 =  mui*(log(exp(h9_both4$v_cash) + exp(h9_both4$v_debit_cost1) + exp(h9_both4$v_prepaid)))
#
# Difference in CS (with debit with cost=1 - before with no debit)
h9_both4$cs_diff_cost1 = h9_both4$cs_debit_cost1 - h9_both4$cs
# rate of change
h9_both4$cs_rate_cost1 = h9_both4$cs_diff_cost1/h9_both4$cs
#
length(h9_both4$cs_diff_cost1)# by payments
summary(h9_both4$cs) # CS before the change
summary(h9_both4$cs_debit_cost1) # CS after debit with cost=1 was added
summary(h9_both4$cs_diff_cost1)
summary(h9_both4$cs_rate_cost1)# equation (6)
(h9_both4_change_rate_med_cost1 = median(h9_both4$cs_rate_cost1, na.rm = T))

# Equation (6): %CS change from providing debit card (non-subsidized)
(debit_cs_rate = 100*median(h9_both4$cs_rate)) #median is irrelevant, as they are all the same
# Conclusion: %CS change from providing debit card (subsidized)
(debit_sub_cs_rate = 100*median(h9_both4$cs_rate_cost1)) #median is irrelevant, as they are 

## Simulations 2: section 4.4
## Now compute CS assuming that cost prepaid =1 (subsidized) (call it "cost1" instead of "after". Basically, prepaid cards are not subsidized)
# # cost of prepaid before the subsidy
assess_cost_prepaid_med # not used
# utility after the cost of prepaid is subsidized
h9_both4$v_prepaid_cost2 = both_ml_coef[1]*1 + both_ml_coef[2]*assess_security_prepaid_med + both_ml_coef[3]*assess_convenience_prepaid_med
#
# utility of prepaid w/ subsidy repored in equation (7)
(v_prepaid_cost2 = h9_both4$v_prepaid_cost2[2])
# #
# CS *after* the prepaid subsidy to none consumers and cost debit =1
h9_both4$cs_prepaid_cost2 =  mui*(log(exp(h9_both4$v_cash) + exp(h9_both4$v_prepaid_cost2)))
#
# Difference in CS (with prepaid with cost=1 - before the subsidy
h9_both4$cs_diff_cost2 = h9_both4$cs_prepaid_cost2 - h9_both4$cs
# rate of change
h9_both4$cs_rate_cost2 = h9_both4$cs_diff_cost2/h9_both4$cs
#
length(h9_both4$cs_diff_cost2)# by payments
summary(h9_both4$cs) # CS before the change
summary(h9_both4$cs_prepaid_cost2) # CS after prepaid with cost=1 was added
summary(h9_both4$cs_diff_cost2)
summary(h9_both4$cs_rate_cost2)# equation (?) in paper, V^S
(h9_both4_change_rate_med_cost2 = median(h9_both4$cs_rate_cost2, na.rm = T))

### Conclusion (section 5)
# recall Table 1
income.df
income.df$Variable
colnames(income.df)

# % of none unbaked with income < 20k
sum(income.df[income.df$Variable=="None unbanked (%)", "0--10k"] + income.df[income.df$Variable=="None unbanked (%)", "10k--20k"])
#
# % of none unbaked with income 20k- 30k
income.df[income.df$Variable=="None unbanked (%)", "20k--30k"]
#
# % of none unbaked with income 30k- 40k
income.df[income.df$Variable=="None unbanked (%)", "30k--40k"]



###### Unused code ########
### Classification tree predicting card adoption by demongraphics. [Not used]
# table(h8_unique$adopt)# Use this one
# table(h8_unique$adopt2)# Don't use, very few are banked with no cards
# 
# adoption_model1 = adopt~ Age +Gender +Marital +Education +Work +HH_income +HH_size #
# 
# # Tree on entire sample (not just training) to generate Fig.1 in paper, & Not tuning to Optimal tree cp, just to demonstrate.
# adoption_tree1 = rpart(adoption_model1, data = h8_unique, method = "class", control = rpart.control(cp = 0.001))# Extremely-long tree first, then prune it
# #Below, plot a tree (Note: Longer than optimal, but needed for later prunning and redrawing).
# prp(adoption_tree1, type = 3, box.palette = "auto", extra = 100, under = T, tweak = 1.0, varlen = 0, faclen = 0)#faclet=0 avoids abvreviations, tweak for char size
# #now search for optimal cp, rpart has cp table built in
# plotcp(adoption_tree1)# plot cp: Not used for this demo plot. See training data below
# names(adoption_tree1)
# adoption_tree1$cptable # List cp, number of splits and errors
# # Below, I choose cp to use for prunning (highest rel error below the dashed line)
# (cp.choice = adoption_tree1$cptable[3, "CP"]) # Corresponds to 6 splits (just for demonstration)
# adoption_prune1 = prune.rpart(adoption_tree1, cp=cp.choice)
# prp(adoption_prune1, type = 3, box.palette = "auto", legend.x=NA, legend.y=NA, extra = 100, under = T, tweak = 1.1, varlen = 0, faclen = 0, Margin = 0.0, digits = -2)#faclet=0 avoids abvreviations, tweak for char size

# ## try dependent None versus Some
# table(h8_unique$adopt)
# h8_unique$adopt3 = ifelse(h8_unique$adopt=="None", "None", "Some")
# table(h8_unique$adopt3)
# 
# adoption_model2 = adopt3 ~ Age +Gender +Marital +Education +Work +HH_income +HH_size #
# 
# # Tree on entire sample (not just training) to generate Fig.1 in paper, & Not tuning to Optimal tree cp, just to demonstrate.
# adoption_tree2 = rpart(adoption_model2, data = h8_unique, method = "class", control = rpart.control(cp = 0.001))# Extremely-long tree first, then prune it
# #Below, plot a tree (Note: Longer than optimal, but needed for later prunning and redrawing).
# prp(adoption_tree2, type = 3, box.palette = "auto", extra = 100, under = T, tweak = 1.0, varlen = 0, faclen = 0)#faclet=0 avoids abvreviations, tweak for char size
# #now search for optimal cp, rpart has cp table built in
# plotcp(adoption_tree2)# plot cp: Not used for this demo plot. See training data below
# names(adoption_tree2)
# adoption_tree2$cptable # List cp, number of splits and errors
# # Below, I choose cp to use for prunning (highest rel error below the dashed line)
# (cp.choice = adoption_tree2$cptable[2, "CP"]) # Corresponds to 6 splits (just for demonstration)
# adoption_prune2 = prune.rpart(adoption_tree2, cp=cp.choice)
# prp(adoption_prune2, type = 3, box.palette = "auto", legend.x=NA, legend.y=NA, extra = 100, under = T, tweak = 1.1, varlen = 0, faclen = 0, Margin = 0.0, digits = -2)#faclet=0 avoids abvreviations, tweak for char size
# 
# # caption Fig 1 (tree)
# #nrow(m8)
# length(unique(h8$uasid))# num resp
# nrow(h8_unique)# same as above

# End of trees: Cancelled b/c most respondents have cards

### Regressing on NONE (no cards) consumers instead of BOTH (debit and credit)
# # This cannot be done because there are no payments made with debit (needed for this regression)
# table(h9$adopt)
# table(h9$adopt2)
# h9_none = subset(h9, adopt2 == "None_unbanked")
# table(h9_none$pi)
# # Restricting to 5 payment instruments
# h9_none = subset(h9_none, pi %in% c("cash", "check", "credit", "debit", "prepaid"))
# h9_none$pi = droplevels(h9_none$pi)
# table(h9_none$pi)
# nrow(h9_none)# num payments
# length(unique(h9_none$uasid))# num respondents
# h9_none_unique = h9_none[!duplicated(h9_none$uasid), ]# each obs is 1 respondent
# dim(h9_none_unique)
# 
# ### Start random utility estimation
# #income_200125.R switch from mlogit logsum function to explicit compuation ###
# names(h9_none)
# h9_none2 = h9_none
# table(h9_none2$pi)
# str(h9_none2$pi)
# h9_none2$pi = factor(h9_none2$pi)
# levels(h9_none2$pi)
# table(h9_none2$pi)
# percent(prop.table(table(h9_none2$pi)))# percentage paid with each method (not final, look for h9_none4 to quote in the paper)
# nrow(h9_none2)# num payments (don't quote here, look below for h9_none4 for actual payments used in the estimation)
# 
# dim(h9_none2)
# # remove unused variables (demographics etc) for random utility analysis
# #h9_none3 = subset(h9_none2, select = c(uasid, pi, assess_cost_cash, assess_cost_credit, assess_cost_debit, assess_cost_prepaid, assess_security_cash, assess_security_credit, assess_security_debit, assess_security_prepaid, assess_convenience_cash, assess_convenience_credit, assess_convenience_debit, assess_convenience_prepaid, assess_records_cash, assess_records_credit, assess_records_debit, assess_records_prepaid))# record not used
# # check of NAs
# h9_none3 = subset(h9_none2, select = c(uasid, pi))
# dim(h9_none3)
# h9_none3[which(is.na(h9_none3)),]
# dim(h9_none3)
# h9_none4 = na.omit(h9_none3)
# dim(h9_none4)
# nrow(h9_none4)# num payments
# length(unique(h9_none4$uasid))# num resp
# nrow(h9_none4)/length(unique(h9_none4$uasid)) # payments per resp
# 
# # turn data into mlogit data
# names(h9_none4)
# table(h9_none4$pi)# reported in Section 3.2
# dim(h9_none4)
# none_data = mlogit.data(h9_none4, choice = "pi", shape = "wide", id.var = "uasid", drop.index = F)
# names(none_data)
# head(none_data)
# 
# # add a column "cost" corresponding to respondent's assessment of each choice (including NOT chosen PI) (none)
# none_data$cost = NA
# none_data[none_data$alt=="cash", ]$cost = assess_cost_cash_med
# none_data[none_data$alt=="check", ]$cost = assess_cost_check_med
# none_data[none_data$alt=="credit", ]$cost = assess_cost_credit_med
# none_data[none_data$alt=="debit", ]$cost = assess_cost_debit_med
# none_data[none_data$alt=="prepaid", ]$cost = assess_cost_prepaid_med
# # add a column "security" corresponding to respondent's assessment of each choice (including NOT chosen PI)
# none_data$security = NA
# none_data[none_data$alt=="cash", ]$security = assess_security_cash_med
# none_data[none_data$alt=="check", ]$security = assess_security_check_med
# none_data[none_data$alt=="credit", ]$security = assess_security_credit_med
# none_data[none_data$alt=="debit", ]$security = assess_security_debit_med
# none_data[none_data$alt=="prepaid", ]$security = assess_security_prepaid_med
# # add a column "convenience" corresponding to respondent's assessment of each choice (including NOT chosen PI)
# none_data$convenience = NA
# none_data[none_data$alt=="cash", ]$convenience = assess_convenience_cash_med
# none_data[none_data$alt=="check", ]$convenience = assess_convenience_check_med
# none_data[none_data$alt=="credit", ]$convenience = assess_convenience_credit_med
# none_data[none_data$alt=="debit", ]$convenience = assess_convenience_debit_med
# none_data[none_data$alt=="prepaid", ]$convenience = assess_convenience_prepaid_med
# #
# # add a column "record" corresponding to respondent's assessment of each choice (including NOT chosen PI) (none) [Not used, produces negative coefficient]
# none_data$record = NA
# none_data[none_data$alt=="cash", ]$record = assess_records_cash_med
# none_data[none_data$alt=="check", ]$record = assess_records_check_med
# none_data[none_data$alt=="credit", ]$record = assess_records_credit_med
# none_data[none_data$alt=="debit", ]$record = assess_records_debit_med
# none_data[none_data$alt=="prepaid", ]$record = assess_records_prepaid_med
# 
# names(none_data)
# tail(none_data)
# head(none_data)
# # model to be estimated (none UNbanked)
# #none_model = mFormula( pi ~ cost + security + convenience + record | -1 ) # with record. Not used b/c it generates negative coefficient
# none_model = mFormula( pi ~ cost + security + convenience | -1 ) # without record
# #none_model = mFormula( pi ~ cost + security + convenience + record | -1 ) # with record => Singularity
# #none_ml = mlogit(none_model, reflevel = "cash", alt.subset = c("cash", "prepaid"),  data = none_data)
# none_ml = mlogit(none_model,  data = none_data)
# summary(none_ml)# Report in equation (3) in paper
# (none_ml_coef = as.vector(none_ml$coefficients[1:3])) # cost, security, convenience
# (none_ml_pvalue = coef(summary(none_ml))[,4]) # extract p-value
# (none_ml_pvalue = as.vector(none_ml_pvalue)) # p-values vector
# (none_ml_sig = as.vector(symnum(none_ml_pvalue, corr = FALSE, na = FALSE, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", " ")))) # sig vector
# 
# # For the paragraph below regression results (equation 3 in paper)
# nrow(h9_none4)# num payments
# length(unique(h9_none4$uasid))
# table(h9_none4$pi)# num payments with each PI
# percent(prop.table(table(h9_none4$pi)))# percentage paid with each method
# sum(percent(prop.table(table(h9_none4$pi))))# verify 100%
# 
# ## Computing manually CS (just like in the cashless paper)
# ## Computations of Consumer Surplus (CS) (None Unbanked) for the FE regression (see below ME)
# # computing utility of cash, debit, and prepaid: Equation 4
# # Note: Utility computations apply to all respondents (none and none), because the assessments are medians of entire sample, and estimation is on none (b/c they have none cards). However, they will be applied to consumers with access to cash and prepaid first, before adding a debit card.
# dim(h9_none4)
# nrow(h9_none4)# num payments
# length(unique(h9_none4$uasid))# num resp
# nrow(h9_none4)/length(unique(h9_none4$uasid)) # payments per resp
# 
# h9_none4$v_cash = NA # # estimated utility from paying cash
# h9_none4$v_cash = none_ml_coef[1]*assess_cost_cash_med + none_ml_coef[2]*assess_security_cash_med + none_ml_coef[3]*assess_convenience_cash_med
# # since all consumers get the same utilty, define
# (v_cash = h9_none4$v_cash[1])# equation (4) in paper
# #
# h9_none4$v_debit = NA # # estimated utility from paying unsbusidized debit
# h9_none4$v_debit = none_ml_coef[1]*assess_cost_debit_med + none_ml_coef[2]*assess_security_debit_med + none_ml_coef[3]*assess_convenience_debit_med
# # since all consumers get the same utilty, define
# (v_debit = h9_none4$v_debit[1])# equation (4) in paper
# #
# h9_none4$v_prepaid = NA # # estimated utility from paying v_prepaid
# h9_none4$v_prepaid = none_ml_coef[1]*assess_cost_prepaid_med + none_ml_coef[2]*assess_security_prepaid_med + none_ml_coef[3]*assess_convenience_prepaid_med
# # since all consumers get the same utilty, define
# (v_prepaid = h9_none4$v_prepaid[1])# equation (4) in paper
# #
# head(h9_none4[, c("uasid", "pi", "v_cash", "v_debit", "v_prepaid")], 20)
# 
# # None: compute CS *before* adding debit based equation (3.10) on page 55 Train's book
# (mui = 1) # marginal utility of income = 1 (no use for that b/c only percentage change in CS is computed), same as logsum
# h9_none4$cs = mui*log(exp(h9_none4$v_cash) + 0 + exp(h9_none4$v_prepaid)) # cash is ref pi here, 0 is for missing debit
# #
# # CS *after* debit option is added to none consumers
# h9_none4$cs_debit =  mui*log(exp(h9_none4$v_cash) + exp(h9_none4$v_debit) + exp(h9_none4$v_prepaid))
# #
# # Difference in CS (with debit - before with no debit)
# h9_none4$cs_diff = h9_none4$cs_debit - h9_none4$cs
# # rate of change
# h9_none4$cs_rate = h9_none4$cs_diff/h9_none4$cs
# #
# length(h9_none4$cs)# by payments
# summary(h9_none4$cs) # CS before adding unsubsidized debit
# summary(h9_none4$cs_debit)# CS after adding unsubsidized debit
# summary(h9_none4$cs_diff)
# summary(h9_none4$cs_rate)# Equation (6) in paper
# head(h9_none4[, c("pi", "cs", "cs_debit", "cs_diff", "cs_rate")])
# 
# ## Now compute CS assuming that cost debit =1 (subsidized) (call it "cost1" instead of "after". Basically, subsidized debit card is added)
# # cost of debit before the subsidy
# assess_cost_debit_med
# h9_none4$v_debit_cost1 = none_ml_coef[1]*1 + none_ml_coef[2]*assess_security_debit_med + none_ml_coef[3]*assess_convenience_debit_med
# # since all consumers gain the same utility, define
# (v_debit_cost1 = h9_none4$v_debit_cost1[1])# utility of debit w/ subsidy repored in equation (4)
# #
# # CS *after* debit option is added to none consumers and cost debit =1
# h9_none4$cs_debit_cost1 =  mui*(log(exp(h9_none4$v_cash) + exp(h9_none4$v_debit_cost1) + exp(h9_none4$v_prepaid)))
# #
# # Difference in CS (with debit with cost=1 - before with no debit)
# h9_none4$cs_diff_cost1 = h9_none4$cs_debit_cost1 - h9_none4$cs
# # rate of change
# h9_none4$cs_rate_cost1 = h9_none4$cs_diff_cost1/h9_none4$cs
# #
# length(h9_none4$cs_diff_cost1)# by payments
# summary(h9_none4$cs) # CS before the change
# summary(h9_none4$cs_debit_cost1) # CS after debit with cost=1 was added
# summary(h9_none4$cs_diff_cost1)
# summary(h9_none4$cs_rate_cost1)# equation (6) in paper, V^S
# (h9_none4_change_rate_med_cost1 = median(h9_none4$cs_rate_cost1, na.rm = T))
# 
# # Conclusion: %CS change from providing debit card (non-subsidized)
# (debit_cs_rate = 100*median(h9_none4$cs_rate)) #median is irrelevant, as they are all the same
# # Conclusion: %CS change from providing debit card (subsidized)
# (debit_sub_cs_rate = 100*median(h9_none4$cs_rate_cost1)) #median is irrelevant, as they are

# ### Start appendix: multinomial logit [cancelled]
# # data
# names(h9_both2)
# h9_both5 = subset(h9_both2, select = c(uasid, pi, amnt, HH_income, HH_size, Work, Marital, Gender, Education, Age))
# # adding median assessments
# head(h9_both5)
# 
# ### add median assessments h9_both5$cost, etc here to h9_both5 and then regress
# # add a column "cost" corresponding to respondent's assessment of each choice (including NOT chosen PI) (both)
# h9_both5$cost = NA
# h9_both5[h9_both5$pi=="cash", ]$cost = assess_cost_cash_med
# h9_both5[h9_both5$pi=="check", ]$cost = assess_cost_check_med
# h9_both5[h9_both5$pi=="credit", ]$cost = assess_cost_credit_med
# h9_both5[h9_both5$pi=="debit", ]$cost = assess_cost_debit_med
# h9_both5[h9_both5$pi=="prepaid", ]$cost = assess_cost_prepaid_med
# # add a column "security" corresponding to respondent's assessment of each choice (including NOT chosen PI)
# h9_both5$security = NA
# h9_both5[h9_both5$pi=="cash", ]$security = assess_security_cash_med
# h9_both5[h9_both5$pi=="check", ]$security = assess_security_check_med
# h9_both5[h9_both5$pi=="credit", ]$security = assess_security_credit_med
# h9_both5[h9_both5$pi=="debit", ]$security = assess_security_debit_med
# h9_both5[h9_both5$pi=="prepaid", ]$security = assess_security_prepaid_med
# # add a column "convenience" corresponding to respondent's assessment of each choice (including NOT chosen PI)
# h9_both5$convenience = NA
# h9_both5[h9_both5$pi=="cash", ]$convenience = assess_convenience_cash_med
# h9_both5[h9_both5$pi=="check", ]$convenience = assess_convenience_check_med
# h9_both5[h9_both5$pi=="credit", ]$convenience = assess_convenience_credit_med
# h9_both5[h9_both5$pi=="debit", ]$convenience = assess_convenience_debit_med
# h9_both5[h9_both5$pi=="prepaid", ]$convenience = assess_convenience_prepaid_med
# #
# # add a column "record" corresponding to respondent's assessment of each choice (including NOT chosen PI) (both) [Not used, produces negative coefficient]
# h9_both5$record = NA
# h9_both5[h9_both5$pi=="cash", ]$record = assess_records_cash_med
# h9_both5[h9_both5$pi=="check", ]$record = assess_records_check_med
# h9_both5[h9_both5$pi=="credit", ]$record = assess_records_credit_med
# h9_both5[h9_both5$pi=="debit", ]$record = assess_records_debit_med
# h9_both5[h9_both5$pi=="prepaid", ]$record = assess_records_prepaid_med
# 
# # model 1: w/ constant term and w/o demographics
# multi_model1 = pi~cost+security+convenience
# logit_multi1 = multinom(multi_model1, data = h9_both5)
# summary(logit_multi1)
# 
# # model 2: w/ constant term and w demographics
# names(h9_both5)
# multi_model2 = pi~cost+security+convenience + log(1+amnt) + HH_income+ HH_size+ Work + Marital + Gender + Education + Age
# logit_multi2 = multinom(multi_model2, data = h9_both5)
# summary(logit_multi2)
# 
# ### Back to binomial logit on cash
# # Note: below, GLM is not needed because xx$fit provides the coefficients that can be cbind with marginal effects
# # model 0: w/0 constant term and w/o demographics => No results, skip model 1
# #logit_model0 = pi~0+cost+security+convenience
# #(logit_glm0 = glm(logit_model1, data = logitdata2, family = "binomial"))
# #summary(logit_glm1) #coefficients
# #(logit_mfx0 = logitmfx(logit_model0, atmean = F, data = logitdata2))# ME
# #summary(logit_mfx0)
# #logit_mfx0$fit# coefficients (same as GLM)
# #logit_mfx0$mfxest# ME
# #=> interpretation: high cost increase cash use by 32.8%
# 
# # model 1: w/ constant term and w/o demographics
# names(h9_both5)# data set
# table(h9_both5$pi)
# h9_both5$pi_cash = ifelse(h9_both5$pi=="cash", 1, 0)
# table(h9_both5$pi_cash)
# #
# logit_model1 = pi_cash~cost+security+convenience
# #(logit_glm1 = glm(logit_model1, data = logitdata2, family = "binomial"))
# #summary(logit_glm1) #coefficients
# (logit_mfx1 = logitmfx(logit_model1, atmean = F, data = h9_both5))#ME
# logit_mfx1$fit# coefficients (same as GLM)
# logit_mfx1$mfxest# ME
# (mfx1_1.df = as.data.frame(logit_mfx1$mfxest))
# logit_mfx1$fit$coefficients# adding coefficients to the takes
# (mfx1_2.df = rbind("Intercept"=c(0,0,0), mfx1_1.df))# blank 1st row b/c coefficients have 4 rows
# (mfx1_3.df = cbind("Coeff" = logit_mfx1$fit$coefficients, mfx1_2.df))# adding coeff 1st column
# dim(mfx1_3.df)
# #
# # adding sig symbol into the data frame
# for (i in 1:nrow(mfx1_3.df)){mfx1_3.df[i,6]=' '}
# for (i in 1:nrow(mfx1_3.df)){
#   if(mfx1_3.df[i,5]<=0.1){mfx1_3.df[i,6]='.'}
# }
# for (i in 1:nrow(mfx1_3.df)){
#   if(mfx1_3.df[i,5]<=0.05){mfx1_3.df[i,6]='*'}
# }
# for (i in 1:nrow(mfx1_3.df)){
#   if(mfx1_3.df[i,5]<=0.01){mfx1_3.df[i,6]='**'}
# }
# for (i in 1:nrow(mfx1_3.df)){
#   if(mfx1_3.df[i,5]<=0.001){mfx1_3.df[i,6]='***'}
# }
# names(mfx1_3.df)
# names(mfx1_3.df)[6] = "Sig"
# mfx1_3.df
# # Remove 3 columns
# (mfx1_4.df = subset(mfx1_3.df, select = -c(3:5)))
# colnames(mfx1_4.df)[2] = "Marg.Eff"
# mfx1_4.df
# 
# # model 2: w/ constant term and w demographics
# logit_model2 = pi_cash~cost+security+convenience + log(1+amnt) + log(1+HH_income)+ HH_size+ Work + Marital + Gender + Education + Age
# #(logit_glm2 = glm(logit_model2, data = logitdata2, family = "binomial"))
# #summary(logit_glm2) #coefficients
# (logit_mfx2 = logitmfx(logit_model2, atmean = F, data = h9_both5))#ME
# logit_mfx2$fit# coefficients (same as GLM)
# logit_mfx2$mfxest# ME
# (mfx2_1.df = as.data.frame(logit_mfx2$mfxest))
# logit_mfx2$fit$coefficients# adding coefficients to the takes
# (mfx2_2.df = rbind("Intercept"=c(0,0,0), mfx2_1.df))# blank 1st row b/c coefficients have 4 rows
# (mfx2_3.df = cbind("Coeff" = logit_mfx2$fit$coefficients, mfx2_2.df))# adding coeff 1st column
# dim(mfx2_3.df)
# #
# # adding sig symbol into the data frame
# for (i in 1:nrow(mfx2_3.df)){mfx2_3.df[i,6]=' '}
# for (i in 1:nrow(mfx2_3.df)){
#   if(mfx2_3.df[i,5]<=0.1){mfx2_3.df[i,6]='.'}
# }
# for (i in 1:nrow(mfx2_3.df)){
#   if(mfx2_3.df[i,5]<=0.05){mfx2_3.df[i,6]='*'}
# }
# for (i in 1:nrow(mfx2_3.df)){
#   if(mfx2_3.df[i,5]<=0.01){mfx2_3.df[i,6]='**'}
# }
# for (i in 1:nrow(mfx2_3.df)){
#   if(mfx2_3.df[i,5]<=0.001){mfx2_3.df[i,6]='***'}
# }
# names(mfx2_3.df)
# names(mfx2_3.df)[6] = "Sig"
# mfx2_3.df
# # Remove 3 columns
# (mfx2_4.df = subset(mfx2_3.df, select = -c(3:5)))
# colnames(mfx2_4.df)[2] = "Marg.Eff"
# mfx2_4.df
# #
# # removing stat insig demog variables (age, employed, and all education)
# dim(mfx2_4.df)
# (mfx2_5.df = mfx2_4.df[-c(5,6, 9, 11:22), ])
# 
# # combining model 1 (const) with model 2 (const+demog) into one df
# mfx2_5.df
# mfx1_4.df
# dim(mfx2_5.df)
# dim(mfx1_4.df)
# # mfx2 misses 3 lines (demog)
# (mfx1_5.df = rbind(mfx1_4.df, c(0,0,0), c(0,0,0), c(0,0,0)))
# dim(mfx1_5.df)
# dim(mfx2_5.df)
# #
# # make row names in model 2 first column in model 1
# (mfx1_6.df = cbind("Variable" = rownames(mfx2_5.df), mfx1_5.df)  )
# #
# # combing mfx1 to the right of mfx2
# (logit1.df = cbind(mfx1_6.df, mfx2_5.df))
# #
# print(xtable(logit1.df, digits = 3), include.rownames = F, hline.after = c(0,1,4,7))
# 
# # for caption and discussion in subsection 3.3
# nrow(logitdata2)# num payments
# length(unique(logitdata2$uasid)) #num resp
# summary(logitdata2$assess_cost_cash)# end of section 3 discussion.
# summary(logitdata2$assess_cost_prepaid)
# 
# ### try mlogit
# h9_both5 = subset(h9_both2, select = c(uasid, pi, amnt, HH_income, HH_size, Work, Marital, Gender, Education, Age))
# # adding median assessments
# head(h9_both5)
# 
# both_data5 = mlogit.data(h9_both5, choice = "pi", shape = "wide", id.var = "uasid", drop.index = F)
# names(both_data5)
# head(both_data5)
# 
# # add a column "cost" corresponding to respondent's assessment of each choice (including NOT chosen PI) (both)
# both_data5$cost = NA
# both_data5[both_data5$alt=="cash", ]$cost = assess_cost_cash_med
# both_data5[both_data5$alt=="check", ]$cost = assess_cost_check_med
# both_data5[both_data5$alt=="credit", ]$cost = assess_cost_credit_med
# both_data5[both_data5$alt=="debit", ]$cost = assess_cost_debit_med
# both_data5[both_data5$alt=="prepaid", ]$cost = assess_cost_prepaid_med
# # add a column "security" corresponding to respondent's assessment of each choice (including NOT chosen PI)
# both_data5$security = NA
# both_data5[both_data5$alt=="cash", ]$security = assess_security_cash_med
# both_data5[both_data5$alt=="check", ]$security = assess_security_check_med
# both_data5[both_data5$alt=="credit", ]$security = assess_security_credit_med
# both_data5[both_data5$alt=="debit", ]$security = assess_security_debit_med
# both_data5[both_data5$alt=="prepaid", ]$security = assess_security_prepaid_med
# # add a column "convenience" corresponding to respondent's assessment of each choice (including NOT chosen PI)
# both_data5$convenience = NA
# both_data5[both_data5$alt=="cash", ]$convenience = assess_convenience_cash_med
# both_data5[both_data5$alt=="check", ]$convenience = assess_convenience_check_med
# both_data5[both_data5$alt=="credit", ]$convenience = assess_convenience_credit_med
# both_data5[both_data5$alt=="debit", ]$convenience = assess_convenience_debit_med
# both_data5[both_data5$alt=="prepaid", ]$convenience = assess_convenience_prepaid_med
# #
# # add a column "record" corresponding to respondent's assessment of each choice (including NOT chosen PI) (both) [Not used, produces negative coefficient]
# both_data5$record = NA
# both_data5[both_data5$alt=="cash", ]$record = assess_records_cash_med
# both_data5[both_data5$alt=="check", ]$record = assess_records_check_med
# both_data5[both_data5$alt=="credit", ]$record = assess_records_credit_med
# both_data5[both_data5$alt=="debit", ]$record = assess_records_debit_med
# both_data5[both_data5$alt=="prepaid", ]$record = assess_records_prepaid_med
# 
# names(both_data5)
# tail(both_data5)
# head(both_data5)
# # model to be estimated (none UNbanked)
# #both_model = mFormula( pi ~ cost + security + convenience + record | -1 ) # with record. Not used b/c it generates negative coefficient
# both_model5 = mFormula( pi ~ cost + security + convenience | -1 ) # without record
# both_model5 = mFormula(pi ~ cost+security+convenience + log(1+amnt) + log(1+HH_income) + HH_size+ Work + Marital + Gender + Education + Age)
# #both_model5 = mFormula( pi ~ cost + security + convenience ) # with costant
# #both_model = mFormula( pi ~ cost + security + convenience + record | -1 ) # with record => Singularity
# #both_ml = mlogit(both_model, reflevel = "cash", alt.subset = c("cash", "prepaid"),  data = both_data5)
# both_ml5 = mlogit(both_model5,  data = both_data5)
# summary(both_ml5)
# (both_ml_coef = as.vector(both_ml$coefficients[1:3])) # cost, security, convenience
# (both_ml_pvalue = coef(summary(both_ml))[,4]) # extract p-value
# (both_ml_pvalue = as.vector(both_ml_pvalue)) # p-values vector
# (both_ml_sig = as.vector(symnum(both_ml_pvalue, corr = FALSE, na = FALSE, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", " ")))) # sig vector


### Assessments by resp "none" (no debit and no credit)
# table(h9$adopt)
# table(h9$adopt2)
# table(h9$adopt2)
# h9_none = subset(h9, adopt2 == "None_unbanked")
# table(h9_none$pi)
# # Restricting to 5 payment instruments
# h9_none = subset(h9_none, pi %in% c("cash", "check", "credit", "debit", "prepaid"))
# #h9_none$pi = droplevels(h9_both$pi)
# table(h9_none$pi)
# nrow(h9_none)# num payments
# length(unique(h9_none$uasid))# num respondents
# h9_none_resp = h9_none[!duplicated(h9_none$uasid), ]# each obs is 1 respondent
# dim(h9_none_resp)
# 
# ## start Table 3 (same as Table 2, but for "none" instead of "all" consumers)
# 
# table(h9_none$assess_cost_cash)# rank dist
# table(h9_none$assess_cost_check)
# table(h9_none$assess_cost_debit)
# table(h9_none$assess_cost_credit)
# table(h9_none$assess_cost_prepaid)
# 
# # population (none only) weighted medians and avg of assessments
# # Compute weighted median assessments for all
# # scale weights for "none" resp
# length(unique(h9_none$uasid))# num resp
# dim(h9_resp_none)
# # construct weight_4 (weights by respondent)
# names(dplyr::select(h9_resp_none, starts_with("w")))
# nrow(h9_resp_none)# num resp
# sum(h9_resp_none$weight_171819_1, na.rm = T)
# h9_resp_none$weight_4 = nrow(h9_resp_none)*h9_resp_none$weight_171819_1/sum(h9_resp_none$weight_171819_1, na.rm = T)
# sum(h9_resp_none$weight_4, na.rm = T)# new rescaled weights
# 
# names(dplyr::select(h9_resp_none, starts_with("assess")))
# #
# (assess_convenience_cash_med = weighted.median(h9_resp_none$assess_convenience_cash, h9_resp_none$weight_4, na.rm = T))
# (assess_convenience_prepaid_med = weighted.median(h9_resp_none$assess_convenience_prepaid, h9_resp_none$weight_4, na.rm = T))
# (assess_convenience_debit_med = weighted.median(h9_resp_none$assess_convenience_debit, h9_resp_none$weight_4, na.rm = T))
# (assess_convenience_credit_med = weighted.median(h9_resp_none$assess_convenience_credit, h9_resp_none$weight_4, na.rm = T))
# 
# ## start assessment weighted median
# (assess_cost_cash_med = weighted.median(h9_resp_none$assess_cost_cash, h9_resp_none$weight_4, na.rm = T))
# (assess_cost_check_med = weighted.median(h9_resp_none$assess_cost_check, h9_resp_none$weight_4, na.rm = T))
# (assess_cost_credit_med = weighted.median(h9_resp_none$assess_cost_credit, h9_resp_none$weight_4, na.rm = T))
# (assess_cost_debit_med = weighted.median(h9_resp_none$assess_cost_debit, h9_resp_none$weight_4, na.rm = T))
# (assess_cost_prepaid_med = weighted.median(h9_resp_none$assess_cost_prepaid, h9_resp_none$weight_4, na.rm = T))
# #
# (assess_security_cash_med = weighted.median(h9_resp_none$assess_security_cash, h9_resp_none$weight_4, na.rm = T))
# (assess_security_check_med = weighted.median(h9_resp_none$assess_security_check, h9_resp_none$weight_4, na.rm = T))
# (assess_security_credit_med = weighted.median(h9_resp_none$assess_security_credit, h9_resp_none$weight_4, na.rm = T))
# (assess_security_debit_med = weighted.median(h9_resp_none$assess_security_debit, h9_resp_none$weight_4, na.rm = T))
# (assess_security_prepaid_med = weighted.median(h9_resp_none$assess_security_prepaid, h9_resp_none$weight_4, na.rm = T))
# #
# (assess_convenience_cash_med = weighted.median(h9_resp_none$assess_convenience_cash, h9_resp_none$weight_4, na.rm = T))
# (assess_convenience_check_med = weighted.median(h9_resp_none$assess_convenience_check, h9_resp_none$weight_4, na.rm = T))
# (assess_convenience_credit_med = weighted.median(h9_resp_none$assess_convenience_credit, h9_resp_none$weight_4, na.rm = T))
# (assess_convenience_debit_med = weighted.median(h9_resp_none$assess_convenience_debit, h9_resp_none$weight_4, na.rm = T))
# (assess_convenience_prepaid_med = weighted.median(h9_resp_none$assess_convenience_prepaid, h9_resp_none$weight_4, na.rm = T))
# 
# #
# (assess_records_cash_med = weighted.median(h9_resp_none$assess_records_cash, h9_resp_none$weight_4, na.rm = T))
# (assess_records_check_med = weighted.median(h9_resp_none$assess_records_check, h9_resp_none$weight_4, na.rm = T))
# (assess_records_credit_med = weighted.median(h9_resp_none$assess_records_credit, h9_resp_none$weight_4, na.rm = T))
# (assess_records_debit_med = weighted.median(h9_resp_none$assess_records_debit, h9_resp_none$weight_4, na.rm = T))
# (assess_records_prepaid_med = weighted.median(h9_resp_none$assess_records_prepaid, h9_resp_none$weight_4, na.rm = T))
# 
# ## start assessment weighted average
# (assess_cost_cash_avg = weighted.mean(h9_resp_none$assess_cost_cash, h9_resp_none$weight_4, na.rm = T))
# (assess_cost_check_avg = weighted.mean(h9_resp_none$assess_cost_check, h9_resp_none$weight_4, na.rm = T))
# (assess_cost_credit_avg = weighted.mean(h9_resp_none$assess_cost_credit, h9_resp_none$weight_4, na.rm = T))
# (assess_cost_debit_avg = weighted.mean(h9_resp_none$assess_cost_debit, h9_resp_none$weight_4, na.rm = T))
# (assess_cost_prepaid_avg = weighted.mean(h9_resp_none$assess_cost_prepaid, h9_resp_none$weight_4, na.rm = T))
# #
# (assess_security_cash_avg = weighted.mean(h9_resp_none$assess_security_cash, h9_resp_none$weight_4, na.rm = T))
# (assess_security_check_avg = weighted.mean(h9_resp_none$assess_security_check, h9_resp_none$weight_4, na.rm = T))
# (assess_security_credit_avg = weighted.mean(h9_resp_none$assess_security_credit, h9_resp_none$weight_4, na.rm = T))
# (assess_security_debit_avg = weighted.mean(h9_resp_none$assess_security_debit, h9_resp_none$weight_4, na.rm = T))
# (assess_security_prepaid_avg = weighted.mean(h9_resp_none$assess_security_prepaid, h9_resp_none$weight_4, na.rm = T))
# 
# #
# (assess_convenience_cash_avg = weighted.mean(h9_resp_none$assess_convenience_cash, h9_resp_none$weight_4, na.rm = T))
# (assess_convenience_check_avg = weighted.mean(h9_resp_none$assess_convenience_check, h9_resp_none$weight_4, na.rm = T))
# (assess_convenience_credit_avg = weighted.mean(h9_resp_none$assess_convenience_credit, h9_resp_none$weight_4, na.rm = T))
# (assess_convenience_debit_avg = weighted.mean(h9_resp_none$assess_convenience_debit, h9_resp_none$weight_4, na.rm = T))
# (assess_convenience_prepaid_avg = weighted.mean(h9_resp_none$assess_convenience_prepaid, h9_resp_none$weight_4, na.rm = T))
# #
# (assess_records_cash_avg = weighted.mean(h9_resp_none$assess_records_cash, h9_resp_none$weight_4, na.rm = T))
# (assess_records_check_avg = weighted.mean(h9_resp_none$assess_records_check, h9_resp_none$weight_4, na.rm = T))
# (assess_records_credit_avg = weighted.mean(h9_resp_none$assess_records_credit, h9_resp_none$weight_4, na.rm = T))
# (assess_records_debit_avg = weighted.mean(h9_resp_none$assess_records_debit, h9_resp_none$weight_4, na.rm = T))
# (assess_records_prepaid_avg = weighted.mean(h9_resp_none$assess_records_prepaid, h9_resp_none$weight_4, na.rm = T))
# #
# 
# ## assessments into vectors
# ## Table of assessments by these resp: cash, prepaid, debit, credit
# table(h9_resp_none$assess_cost_cash)
# table(h9_resp_none$assess_cost_check)
# table(h9_resp_none$assess_cost_credit)
# table(h9_resp_none$assess_cost_debit)
# table(h9_resp_none$assess_cost_prepaid)
# 
# #
# table(h9_resp_none$assess_security_cash)
# table(h9_resp_none$assess_security_check)
# table(h9_resp_none$assess_security_credit)
# table(h9_resp_none$assess_security_debit)
# table(h9_resp_none$assess_security_prepaid)
# #
# table(h9_resp_none$assess_convenience_cash)
# table(h9_resp_none$assess_convenience_check)
# table(h9_resp_none$assess_convenience_credit)
# table(h9_resp_none$assess_convenience_debit)
# table(h9_resp_none$assess_convenience_prepaid)
# #
# table(h9_resp_none$assess_records_cash)
# table(h9_resp_none$assess_records_check)
# table(h9_resp_none$assess_records_credit)
# table(h9_resp_none$assess_records_debit)
# table(h9_resp_none$assess_records_prepaid)
# 
# # make the above into vectors of percentages
# (assess_cost_cash.vec = as.vector(prop.table(table(h9_resp_none$assess_cost_cash))))
# (assess_cost_check.vec = as.vector(prop.table(table(h9_resp_none$assess_cost_check))))
# (assess_cost_credit.vec = as.vector(prop.table(table(h9_resp_none$assess_cost_credit))))
# (assess_cost_debit.vec = as.vector(prop.table(table(h9_resp_none$assess_cost_debit))))
# (assess_cost_prepaid.vec = as.vector(prop.table(table(h9_resp_none$assess_cost_prepaid))))
# #
# (assess_security_cash.vec = as.vector(prop.table(table(h9_resp_none$assess_security_cash))))
# (assess_security_check.vec = as.vector(prop.table(table(h9_resp_none$assess_security_check))))
# (assess_security_credit.vec = as.vector(prop.table(table(h9_resp_none$assess_security_credit))))
# (assess_security_debit.vec = as.vector(prop.table(table(h9_resp_none$assess_security_debit))))
# (assess_security_prepaid.vec = as.vector(prop.table(table(h9_resp_none$assess_security_prepaid))))
# #
# (assess_convenience_cash.vec = as.vector(prop.table(table(h9_resp_none$assess_convenience_cash))))
# (assess_convenience_check.vec = as.vector(prop.table(table(h9_resp_none$assess_convenience_check))))
# (assess_convenience_credit.vec = as.vector(prop.table(table(h9_resp_none$assess_convenience_credit))))
# (assess_convenience_debit.vec = as.vector(prop.table(table(h9_resp_none$assess_convenience_debit))))
# (assess_convenience_prepaid.vec = as.vector(prop.table(table(h9_resp_none$assess_convenience_prepaid))))
# #
# (assess_records_cash.vec = as.vector(prop.table(table(h9_resp_none$assess_records_cash))))
# (assess_records_check.vec = as.vector(prop.table(table(h9_resp_none$assess_records_check))))
# (assess_records_credit.vec = as.vector(prop.table(table(h9_resp_none$assess_records_credit))))
# (assess_records_debit.vec = as.vector(prop.table(table(h9_resp_none$assess_records_debit))))
# (assess_records_prepaid.vec = as.vector(prop.table(table(h9_resp_none$assess_records_prepaid))))
# 
# # adding median and average assessment level to each vector
# (assess_cost_cash2.vec = c(100*assess_cost_cash.vec, assess_cost_cash_med, assess_cost_cash_avg))
# (assess_cost_check2.vec = c(100*assess_cost_check.vec, assess_cost_check_med, assess_cost_check_avg))
# (assess_cost_credit2.vec = c(100*assess_cost_credit.vec, assess_cost_credit_med, assess_cost_credit_avg))
# (assess_cost_debit2.vec = c(100*assess_cost_debit.vec, assess_cost_debit_med, assess_cost_debit_avg))
# (assess_cost_prepaid2.vec = c(100*assess_cost_prepaid.vec, assess_cost_prepaid_med, assess_cost_prepaid_avg))
# 
# #
# (assess_security_cash2.vec = c(100*assess_security_cash.vec, assess_security_cash_med, assess_security_cash_avg))
# (assess_security_check2.vec = c(100*assess_security_check.vec, assess_security_check_med, assess_security_check_avg))
# (assess_security_credit2.vec = c(100*assess_security_credit.vec, assess_security_credit_med, assess_security_credit_avg))
# (assess_security_debit2.vec = c(100*assess_security_debit.vec, assess_security_debit_med, assess_security_debit_avg))
# (assess_security_prepaid2.vec = c(100*assess_security_prepaid.vec, assess_security_prepaid_med, assess_security_prepaid_avg))
# 
# #
# (assess_convenience_cash2.vec = c(100*assess_convenience_cash.vec, assess_convenience_cash_med, assess_convenience_cash_avg))
# (assess_convenience_check2.vec = c(100*assess_convenience_check.vec, assess_convenience_check_med, assess_convenience_check_avg))
# (assess_convenience_credit2.vec = c(100*assess_convenience_credit.vec, assess_convenience_credit_med, assess_convenience_credit_avg))
# (assess_convenience_debit2.vec = c(100*assess_convenience_debit.vec, assess_convenience_debit_med, assess_convenience_debit_avg))
# (assess_convenience_prepaid2.vec = c(100*assess_convenience_prepaid.vec, assess_convenience_prepaid_med, assess_convenience_prepaid_avg))
# #
# (assess_records_cash2.vec = c(100*assess_records_cash.vec, assess_records_cash_med, assess_records_cash_avg))
# (assess_records_check2.vec = c(100*assess_records_check.vec, assess_records_check_med, assess_records_check_avg))
# (assess_records_credit2.vec = c(100*assess_records_credit.vec, assess_records_credit_med, assess_records_credit_avg))
# (assess_records_debit2.vec = c(100*assess_records_debit.vec, assess_records_debit_med, assess_records_debit_avg))
# (assess_records_prepaid2.vec = c(100*assess_records_prepaid.vec, assess_records_prepaid_med, assess_records_prepaid_avg))
# #
# 
# # Finalizing Table 3 (assessments)
# (assess.df = rbind(assess_cost_cash2.vec, assess_cost_check2.vec, assess_cost_credit2.vec, assess_cost_debit2.vec, assess_cost_prepaid2.vec))
# dim(assess.df)
# (assess2.df = rbind(assess.df, assess_security_cash2.vec, assess_security_check2.vec, assess_security_credit2.vec, assess_security_debit2.vec, assess_security_prepaid2.vec))
# dim(assess2.df)
# (assess3.df = rbind(assess2.df, assess_convenience_cash2.vec, assess_convenience_check2.vec, assess_convenience_credit2.vec, assess_convenience_debit2.vec,  assess_convenience_prepaid2.vec))
# (assess4.df = rbind(assess3.df, assess_records_cash2.vec, assess_records_check2.vec, assess_records_credit2.vec, assess_records_debit2.vec, assess_records_prepaid2.vec))
# assess4.df = assess3.df
# (colnames(assess4.df) = c("1 (%)", "2 (%)", "3 (%)", "4 (%)", "5 (%)", "Median", "Average"))
# assess4.df
# # rename rows
# (assess5.df = as.data.frame(assess4.df))
# #(rownames(assess4.df) = c("Cash cost assessment", "Prepaid cost assessment", "Debit cost assessment", "Credit cost assessment", "Cash security assessment", "Prepaid security assessment", "Debit security assessment", "Credit security assessment", "Cash convenience assessment", "Prepaid convenience assessment", "Debit convenience assessment", "Credit convenience assessment", "Cash record assessment", "Prepaid record assessment", "Debit record assessment", "Credit record assessment"))# with record
# (rownames(assess5.df) = c("Cash cost assessment", "Check cost assessment",  "Credit cost assessment", "Debit cost assessment", "Prepaid cost assessment",  "Cash security assessment", "Check security assessment", "Credit security assessment", "Debit security assessment", "Prepaid security assessment",  "Cash convenience assessment", "Check convenience assessment", "Credit convenience assessment", "Debit convenience assessment", "Prepaid convenience assessment"))# without record
# 
# assess5.df
# str(assess5.df)
# (colnames(assess5.df) = c("1 (%)", "2 (%)", "3 (%)", "4 (%)", "5 (%)", "Median(w)", "Average(w)"))
# dim(assess5.df)
# 
# #(digitm = matrix(rep(c(0, rep(2,7+1)),12), nrow = 12, ncol = 8+1, byrow = T))
# #
# #print(xtable(assess4.df, digits = 2), include.rownames = T, hline.after = c(0,4,8,12,16))# with record
# print(xtable(assess5.df, digits = 1), include.rownames = T, hline.after = c(0,5,10,15))# Table 3
# 
# #caption of Table 3, num of respondents
# nrow(h9_none)# num payments
# length(unique(h9_none$uasid))# num respo
# #
# # for paper discussion of Table 3  (assessments):
# round(assess5.df[1,1] + assess5.df[1,2], 1)# frac who assess cost as low
# round(assess5.df[3,1] + assess5.df[3,2], 1)# frac who assess debit as low
# sum(assess5.df[1,1:5])# check sum to 100%
# sum(assess5.df[3,1:5])
# 
# 
# ### Simulation 3: Same as experiment 1, except now using assessment by consumers who do not have debit or credit cards
# ## Simulation 3: None: compute CS *before* adding debit based equation (3.10) on page 55 Train's book 
# 
# h9_none$v_cash = NA # # estimated utility from paying cash
# h9_none$v_cash = both_ml_coef[1]*assess_cost_cash_med + both_ml_coef[2]*assess_security_cash_med + both_ml_coef[3]*assess_convenience_cash_med 
# # since all consumers get the same utilty, define
# (v_cash = h9_none$v_cash[1])# equation (4) in paper
# #
# h9_none$v_debit = NA # # estimated utility from paying unsbusidized debit
# h9_none$v_debit = both_ml_coef[1]*assess_cost_debit_med + both_ml_coef[2]*assess_security_debit_med + both_ml_coef[3]*assess_convenience_debit_med 
# # since all consumers get the same utilty, define
# (v_debit = h9_none$v_debit[1])# equation (4) in paper
# #
# h9_none$v_prepaid = NA # # estimated utility from paying v_prepaid 
# h9_none$v_prepaid = both_ml_coef[1]*assess_cost_prepaid_med + both_ml_coef[2]*assess_security_prepaid_med + both_ml_coef[3]*assess_convenience_prepaid_med 
# # since all consumers get the same utilty, define
# (v_prepaid = h9_none$v_prepaid[1])# equation (7?) in paper
# #
# head(h9_none[, c("uasid", "pi", "v_cash", "v_debit", "v_prepaid")], 20)
# 
# 
# (mui = 1) # marginal utility of income = 1 (no use for that b/c only percentage change in CS is computed), same as logsum
# h9_none$cs = mui*log(exp(h9_none$v_cash) + 0 + exp(h9_none$v_prepaid)) # cash is ref pi here, 0 is for missing debit
# #
# # CS *after* debit option is added to none consumers 
# h9_none$cs_debit =  mui*log(exp(h9_none$v_cash) + exp(h9_none$v_debit) + exp(h9_none$v_prepaid))
# #
# # Difference in CS (with debit - before with no debit)
# h9_none$cs_diff = h9_none$cs_debit - h9_none$cs
# # rate of change
# h9_none$cs_rate = h9_none$cs_diff/h9_none$cs
# #
# length(h9_none$cs)# by payments
# summary(h9_none$cs) # CS before adding unsubsidized debit
# summary(h9_none$cs_debit)# CS after adding unsubsidized debit
# summary(h9_none$cs_diff)
# summary(h9_none$cs_rate)# Equation (8?) in paper
# head(h9_none[, c("pi", "cs", "cs_debit", "cs_diff", "cs_rate")])
# 
# ## Now compute CS assuming that cost debit =1 (subsidized) (call it "cost1" instead of "after". Basically, subsidized debit card is added)
# # cost of debit before the subsidy
# assess_cost_debit_med
# h9_none$v_debit_cost1 = both_ml_coef[1]*1 + both_ml_coef[2]*assess_security_debit_med + both_ml_coef[3]*assess_convenience_debit_med 
# # since all consumers gain the same utility, define
# (v_debit_cost1 = h9_none$v_debit_cost1[1])# utility of debit w/ subsidy repored in equation (4)
# #
# # CS *after* debit option is added to none consumers and cost debit =1
# h9_none$cs_debit_cost1 =  mui*(log(exp(h9_none$v_cash) + exp(h9_none$v_debit_cost1) + exp(h9_none$v_prepaid)))
# #
# # Difference in CS (with debit with cost=1 - with no debit)
# h9_none$cs_diff_cost1 = h9_none$cs_debit_cost1 - h9_none$cs
# # rate of change
# h9_none$cs_rate_cost1 = h9_none$cs_diff_cost1/h9_none$cs
# #
# length(h9_none$cs_diff_cost1)# by payments
# summary(h9_none$cs) # CS before the change
# summary(h9_none$cs_debit_cost1) # CS after debit with cost=1 was added
# summary(h9_none$cs_diff_cost1)
# summary(h9_none$cs_rate_cost1)# equation (6) in paper, V^S
# (h9_none_change_rate_med_cost1 = median(h9_none$cs_rate_cost1, na.rm = T))
# 
# ### Simulation 4: A different simulation: Do not add debit, but subsidize prepaid card
# ## Now compute CS assuming that cost prepaid =1 (subsidized) (call it "cost1" instead of "after". Basically, prepaid cards are not subsidized)
# # cost of prepaid before the subsidy
# assess_cost_prepaid_med # not used
# # New cost of prepaid after subsidy
# h9_none$v_prepaid_cost2 = both_ml_coef[1]*1 + both_ml_coef[2]*assess_security_prepaid_med + both_ml_coef[3]*assess_convenience_prepaid_med 
# #
# # utility of prepaid w/ subsidy repored in equation (4)
# (v_prepaid_cost2 = h9_none$v_prepaid_cost2[2])
# #
# # CS *after* the prepaid subsidy to none consumers and cost debit =1
# h9_none$cs_prepaid_cost2 =  mui*(log(exp(h9_none$v_cash) + exp(h9_none$v_prepaid_cost2)))
# #
# # Difference in CS (with prepaid with cost=1 - before the subsidy
# h9_none$cs_diff_cost2 = h9_none$cs_prepaid_cost2 - h9_none$cs
# # rate of change
# h9_none$cs_rate_cost2 = h9_none$cs_diff_cost2/h9_none$cs
# #
# length(h9_none$cs_diff_cost2)# by payments
# summary(h9_none$cs) # CS before the change
# summary(h9_none$cs_prepaid_cost2) # CS after prepaid with cost=1 was added
# summary(h9_none$cs_diff_cost2)
# summary(h9_none$cs_rate_cost2)# equation (?) in paper, V^S
# (h9_none_change_rate_med_cost2 = median(h9_none$cs_rate_cost2, na.rm = T))




#################
### End of income code ###

