library(tidyverse)
library(survey)
library(haven)

# adding mortality data
setwd("xxxxxx")

# we use the weighting for wave g because mortality weighting only go up to wave 9

mortus <- read_dta("xwavedat.dta") %>%
  select("pidp", "g_mortus_tw")


# loading base wave data

c_indresp <- read_dta("c_indresp.dta") %>%
  select("c_scghq1_dv", "c_sf12mcs_dv","c_attacked_dv",
         "c_avoided_dv", "c_insulted_dv", "c_unsafe_dv" ,"c_voteintent",
         "c_dvage","c_psu","c_strata", "c_vote3", "c_distmov_dv",
         "c_hiqual_dv", "c_jbstat","c_scghqb","c_pno","pidp","c_dvage", 
         "c_nbrsnci_dv","c_vote3", "c_distmov_dv","c_hiqual_dv", 
         "c_jbstat","c_closenum","c_vote2","c_vote4", "c_indscub_lw",
         "c_ethn_dv", "c_sex_dv", "c_urban_dv", "c_basrest", "c_fimnsben_dv")
rm(adjusted)
# tenure is in hhresp, consider

write.csv(c_indresp, file = "first wave indresp data for weighting.csv",row.names = FALSE)

read.csv("first wave indresp data for weighting.csv")

c_indresp <- c_indresp %>% 
  distinct(pidp, .keep_all = TRUE)

cf <- c_indresp %>%
  inner_join(read_dta("f_indresp.dta"), by = "pidp") %>%
  select(matches("^c_"),f_voteintent,pidp)

cfi <- cf %>%
  inner_join(read_dta("i_indresp.dta"), by = "pidp") %>%
  select(matches("^c_"),matches("^f_"),
         i_voteintent,i_psu,i_strata,i_indscub_lw,pidp)


rm(c_indresp, cf, adjusted)

# comparing unweighted sample size with existing sample size

cfi %>%
  filter(c_voteintent >= 1 & c_voteintent <= 10,
         f_voteintent >= 1 & f_voteintent <= 10,
         i_voteintent >= 1 & i_voteintent <= 10) %>%
  summarise(count = n_distinct(pidp))

# unweighted count of valid voteintent values is 15,750 initially

cfi %>%
  filter(c_voteintent >= 1 & c_voteintent <= 10,
         f_voteintent >= 1 & f_voteintent <= 10,
         i_voteintent >= 1 & i_voteintent <= 10,
         i_indscub_lw > 0) %>%
  summarise(count = n_distinct(pidp))

# weighted count of valid values is 10,152, a significant reduction by around 1/3


# Creating tailored weight

# We would use the longitudinal weight from wave c as the base weight in this case

# First we need to look at variables _finloc, _ivfho, _ivfio

c_indsamp <- read_dta("c_indsamp.dta") %>%
  select("pidp","c_finloc","c_ivfho","c_ivfio")

d_indsamp <- read_dta("d_indsamp.dta") %>%
  select("pidp","d_finloc","d_ivfho","d_ivfio")

e_indsamp <- read_dta("e_indsamp.dta") %>%
  select("pidp","e_finloc","e_ivfho","e_ivfio")

f_indsamp <- read_dta("f_indsamp.dta") %>%
  select("pidp","f_finloc","f_ivfho","f_ivfio")

g_indsamp <- read_dta("g_indsamp.dta") %>%
  select("pidp","g_finloc","g_ivfho","g_ivfio")

h_indsamp <- read_dta("h_indsamp.dta") %>%
  select("pidp","h_finloc","h_ivfho","h_ivfio")

i_indsamp <- read_dta("i_indsamp.dta") %>%
  select("pidp","i_finloc","i_ivfho","i_ivfio")

unique(c_indsamp$c_finloc)
unique(c_indsamp$c_ivfio)
unique(c_indsamp$c_ivfho)

mortality <- merge(c_indsamp, d_indsamp, by = "pidp")

mortality <- merge(mortality, e_indsamp, by = "pidp")

additional_dfs <- list(f_indsamp, g_indsamp, h_indsamp, i_indsamp)

# Loop through the list and merge each data frame with 'oos'
for(df in additional_dfs) {
  mortality <- merge(mortality, df, by = "pidp")
}

# create variable for out scope individuals

mortality <- mortality %>%
  mutate(oos = if_else(c_finloc == 2 | c_ivfho %in% c(81,90) | c_ivfio %in% c(99,54)|
                       d_finloc == 2 | d_ivfho %in% c(81,90) | d_ivfio %in% c(99,54)|
                       e_finloc == 2 | e_ivfho %in% c(81,90) | e_ivfio %in% c(99,54)|
                       f_finloc == 2 | f_ivfho %in% c(81,90) | f_ivfio %in% c(99,54)|
                       g_finloc == 2 | g_ivfho %in% c(81,90) | g_ivfio %in% c(99,54)|
                       h_finloc == 2 | h_ivfho %in% c(81,90) | h_ivfio %in% c(99,54)|
                       i_finloc == 2 | i_ivfho %in% c(81,90) | i_ivfio %in% c(99,54), 1, 0))


# 660 individuals will be dropped due to being out-of-scope

nrow(mortality)

# this will result in a sample size of 58553 - 660

mort.weight <- mortality[mortality$oos == 0,]

merged.mort <- merge(mort.weight, mortus, by = "pidp")

merged.mort1 <- merge(merged.mort, cfi, by = "pidp")

rm(c_indsamp,d_indsamp,e_indsamp,f_indsamp,g_indsamp,h_indsamp,i_indsamp, 
   merged.mort, mort.weight, mortality, additional_dfs, df)


# creating starting weight

merged.mort1$start_wgt <- merged.mort1$g_mortus_tw*merged.mort1$i_indscub_lw

# creating tailored weights

# starting by adding a variable for response

merged.mort1$resp1 <- ifelse(merged.mort1$f_ivfio == 1 & merged.mort1$i_ivfio == 1 & 
                         merged.mort1$start_wgt != 0, 1, 0)

# now predict non-response

# predictors for non-response must come from first wave and should
# not include any missingness. Can use multiple imputation to avoid it 

# predictors should be related to topic of study with no multicollinearity

# Recoding missing values in data as NA
# subjective wellbeing scale
 
unique(merged.mort1$c_scghq1_dv)

merged.mort1$c_scghq1_dv[merged.mort1$c_scghq1_dv <0] <- NA

table(merged.mort1$c_scghq1_dv)

# whether the respondent has been attacked

unique(merged.mort1$c_attacked_dv)

merged.mort1$c_attacked_dv[merged.mort1$c_attacked_dv %in% c(-9,-8,-1)] <- 0
merged.mort1$c_attacked_dv[merged.mort1$c_attacked_dv %in% c(-7,-2)] <- NA

table(merged.mort1$c_attacked_dv)

# age

unique(merged.mort1$c_dvage)

merged.mort1$c_dvage[merged.mort1$c_dvage <16] <- NA

table(merged.mort1$c_dvage)

# distance moved since last wave

unique(merged.mort1$c_distmov_dv)
table(merged.mort1$c_distmov_dv)

merged.mort1$c_distmov_dv[merged.mort1$c_distmov_dv %in% c(-8,-1)] <- 0
sum(merged.mort1$c_distmov_dv == 0)

merged.mort1$c_distmov_dv[merged.mort1$c_distmov_dv <0] <- NA

# creating a variable for whether the respondent has stayed at the same address since the
# last wave
merged.mort1$longstanding <- ifelse(merged.mort1$c_distmov_dv > 0, 0, 1)

# job status

unique(merged.mort1$c_jbstat)

merged.mort1$c_jbstat[merged.mort1$c_jbstat <1] <- NA

table(merged.mort1$c_jbstat)

# highest qualification

unique(merged.mort1$c_hiqual_dv)
merged.mort1$c_hiqual_dv[merged.mort1$c_hiqual_dv %in% c(-9,-8)] <- NA
table(merged.mort1$c_hiqual_dv)

# neighbourhood cohesion (proxy for social trust)

unique(merged.mort1$c_nbrsnci_dv)

merged.mort1$c_nbrsnci_dv[merged.mort1$c_nbrsnci_dv <1] <- NA

table(merged.mort1$c_nbrsnci_dv)

# whether the respondent has any close friends

unique(merged.mort1$c_closenum)
merged.mort1$c_closenum[merged.mort1$c_closenum <0] <- NA

# sex is male or female

unique(merged.mort1$c_sex_dv)
merged.mort1$c_sex_dv[merged.mort1$c_sex_dv <1] <- NA

# ethnicity

unique(merged.mort1$c_ethn_dv)
merged.mort1$c_ethn_dv[merged.mort1$c_ethn_dv <1] <- NA

# urban or rural
unique(merged.mort1$c_urban_dv)
merged.mort1$c_urban_dv[merged.mort1$c_urban_dv <1] <- NA

# whether they receive social benefit income
unique(merged.mort1$c_fimnsben_dv)
merged.mort1$c_fimnsben_dv[merged.mort1$c_fimnsben_dv %in% c(-9,-8,-1)] <- 0
merged.mort1$c_fimnsben_dv[merged.mort1$c_fimnsben_dv == -2] <- NA

# checking for no. of na variables amongst columns

sum(is.na(merged.mort1$c_scghq1_dv)) # 3344
sum(is.na(merged.mort1$c_attacked_dv)) # 1095
sum(is.na(merged.mort1$c_dvage)) # none
sum(is.na(merged.mort1$c_distmov)) # none
sum(is.na(merged.mort1$c_voteintent)) # none
sum(is.na(merged.mort1$jbstat)) # none 
sum(is.na(merged.mort1$c_hiqual_dv)) # 126
sum(is.na(merged.mort1$c_nbrscnci_dv)) # none
sum(is.na(merged.mort1$c_closenum)) # 1292
sum(is.na(merged.mort1$c_fimnsben_dv)) # none
sum(is.na(merged.mort1$c_urban_dv)) # two
sum(is.na(merged.mort1$c_ethn_dv)) # 17
sum(is.na(merged.mort1$c_sex_dv)) # none


# removing haven labels

merged.mort1$c_scghq1_dv <- as.numeric(as_factor(merged.mort1$c_scghq1_dv))
merged.mort1$c_attacked_dv <- as.numeric(as_factor(merged.mort1$c_attacked_dv))
merged.mort1$c_urban_dv <- as.numeric(as_factor(merged.mort1$c_urban_dv))
merged.mort1$c_ethn_dv <- as.numeric(as_factor(merged.mort1$c_ethn_dv))
merged.mort1$c_nbrsnci_dv <- as.numeric(as_factor(merged.mort1$c_nbrsnci_dv))
merged.mort1$c_hiqual_dv <- as.numeric(as_factor(merged.mort1$c_hiqual_dv))
table(merged.mort1$c_hiqual_dv)

library(mice)
# multiple imputation 
imputed_data <- 
  mice(merged.mort1[, c("c_scghq1_dv", "c_attacked_dv", "c_urban_dv","c_ethn_dv", "c_nbrsnci_dv", "c_hiqual_dv")],m=5, method='pmm', maxit=50)

completed_data <- complete(imputed_data, 1)

rm(imputed_data)

merged.mort1$c_scghq1_dv[is.na(merged.mort1$c_scghq1_dv)] <- completed_data$c_scghq1_dv[is.na(merged.mort1$c_scghq1_dv)]
merged.mort1$c_attacked_dv[is.na(merged.mort1$c_attacked_dv)] <- completed_data$c_attacked_dv[is.na(merged.mort1$c_attacked_dv)]
merged.mort1$c_urban_dv[is.na(merged.mort1$c_urban_dv)] <- completed_data$c_urban_dv[is.na(merged.mort1$c_urban_dv)]
merged.mort1$c_ethn_dv[is.na(merged.mort1$c_ethn_dv)] <- completed_data$c_ethn_dv[is.na(merged.mort1$c_ethn_dv)]
merged.mort1$c_nbrsnci_dv[is.na(merged.mort1$c_nbrsnci_dv)] <- completed_data$c_nbrsnci_dv[is.na(merged.mort1$c_nbrsnci_dv)]
merged.mort1$c_hiqual_dv[is.na(merged.mort1$c_hiqual_dv)] <- completed_data$c_hiqual_dv[is.na(merged.mort1$c_hiqual_dv)]


# now there should be no NAs for the relevant variables

########
# predicting nonresponse with logit

df_subset <- subset(merged.mort1, start_wgt != 0)


#recode longstanding and c_nbrscnci

model <- glm(resp1 ~ c_scghq1_dv + as.factor(c_sex_dv) + c_distmov_dv + 
               as.factor(c_hiqual_dv) + as.factor(c_jbstat) + c_fimnsben_dv + 
               longstanding + c_attacked_dv + as.factor(c_sex_dv) + 
               as.factor(c_ethn_dv) + as.factor(c_urban_dv) + c_nbrsnci_dv + 
               c_dvage + c_indscub_lw,
               family=binomial(link="logit"), data=df_subset)

summary(model)

library("pROC")

# plotting an ROC plot to evaluate the model

predicted_probs <- predict(model, type = "response")
roc_obj <- roc(df_subset$resp1, predicted_probs)
plot(roc_obj, main="ROC Curve", col="#1c61b6", lwd=2)

auc_value <- auc(roc_obj)
plot(roc_obj, main="ROC Curve for the Nonresponse Model")
text(x=0.2, y=0.2, paste("AUC =", round(auc_value, 3)), cex=1.2)

# the model returns a AUC value of 0.843, which indicates it is a good measure
# of separability and can differentiate between response and nonresponse

# now using predictions with nonzero weights to predict likelihood of response

prediction_subset <- subset(df_subset, resp1 == 1 & start_wgt != 0)

prob_predictions <- predict(model, newdata=prediction_subset, type="response")

prediction_subset$prob1 <- prob_predictions

# creating new weights that account for nonresponse based on predictions

prediction_subset$adj1 <- with(prediction_subset, ifelse(prob_predictions == 0, NA, 1/prob_predictions))
prediction_subset$i_indscub_lw1 <- with(prediction_subset, start_wgt * adj1)
prediction_subset$i_indscub_lw1[prediction_subset$i_indscub_lw1 == 0] <- NA

# checking new weight vs old weight

#prediction_subset <- merge(prediction_subset, merged.mort1, by = "pidp")

suboptimal_weight_count <- nrow(subset(prediction_subset, c_indscub_lw !=0 & i_indscub_lw > 0))
cat("Suboptimal weight count:", suboptimal_weight_count, "\n")

# optimal weight count

new_weight_count <- nrow(subset(prediction_subset, c_indscub_lw !=0 & i_indscub_lw1 != 0))
cat("New weight count:", new_weight_count, "\n")

library("survey")


sd1 <- svydesign(ids = ~i_psu, weights = ~i_indscub_lw, strata = ~i_strata, 
                              data = prediction_subset, nest = TRUE, singleunit = "adjust")


sd2 <- svydesign(ids = ~i_psu, weights = ~i_indscub_lw1, strata = ~i_strata,
                              data = prediction_subset, nest = TRUE, singleunit = "adjust")


options(survey.lonely.psu="adjust")


svymean(~c_voteintent, design = sd1)

svymean(~c_voteintent, design = sd2)

prediction_subset %>%
  filter(c_voteintent >= 1 & c_voteintent <= 10,
         f_voteintent >= 1 & f_voteintent <= 10,
         i_voteintent >= 1 & i_voteintent <= 10,
         i_indscub_lw1 > 0)  %>%
  summarise(count = n_distinct(pidp))

# the tailored weights do not result in any new nonzero weights, they also compromise on accuracy
# increasing the standard error. However, they could still be valuable to reduce
# bias in my estimates by correcting for differences between response and 
# nonresponse groups.


# creating a dataset with just new weights and cross-wave personal identifiers

adjusted.weight <- prediction_subset[, c("pidp","i_indscub_lw","i_indscub_lw1")]

write.csv(adjusted.weight, "adjusted.weight.csv", row.names = FALSE)

adjusted <- read.csv("adjusted.weight.csv")


# now find the mean weights for recurring pidp values so the data is mergeable

mean_weights <- adjusted %>%
  group_by(pidp) %>%
  summarise(mean_i_indscub_lw1 = mean(i_indscub_lw1))

mean_weights %>%
  group_by(pidp) %>%  
  filter(n() > 1) %>%
  ungroup() %>%
  count(pidp)

write.csv(mean_weights, "new.weights.csv", row.names = FALSE)
