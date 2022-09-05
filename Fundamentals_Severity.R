# Final code for fundamentals file

# set working directory and load libraries
setwd("~/Desktop/Capstone Project/Fundamentals Data")
library(tidyverse)
library(openxlsx)
library(caret)

#---------------------
# load raw data
fund <- read.csv("fundamentals.csv")
fundsec <- fund  # Do not filter for technical sector for severity estimation
fundsec_std <- filter(fundsec, datafmt == "STD") # filter for reporting format

#--------------------
# perform feature selection by calculating missingness
missingness <- as.matrix(sapply(fundsec_std, 
                                function(x) sum(is.na(x))/nrow(fundsec_std)*100))
missing <- as.data.frame(cbind(Mnemomic = dimnames(missingness)[[1]] , 
                               perc = missingness)) # dataframe of % missingness
key_vars <- missing[as.numeric(missing$V2) <= 100,] # filter for less than 10%
fundsec_keyvars <- fundsec[,key_vars$Mnemomic] # subset fundsec for keyvars
fundsec_keyvars_std <- fundsec_keyvars[fundsec_keyvars$datafmt=="STD",] # STD reporting
remove(missing, missingness, key_vars) # clean up

#--------------------
# perform feature selection
# based on key ratio generation
fundsec_keyvars2 <- select (fundsec_keyvars, c(gvkey, fyear, datafmt, 
                                               tic, conm, revt, ni,
                                               at, act, lct, ch, dltt, 
                                               teq , sale, capx, dvt, 
                                               gdwl, wcap, intan,
                                               auop, pi))
# added ap, auop
# need to re-level auop,  1=unqualified opionion, 2 = otherwise

fundsec_keyvars2_std <- filter(fundsec_keyvars2, datafmt == "STD")
fundsec_keyvars2_nonstd <- filter(fundsec_keyvars2, datafmt == "SUMM_STD")
remove(fundsec, fundsec_std, fundsec_keyvars, fundsec_keyvars_std)

#-----------------------------------------
# feature engineering - producing KPI ratios

fund_df_final <- fundsec_keyvars2_std
fund_df_final <- mutate (fund_df_final, 
                         tang = (at-intan) / at,
                         npmargin = ni / revt,
                         roa = ni / at,
                         curr = act / lct,
                         opcfratio = ch / lct,
                         debt_assets = dltt / at,
                         tat = sale/at)

  # tang, tangibility of assets (what proportion of assets are tangible?)
# npmargin, net profit margin, net profits/total revenue
# roa, return on assets = ni / at = net income / total assets
# curr, current ratio = act / lct = current assetts / current liabilities
# opcfratio, operating cash flow ratio = cash flow / current liabilities
# debt_assets, debt to assets ratio = dltt / at = total debt / total assets
# tat, total asset turnover = sale/at = sales/total assets

# Convert the npmargin for all zero revenue companies to zero
fund_df_final$npmargin <- ifelse(fund_df_final$revt==0,
                                 0, fund_df_final$npmargin)

# Asset tangibility set to zero for firms with zero assets
fund_df_final$tang <- ifelse(fund_df_final$at==0,
                                 0, fund_df_final$tang)

# ROA set to zero for firms with zero assets
fund_df_final$roa <- ifelse(fund_df_final$at==0,
                             0, fund_df_final$roa)

# if both debt and assets are zero, set debt_asset ratio to zero 
fund_df_final$debt_assets <- ifelse(fund_df_final$at==0 & 
                                    fund_df_final$dltt==0,
                                    0, fund_df_final$debt_assets)

# For firms with zero sales,  it is safe to say they have a tat of zero
fund_df_final$tat <- ifelse(fund_df_final$sale==0,
                            0, fund_df_final$tat)

# Convert NaN and Inf to zero values.
# these arise from dividing by zero and dividing zero.
# Assign zero to these ratios.

fund_df_final[is.nan(fund_df_final$tang), "tang"] <- 0 
fund_df_final[is.nan(fund_df_final$npmargin), "npmargin"] <- 0 
fund_df_final[is.nan(fund_df_final$roa), "roa"] <- 0 
fund_df_final[is.nan(fund_df_final$opcfratio), "opcfratio"] <- 0
fund_df_final[is.nan(fund_df_final$debt_assets), "debt_assets"] <- 0
fund_df_final[is.nan(fund_df_final$tat), "tat"] <- 0
fund_df_final[is.nan(fund_df_final$curr), "curr"] <- 0

#Inf Values
fund_df_final[is.infinite(fund_df_final$tang), "tang"] <- 0 
fund_df_final[is.infinite(fund_df_final$npmargin), "npmargin"] <- 0 
fund_df_final[is.infinite(fund_df_final$roa), "roa"] <- 0 
fund_df_final[is.infinite(fund_df_final$opcfratio), "opcfratio"] <- 0
fund_df_final[is.infinite(fund_df_final$debt_assets), "debt_assets"] <- 0
fund_df_final[is.infinite(fund_df_final$tat), "tat"] <- 0
fund_df_final[is.infinite(fund_df_final$curr), "curr"] <- 0
 
fund_df_final[is.na(fund_df_final$auop), "auop"] <- 0

#------------------------------------------------------------------------
# Aggregate data by gvkey and calculate the mean and 
# coefficient of variation for each numeric.  Use pop sd - to afford zero for single values

fund_final_agg <- 
  group_by(fund_df_final, gvkey, tic, conm) %>%
  summarize (revt_mean = mean(revt),
             revt_cv = (sqrt(sum((revt-mean(revt))^2)/n()))/mean(revt),
             at_mean = mean(at),
             at_cv = (sqrt(sum((at-mean(at))^2)/n()))/mean(at),
             act_mean = mean(act),
             act_cv = (sqrt(sum((act-mean(act))^2)/n()))/mean(act),
             lct_mean = mean(lct),
             lct_cv = (sqrt(sum((lct-mean(lct))^2)/n()))/mean(lct),
             ch_mean = mean(ch),
             ch_cv = (sqrt(sum((ch-mean(ch))^2)/n()))/mean(ch),
             teq_mean = mean(teq),
             teq_cv = (sqrt(sum((teq-mean(teq))^2)/n()))/mean(teq),
             capx_mean = mean(capx),
             capx_cv = (sqrt(sum((capx-mean(capx))^2)/n()))/mean(capx),
             dvt_mean = mean(dvt),
             dvt_cv = (sqrt(sum((dvt-mean(dvt))^2)/n()))/mean(dvt),
             gdwl_mean = mean(gdwl),
             gdwl_cv = (sqrt(sum((gdwl-mean(gdwl))^2)/n()))/mean(gdwl),
             wcap_mean = mean(wcap),
             wcap_cv = (sqrt(sum((wcap-mean(wcap))^2)/n()))/mean(wcap),
             intan_mean = mean(intan),
             intan_cv = (sqrt(sum((intan-mean(intan))^2)/n()))/mean(intan),
             tang_mean = mean(tang),
             tang_cv = (sqrt(sum((tang-mean(tang))^2)/n()))/mean(tang),
             npmargin_mean = mean(npmargin),
             npmargin_cv = (sqrt(sum((npmargin-mean(npmargin))^2)/n()))/mean(npmargin),
             roa_mean = mean(roa),
             roa_cv = (sqrt(sum((roa-mean(roa))^2)/n()))/mean(roa),
             curr_mean = mean(curr),
             curr_cv = (sqrt(sum((curr-mean(curr))^2)/n()))/mean(curr),
             opcfratio_mean = mean(opcfratio),
             opcfratio_cv = (sqrt(sum((opcfratio-mean(opcfratio))^2)/n()))/mean(opcfratio),
             debt_assets_mean = mean(debt_assets),
             debt_assets_cv = (sqrt(sum((debt_assets-mean(debt_assets))^2)/n()))/mean(debt_assets),
             tat_mean = mean(tat),
             tat_cv = (sqrt(sum((tat-mean(tat))^2)/n()))/mean(tat),
             auop_avg = ifelse(sum(auop)/n()==1, "Unqualified", "Otherwise")
  )

fund_final_agg <- fund_final_agg %>%
mutate_all(~replace(., is.nan(.), 0))

# clean up aggregated data
fund_final_agg$auop_avg <- as.factor(fund_final_agg$auop_avg)

# remove any companies with all NA values for key numerics
fund_final_agg <- na.omit(fund_final_agg)

#----------------------------------------------------------
# Restatements
# Any material (>5%) changes in sales, assets, equity, capital or income?


fund_restate_df <- select(fundsec_keyvars2, gvkey, fyear, 
                          datafmt, tic, conm, sale, 
                          at, teq, wcap, pi) # select key metrics 

fund_og <- filter(fund_restate_df, datafmt=="STD") # split into standard report
fund_restated <- filter(fund_restate_df, datafmt=="SUMM_STD") # restated report
fund_restated <- select(fund_restated, -c("tic", "conm", "datafmt"))

colnames(fund_restated) <- c("gvkey", "fyear",
                             "sale_restate",
                             "at_restate",
                             "teq_restate", "wcap_restate",
                             "pi_restate") # change var names for restated metrics

fund_merge <- merge(fund_og, fund_restated, by=c("gvkey", "fyear"), 
                    suffixes = FALSE) # merge df's

fund_merge <- mutate(fund_merge,
                     sale_percdiff = (sale_restate-sale)/sale*100,
                     at_percdiff = (at_restate-at)/at*100,
                     teq_percdiff = (teq_restate-teq)/teq*100,
                     wcap_percdiff = (wcap_restate-wcap)/wcap*100,
                     pi_percdiff = (pi_restate-pi)/pi*100) # calc perc differences

# create a binary variable for any metric that changed by 5% 
fund_merge <- mutate(fund_merge,
                     restate_binary = ifelse(
                       sale_percdiff < -5 |
                         at_percdiff < -5 |
                         teq_percdiff < -5 |
                         wcap_percdiff < -5 |
                         pi_percdiff < -5 , 1, 0))



# aggregate by gvkey, provide new var that indicates 1 for any restatement
fund_merge_grouped <- group_by(fund_merge, gvkey) %>%
  summarize(sum_restate=sum(restate_binary)) %>% 
  mutate(restate_bin = ifelse(sum_restate>1, 1, 0))

fund_merge_grouped$restate_bin <- 
  replace_na(fund_merge_grouped$restate_bin, 0) 

# assume that the NA's do not represent a restatement

fund_merge_grouped$sum_restate <- NULL # remove superfluous column

#-------------------------------------------------------------------
# merge and clean up

fund_df_merged <- merge(fund_final_agg, fund_merge_grouped, 
                        "gvkey", all.x = TRUE)

fund_df_merged$restate_bin <- 
  replace_na(fund_df_merged$restate_bin, 0) 

remove(fund_df_final, fund_final_agg, fund_merge_grouped, 
       fund_merge, fund_og, fund_restate_df, fund_restated)

fund_df_merged$restate_bin <- as.factor(fund_df_merged$restate_bin)
fund_df_merged$gvkey <- as.character(fund_df_merged$gvkey) # for scaling

#-------------------------------------------------------------------
# Take care of skewness due to small denominators on ratios

fund_df_merged$npmargin_mean <- Winsorize(fund_df_merged$npmargin_mean,
                                   probs = c(0.03, 1))
fund_df_merged$npmargin_cv <- Winsorize(fund_df_merged$npmargin_cv,
                                   probs = c(0.03, 1))

fund_df_merged$roa_mean <- Winsorize(fund_df_merged$roa_mean,
                                   probs = c(0.03, 1))
fund_df_merged$roa_cv <- Winsorize(fund_df_merged$roa_cv,
                                 probs = c(0.03, 1))

fund_df_merged$debt_assets_mean <- Winsorize(fund_df_merged$debt_assets_mean,
                              probs = c(0, 0.97))
fund_df_merged$debt_assets_cv <- Winsorize(fund_df_merged$debt_assets_cv,
                            probs = c(0.0, 0.97 ))

#-------------------------------------------------------------------
# merge with fund3 output (Baneish)

fund_ben <- read.csv("fund_final_ben.csv")

fund_df_final <- merge(fund_df_merged ,fund_ben, by="gvkey")

# write data
write.csv(fund_df_final, "fund_df_final_sev.csv", row.names = FALSE)

#--------------------------------------
fund_df_final2 <- select (fund_df_final, gvkey, conm, tic, 
                          at_mean, restate_bin)





