
#--------------------------------
# Building a separate data set to explore severity of SCA
# Need to model SCA severity as a function of 
# 1) Market Cap, Shareturnover, potential price decline (downside dev?)

setwd("~/Desktop/Capstone Project/Severity Prediction")

library(readxl)
sca_df <- readxl::read_xlsx("sca_filings.xlsx", na = "#NULL!")
sca_df$sca <- "1"
sca_df <- select(sca_df, tic = Ticker, 
                 settlement = SettlementAmount,
                 FilingYear)

# Need to pull total assets and restatement data
#---------------------
# load raw data
fund <- read.csv("fundamentals.csv")
#fundsec <- filter(fund, gsector == 30 ) 
fundsec_std <- filter(fund, datafmt == "STD") # filter for reporting format

#--------------------
# Filter for just total assets and cash
# Sector Specific
fundsec_assets <- select (fundsec_std, c(gvkey, fyear, datafmt, 
                                         tic, conm, 
                                         revt, ni,
                                         at, act, lct, ch, dltt, 
                                         teq , sale, capx, dvt, 
                                         gdwl, wcap, intan,
                                         auop, pi))

# Assets By Ticker Symbol
fund_assets <- 
  group_by(fundsec_assets, tic) %>%
  summarize (at_mean = mean(at, na.rm = TRUE),
             teq_mean = mean(teq, na.rm = TRUE))

#--------------------------------------
# Adding Average Monthly Volume as Feature
sec_raw <- read.csv("Securities_Full.csv")
sec_df <- group_by(sec_raw, gvkey)
sec_df <- mutate(sec_df,marketcap = (cshom*prccm))
ungroup(sec_df)

sec_returns <- group_by(sec_df, tic) %>%
  summarize(avg_volume = mean(cshtrm, na.rm = TRUE),
            mean_marketcap = mean(marketcap, na.rm = TRUE))


#--------------------------------------
sev_merge2 <- merge(fund_assets, sca_df, by = "tic",
                   all.x = TRUE, all.y = TRUE)

sev_merge3 <- merge(sev_merge2, sec_returns, by = "tic",
                    all.x = TRUE)

df_final <- sev_merge3[!is.na(sev_merge3$settlement),]
df_final <- na.omit(df_final)

#---------------------------------------
sev_final_cap <- read.csv("sev_final_cap.csv")
sev_merge4 <- merge(df_final, sev_final_cap, by = "tic",
                    all.x = TRUE)

df_sev <- select(sev_merge4, 
                 tic,
                 at_mean,
                 teq_mean,
                 settlement = settlement.x,
                 avg_volume,
                 mean_return,
                 markcap)

#-----------------------------------------
# log transform all right skewed features

#-----------------------------------------
# Histogram Plots

markcap_plot <- ggplot(df_sev, aes(x=markcap))+
  geom_histogram(aes(y=..density..), 
                 colour="black", 
                 fill="steel blue",
                 bins=100)+
  labs(y=NULL,
       x ="\nMarket Capitalization")+
  theme_bw()+
  scale_x_continuous(labels=label_number(prefix = "$ ",  
                                         suffix = "M", scale = 1e-9))+
  theme(axis.title = element_text(size=22, face="bold"),
        axis.text = element_text(size=20),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())+
  geom_density(trim=TRUE, size=1)+
  coord_cartesian(ylim=c(0,0.00000000005),
                  xlim=c(0, 100000000000))

ggsave("markcap_plot.png", markcap_plot, dpi=400, height = 6, width = 9 )

markcap_logplot<- ggplot(df_sev, aes(x=log(markcap)))+
  geom_histogram(aes(y=..density..), 
                 colour="black", 
                 fill="steel blue",
                 bins=20)+
  labs(y=NULL,
       x ="\nLog-Market Capitalization")+
  theme_bw()+
  theme(axis.title = element_text(size=22, face="bold"),
        axis.text = element_text(size=20),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())+
  geom_density(trim=TRUE, size=1)

ggsave("markcap_logplot.png", markcap_logplot, dpi=400, height = 6, width = 9 )

settle_plot <- ggplot(df_sev, aes(x=settlement))+
  geom_histogram(aes(y=..density..), 
                 colour="black", 
                 fill="steel blue",
                 bins=200)+
  scale_x_continuous(labels=label_number(prefix = "$ ",  
                                         suffix = "M", scale = 1e-6))+
  labs(y=NULL,
       x ="\nSettlement Amount")+
  theme_bw()+
  theme(axis.title = element_text(size=22, face="bold"),
        axis.text = element_text(size=20),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())+
  geom_density(trim=TRUE, size=1)+
  coord_cartesian(xlim=c(0, 300000000),
                  ylim=c(0, 0.000000007))

ggsave("settle_plot.png", settle_plot, dpi=400, height = 6, width = 9 )

settle_logplot <- ggplot(df_sev, aes(x=log(settlement)))+
  geom_histogram(aes(y=..density..), 
                 colour="black", 
                 fill="steel blue",
                 bins=30)+
  labs(y=NULL,
       x ="\nLog-Settlement Amount")+
  theme_bw()+
  theme(axis.title = element_text(size=22, face="bold"),
        axis.text = element_text(size=20),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())+
  geom_density(trim=TRUE, size=1)
  #coord_cartesian(xlim=c(0, 300000000),
                  #ylim=c(0, 0.000000007))

ggsave("settle_logplot.png", settle_logplot, dpi=400, height = 6, width = 9 )



hist(df_sev$at_mean)
hist(df_sev$teq_mean)
hist(df_sev$avg_volume)
hist(df_sev$markcap,
     ylim=c(0,10))

hist(log(df_sev$at_mean))
hist(log(df_sev$teq_mean))
hist(log(df_sev$avg_volume))
hist(log(df_sev$mean_marketcap))

df_sev$log_teq <- log(df_sev$teq_mean+0.001-min(df_final$teq_mean))
df_sev$log_vol <- log(df_sev$avg_volume)
df_sev$log_at <- log(df_sev$at)
df_sev$log_markcap <- log(df_sev$markcap)
df_sev$log_return <- log(df_sev$mean_return+0.0001-min(df_final$teq_mean))

df_sev <- select (df_sev,
                    settlement,
                    log_at,
                    log_teq,
                    log_vol,
                    log_markcap,
                  log_return)

M <- cor(df_sev[,c(2:6)])
corrplot::corrplot(M, method="number")

lm_fit <- lm(log(settlement)~log(markcap), data=df_sev)
summary(lm_fit)

df_sev$preds <- predict(lm_fit, newdata = df_sev[,c(2:7)])
df_sev$preds1 <- exp(df_sev$preds)

#-----------------------------------------------------------
#predicted vs. actual plot
library(scales)
pred.actual <- ggplot(df_sev, aes(y=preds1, x=settlement)) +
               geom_point()+
               scale_x_log10 (labels = label_number(suffix = " M", scale = 1e-6))+
               scale_y_log10 (labels = label_number(suffix = " M", scale = 1e-6))+
               geom_smooth(method=lm)+
               theme_bw()+
               labs(x="\nSettlement Amount-Actual (Log Scale)",
               y = "Settlement Amount-Predicted (Log Scale) \n")+
               theme(axis.text = element_text(size = 14),
               axis.title = element_text(size = 16, face="bold"))

ggsave("pred_actual.png", pred.actual, dpi=400, height = 6, width = 9 )

#------------------Log Actual Plot
library(scales)
log.actual <- ggplot(df_sev, aes(y=log(settlement), x=log(markcap))) +
  geom_point()+
  geom_smooth(method=lm)+
  theme_bw()+
  labs(x="\nLog(Market Cap)",
       y = "Log(Settlement) \n")+
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16, face="bold"))

ggsave("log.actual.png", log.actual, dpi=400, height = 6, width = 9 )

saveRDS(lm_fit, "LM_Fit_final.RDS")
write.csv(df_sev, "finalsevdf.csv")

#-------------------------------- Severity Predictions

lm_fit <- readRDS("LM_Fit_final.RDS")
x <- as.data.frame((20336681016.6667))
colnames(x) <- c("markcap")

predict(lm_fit,  interval = "confidence", newdata = x)
predict(lm_fit,  interval = "prediction", newdata = x)
