
###########################################################################
## Section 1: Fixed Effects                                              ##
###########################################################################

###########################################################################
## Data Preparation                                                      ##
###########################################################################

# Set working directory, load packages, and clear the workspace
rm(list=ls())
setwd("/Users/hazelchui/Desktop/demo")

install.packages("readstata13")
library(readstata13)
library(plm)
library(lmtest)
library(sandwich)
library(boot)
library(clusterSEs)
library(plyr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggpubr)

# Load the dataset
df <- read.dta13('FE_data.dta')
df <- pdata.frame(df, index = c("idcode","year"), drop.index = FALSE)

###########################################################################
## Figures                                                               ##
###########################################################################


# prepare data for plots
df1 <- df %>% group_by(year, race, marriage) %>% summarise(mean(ln_wage)) 
df1 <- df1[df1$race != "other", ]
colnames(df1)[4] <- "log_wage"
df1 <- na.omit(df1)
df1$marital_status[df1$marriage == 1] <- "married"
df1$marital_status[df1$marriage == 0] <- "unmarried"
df1$marital_status <- as.factor(df1$marital_status)

df2 <- df %>% group_by(year, collgrad, marriage) %>% summarise(mean(ln_wage)) 
colnames(df2)[4] <- "log_wage"
df2 <- na.omit(df2)
df2$marital_status[df2$marriage == 1] <- "married"
df2$marital_status[df2$marriage == 0] <- "unmarried"
df2$marital_status <- as.factor(df2$marital_status)


# Figure 1
f1a <- ggplot() + geom_line(data = subset(df1, race == "white"), 
                            mapping = aes(x = year, y = log_wage, color = marital_status, group = marital_status)) + ylab("log real wage") + 
  labs(title = "White") + theme(plot.title = element_text(hjust = 0.5)) + scale_x_discrete(breaks=c(70,80))
f1a

f1b <- ggplot() + geom_line(data = subset(df1, race == "black"), 
                            mapping = aes(x = year, y = log_wage, color = marital_status, group = marital_status)) + ylab("log real wage") + 
  labs(title = "Black") + theme(plot.title = element_text(hjust = 0.5)) + scale_x_discrete(breaks=c(70,80))
f1b

f1 <- ggarrange(f1a, f1b, ncol=2, nrow=1, common.legend = T, legend="bottom")  # combine graphs, common lengnds
f1


# Figure 2
f2a <- ggplot() + geom_line(data = subset(df2, collgrad == 0) , 
                            mapping = aes(x = year, y = log_wage, color = marital_status, group = marital_status)) + ylab("log real wage") +
  labs(title = "Non-college Graduate") + theme(plot.title = element_text(hjust = 0.5)) + scale_x_discrete(breaks=c(70,80))
f2a

f2b <- ggplot() + geom_line(data = subset(df2, collgrad == 1), 
                            mapping = aes(x = year, y = log_wage, color = marital_status, group = marital_status)) + ylab("log real wage") +
  labs(title = "College Graduate") + theme(plot.title = element_text(hjust = 0.5)) + scale_x_discrete(breaks=c(70,80))
f2b

f2 <- ggarrange(f2a, f2b, ncol=2, nrow=1, common.legend = T, legend="bottom")  # combine graphs, common lengnds
f2


###########################################################################
## Functions for bootstrapped SE                                         ##
###########################################################################
# for pooled regression
reg_boot <- function(formula, data, indices) {
  d <- data[indices,] 
  fit <- lm(formula, data=d)
  return(summary(fit)$coef)
} 

# for individual FE
preg_boot_1 <- function(formula, data, indices) {
  d <- data[unique(indices),]
  fit <- plm(formula, data=d, model = "within", effect = "individual", index = c("idcode","year"))
  return(summary(fit)$coef)
} 

# for twoways FE
preg_boot_2 <- function(formula, data, indices) {
  d <- data[unique(indices),] 
  fit <- plm(formula, data=d, model = "within", effect = "twoway", index = c("idcode","year"))
  return(summary(fit)$coef)
} 

###########################################################################
## Estimation                                                            ##
###########################################################################

# Model 1
m1_equation <- ln_wage ~ marriage + age + union + tenure + hours + race + collgrad
m1 <- lm(m1_equation, data = df)
coeftest(m1, vcov = vcovHC(m1, type = "HC1")) # simple robust SE

m1_boot_SE <- boot(data = df, statistic=reg_boot, R = 50, formula=m1_equation) 
m1_boot_SE # bootstrap SE


# Model 2
m2_equation <- ln_wage ~ marriage + age + union + tenure + hours 
m2 <- plm(m2_equation, df, model = "within", effect = "individual", index = c("idcode","year"))
coeftest(m2, vcov = vcovHC(m2, cluster = "group")) # clustered SE

m2_boot_SE <- boot(data = df, statistic=preg_boot_1, R = 50, formula=m2_equation) 
m2_boot_SE # bootstrap SE


# Model 3
m3_equation <- ln_wage ~ marriage + age + union + tenure + hours 
m3 <- plm(m3_equation, df, model = "within", effect = "twoways", index = c("idcode","year"))
coeftest(m3, vcov = vcovHC(m3, cluster = "group")) # clustered SE

m3_boot_SE <- boot(data = df, statistic=preg_boot_2, R = 50, formula=m3_equation) 
m3_boot_SE # bootstrap SE


# Model 4
df$ind_code_f <- factor(df$ind_code) # industry code as factor
m4_equation <- ln_wage ~ marriage + age + union + tenure + hours + ind_code_f
m4 <- plm(m4_equation, df, model = "within", effect = "individual", index = c("idcode","year"))
coeftest(m4, vcov = vcovHC(m4, cluster = "group")) # clustered SE

m4_boot_SE <- boot(data = df, statistic=preg_boot_1, R = 50, formula=m4_equation) 
m4_boot_SE # bootstrap SE


# Model 5
m5_equation <- ln_wage ~ marriage + age + union + tenure + hours + ind_code_f
m5 <- plm(m5_equation, df, model = "within", effect = "twoways", index = c("idcode","year"))
coeftest(m5, vcov = vcovHC(m5, cluster = "group")) # clustered SE

m5_boot_SE <- boot(data = df, statistic=preg_boot_2, R = 50, formula=m5_equation) 
m5_boot_SE # bootstrap SE



###########################################################################
## Section 2: DiD                                                        ##
###########################################################################

rm(list=ls())

# Load the dataset drop useless columns
df <- read.dta13('DiD_data.dta')
df <- pdata.frame(df, index = c("fcode","year"), drop.index = FALSE)
df <- select(df, year, fcode, grant, training)
df$training[is.na(df$training)] <- 0

# generate useful variables
# time
df$time <- ifelse(df$year == 1987, 1, ifelse(df$year == 1988, 2, 3))

# firm specific time trend
for (code in unique(df$fcode)){
  varname <- paste0('T',code)
  df$new <- ifelse(df$fcode == code, df$time, 0)
  colnames(df)[length(df)] <- varname
}

df$year_f <- factor(df$year)
df$fcode_f <- factor(df$fcode)

# Estimation

did1 <- lm(training ~ grant + year_f + fcode_f,df)
coeftest(did1, vcov = vcovHC(did1, cluster = "group")) # clustered SE

did2 <- lm(training ~ . - year - fcode - time, df)
coeftest(did2, vcov = vcovHC(did2, cluster = "group")) # clustered SE


