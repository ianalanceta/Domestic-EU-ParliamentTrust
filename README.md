# Step 1: Prepare Data Set -----------------------------------------------------
# This section of the script uses the exact code provided in the assignment 
# instructions in order to clean and set the data for analysis in the following 
# sections. To skip data preparation, and go into the analysis sections, 
# go to line 51.

# This part trims, renames, and re-codes NA values in the data.
ESS <- read.csv("ESS10.csv")
ESS <- ESS[ , c("cntry", "agea", "gndr", "eisced", "brncntr", "ppltrst", "nwspol", 
                "trstprl", "trstep", "stflife", "imbgeco", "hhmmb", "respc19")]
library(dplyr)
ESS <- rename(ESS, country=cntry, age=agea, gender=gndr, education=eisced, 
              bornincountry=brncntr, soctrust=ppltrst, news=nwspol, 
              parltrust=trstprl, eptrust=trstep, lifesat=stflife, 
              immig=imbgeco, household=hhmmb, covid=respc19)
ESS[, c(2,4,6:12)][ESS[, c(2,4,6:12)] > 50] <- NA 
ESS[, c(3,5,13)][ESS[, c(3,5,13)] > 5] <- NA 

# Meanwhile, this part re-codes dummy variables and defines them as factors.

# Gender
ESS$gender <- ifelse(ESS$gender==2, 1, 0)
ESS$gender <- as.factor(ESS$gender)
# "Female" is now 1, "Male" is 0.

# Born in country:
ESS$bornincountry <- ifelse(ESS$bornincountry==2, 0, 1)
ESS$bornincountry <- as.factor(ESS$bornincountry)
# "Yes" is now 1, "No" is 0.

# Covid:
ESS$covid <- ifelse(ESS$covid==1, 1, 0)
ESS$covid <- as.factor(ESS$covid)
# "Yes" is now 1, "No" is 0.

# In addition to the data preparation, it is also important to load the packages
# and set the notations that will be used for the analysis. 

library(AER)
library(stargazer)
library(ggplot2)
library(GGally)
library(corrplot) 
library(lmtest)  
library(sandwich) 
library(car)  

options(scipen = 999)


# Section 2: Descriptive Statistics for Dependent and Independent Variables ----
# Below is an overall look of the variables in the model.

summary(ESS[, c("eptrust", "parltrust", "gender", "age", "news", "lifesat")])

# Next, we will save the output table into a text file and adjust the labels.
stargazer(ESS[,c('eptrust','parltrust','gender','age','news','lifesat')],
          covariate.labels  = c('EP Trust','Parliamentary Trust','Gender',
                                'Age','News Consumption','Life Satisfaction'),
          type="text",summary.stat=c('n','mean','median','sd','min','max','p25',
                                     'p75'),out="summary.txt",digits=1)

# It would also be helpful to visualize the distribution of trust in the
# European Parliament ("EP Trust") by generating a histogram of the data. 

hist(ESS$eptrust, col = "#8EAADB", breaks = 10, xlab = "Trust in the European 
     Parliament", 
     main = "Trust in the EP")

table(ESS$eptrust)
write.csv(t(as.data.frame(table(ESS_filtered$eptrust))),"frequency-table.csv")

# Although there is no apparent skew to the data, there appears to be
# a slight bi-modal pattern in the histogram which violates the assumption of 
# a normal distribution.

# As can be seen in the frequency table below, this is indeed true as there  
# are two modes, one at "0" (for no trust at all) and "5" (a medium level of
# trust). In order to correct for this distribution error, we will filter for the
# lower extreme mode of "0".

ESS_filtered <- ESS %>% filter(eptrust!=0)

# As can be seen below, the values for the table are correct as there is only
# one mode, and it falls in the centre of the distribution. 
table(ESS_filtered$eptrust)
write.csv(t(as.data.frame(table(ESS_filtered$eptrust))),"updated-frequency-table.csv")


# Section 3: Model Estimation --------------------------------------------------
# Theory: As a result of the positive transference, trust in one's domestic 
# parliament will positively influence their trust in the EP. The research question
# is therefore: how does trust in one’s domestic parliament affect their trust 
# in the European Parliament? And the null and alternative are as follows:

# H1: There is no significant relationship between an individual’s level of trust 
# in their domestic parliament and their level of trust in the European Parliament.

# H0: There is a significant positive relationship between an individual’s level 
# of trust in their domestic parliament and their level of trust in the 
# European Parliament. 

# First we will estimate the model without the control variables.
model1 <- lm(eptrust ~ parltrust, ESS_filtered)  
summary(model1)
capture.output(print(summary(model1)),file="IV-DV Ouput.txt")

# Already, we see a highly significant p-value and a reasonably good adjusted R2 
# value of 0.2436. However, as outlined in the analysis document, there are other
# factors that may influence EP trust. These factors are added as control
# variables in the model.
model2 <- lm(eptrust ~ parltrust + gender + age + news + lifesat, ESS_filtered)  
summary(model2)
capture.output(print(summary(model2)),file="IV-DV-Control Ouput.txt")

# This can also be seen as a stargzaer style table.
stargazer(model1, model2,type="text")

# Given the strengthened coefficient and adjusted R2 values, it seems possible
# to reject the null hypothesis and accept the alternative hypothesis. However,
# before confidently doing so, the model will be subject to several post-
# estimation diagnostics. 


# Section 4: Post Estimation Diagnostics----------------------------------------
# First, we will check if there is any multicollinearity within the variables 
# of the model using a VIF test on our complete model (i.e. model 2)

vif_check <- vif(model2)
vif_check
capture.output(print(vif_check),file="vif-check.txt")

# With good results from the multicollinearity check, the next step will be to 
# check for linearity. 

plot(x=fitted(model2),y=sqrt(rstandard(model2)), xlab="Fitted Values of 
     Trust in EP", ylab="Residuals")

# As can be seen by the many concave lines in the below plot, it is evident that 
# the model is not linear. However, in an attempt to correct for this lack of 
# linearity, the model can be logged. 

model2_linear <- lm(log(eptrust) ~ parltrust + gender + age + news + 
                      lifesat, data = ESS_filtered) 
plot(model2_linear, 1) 

# My model might be better fitted with a non-linear model because the main 
# dependent variable (i.e. trust in the EP) is categorical by nature (even though 
# we are meant to treat it as an interval level for this analysis), so the data 
# will be difficult to fit on a linear regression even if it is logged. 

# The plot generated above already provides a strong hint that the model violates 
# the assumption of homoskedasticity and is in fact heteroskedastic. However, 
# this can be properly confirmed by running a Breusch-Pagan test in R.

bptest(model2)
capture.output(print(bptest(model2)),file="bptest.txt")

# Based on the highly signficant p value from the test, the null hypothesis of 
# homoskedasticity is rejected. However, this can be counteracted using a 
# coefftest.
model2_linear
model2_robust <- coeftest(model2, vcov = vcovHC(model2,type = "HC1")) 
model2_robust
capture.output(print(model2_robust),file="model2-robust.txt")

# Notice that the coefficients stay the same, but the standard errors have now 
# been made robust to heteroskedasticity and the p values are still very 
# significant (much less than 0.05).
