
# Running the model -------------------------------------------------------
### This is the code for the PBS Journal, quantitative research group 1. 
### Research: testing how the happiness index from The Happiness Report varies with the
### Worldwide Governance Indicators from the World Bank.
### The idea is to understand how freedom is predicting happiness across countries and over time.

### Loading the packages

{
  install.packages('readxl')
  install.packages('dplyr')
  install.packages('tidyverse')
  install.packages('fancycut')
  install.packages('plm')
  install.packages('plyr')
  install.packages('kimisc')
  install.packages("lmtest")
  install.packages('stargazer')
  install.packages("see")
  install.packages("performance")
  install.packages("broom")
  install.packages("kableExtra")
  install.packages("magrittr")
  install.packages("webshot")
  webshot::install_phantomjs()
}
  
{
  library(readxl)
  library(dplyr)
  library(tidyverse)
  library(fancycut)
  library(plm)
  library(plyr)
  library(kimisc)
  library(lmtest)
  library(stargazer)
  library(see)
  library(performance)
  library(sandwich)
  library(broom)
  library(kableExtra)
  library(magrittr)
  library(webshot)
}

install.packages("performance")
library(performance)
install.packages("patchwork")
library(patchwork)

# LOADING THE DATABASE -----------------------------------------------------

dataset_without_missing <- 
  read_excel("/Users/medydcampos/Documents/LSE/AT/PBS Journal/Model/dataset_without_missing.xlsx")

# I will rename some variables so the interpretation is more straighforward 

colnames(dataset_without_missing)[colnames(dataset_without_missing) == "Corruption"] <- "control_of_corruption"


# STANDARDIZED INDICATORS  -------------------------------------------------

### Ok, let's now see if we need to standardize our indicators.

### The range of -2.5 to +2.5 suggests that the indicators of World Bank may already be standardized 
### or normalized, but it's not possible to definitively determine without additional information. 
### Standardization typically refers to transforming data to have a mean of 0 and a standard deviation
### of 1, which might not be the case with your indicators.

#### corruption

mean_corruption <- mean(dataset_without_missing$Corruption)
sd_corruption <- sd(dataset_without_missing$Corruption)

print(paste("Mean:", mean_corruption))
print(paste("Standard Deviation:", sd_corruption))

# Corruption looks fine. 
# Mean: 0.049
# Standard Deviation: 1.029

#### government effectiveness

mean_gov_effectivenss <- mean(dataset_without_missing$Gov_efectiveness)
sd_gov_effectiveness <- sd(dataset_without_missing$Gov_efectiveness)

print(paste("Mean:", mean_gov_effectivenss))
print(paste("Standard Deviation:", sd_gov_effectiveness))

# Gov effectivenss looks fine. 
# Mean: 0.130
# Standard Deviation: 0.966

#### political stability

mean_political_stability <- mean(dataset_without_missing$Political_stability)
sd_political_stability <- sd(dataset_without_missing$Political_stability)

print(paste("Mean:", mean_political_stability))
print(paste("Standard Deviation:", sd_political_stability))

# Mean: -0.075
# Standard Deviation: 0.879
# Is there any problem with a negative mean?

#### rule of law

mean_Rule_Of_Law <- mean(dataset_without_missing$Rule_Of_Law)
sd_Rule_Of_Law <- sd(dataset_without_missing$Rule_Of_Law)

print(paste("Mean:", mean_Rule_Of_Law))
print(paste("Standard Deviation:", sd_Rule_Of_Law))

# Mean: 0.102
# Standard Deviation: 0.986

#### regulatory quality

mean_Regulatory_Quality <- mean(dataset_without_missing$Regulatory_quality)
sd_Regulatory_Quality <- sd(dataset_without_missing$Regulatory_quality)

print(paste("Mean:", mean_Regulatory_Quality))
print(paste("Standard Deviation:", sd_Regulatory_Quality))

# Mean: 0.221
# Standard Deviation: 0.948

#### voice and accountability

mean_voice_account <- mean(dataset_without_missing$Voice_Accountability)
sd_voice_account <- sd(dataset_without_missing$Voice_Accountability)

print(paste("Mean:", mean_voice_account))
print(paste("Standard Deviation:", sd_voice_account))

# Mean: 0.198
# Standard Deviation: 0.870

#### CONCLUSION: indicators seem pretty much standardized. 


# MODEL -------------------------------------------------------------------

# fixed effects two way (method chosen).

## Why are we choosing the two way?
## The two way method is used when we want to control fixed effects for two factors, in our case:
## time and country fixed effects. 


# FIRST ATTEMPT OF RUNNING THE MODEL --------------------------------------

#### Government Effectiveness, Rule Of Law, Voice and accountability, Regulatory Quality, Corruption, Political Stability.

# Dropping rule of law

model_fe_tw1_h <- plm(happiness_score ~ Gov_efectiveness + Voice_Accountability + Regulatory_quality + Political_stability + control_of_corruption + social_support + 
                     log_GDPpc + life_expectancy + Generosity, 
                   data = pdata_data,
                   model = "within", 
                   effect = "twoways")
summary(model_fe_tw1_h)

### Government effectiveness is significant at 10%. 

# Dropping goverment effectiveness 

model_fe_tw2_h <- plm(happiness_score ~ Rule_Of_Law + Voice_Accountability + Regulatory_quality + Political_stability + control_of_corruption + social_support + 
                      log_GDPpc + life_expectancy + Generosity, 
                    data = pdata_data,
                    model = "within", 
                    effect = "twoways")
summary(model_fe_tw2_h)

# Rule of law is significant at 5%. 
# Corruption is significant at 10%.

### Dropping voice and accountability

model_fe_tw3_h <- plm(happiness_score ~ Rule_Of_Law + Regulatory_quality + Political_stability + Gov_efectiveness + control_of_corruption + social_support + 
                      log_GDPpc + life_expectancy + Generosity, 
                    data = pdata_data,
                    model = "within", 
                    effect = "twoways")
summary(model_fe_tw3_h)

# Rule of Law is significant at 10%. Goverment effectiveness is significant at 5%. Corruption is significant at 10%. 

# Dropping regulatory quality

model_fe_tw4_h <- plm(happiness_score ~ Rule_Of_Law + Political_stability + Gov_efectiveness + Voice_Accountability + control_of_corruption + social_support + 
                      log_GDPpc + life_expectancy + Generosity, 
                    data = pdata_data,
                    model = "within", 
                    effect = "twoways")

summary(model_fe_tw4_h)

# Rule of law is significant at 5%. Goverment effectiveness is significant at 5%. 

# Dropping corruption

model_fe_tw5_h <- plm(happiness_score ~ Rule_Of_Law + Political_stability + Gov_efectiveness + Voice_Accountability + Regulatory_quality + social_support + 
                      log_GDPpc + life_expectancy + Generosity, 
                    data = pdata_data,
                    model = "within", 
                    effect = "twoways")

summary(model_fe_tw5_h)

# Rule of Law is significant at 5%. Government effectiveness is significant at 5%. Voice and accountability is significant
# at 10%. 

# Dropping political stability

model_fe_tw6_h <- plm(happiness_score ~ Rule_Of_Law + Gov_efectiveness + Voice_Accountability + Regulatory_quality + control_of_corruption + social_support + 
                      log_GDPpc + life_expectancy + Generosity, 
                    data = pdata_data,
                    model = "within", 
                    effect = "twoways")
summary(model_fe_tw6_h)

#### checking for robustness 

# CHECKING FOR ROBUSTNESS  ------------------------------------------------

## We should account for: homogeneity of variance (heterocedasticy); influential observations (check outliers);
## multicolinearity; residuals for generalized linear models (if the erros are normally distributed); 

## homogeneity of variance (HETEROCEDASTICITY)

test_hetero_1 <- bptest(model_fe_tw1)
print(test_hetero_1)

test_hetero_2 <- bptest(model_fe_tw2)
print(test_hetero_2)

test_hetero_3 <- bptest(model_fe_tw3)
print(test_hetero_3)

test_hetero_4 <- bptest(model_fe_tw4)
print(test_hetero_4)

test_hetero_5 <- bptest(model_fe_tw5)
print(test_hetero_5)

test_hetero_6 <- bptest(model_fe_tw6)
print(test_hetero_6)

# H0: the variance of the erros is constant. 
# P-value is too low in all models, so we rejected the null. We have heterocedasticity in all the models. 
# How to account for that?
# Cluster standard errors in countries. 

## Influential observations (CHECK OUTLIERS)

# Happiness score: dependent variable 

summary(dataset_without_missing$happiness_score) 
hist(dataset_without_missing$happiness_score, main = "Histogram Happiness", xlab = "Happiness")

# Doing the standardize variable

happiness_standardize <- scale(dataset_without_missing$happiness_score)
hist(happiness_standardize, main = "Histogram standardize happiness", xlab = "Standardize Happiness")

# Doing the boxplot

boxplot(happiness_standardize, main = "Boxplot for the Happiness Standardize")

# Counting the outliers

z_threshold <- 3
outliers_happiness <- abs(happiness_standardize) > z_threshold
num_outliers <- sum(outliers_happiness)
print(paste("Number of outliers:", num_outliers))

# No outliers.

#### Main independent variables

# Control of Corruption

summary(dataset_without_missing$control_of_corruption) 
hist(dataset_without_missing$control_of_corruption, main = "Histogram Corruption", xlab = "Corruption")

# Doing the standardize variable

control_of_corruption_standardize <- scale(dataset_without_missing$control_of_corruption)
hist(control_of_corruption_standardize, main = "Histogram standardize Control Of Corruption", xlab = "Standardize Control Of Corruption")

# Doing the boxplot

boxplot(control_of_corruption_standardize, main = "Boxplot for the Control Of Corruption Standardize")

# Counting the outliers

z_threshold <- 3
outliers_control_of_corruption <- abs(control_of_corruption_standardize) > z_threshold
num_outliers_control_of_corruption <- sum(outliers_control_of_corruption)
print(paste("Number of outliers:", num_outliers_control_of_corruption))

# We don't have outliers for the variable of control of corruption. 

# Goverment Effectiveness 

summary(dataset_without_missing$Gov_efectiveness) 
hist(dataset_without_missing$Gov_efectiveness, main = "Histogram Governement Efectiveness", xlab = "Government Efectiveness")

# Doing the standardize variable

gov_effect_standardize <- scale(dataset_without_missing$Gov_efectiveness)
hist(gov_effect_standardize, main = "Histogram standardize government effectiveness", xlab = "Standardize Government Effectiveness")

# Doing the boxplot

boxplot(gov_effect_standardize, main = "Boxplot for the Government Effectiveness Standardize")

# Counting the outliers

z_threshold <- 3
outliers_gov_effectiveness <- abs(gov_effect_standardize) > z_threshold
num_outliers_gov_effectiveness <- sum(outliers_gov_effectiveness)
print(paste("Number of outliers:", num_outliers_gov_effectiveness))

# We don't have outliers for the varibale of governance. 

# Political Stability

summary(dataset_without_missing$Political_stability) 
hist(dataset_without_missing$Political_stability, main = "Histogram Political Stability", xlab = "Political Stability")

# Doing the standardize variable

political_stability_standardize <- scale(dataset_without_missing$Political_stability)
hist(political_stability_standardize, main = "Histogram standardize Political Stability", xlab = "Standardize Political Stability")

# Doing the boxplot

boxplot(political_stability_standardize, main = "Boxplot for the Political Stability Standardize")

## The boxplot shows outliers. 

# Counting the outliers

z_threshold <- 3
outliers_political_stability <- abs(political_stability_standardize) > z_threshold
num_outliers_political_stability <- sum(outliers_political_stability)
print(paste("Number of outliers:", num_outliers_political_stability))

# We have two outliers. i would say that this is not enough to make our model imprecise.  

# Rule of Law

summary(dataset_without_missing$Rule_Of_Law) 
hist(dataset_without_missing$Rule_Of_Law, main = "Histogram Rule Of Law", xlab = "Rule Of Law")

# Doing the standardize variable

rule_of_law_standardize <- scale(dataset_without_missing$Rule_Of_Law)
hist(rule_of_law_standardize, main = "Histogram standardize Rule Of Law", xlab = "Standardize Rule Of Law")

# Doing the boxplot

boxplot(rule_of_law_standardize, main = "Boxplot for the Rule Of Law Standardize")

## The boxplot shows no outliers. 

# Counting the outliers

z_threshold <- 3
outliers_rule_of_law <- abs(rule_of_law_standardize) > z_threshold
num_outliers_rule_of_law <- sum(outliers_rule_of_law)
print(paste("Number of outliers:", num_outliers_rule_of_law))

# We don't have any outliers. 

# Regulatory Quality 

summary(dataset_without_missing$Regulatory_quality) 
hist(dataset_without_missing$Regulatory_quality, main = "Histogram Regulatory Quality", xlab = "Regulatory Quality")

# Doing the standardize variable

Regulatory_quality_standardize <- scale(dataset_without_missing$Rule_Of_Law)
hist(Regulatory_quality_standardize, main = "Histogram standardize Regulatory Quality", xlab = "Standardize Regulatory Quality")

# Doing the boxplot

boxplot(Regulatory_quality_standardize, main = "Boxplot for the Regulatory Quality Standardize")

## The boxplot shows  no outliers. 

# Counting the outliers

z_threshold <- 3
outliers_Regulatory_quality <- abs(Regulatory_quality_standardize) > z_threshold
num_outliers_Regulatory_quality <- sum(outliers_Regulatory_quality)
print(paste("Number of outliers:", num_outliers_Regulatory_quality))

# We don't have any outliers.

#Voice and Accountability

summary(dataset_without_missing$Voice_Accountability) 
hist(dataset_without_missing$Voice_Accountability, main = "Histogram Voice and Accountability", xlab = "Voice And Accountability")

# Doing the standardize variable

voice_account_standardize <- scale(dataset_without_missing$Voice_Accountability)
hist(voice_account_standardize, main = "Histogram standardize Voice and Accountability", xlab = "Standardize Voice and Accountability")

# Doing the boxplot

boxplot(voice_account_standardize, main = "Boxplot for the Voice and Accountability Standardize")

## The boxplot shows  no outliers. 

# Counting the outliers

z_threshold <- 3
outliers_voice_account <- abs(voice_account_standardize) > z_threshold
num_outliers_voice_account <- sum(outliers_voice_account)
print(paste("Number of outliers:", num_outliers_voice_account))

# We don't have any outliers.

# Controls

# Log GDPpc

summary(dataset_without_missing$log_GDPpc) 
hist(dataset_without_missing$log_GDPpc, main = "Histogram Log GPD per capita", xlab = "Log GDP per capita")

# Doing the standardize variable

gdp_standardize <- scale(dataset_without_missing$log_GDPpc)
hist(gdp_standardize, main = "Histogram standardize GDP Per Capita", xlab = "Standardize GDP Per Capita")

# Doing the boxplot

boxplot(gdp_standardize, main = "Boxplot for the GDP Per Capita Standardize")

## The boxplot shows  no outliers. 

# Counting the outliers

z_threshold <- 3
outliers_gdp <- abs(gdp_standardize) > z_threshold
num_outliers_gdp <- sum(outliers_gdp)
print(paste("Number of outliers:", num_outliers_gdp))

# We don't have any outliers.

# Social support

summary(dataset_without_missing$social_support) 
hist(dataset_without_missing$social_support, main = "Histogram Social Support", xlab = "Social Support")

# Doing the standardize variable

social_support_standardize <- scale(dataset_without_missing$social_support)
hist(social_support_standardize, main = "Histogram standardize Social Support", xlab = "Standardize Social Support")

# Doing the boxplot

boxplot(social_support_standardize, main = "Boxplot for the Social Support Standardize")

## The boxplot shows  no outliers. 

# Counting the outliers

z_threshold <- 3
outliers_social_support <- abs(social_support_standardize) > z_threshold
num_outliers_social_support <- sum(outliers_social_support)
print(paste("Number of outliers:", num_outliers_social_support))

# life expectancy

summary(dataset_without_missing$life_expectancy) 
hist(dataset_without_missing$life_expectancy, main = "Histogram Life Expectancy", xlab = "Life Expectancy")

# Doing the standardize variable

life_expectancy_standardize <- scale(dataset_without_missing$life_expectancy)
hist(life_expectancy_standardize, main = "Histogram standardize Life Expectancy", xlab = "Standardize Life Expectancy")

# Doing the boxplot

boxplot(life_expectancy_standardize, main = "Boxplot for the Life Expectancy Standardize")

## The boxplot shows  no outliers. 

# Counting the outliers

z_threshold <- 3
outliers_life_expectancy <- abs(life_expectancy_standardize) > z_threshold
num_outliers_life_expectancy <- sum(outliers_life_expectancy)
print(paste("Number of outliers:", num_outliers_life_expectancy))

# generosity
summary(dataset_without_missing$Generosity) 
hist(dataset_without_missing$Generosity, main = "Histogram Generosity", xlab = "Generosity")

# Doing the standardize variable

generosity_standardize <- scale(dataset_without_missing$Generosity)
hist(generosity_standardize, main = "Histogram standardize Generosity", xlab = "Standardize Generosity")

# Doing the boxplot

boxplot(generosity_standardize, main = "Boxplot for the Generosity Standardize")

## The boxplot shows outliers. 

# Counting the outliers

z_threshold <- 3
outliers_generosity <- abs(generosity_standardize) > z_threshold
num_outliers_generosity <- sum(outliers_generosity)
print(paste("Number of outliers:", num_outliers_generosity))

# Doing Scatter plots

#### Main independent variables

# Control of Corruption

## Running the linear fit
linear_corruption <- lm(happiness_score ~ control_of_corruption, data = dataset_without_missing)

## creating the scatter plot

plot(dataset_without_missing$control_of_corruption, dataset_without_missing$happiness_score, 
     main = "Scatter Plot with Linear Fit for Control Of Corruption",
     xlab = "Control Of Corruption",
     ylab = "Happiness Score")

## adding linear tendency

abline(linear_corruption, col = "red")

# Checking all the assumptions

check_model(linear_corruption)

# Goverment Effectiveness 

## Running the linear fit
linear_gov <- lm(happiness_score ~ Gov_efectiveness, data = dataset_without_missing)

## creating the scatter plot

plot(dataset_without_missing$Gov_efectiveness, dataset_without_missing$happiness_score, 
     main = "Scatter Plot with Linear Fit for Goverment Effectiveness",
     xlab = "Goverment Effectiveness",
     ylab = "Happiness Score")

## adding linear tendency

abline(linear_gov, col = "red")

## Checking all the assumptions

check_model(linear_gov)

# Political Stability

## Running the linear fit
linear_political <- lm(happiness_score ~ Political_stability, data = dataset_without_missing)

## creating the scatter plot

plot(dataset_without_missing$Political_stability, dataset_without_missing$happiness_score, 
     main = "Scatter Plot with Linear Fit for Political Stability",
     xlab = "Political Stability",
     ylab = "Happiness Score")

## adding linear tendency

abline(linear_political, col = "red")

# Checking all the assumptions

check_model(linear_political)


# Rule of Law

## Running the linear fit
linear_law <- lm(happiness_score ~ Rule_Of_Law, data = dataset_without_missing)

## creating the scatter plot

plot(dataset_without_missing$Rule_Of_Law, dataset_without_missing$happiness_score, 
     main = "Scatter Plot with Linear Fit for Rule Of Law",
     xlab = "Rule Of Law",
     ylab = "Happiness Score")

## adding linear tendency

abline(linear_law, col = "red")

# Checking all the assumptions

check_model(linear_law)

# Regulatory Quality 

# Running the linear fit
linear_quality <- lm(happiness_score ~ Regulatory_quality, data = dataset_without_missing)

## creating the scatter plot

plot(dataset_without_missing$Regulatory_quality, dataset_without_missing$happiness_score, 
     main = "Scatter Plot with Linear Fit for Regulatory Quality",
     xlab = "Regulatory Quality",
     ylab = "Happiness Score")

## adding linear tendency

abline(linear_quality, col = "red")

# Checking all the assumptions

check_model(linear_quality)

#Voice and Accountability

# Running the linear fit
linear_voice <- lm(happiness_score ~ Voice_Accountability, data = dataset_without_missing)

## creating the scatter plot

plot(dataset_without_missing$Voice_Accountability, dataset_without_missing$happiness_score, 
     main = "Scatter Plot with Linear Fit for Voice and Accountability",
     xlab = "Voice and Accountability",
     ylab = "Happiness Score")

## adding linear tendency

abline(linear_voice, col = "red")

# Checking all the assumptions

check_model(linear_voice)

# Controls

# Log GDPpc

## Running the linear fit
linear_gdp <- lm(happiness_score ~ log_GDPpc, data = dataset_without_missing)

## creating the scatter plot

plot(dataset_without_missing$log_GDPpc, dataset_without_missing$happiness_score, 
     main = "Scatter Plot with Linear Fit for GDP per capita (log)",
     xlab = "Log GDP per capita",
     ylab = "Happiness Score")

## adding linear tendency

abline(linear_gdp, col = "red")

# Social support

## Running the linear fit
linear_social_support <- lm(happiness_score ~ social_support, data = dataset_without_missing)

## creating the scatter plot

plot(dataset_without_missing$social_support, dataset_without_missing$happiness_score, 
     main = "Scatter Plot with Linear Fit for Social Support",
     xlab = "Social Support",
     ylab = "Happiness Score")

## adding linear tendency

abline(linear_social_support, col = "red")

# Life expectancy

## Running the linear fit
linear_life <- lm(happiness_score ~ life_expectancy, data = dataset_without_missing)

## creating the scatter plot

plot(dataset_without_missing$life_expectancy, dataset_without_missing$happiness_score, 
     main = "Scatter Plot with Linear Fit for Life Expectancy",
     xlab = "Life Expectancy",
     ylab = "Happiness Score")

## adding linear tendency

abline(linear_life, col = "red")

# Generosity

## Running the linear fit
linear_generosity <- lm(happiness_score ~ Generosity, data = dataset_without_missing)

## creating the scatter plot

plot(dataset_without_missing$Generosity, dataset_without_missing$happiness_score, 
     main = "Scatter Plot with Linear Fit for Generosity",
     xlab = "Generosity",
     ylab = "Happiness Score")

## adding linear tendency

abline(linear_generosity, col = "red")


# Main conclusions --------------------------------------------------------

# We have heterocedasticity: we have to cluster the errors within each country. 
# The graph of disperstion looks fine for almost all variables. We keep
# generosity because is an important control from literature. 
# We are not removing outliers. They are not so many and we don't
# want to take the risk of losing generalization. This means we don't want to
# make a model that only explains what is happening in our specific sample. 
# Residuals for all indicators are not normally distributed. 


# Final models -------------------------------------------------------------


#### Government Effectiveness, Rule Of Law, Voice and accountability, Regulatory Quality, Corruption, Political Stability.

# Running pdata

dataset_without_missing <- dataset_without_missing %>%
  dplyr::rename(Country = Country_Name)

pdata_data <- pdata.frame(dataset_without_missing, index = c("Country", "Year"))

# Political stability

model_fe_tw1 <- plm(happiness_score ~  Political_stability + social_support + 
                      log_GDPpc + life_expectancy + Generosity, 
                    data = pdata_data,
                    model = "within", 
                    effect = "twoways",
                    vcov = vcovHC,
                    cluster = "country")
summary(model_fe_tw1)

model_fe_tw1 <- plm(happiness_score ~  Political_stability + Rule_Of_Law +
                      Gov_efectiveness + Regulatory_quality + Voice_Accountability + social_support + 
                      log_GDPpc + life_expectancy + Generosity, 
                    data = pdata_data,
                    model = "within", 
                    effect = "twoways",
                    vcov = vcovHC,
                    cluster = "country")
summary(model_fe_tw1)

variables <- c(dataset_without_missing$control_of_corruption, dataset_without_missing$Gov_efectiveness,
               dataset_without_missing$Political_stability, dataset_without_missing$Rule_Of_Law,
               dataset_without_missing$Regulatory_quality)

# Political stability is not significant.

# Goverment effectiveness 

model_fe_tw2 <- plm(happiness_score ~ Gov_efectiveness + social_support + 
                      log_GDPpc + life_expectancy + Generosity, 
                    data = pdata_data,
                    model = "within", 
                    effect = "twoways",
                    vcov = vcovHC,
                    cluster = "country")
summary(model_fe_tw2)

# Government effectiveness is significant at 5%. 
# The coefficient is positive: higher the government effectiveness,
# higher the happiness score. 

### voice and accountability

model_fe_tw3 <- plm(happiness_score ~ Voice_Accountability + social_support + 
                      log_GDPpc + life_expectancy + Generosity, 
                    data = pdata_data,
                    model = "within", 
                    effect = "twoways",
                    vcov = vcovHC,
                    cluster = "country")
summary(model_fe_tw3)

# Voice and accountability is not significant. 
# The main proxy for democracy is not significant. 


# regulatory quality

model_fe_tw4 <- plm(happiness_score ~ Regulatory_quality + social_support + 
                      log_GDPpc + life_expectancy + Generosity, 
                    data = pdata_data,
                    model = "within", 
                    effect = "twoways",
                    vcov = vcovHC,
                    cluster = "country")
summary(model_fe_tw4)

# Regulatory quality is not significant.

# Rule of Law

model_fe_tw5 <- plm(happiness_score ~ Rule_Of_Law +  + social_support + 
                      log_GDPpc + life_expectancy + Generosity, 
                    data = pdata_data,
                    model = "within", 
                    effect = "twoways",
                    vcov = vcovHC,
                    cluster = "country")
summary(model_fe_tw5)

# Rule of law is not significant.

# control of corruption

model_fe_tw6 <- plm(happiness_score ~ control_of_corruption  + social_support + 
                      log_GDPpc + life_expectancy + Generosity, 
                    data = pdata_data,
                    model = "within", 
                    effect = "twoways",
                    vcov = vcovHC,
                    cluster = "country")
summary(model_fe_tw6)

# Control of corruption is significant at 10%. 
# This is alligned with literature: corruption is usually negatively 
# correlated with happiness.


# Main conclusion ---------------------------------------------------------

## Control of corruption and government effectiveness are the significant ones.

## What does the world bank say?

## Government effectiveness: captures perceptions of the quality of public services, the quality of the civil service and the degree
## of its independence from political pressures, the quality of policy formulation and implementation, and the credibility of the
## government's commitment to such policies.

## Control of corruption: reflects perceptions of the extent to which public power 
## is exercised for private gain, including both petty and grand
## forms of corruption, as well as "capture" of the state by elites 
## and private interests.


# exporting tables --------------------------------------------------------


install.packages("sjPlot")
library(sjPlot)

tab_model(model_fe_tw1) 
tab_model(model_fe_tw2)
tab_model(model_fe_tw3)
tab_model(model_fe_tw4)
tab_model(model_fe_tw5)
tab_model(model_fe_tw6)



