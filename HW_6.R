##Part 1

setwd("/Users/jjimmyk/downloads")

Lexical_Data <- read.csv("LexicalData.csv")
Items <- read.csv("Items.csv")

head(Lexical_Data)
head(Items)

##First Action: Create a new variable that contains the values within D_RT as numeric and 
## without commas.

New_D_RT <- as.numeric(gsub(",","",Lexical_Data$D_RT))
head(New_D_RT)

##Replace the original D_RT values with the New_D_RT values within UncleanData;
## Now, the column will have numerical values without commas.
Lexical_Data$D_RT <- New_D_RT

head(Lexical_Data)

library(tidyverse)

combined_data <- left_join(Lexical_Data,Items, by=c("D_Word"="Word"))

head(combined_data)

combined_data <- subset(combined_data, select = -Freq_HAL)

head(combined_data)

## The below yields error, "vector memory exhausted"
##new_frame <- merge(x = Lexical_Data, y = Items)

##Problem: different number of rows in each dataframe.
##Lexical_Data$Length <- Items$Length

##joined_data <- data.frame(Lexical_Data, Items)

##First, fit a linear model with Log_Freq_HAL and Length as predictors, 
##and D_RT as the output. 
##Include an interaction term. Use summary() to look at the model output.

lm.fit = lm(D_RT~Log_Freq_HAL+Length, data = combined_data)

summary(lm.fit)

##plot
plot(D_RT~Log_Freq_HAL+Length, data = combined_data) +
  geom_line()

install.packages("lme4")
library(lme4)

##Residuals are way smaller here. There must have been a lot of variation dependent on 
##the subject.
mixed_effects = lmer(D_RT ~ Log_Freq_HAL + Length + (1 | Sub_ID), data = combined_data)

summary(mixed_effects)

##The larger the t value is (in absolute value), the less likely it is that
## the results are by chance. Considering the impact of subject ID (the difference
## in response time caused by individual, not-modeled traits) improves the reliability of the model,
## reducing its error and thus the residuals.

##degrees of freedom = parameters. lm.fit has 4 parameters: intercept, ind var 1, ind var 2
## and something else (maybe the residual).
## The smaller the AIC is, the better the fit.
comparison = AIC(lm.fit, mixed_effects)
comparison

