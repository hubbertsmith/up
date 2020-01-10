#not sure what libraries we are going to need. I copied these over from previous code
library(haven)
library(descr)
library(ggplot2)
library(stargazer)
library(tidyverse)

#rename the data
ANES2016 <- da36824.0001

#split the sample to test the model?

#clean data
#newdata
#does including employment status increase multicollinearity? 
anes_subset02 <- subset(ANES2016, select = c(V161021, V161112, V161342, V161307, V161019,
                                             V161270, V161113, V161267, V161277, V161004))
anes_subset02 <- na.omit(anes_subset01) #na.fail? library(mice)?
#need to code numbers with or without quotes 

anes_subset02$primary2016 <- recode(anes_subset02$V161021,
                                    "(1) 1. Yes, voted in primary or caucus" = 1, 
                                    "(2) 2. No, didn't vote in primary or caucus" = 0)
anes_subset02$V161342 <- recode(anes_subset02$V161342, 
                                "(2) 2. Female" = 1, "(1) 1. Male" = 0)
anes_subset02$V161307 <- recode(anes_subset02$V161307, 
                                "(1) 1. Lower class" = 0, 
                                "(2) 2. Working class" = 0, 
                                "(3) 3. Middle class" = 1, 
                                "(4) 4. Upper class" = 1)

summary(anes_subset02$V161019)

anes_subset02$V161019 <- recode(anes_subset02$V161019,
                                "(1) 1. Democratic party" = 0,
                                "(2) 2. Republican party" = 1, 
                                "(4) 4. None or 'independent" = 3,
                                "(5) 5. Other SPECIFY" = 3)

#need to exclude 90 & 95?
anes_subset02$V161270 <- recode(anes_subset02$V161270, 
                                "(02) 2. 1st, 2nd, 3rd or 4th grade" = 2, 
                                "(03) 3. 5th or 6th grade" = 3, 
                                "(04) 4. 7th or 8th grade" = 4, 
                                "(05) 5. 9th grade" = 5, 
                                "(06) 6. 10th grade" = 6, 
                                "(07) 7. 11th grade" = 7, 
                                "(08) 8. 12th grade no diploma" = 8, 
                                "(09) 9. High school graduate- high school diploma or equivalent (for example: GED)" = 9, 
                                "(10) 10. Some college but no degree" = 10,
                                "(11) 11. Associate degree in college - occupational/vocational program" = 11,
                                "(12) 12. Associate degree in college -- academic program" = 12,
                                "(13) 13. Bachelor's degree (for example: BA, AB, BS)" = 13,
                                "(14) 14. Master's degree (for example: MA, MS, MENG, MED, MSW, MBA)" = 14,
                                "(15) 15. Professional school degree (for example: MD, DDS, DVM, LLB, JD)" = 15,
                                "(16) 16. Doctorate degree (for example: PHD, EDD)" = 16)
anes_subset02$V161112 <- recode(anes_subset02$V161112, "(1) 1. Yes" = 1, 
                                "(2) 2. No" = 0)
anes_subset02$V161113 <- recode(anes_subset02$V161113, "(1) 1. Favor" = 1, 
                                "(2) 2. Oppose" = 2, 
                                "(3) 3. Neither favor nor oppose" = 3)
anes_subset02$V161277 <- recode(anes_subset02$V161277, "(1) 1. Initial employment status: working now" = 1,
                                "2) 2. Initial employment status: temporarily laid off" = 0,
                                "(4) 4. Initial employment status: unemployed" = 0,
                                "(5) 5. Initial employment status: retired" = 0,
                                "(6) 6. Initial employment status: permanently disabled" = 0, 
                                "(7) 7. Initial employment status: homemaker" = 0, 
                                "(8) 8. Initial employment status: student" = 0)
anes_subset02$V161004 <- recode(anes_subset02$V161004, "(1) 1. Very much interested" = 1,
                                "(2) 2. Somewhat interested" = 2,
                                "(3) 3. Not much interested" = 3)



#dependent variable 
#pre 2016: voted in presidential primary
anes_subset02$dep <- -1
anes_subset02$dep[anes_subset02$primary2016== 1] <- "voted"
anes_subset02$dep[anes_subset02$primary2016== 0] <- "abstained"
anes_subset02$dep <- factor(anes_subset02$dep)


#new variable: gender
anes_subset02$sex <- -1
anes_subset02$sex[anes_subset02$V161342== 1] <- "female"
anes_subset02$sex[anes_subset02$V161342== 0] <- "male"
anes_subset02$sex <- factor(anes_subset02$sex)

#new variable: socioeconomic status 
anes_subset02$socio <- -1
anes_subset02$socio[anes_subset02$V161307== 0] <- "low"
anes_subset02$socio[anes_subset02$V161307== 1] <- "high"
anes_subset02$socio <- factor(anes_subset02$socio)

#new variable: Education 
#need to get rid of "others" (90 & 95)
anes_subset02$edu <- -1 
anes_subset02$edu[anes_subset02$V161270 <= 9] <- "low"
anes_subset02$edu[anes_subset02$V161270 >= 10]<- "middle" #10-13
anes_subset02$edu[anes_subset02$V161270 >= 14] <- "high" #14-16
anes_subset02$edu <- factor(anes_subset02$edu)

#independent variable: health insurance 
#does the subject have health insurance?
anes_subset02$insurance <- -1
anes_subset02$insurance[anes_subset02$V161112== 1] <- "coverage"
anes_subset02$insurance[anes_subset02$V161112== 0] <- "no coverage"
anes_subset02$insurance <- factor(anes_subset02$insurance)

#new variable: favor or oppose ACA
anes_subset02$ACA <- -1
anes_subset02$ACA[anes_subset02$V161113== 1] <- "favor"
anes_subset02$ACA[anes_subset02$V161113== 2] <- "oppose"
anes_subset02$ACA[anes_subset02$V161113== 3] <- "neutral"
anes_subset02$ACA <- factor(anes_subset02$ACA)

#new variable: age 
anes_subset02$age <- -1
anes_subset02$age[anes_subset02$V161267 >= 18] <- "18-30 years old"
anes_subset02$age[anes_subset02$V161267 >= 30] <- "30-39 years old"
anes_subset02$age[anes_subset02$V161267 >= 40] <- "40-49 years old"
anes_subset02$age[anes_subset02$V161267 >= 50] <- "50-59 years old"
anes_subset02$age[anes_subset02$V161267 >= 60] <- "60+ years old"
anes_subset02$age <- factor(anes_subset02$age)

#new variable: employment status
anes_subset02$job <- -1
anes_subset02$job[anes_subset02$V161277== 1] <- "Employed"
anes_subset02$job[anes_subset02$V161277== 0] <- "Unemployed"
anes_subset02$job <- factor(anes_subset02$job)

anes_subset02$party <- -1
anes_subset02$party[anes_subset02$V161019== 0] <- "dem"
anes_subset02$party[anes_subset02$V161019== 1] <- "rep"
anes_subset02$party[anes_subset02$V161019== 2] <- "ind_other"
anes_subset02$party[anes_subset02$V161019== 3] <- "ind_other"
anes_subset02$party <- factor(anes_subset02$party)



anes_subset03 <- na.omit(anes_subset02)

#Linear models

output1 <- lm(primary2016 ~ V161112, data=anes_subset03)
summary(output1)

output2 <- lm(primary2016 ~ V161112 + V161277 + V161267 + V161113 + V161270 + V161307 + V161342, data = anes_subset03)
summary(output2)

anes_subset03$insurance <- relevel(anes_subset03$insurance, ref = "no coverage")
output3 <- lm(primary2016 ~ insurance, data = anes_subset03)
summary(output3)

output4 <- lm(primary2016 ~ insurance + job + age + ACA + edu + socio + sex, data = anes_subset03)
summary(output4)
