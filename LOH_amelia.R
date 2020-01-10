#not sure what libraries we are going to need. I copied these over from previous code
library(haven)
library(descr)
library(ggplot2)
library(stargazer)
library(tidyverse)
library(Amelia)

#rename the data

#split the sample to test the model?

#clean data
#newdata
#does including employment status increase multicollinearity? 
ANES2016 <- subset(da36824.0001, select = c(V161021, V161112, V161342, V161307, V161019,
                                             V161270, V161113, V161267, V161277, V161004))

 #na.fail? library(mice)?
#need to code numbers with or without quotes 
primary2016 <- recode(ANES2016$V161021,
                                "(1) 1. Yes, voted in primary or caucus" = 1, 
                                "(2) 2. No, didn't vote in primary or caucus" = 0)
sex <- recode(ANES2016$V161342, 
                                "(2) 2. Female" = 1, "(1) 1. Male" = 0)
socio <- recode(ANES2016$V161307, 
                                "(1) 1. Lower class" = 0, 
                                "(2) 2. Working class" = 0, 
                                "(3) 3. Middle class" = 1, 
                                "(4) 4. Upper class" = 1)

#need to exclude 90 & 95?
edu<- recode(ANES2016$V161270, 
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
insurance <- recode(ANES2016$V161112, "(1) 1. Yes" = 1, 
                                "(2) 2. No" = 0)
ACA <- recode(ANES2016$V161113, "(1) 1. Favor" = 1, 
                                 "(2) 2. Oppose" = 2, 
                                 "(3) 3. Neither favor nor oppose" = 3)
job <- recode(ANES2016$V161277, "(1) 1. Initial employment status: working now" = 1,
                                "2) 2. Initial employment status: temporarily laid off" = 0,
                                "(4) 4. Initial employment status: unemployed" = 0,
                                "(5) 5. Initial employment status: retired" = 0,
                                "(6) 6. Initial employment status: permanently disabled" = 0, 
                                "(7) 7. Initial employment status: homemaker" = 0, 
                                "(8) 8. Initial employment status: student" = 0)
interest <- recode(ANES2016$V161004, "(1) 1. Very much interested" = 1,
                                "(2) 2. Somewhat interested" = 2,
                                "(3) 3. Not much interested" = 3)
#fix party
party <- recode(ANES2016$V161019,
                                "(1) 1. Democratic party" = 0,
                                "(2) 2. Republican party" = 1, 
                                "(4) 4. None or 'independent" = 2,
                                "(5) 5. Other SPECIFY" = 2)

  
  
#dependent variable 
#pre 2016: voted in presidential primary
dep <- NA
dep[ANES2016$primary2016== 1] <- "voted"
dep[ANES2016$primary2016== 0] <- "abstained"
dep <- factor(ANES2016$dep)


#new variable: gender
sex <- NA
sex[ANES2016$V161342== 1] <- "female"
sex[ANES2016$V161342== 0] <- "male"
sex <- factor(ANES2016$sex)

#new variable: socioeconomic status 
socio <- NA
socio[ANES2016$V161307== 0] <- "low"
socio[ANES2016$V161307== 1] <- "high"
socio <- factor(ANES2016$socio)

#new variable: Education 
#need to get rid of "others" (90 & 95)
edu <- NA
edu[ANES2016$V161270 >= 9] <- "low"
edu[ANES2016$V161270 >= 10]<- "middle" #10-13
edu[ANES2016$V161270 >= 14] <- "high" #14-16
edu <- factor(ANES2016$edu)

#independent variable: health insurance 
#does the subject have health insurance?
insurance <- NA
insurance[ANES2016$V161112== 1] <- "coverage"
insurance[ANES2016$V161112== 0] <- "no coverage"
insurance <- factor(ANES2016$insurance)

#new variable: favor or oppose ACA
ACA <- NA
ACA[ANES2016$V161113== 1] <- "favor"
ACA[ANES2016$V161113== 2] <- "oppose"
ACA[ANES2016$V161113== 3] <- "neutral"
ACA <- factor(ANES2016$ACA)

#new variable: age 
age <- NA
age[ANES2016$V161267 >= 18] <- "18-30 years old"
age[ANES2016$V161267 >= 30] <- "30-39 years old"
age[ANES2016$V161267 >= 40] <- "40-49 years old"
age[ANES2016$V161267 >= 50] <- "50-59 years old"
age[ANES2016$V161267 >= 60] <- "60+ years old"
age <- factor(ANES2016$age)

#new variable: employment status
job <- NA
job[ANES2016$V161277== 1] <- "Employed"
job[ANES2016$V161277== 0] <- "Unemployed"
job <- factor(ANES2016$job)

party <- NA
party[ANES2016$V161019== 0] <- "dem"
party[ANES2016$V161019== 1] <- "rep"
party[ANES2016$V161019== 2] <- "ind_other"
party[ANES2016$V161019== 3] <- "ind_other"
party <- factor(ANES2016$party)



am_output <- amelia(m =5, noms = c("socio","age","edu"), ords = c("sex", "dep","insurance", "ACA", "job", "party"))

summary(am_output)




