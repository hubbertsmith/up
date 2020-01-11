library(ggplot2)
library(dplyr)

#rename

#dependent variable 
#pre 2016: voted in presidential primary

am_clean$dep[am_clean$primary2016== 1] <- "voted"
am_clean$dep[am_clean$primary2016== 0] <- "abstained"
am_clean$dep <- factor(am_clean$dep)


#new variable: gender

am_clean$sex[am_clean$V161342== 1] <- "female"
am_clean$sex[am_clean$V161342== 0] <- "male"
am_clean$sex <- factor(am_clean$sex)

#new variable: socioeconomic status 

am_clean$socio[am_clean$V161307== 0] <- "low"
am_clean$socio[am_clean$V161307== 1] <- "high"
am_clean$socio <- factor(am_clean$socio)

#new variable: Education 
#need to get rid of "others" (90 & 95)

am_clean$edu[am_clean$V161270 <= 9] <- "low"
am_clean$edu[am_clean$V161270 >= 10]<- "middle" #10-13
am_clean$edu[am_clean$V161270 >= 14] <- "high" #14-16
am_clean$edu <- factor(am_clean$edu)

#independent variable: health insurance 
#does the subject have health insurance?

am_clean$insurance[am_clean$V161112== 1] <- "coverage"
am_clean$insurance[am_clean$V161112== 0] <- "no coverage"
am_clean$insurance <- factor(am_clean$insurance)

#new variable: favor or oppose ACA

am_clean$ACA[am_clean$V161113== 1] <- "favor"
am_clean$ACA[am_clean$V161113== 2] <- "oppose"
am_clean$ACA[am_clean$V161113== 3] <- "neutral"
am_clean$ACA <- factor(am_clean$ACA)

#new variable: age 
am_clean$age <- -1
am_clean$age[am_clean$V161267 >= 18] <- "18-30"
am_clean$age[am_clean$V161267 >= 30] <- "30-39"
am_clean$age[am_clean$V161267 >= 40] <- "40-49"
am_clean$age[am_clean$V161267 >= 50] <- "50-59"
am_clean$age[am_clean$V161267 >= 60] <- "60+"
am_clean$age <- factor(am_clean$age)

#new variable: employment status
am_clean$job[am_clean$V161277== 1] <- "Employed"
am_clean$job[am_clean$V161277== 0] <- "Unemployed"
am_clean$job <- factor(am_clean$job)


am_clean$party[am_clean$V161019== 0] <- "dem"
am_clean$party[am_clean$V161019== 1] <- "rep"
am_clean$party[am_clean$V161019== 2] <- "ind_other"
am_clean$party[am_clean$V161019== 3] <- "ind_other"
am_clean$party <- factor(am_clean$party)

#regressions 
output1 <- lm(primary2016 ~ V161112, data=am_clean)
summary(output1)

output2 <- lm(primary2016 ~ V161112 + V161277 + V161267 + V161113 + V161270 + V161307 + V161342, data = am_clean)
summary(output2)

anes_subset03$insurance <- relevel(am_clean$insurance, ref = "no coverage")
output3 <- lm(primary2016 ~ insurance, data = am_clean)
summary(output3)

output4 <- lm(primary2016 ~ insurance + job + age + ACA + edu + socio + sex, data = am_clean)
summary(output4)

table(output4$coefficients)

coefficients <- output4$coefficients

hist(coefficients, col= 'slate grey')


