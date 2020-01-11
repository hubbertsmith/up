# data with Amelia code 
library(ggplot2)
library(tidyverse)
library(Amelia)
library(tibble)
library(ggplot2)
library(dplyr)
library(repmis)

source_data("https://github.com/hubbertsmith/up/blob/master/36824-0001-Data.rda?raw=true")

ANES2016 <- da36824.0001

anes_subset02 <- subset(ANES2016, select = c(V161021, V161112, V161342, V161307, V161019,
                                             V161270, V161113, V161267, V161277, V161004))

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


anes_subset02$V161019 <- recode(anes_subset02$V161019,
                                "(1) 1. Democratic party" = 0,
                                "(2) 2. Republican party" = 1, 
                                "(4) 4. None or 'independent" = 2,
                                "(5) 5. Other SPECIFY" = 2)

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
anes_subset02$V161004 <- recode(anes_subset02$V161004, "(1) 1. Very much interested" = 3,
                                "(2) 2. Somewhat interested" = 2,
                                "(3) 3. Not much interested" = 1)

am_output <- amelia(anes_subset02, idvars = "primary2016", noms = c("V161307","V161113", "V161277", "V161021"), 
                    ords =c ("V161112", "V161270", "V161019", "V161267"))

am_output2 <- bind_rows(unclass(am_output$imputations), .id = "m") %>%
  group_by(m) %>%
  nest()

am_clean <- rbind(am_output2[[2]][[1]],am_output2[[2]][[2]],am_output2[[2]][[3]],am_output2[[2]][[4]],am_output2[[2]][[5]])

#rename

#dependent variable 
#pre 2016: voted in presidential primary
am_clean$dep <- -1
am_clean$dep[am_clean$primary2016== 1] <- "voted"
am_clean$dep[am_clean$primary2016== 0] <- "abstained"
am_clean$dep <- factor(am_clean$dep)


#new variable: gender
am_clean$sex <- -1
am_clean$sex[am_clean$V161342== 1] <- "female"
am_clean$sex[am_clean$V161342== 0] <- "male"
am_clean$sex <- factor(am_clean$sex)

#new variable: socioeconomic status 
am_clean$socio <- -1
am_clean$socio[am_clean$V161307== 0] <- "low"
am_clean$socio[am_clean$V161307== 1] <- "high"
am_clean$socio <- factor(am_clean$socio)

#new variable: Education 
#need to get rid of "others" (90 & 95)
am_clean$edu <- -1
am_clean$edu[am_clean$V161270 <= 9] <- "low"
am_clean$edu[am_clean$V161270 >= 10]<- "middle" #10-13
am_clean$edu[am_clean$V161270 >= 14] <- "high" #14-16
am_clean$edu <- factor(am_clean$edu)

#independent variable: health insurance 
#does the subject have health insurance?
am_clean$insurance <- -1
am_clean$insurance[am_clean$V161112== 1] <- "coverage"
am_clean$insurance[am_clean$V161112== 0] <- "no coverage"
am_clean$insurance <- factor(am_clean$insurance)

#new variable: favor or oppose ACA
am_clean$ACA <- -1
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
am_clean$job <- -1
am_clean$job[am_clean$V161277== 1] <- "Employed"
am_clean$job[am_clean$V161277== 0] <- "Unemployed"
am_clean$job <- factor(am_clean$job)

am_clean$party <- -1
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

am_clean$insurance <- relevel(am_clean$insurance, ref = "no coverage")
output3 <- lm(primary2016 ~ insurance, data = am_clean)
summary(output3)

output4 <- lm(primary2016 ~ insurance + job + age + ACA + edu + socio + sex, data = am_clean)
summary(output4)

ggplot(data = am_clean) + 
  geom_bar(mapping = aes(x= age, fill = edu))

table(output4$coefficients)

coefficients <- output4$coefficients

hist(coefficients, col= 'slate grey')


