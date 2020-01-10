# data with Amelia code 
library(descr)
library(ggplot2)
library(tidyverse)
library(Amelia)

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
output1 <- lm(primary2016 ~ V161112, data = am_clean)
summary(output1)
output2 <- lm(primary2016 ~ V161112 + V161342 + V161307 + V161019 + V161270 + V161113 + V161267 + V161277 + V161004, data = am_clean)
summary(output2)
