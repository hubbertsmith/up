library(ggplot2)

ggplot(data = anes_subset02) +
  geom_bar(mapping = aes(x= sex, fill = sex))

ggplot(data = anes_subset02) +
  geom_bar(mapping = aes(x= age, fill =age))

summary(anes_subset02$age)


anes_subset02$age <- "NA"
anes_subset02$age[anes_subset02$V161267>= 18 | anes_subset02$V161267<= 29] <- "18-30"
anes_subset02$age[anes_subset02$V161267>= 30 | anes_subset02$V161267<= 39] <- "30-39"
anes_subset02$age[anes_subset02$V161267>= 40 | anes_subset02$V161267<= 49] <- "40-49"
anes_subset02$age[anes_subset02$V161267>= 50 | anes_subset02$V161267<= 59] <- "50-59"
anes_subset02$age[anes_subset02$V161267>= 60] <- "60+"
anes_subset02$age <- factor(anes_subset02$age)

summary(anes_subset02$age)
