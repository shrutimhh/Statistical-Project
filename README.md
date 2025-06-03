# Statistical-Project
Crime Statistics 

install.packages("dplyr")
install.packages("stats")

library(readxl)
library(tidyr)
library(dplyr)
library(stats)

Crime_data <- read_excel(file.choose())
Crime_data

C_data <- Crime_data %>%
  pivot_longer(cols = c('Sexual Assault', 'Aggravated Assault', Theft), names_to = "Offenses", values_to = "Crime_Type")

summary(Crime_data)
summary(C_data)

boxplot(Crime_Type ~ Offenses, data = C_data,
        main = "Crime Data Analysis",xlab = "Crime_Type",
        ylab = "Offenses")

anova <- aov(Crime_Type ~ Offenses, data = C_data)
summary(anova)

pairwise.t.test(C_data$Crime_Type, C_data$Offenses, p.adj = "bonferroni")


set.seed <- (30)
dplyr::sample_n(Crime_data, 6)
shapiro.test(Crime_data$'Sexual Assault')
shapiro.test(Crime_data$'Aggravated Assault')
shapiro.test(Crime_data$Theft)


xtab <- table(C_data$Offenses, C_data$Crime_Type)
xtab
         
chi_test <- chisq.test(xtab)
chi_test

