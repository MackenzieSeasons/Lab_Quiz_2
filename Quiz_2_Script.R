library(tidyverse)
bfi_data <- psych::bfi

#View(bfi_data)

categorical_variables <- select (bfi_data, gender)
categorical_variables$gender <- as.factor(categorical_variables$gender)
levels(categorical_variables$gender) <- list("Male"=1, "Female"=2)

categorical_variables <- select (bfi_data, education)


agreeableness_items <- select(bfi_data, A1, A2, A3, A4, A5)
extraversion_items <- select(bfi_data, E1, E2, E3, E4, E5)
neuroticism_items <- select(bfi_data, N1, N2, N3, N4, N5)
gender <- select(bfi_data, gender)
age <- select(bfi_data, age)
education <- select(bfi_data, education)

#reverse keys

agreeableness_items <- mutate(agreeableness_items, A1=7-A1)
extraversion_items <- mutate(extraversion_items, E1=7-E1)
extraversion_items <- mutate(extraversion_items, E2=7-E2)

agreeableness <- psych::alpha(as.data.frame(agreeableness_items),check.keys = FALSE)$scores
extraversion <- psych::alpha(as.data.frame(extraversion_items),check.keys = FALSE)$scores
neuroticism <- psych::alpha(as.data.frame(neuroticism_items),check.keys = FALSE)$scores


analytic_data <- cbind(agreeableness, extraversion, neuroticism, gender, education, age)
write_csv(analytic_data, path="analytic_data.csv")

library(apaTables)

analytic_data_no_sex <- select(analytic_data, agreeableness, extraversion, neuroticism, education, age)
apa.cor.table(analytic_data_no_sex, filename = "Table_1.doc", table.number = 1)

analytic_data_men_over_40 <- filter(analytic_data, gender=="male")
analytic_data_men_over_40 <- filter(analytic_data, age==40)
apa.cor.table(analytic_data_men_over_40, filename = "Table_2.doc", table.number = 2)

my.plot <- qplot(agreeableness,extraversion,data=analytic_data_men_over_40)
print(my.plot)
my.plot <- my.plot + theme_classic()
my.plot <- my.plot + theme(axis.line.x=element_line(colour = 'black', size=0.5, 	linetype='solid'),axis.line.y=element_line(colour='black', size=0.5,linetype = 'solid'))
print(my.plot)

cor.test(x=analytic_data_men_over_40$agreeableness, y=analytic_data_men_over_40$extraversion)







