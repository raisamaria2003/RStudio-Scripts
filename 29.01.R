# Setting Working Directory -----------------------------------------------

setwd("d:/documente/Essex coursework YEAR 3/SC385 Modelling Crime in Society/R Databases")

# Importing and Cleaning/Preparing Data -----------------------------------
# Read and store comma separated values data
Exp <- read.csv("STAR.csv")
# Data Analysis and Visualization -----------------------------------------

#Let's start to explore the dataset by tabulating the treatment/independent variable
table(Exp$classtype) 
table (Exp$math)

summarise(Exp, summarise(Exp, AVG_reading = mean(reading)) 
summarise(Exp,SE_reading = std.error(reading))
summarise(Exp, CI_reading = CI(reading))

table7 <- summarise(Exp, 
                    AVG_reading = mean(reading), 
                    SE_reading = std.error(reading),
                    AVG_reading = mean(reading),
                    SE_reading = std.error(reading)) 
print(table7)
table7$CI_reading = table7$SE_reading * 1.96 
print(table7)

table8 <- summarise(Exp,
                    .by = classtype, # specify grouping variable
                    AVG_reading = mean(reading), # Compute mean
                    SE_reading = std.error(reading))
            
print(table8)
table8$CI_reading = table8$SE_reading * 1.98
print(table8)

table8$lower_CI_reading = table8$AVG_reading - table8$CI_reading
table8$upper_CI_reading = table8$AVG_reading + table8$CI_reading
print(table8)

summary(t.test(reading ~ classtype, data = Exp)) # T-test
summary(lm(reading ~ classtype, data = Exp)) # OLS regression

mean(Exp$reading[Exp$small==1]) - mean(Exp$reading[Exp$small==0])

summary(lm(math ~ classtype, data = Exp)) 

Logit <- glm(graduated ~ classtype, data = Exp, family = "binomial")
print(Logit)

