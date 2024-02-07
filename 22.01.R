# Install/Load Packages ---------------------------------------------------
Sys.setenv(LANG = "en") # setting right language

help (print)

# Setting Working Directory -----------------------------------------------

setwd("d:/documente/Essex coursework YEAR 3/SC385 Modelling Crime in Society/R Scripts")

# Importing and Cleaning/Preparing Data -----------------------------------
# Read and store comma separated values data
Exp <- read.csv("lab2_exp1.csv") 

# Data Analysis and Visualization -----------------------------------------

#Let's start to explore the dataset by tabulating the treatment/independent variable
table(Exp$condition_num) 

# Let's check some descriptive statistics 
summarise (Exp, AVG_Gender = mean(Gender)) 
# The output will appear in the console but it is not stored in R, to save the output you need to store it in an R object:
table1 <- summarise(Exp, 
                    AVG_Gender = mean(Gender)) 
print(table1) # print R object

# Now we can update our table & include more information
table1 <- summarise(Exp, 
                    AVG_Gender = mean(Gender), 
                    AVG_age = mean(age),AVG_Education = mean(Education))

summarise(Exp, AVG_Education = mean(Education))
table <- summarise(Exp, AVG_Education = mean(Education))
print(table1)

help(std.error)

# Let's now add Standard Errors and Confidence Intervals.
table1 <- summarise(Exp, 
                    AVG_Gender = mean(Gender), 
                    SE_Gender = std.error(Gender), # Compute SEs
                    AVG_age = mean(age),
                    SE_age = std.error(age)) #specify data and statistic to be computed
print(table1)

# Confidence intervals give us a range where we can expect the true mean to fall. Here, we calculate the confidence interval using the standard error.
table1$CI_Age = table1$SE_age * 1.96 
# We add the lower and upper bounds of the confidence interval to our summary table.
table1$lower_CI_Age = table1$AVG_age - table1$CI_Age
table1$upper_CI_Age = table1$AVG_age + table1$CI_Age

print(table1)

table2 <- summarise(Exp, # specify data 
                    .by = condition_num, # specify grouping variable
                    AVG_age = mean(age), # Compute mean
                    SE_age = std.error(age), # Compute SEs
) #specify data, grouping variable, statistic to be computed
print(table2)

# Confidence intervals give us a range where we can expect the true mean to fall. Here, we calculate the confidence interval using the standard error.
table2$CI_Age = table2$SE_age * 1.96 
# We add the lower and upper bounds of the confidence interval to our summary table.
table2$lower_CI_Age = table2$AVG_age - table2$CI_Age
table2$upper_CI_Age = table2$AVG_age + table2$CI_Age
print(table2)

table3 <- summarise(Exp, # specify data 
                    .by = condition_num, # specify grouping variable
                    AVG_S = mean(Story_bias), # Compute mean
                    SE_S = std.error(Story_bias), # Compute SEs
) #specify data, grouping variable, statistic to be computed
print(table3)


table3$CI_S = table3$SE_S * 1.96
table3$lower_CI_S = table3$AVG_S - table3$CI_S
table3$upper_CI_S = table3$AVG_S + table3$CI_S
print(table3)

summary(t.test(Story_bias ~ condition_num, data = Exp)) # T-test
summary(aov(Story_bias ~ condition_num, data = Exp)) # ANOVA  
summary(lm(Story_bias ~ condition_num, data = Exp)) # OLS regression

summary(lm(age ~ condition_num, data = Exp))


