# "The Forestry data set is on the website. Download it and read it
# into R. The data set has the following 4 variables:
# 
# - MDBH: Mean diameter at breast height of the trees (response).    
# This measures the total amount timber in the stand.
# - AGE: Age in years of the stand of trees
# - HD: Average Height of dominant trees in the stand
# - The number of trees in the stand
# 
# There is interest in testing if the MDBH can be well predicted 
# using the easier to collect variables, age, HD and n. Forestry 
# science suggests using the following as predictor variables:
# 
# Using R, create these three predictor variables;
# (1) HD
# (2) tree_years = Age*n - total number of tree years in the stand.
# (3) competition = hd/n - a measure of competition in the stand
# 
# Creata a dataframe with the original variables plus these new variables. Write this dataframe to a csv file in your working directory and make sure there are no rownames included.
# 
# Using lm() fit a linear model with MDBH as the response variable
# and HD, tree_years, competition as the predictor variables. 
# 
# Is there evidence that any of the predictors are related to 
# the response? From the output of the summary() funciton recover the entire ANOVA table. How would you decide to accept/reject the null hypothesis given in the table using the F-tables?
# 
# [Check your results - you could use the anovatab.R source for this.]
# 
# What do you conclude about the need for each variable in the model?  Use the drop1() function to explore this issue for each predictor.
# 
# Calculate a 90% CI and PI for the following stands:
# (a) HD=50, tree_years=10000, comp=0.07
# (b) HD=60, tree_years=6000, comp=0.05
# (c) HD=34, tree_years=12000, comp=0.10
# "

current_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))
forestry <- read.table(file = 'forestry.txt', header = T)
forestry$tree_years <- forestry$age * forestry$n
forestry$competition <- forestry$hd / forestry$n

write.csv(forestry,"new_forestry.csv", row.names = FALSE)

fit_forestry <- lm(mdbh~hd+tree_years+competition, data=forestry)
summary(fit_forestry)

source('anovatab.R')

anovatab(fit_forestry)
#reject the null hypothesis, there is at least one non-zero predictor

#stage 2 analisys
#variance-covariance table
vcov(fit_forestry)

# (a) HD=50, tree_years=10000, comp=0.07
# (b) HD=60, tree_years=6000, comp=0.05
# (c) HD=34, tree_years=12000, comp=0.10
predict_data <- data.frame(hd=c(50,60,34), tree_years=c(10000,6000,12000) , competition=c(0.07,0.05,0.1))
predict(fit_forestry, newdata = predict_data, interval = "confidence", level = 0.90)
predict(fit_forestry, newdata = predict_data, interval = "prediction", level = 0.90)