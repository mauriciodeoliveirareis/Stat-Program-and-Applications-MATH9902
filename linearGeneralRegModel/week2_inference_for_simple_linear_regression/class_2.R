# A study is conducted on 252 men and their percentage body fat is measured along with the circumference of their neck. 
# 
# The data are collected in the file called `bf_study.csv' which is available to download. A simple linear regression model is to be fitted to these data where neck_circumference is the predictor and  body_fat is the response.  
#  
# [NB: You should summarise your answers in writing. ]
# 
# (1) Read these data into R using programming statements.
# (2) Get a report ready scatter plot of these data, including suitable axes labels.
# (3) Fit a simple least squares model to these data. Interpret the LS parameter estimates.
# (4) Conduct a suitable hypothesis test to determine if the predictor is related to the response. State the null and alternative hypotheses, the test statistic, its distribution under the null hypothesis and state your conclusions clearly.
# (5) Find and interpret a 90% CI for the slope.
# (6) Use R to add a plot of the LS line to the scatter plot, add an appropriate legend and export the plot to a pdf graphic file.

current_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))
bf_study <- read.csv(file = 'bf_study.csv')
plot(bf_study$neck_circumference, 
     bf_study$body_fat, 
     main = "Body fat predicted by neck circunference",
     xlab = "Neck Circumference",
     ylab = "Body fat")

fit_bf_study=lm(body_fat~neck_circumference,data=bf_study)
abline(fit_bf_study,col='red',lwd=2)
summary(fit_bf_study)
#body fat increases 1.68 for each unit of neck circumference 
#P value indicated that it's significant 

#hypothesis testing 
#1) H0: B1=0   2)HA: B1!=0
#3)test statistic 
sigmaSq = summary(fit_bf_study)$sigma^2
t = 1.6889 / (sqrt(sigmaSq/ (var(bf_study$neck_circumference) * 251) ))
t

#4) get rejection region for 95% two tailed df = 250
rejectionReg <- qt(c(.025, .975), df=250)

#5) t is way larger than 1.969 so we reject the null hypothesis and can say that neck size 
# can be used to predict body weight

#90% CI for the slope 
1.688927 + qt(.1, df=250) * 0.1897287
1.688927 - qt(.1, df=250) * 0.1897287
