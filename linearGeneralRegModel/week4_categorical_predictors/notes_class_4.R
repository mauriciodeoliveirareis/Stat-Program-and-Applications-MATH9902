current_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))
clinical <- read.table(file = 'clinical.txt', header = T)

fit_clinical <- lm(response~treatment, data=clinical)
summary(fit_clinical)

clinical$d1=clinical$d2=clinical$d3=0
clinical$d1[clinical$treatment==1]=1 
clinical$d2[clinical$treatment==2]=1 
clinical$d3[clinical$treatment==3]=1 

#fit_clinical <- lm(response~d1+d2+d3, data=clinical)
#drop d1 as we can't fit all the 3 dummy model
fit_clinical <- lm(response~d2+d3, data=clinical)
summary(fit_clinical)

#using factor to do exactly what we did above:
fit_clinical <- lm(response~factor(treatment), data=clinical)
summary(fit_clinical)
#B0 would be the expected response for the treatment 1!
