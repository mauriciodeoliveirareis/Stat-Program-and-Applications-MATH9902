"VARIABLE DESCRIPTIONS:
survival        Survival
                (0 = No; 1 = Yes)
pclass          Passenger Class
(1 = 1st; 2 = 2nd; 3 = 3rd)
name            Name
sex             Sex
age             Age
sibsp           Number of Siblings/Spouses Aboard
parch           Number of Parents/Children Aboard
ticket          Ticket Number
fare            Passenger Fare
cabin           Cabin
embarked        Port of Embarkation
(C = Cherbourg; Q = Queenstown; S = Southampton)

SPECIAL NOTES:
Pclass is a proxy for socio-economic status (SES)
1st ~ Upper; 2nd ~ Middle; 3rd ~ Lower

Age is in Years; Fractional if Age less than One (1)
If the Age is Estimated, it is in the form xx.5

With respect to the family relation variables (i.e. sibsp and parch)
some relations were ignored.  The following are the definitions used
for sibsp and parch.

Sibling:  Brother, Sister, Stepbrother, or Stepsister of Passenger Aboard Titanic
Spouse:   Husband or Wife of Passenger Aboard Titanic
Parent:   Mother or Father of Passenger Aboard Titanic
Child:    Son, Daughter, Stepson, or Stepdaughter of Passenger Aboard Titanic

Other family relatives excluded from this study include cousins,
nephews/nieces, aunts/uncles, and in-laws.  Some children travelled
only with a nanny, therefore parch=0 for them.  As well, some
travelled with very close friends or neighbors in a village, however,
the definitions do not support such relations."

#########################################################################
current_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))

data=read.csv("titanic.csv",header=T)
attributes(data)$names=tolower(attributes(data)$names)
names=attributes(data)$names
names
datac=data[complete.cases(data),]

### reproducible random stream 
## split data in training data set (approx. 66%) and validation data set (aprox. 33%)
set.seed(32441582)
include=rbinom(nrow(datac),1,2/3)
train=subset(datac,include==1)
validation=subset(datac,include==0)
## most naive model - just flip a coin!
attach(validation)
guess1=rbinom(nrow(validation),1,.5)
tab1=table(survived,guess1)
tab1
round(prop.table(tab1)*100,0)
### slightly better guess - flip a weighted coin.
p=sum(train$survived)/nrow(train);p
guess2=rbinom(nrow(validation),1,p)
tab2=table(survived,guess2)
round(prop.table(tab2)*100,0)
detach(validation)
## try logistic regression 
fit=glm(survived~fare,family=binomial,data=train)

### Get a plot of the age relationship to survival probability
fares=data.frame(fare=1:500)
fitted=predict(fit,newdata=fares,type="response")
plot(fares$fare,fitted,typ='l',lwd=3,col='blue')

## Apply this form of prediction to the validation data
predicted=predict(fit,newdata=validation,type="response")
guess3=NA
guess3[predicted>=0.5]=1;
guess3[predicted<0.5]=0
tab3=table(validation$survived,guess3)
round(prop.table(tab3)*100,0)

## Use a different decision threshold?
summary(predicted)
quantile(predicted,p=seq(0,1,by=.1))
guess4=NA
guess4[predicted>=0.35]=1;
guess4[predicted<0.35]=0
tab4=table(validation$survived,guess4)
round(prop.table(tab4)*100,0)

#Mauricio method TODO finish this
guess5=NA
guess5=rbinom(nrow(predicted),1,predicted);
guess5[predicted<0.35]=0
tab5=table(validation$survived,guess5)
round(prop.table(tab4)*100,0)



# considering sensitivity and specificity?
round(prop.table(tab3,1)*100,0)
round(prop.table(tab4,1)*100,0)
#install.packages("ROCR")
library(ROCR)
p.hat=matrix(0,nrow=nrow(validation),ncol=2)
p.hat[,2]=predict(fit,newdata=validation,level='reponse')
p.hat[,1]=1-p.hat[,2]
p.scores <- prediction(p.hat[,2],validation$survived)
p.perf <- performance(p.scores, "tpr", "fpr")
# Plot the ROC curve
plot(p.perf, col = "blue", lwd = 2,xlab='1-Specificity (FPR)',ylab='Sensitivity (TPR)',main='ROC analysis: predictting survival as positive case')
abline(a=0,b=1,col='red',lty=3,lwd=2)
#AUC
p.auc <- performance(p.scores, "auc")
text(0.1,1, paste("AUC=",round(p.auc@y.values[[1]],3)),col='red')
## note the way we need to extract elements of an s4 class 
##object
legend("bottomright",legend=c("ROC","Line of No Discrimination"),col=c('blue','red'),lty=c(1,3),lwd=2)
