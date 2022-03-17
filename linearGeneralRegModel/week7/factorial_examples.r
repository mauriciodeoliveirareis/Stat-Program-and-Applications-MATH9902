#TODO FIND here how to compare all categorical values

current_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))
source('anovatab.R')
## Factorial Analysis
# pain data: main effects model
pain=read.table("pain.txt",header=T)
pain$Codeine=factor(pain$Codeine);pain$Acupuncture=factor(pain$Acupuncture);
fit_factorial1=lm(Relief~Codeine+Acupuncture,data=pain)
anovatab(fit_factorial1)
drop1(fit_factorial1,test='F')
library(emmeans)
emmip(fit_factorial1, Codeine ~ Acupuncture)

# pain data: interaction model
fit_factorial2=update(fit_factorial1,.~.+Codeine:Acupuncture)
emmip(fit_factorial2, Codeine ~ Acupuncture)
drop1(fit_factorial2,test='F')

# pain data: interaction & blocks included
pain$PainLevel=factor(pain$PainLevel)
fit_factorial3=update(fit_factorial2,.~PainLevel+.)
drop1(fit_factorial3,test='F')
emmip(fit_factorial3, Codeine ~ Acupuncture)


## crop data
crop=read.csv("crop.csv",header=T)
crop$Fertiliser=factor(crop$Fertiliser);crop$Promoter=factor(crop$Promoter)
fit_crop=lm(Yield~Fertiliser+Promoter,data=crop)
anovatab(fit_crop)
drop1(fit_crop,test='F')
emmip(fit_crop, Fertiliser ~ Promoter)

# interaction model
fit_crop=update(fit_crop,.~.+Fertiliser:Promoter,data=crop)
anovatab(fit_crop)
drop1(fit_crop,test='F')
summary(fit_crop)
emmip(fit_crop, Promoter ~ Fertiliser)
emmeans(fit_crop,pairwise~Fertiliser:Promoter,adjust='fdr')


