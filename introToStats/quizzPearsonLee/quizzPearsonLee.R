library(HistData)
data(PearsonLee)

summary(PearsonLee)
names(PearsonLee)
head(PearsonLee)


PEARSON_SUBSET<-subset(PearsonLee,par=="Father")
mean(PEARSON_SUBSET$parent)

mean(PearsonLee$parent[PearsonLee$par=="Father"])

FATHER_ONLY<-PearsonLee$par=="Father"
mean(PearsonLee$parent[FATHER_ONLY])


PearsonLee$child/PearsonLee$parent