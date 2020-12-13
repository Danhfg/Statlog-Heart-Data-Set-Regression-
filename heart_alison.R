install.packages('tidyverse')
library(tidyverse)
install.packages("gmodels")

### Leitura

hpt <- read.csv('heart_c.csv', header=TRUE, na='?')
hpt.names <- read.delim('heart-disease.names')
hpt.names <- hpt.names %>%
  slice(96:109) %>%
  extract(1, into = 'name', regex= "(?:\\()(\\w+)(?:\\))")
names(hpt) <- str_trim(hpt.names$name)
# rename variables!
##write.table(hpt, 'hpt.csv')

### Variaveis Categoricas

summary(hpt)
colnames(hpt)

##sex
t <- table(hpt[,2])
addmargins(t)
round(prop.table(t), digits=2)
round(100*prop.table(t), digits=1)
t

##cp
t <- table(hpt[,3])
addmargins(t)
round(prop.table(t), digits=2)
round(100*prop.table(t), digits=1)
t

##fbs
t <- table(hpt[,6])
addmargins(t)
round(prop.table(t), digits=2)
round(100*prop.table(t), digits=1)
t

##num
t <- table(hpt[,14])
addmargins(t)
round(prop.table(t), digits=2)
round(100*prop.table(t), digits=1)
t

##restcg
t <- table(hpt[,7])
addmargins(t)
round(prop.table(t), digits=2)
round(100*prop.table(t), digits=1)
t

##exang
t <- table(hpt[,9])
addmargins(t)
round(prop.table(t), digits=2)
round(100*prop.table(t), digits=1)
t

##slop
t <- table(hpt[,11])
addmargins(t)
round(prop.table(t), digits=2)
round(100*prop.table(t), digits=1)
t

#thal
t <- table(hpt[,13])
addmargins(t)
round(prop.table(t), digits=2)
round(100*prop.table(t), digits=1)
t

library(gmodels)
##sex
CrossTable(hpt[,2], hpt[,14])
##CP
CrossTable(hpt[,3], hpt[,14])
##FBS
CrossTable(hpt[,6], hpt[,14])
##restcg
CrossTable(hpt[,7], hpt[,14])
##exang
CrossTable(hpt[,9], hpt[,14])
##slope
CrossTable(hpt[,11], hpt[,14])
#thal
CrossTable(hpt[,13], hpt[,14])


cor.test(hpt[,2], hpt[,14], use='complete.obs', method = 'spearman', exact = FALSE)
cor.test(hpt[,3], hpt[,14], use='complete.obs', method = 'spearman', exact = FALSE)
cor.test(hpt[,6], hpt[,14], use='complete.obs', method = 'spearman', exact = FALSE)
cor.test(hpt[,7], hpt[,14], use='complete.obs', method = 'spearman', exact = FALSE)
cor.test(hpt[,9], hpt[,14], use='complete.obs', method = 'spearman', exact = FALSE)
cor.test(hpt[,11], hpt[,14], use='complete.obs', method = 'spearman', exact = FALSE)
cor.test(hpt[,13], hpt[,14], use='complete.obs', method = 'spearman', exact = FALSE)

df <- hpt %>%
  select(c(2, 3,6,7,9,11,13))
cor_matrix <- round(cor(df, method = 'spearman', use='complete.obs'), 2)
cor_matrix

library(corrplot)

pairs(df)
corrplot(cor_matrix, method = "circle")

#gender
b_gender <- lm(hpt[,14] ~ hpt[,2], data = hpt)
summary(b_gender)
confint(b_gender)

plot(hpt[,2], hpt[,14], xlab = 'gender', ylab = "DIE/ALIVE")

### Variaveis Numéricas

#age
t <- table(hpt[,1])
addmargins(t)
round(prop.table(t), digits=2)
round(100*prop.table(t), digits=1)
t

#trestbps
t <- table(hpt[,4])
addmargins(t)
round(prop.table(t), digits=2)
round(100*prop.table(t), digits=1)
t

#chol
t <- table(hpt[,5])
addmargins(t)
round(prop.table(t), digits=2)
round(100*prop.table(t), digits=1)
t

#thalach
t <- table(hpt[,8])
addmargins(t)
round(prop.table(t), digits=2)
round(100*prop.table(t), digits=1)
t

#oldpeak
t <- table(hpt[,10])
addmargins(t)
round(prop.table(t), digits=2)
round(100*prop.table(t), digits=1)
t

#ca
t <- table(hpt[,12])
addmargins(t)
round(prop.table(t), digits=2)
round(100*prop.table(t), digits=1)
t

##age
CrossTable(hpt[,1], hpt[,14])
##trestbps
CrossTable(hpt[,4], hpt[,14])
##chol
CrossTable(hpt[,5], hpt[,14])
##thalach
CrossTable(hpt[,8], hpt[,14])
##olspeak
CrossTable(hpt[,10], hpt[,14])
##ca
CrossTable(hpt[,12], hpt[,14])

cor.test(hpt[,1], hpt[,14], use='complete.obs', method = 'spearman', exact = FALSE)
cor.test(hpt[,4], hpt[,14], use='complete.obs', method = 'spearman', exact = FALSE)
cor.test(hpt[,5], hpt[,14], use='complete.obs', method = 'spearman', exact = FALSE)
cor.test(hpt[,8], hpt[,14], use='complete.obs', method = 'spearman', exact = FALSE)
cor.test(hpt[,10], hpt[,14], use='complete.obs', method = 'spearman', exact = FALSE)
cor.test(hpt[,12], hpt[,14], use='complete.obs', method = 'spearman', exact = FALSE)

df <- hpt %>%
  select(c(1,4,5,8,10,12))
cor_matrix <- round(cor(df, method = 'spearman', use='complete.obs'), 2)
cor_matrix

library(corrplot)

pairs(df)
corrplot(cor_matrix, method = "circle")

#age
b_age <- lm(hpt[,14] ~ hpt[,1], data = hpt)
summary(b_age)
confint(b_age)

plot(hpt[,1], hpt[,14], xlab = 'gender', ylab = "DIE/ALIVE")

#Regressão

summary(lm(num~ sex + cp , data = hpt))
summary(lm(num~ cp + exang, data = hpt))

summary(lm(num~ sex + thal, data = hpt))
summary(lm(num~ cp + exang + sex + thal, data = hpt))
summary(lm(num~ cp + exang + thal, data = hpt))

summary(lm(num~ cp + exang + ca, data = hpt))

summary(lm(num~ cp + exang + thal + ca, data = hpt))


summary(lm(num~ age + ca + trestbps + thalach + oldpeak, data = hpt))
summary(lm(num~ ca + thalach + oldpeak, data = hpt))
summary(lm(num~ sex + cp + fbs + restecg + exang + slope + thal, data = hpt))
summary(lm(num~ cp + exang + slope + thal, data = hpt))
summary(lm(num~ ca + oldpeak + cp + exang + thal, data = hpt))


summary(lm(num~ ca + thalach + oldpeak + cp + restecg + thal, data = hpt))


summary(lm(num~ age + ca + trestbps + chol + thalach + oldpeak + sex + cp + fbs + restecg + exang + slope + thal, data = hpt))


summary(lm(num~ ca + thalach + oldpeak + cp + thal, data = hpt))

hpt[,15] <- (hpt[,14]>0)+0

##############

###data train dan data testing###
install.packages("caTools")
library(caTools)
#randomly split data
set.seed(123)
split=sample.split(hpt$num, SplitRatio = 0.75)
split

qualityTrain=subset(hpt,split == TRUE)
qualityTest=subset(hpt,split == FALSE)
nrow(qualityTrain)
nrow(qualityTest)

x <- lm(num~ ca + oldpeak + cp + exang + thal, data = qualityTrain)
summary(x)
confint(x)

install.packages("prediction")
library(prediction)

predictTest=predict(x, newdata = qualityTest,type = "response")
table(qualityTest$num,predictTest >=0.7)

#accuracia
(117+78)/257

summary(lm(V15~ ca + thalach + sex + cp + exang + thal, data = hpt))

summary(lm(V15~ cp + exang + sex + thal, data = hpt))

summary(lm(V15~ ca + thalach + oldpeak, data = hpt))

summary(lm(V15~ cp + exang + sex + thal + ca + thalach + oldpeak, data = hpt))

library(ggplot2)
ggplot(hpt, aes(x=chol, y=trestbps)) + 
  geom_point(aes(col=V15))
