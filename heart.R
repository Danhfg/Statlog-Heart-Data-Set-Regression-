install.packages('tidyverse')
library(tidyverse)
install.packages("gmodels")

hpt <- read.csv('processed.cleveland.data', header=FALSE)
hpt.names <- read.delim('heart-disease.names')
hpt.names <- hpt.names %>%
     slice(96:109) %>%
     extract(1, into = 'name', regex= "(?:\\()(\\w+)(?:\\))")
names(hpt) <- str_trim(hpt.names$name)
# rename variables!
##write.table(hpt, 'hpt.csv')     

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


cor.test(hpt[,2], hpt[,14], use='complete.obs', method = 'spearman', exact = FALSE)
cor.test(hpt[,3], hpt[,14], use='complete.obs', method = 'spearman', exact = FALSE)
cor.test(hpt[,6], hpt[,14], use='complete.obs', method = 'spearman', exact = FALSE)
cor.test(hpt[,7], hpt[,14], use='complete.obs', method = 'spearman', exact = FALSE)
cor.test(hpt[,9], hpt[,14], use='complete.obs', method = 'spearman', exact = FALSE)
cor.test(hpt[,11], hpt[,14], use='complete.obs', method = 'spearman', exact = FALSE)

df <- hpt %>%
        select(c(2, 3,6,7,9,11))
cor_matrix <- round(cor(df, method = 'spearman', use='complete.obs'), 2)
cor_matrix
