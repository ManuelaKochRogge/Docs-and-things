# Calculating marginal effects models 1-3 University Drop-Outs HHS #######

install.packages("margins")

library(ggplot2)
library(tibble)
library(broom)
library(margins)
library(Ecdat)
library(openxlsx)


# data model 123 ##################

m_123<- read.xlsx('all_data_s3.xlsx', colNames = TRUE)
str(m_123)
m_123[  ,c(1,3,4,6,8)] <- lapply(m_123[  ,c(1,3,4,6,8)], as.factor) # Faktorisierung der nominalen Variablen


m_0<-m_123[  ,c(1:9)] # nur demografische Variablen
str(m_0)

logit_m_0 = glm(cluster ~ ., data = m_0, family = "binomial")
tidy(logit_m_0)
summary(logit_m_0) # nicht signifikant: Dienst, HS-Semester vor aktuellem Studium


sink("logit_m_0.txt")
print(summary(logit_m_0))
sink()

# exclude non-significant variables
m_123<-m_123[   ,-c(4,7)]
str(m_123)
m_0<- m_123[   c(1:7)] # 7 Variablen
str(m_123)

# Building models

m_123   # Model 1+2+3 (alle Variablen)
m_1<-m_123[ ,1:8] # Model 1 (Durchschnittsnote)
m_2<-m_123[  ,c(1:7,10)] # Model 2 (schwierige Fächer)
m_3<-m_123[  ,c(1:7,9)] # Model 3 (Versuche)
m_1_2<-m_123[  ,c(1:8,10)]# Note und schwierige Föcher
m_1_3<-m_123[  ,1:9] # Note und Versuch
m_2_3<-m_123[  ,c(1:7,9,10)] # Versuch und schwierige Fächer



logit_m_0 = glm(cluster ~ ., data = m_0, family = "binomial")
logit_m_1 = glm(cluster ~ ., data = m_1, family = "binomial")
logit_m_2 = glm(cluster ~ ., data = m_2, family = "binomial")
logit_m_3 = glm(cluster ~ ., data = m_3, family = "binomial")
logit_m_1_2 = glm(cluster ~ ., data = m_1_2, family = "binomial")
logit_m_1_3 = glm(cluster ~ ., data = m_1_3, family = "binomial")
logit_m_2_3 = glm(cluster ~ ., data = m_2_3, family = "binomial")
logit_m_123 = glm(cluster ~ ., data = m_123, family = "binomial")



#Calculate margins#######e
x.0 = margins(logit_m_0) 
x.1= margins(logit_m_1) 
x.2= margins(logit_m_2) 
x.3= margins(logit_m_3) 
x.1_2= margins(logit_m_1_2) 
x.1_3 = margins(logit_m_1_3) 
x.2_3 = margins(logit_m_2_3) 
x.123= margins(logit_m_123) 



x.0<-summary(x.0)
x.1<-summary(x.1)
x.2<-summary(x.2)
x.3<-summary(x.3)
x.1_2<-summary(x.1_2)
x.1_3<-summary(x.1_3)
x.2_3<-summary(x.2_3)
x.123<-summary(x.123)

write.xlsx(x.0, file = "x0.xlsx", sheetName = "x0", append = TRUE)
write.xlsx(x.1, file = "x1.xlsx", sheetName = "x1", append = TRUE)
write.xlsx(x.2, file = "x2.xlsx", sheetName = "x2", append = TRUE)
write.xlsx(x.3, file = "x3.xlsx", sheetName = "x3", append = TRUE)
write.xlsx(x.1_2, file = "x1_2.xlsx", sheetName = "x1_2", append = TRUE)
write.xlsx(x.1_3, file = "x1_3.xlsx", sheetName = "x1_3", append = TRUE)
write.xlsx(x.2_3, file = "x2_3.xlsx", sheetName = "x2_3", append = TRUE)
write.xlsx(x.123, file = "x123a.xlsx", sheetName = "x123", append = TRUE)

