getwd()
setwd("C:/Users/Mitarbeiter/Documents/R")
setwd("C:/Users/manuela_koch-rogge/R Dokumente")

#packages

install.packages("psych")
library(readxl)
library(Rtsne)
library(tidyverse)
library(DataCombine)
library(cluster)
library(ggplot2)
library(sjstats)
library(ClustOfVar)
library(xlsx)
library(ggplot2)
library(plyr)

# 1. Daten einlesen und filtern

Hilfsabfrage<- read_excel("Hilfsabfrage.xlsx", col_names = TRUE)
str(Hilfsabfrage)
Hilfsabfrage_filter<- Hilfsabfrage[   ,-c(5,7,8,9,10,12)] # ohne Urlaubssemester, stud_text, studiengang, fachbereich:txt
str(Hilfsabfrage_filter)


Studentendaten<-read_excel("Studentendaten.xlsx", col_names = TRUE)
str(Studentendaten)
Studentendaten_filter<- Studentendaten[   ,-c(4,6,8)] # ohne staat, hzb_bundesland, hzb_art_txt
str(Studentendaten_filter)


# 2. gefilterte Daten mergen
clust_Abbrecher<- merge(Studentendaten_filter, Hilfsabfrage_filter, by = "matrikelnummer")
str(clust_Abbrecher) # n = 1032
head(clust_Abbrecher, 25)

# 3. Daten zur Analyse vorbereiten

# 3.1. "hzb_art" aggregieren

# allgemeine Hochschulreife 
Replaces <- data.frame(from = c("03", "06", "09", "12", "18", "21", "27", "28", "29", "31", "67"),
                       to = c("AHZB", "AHZB", "AHZB","AHZB", "AHZB", "AHZB", "AHZB","AHZB","AHZB","AHZB", "AHZB" ))
clust_Abbrecher <- FindReplace(data =clust_Abbrecher, Var = "hzb_art", replaceData = Replaces,
                       from = "from", to = "to", exact = TRUE)

# Fachhochschulreife
Replaces <- data.frame(from = c("60", "64", "65", "66", "70", "71", "72", "73", "75", "76"),
                       to = c("FHZB", "FHZB", "FHZB","FHZB", "FHZB", "FHZB", "FHZB","FHZB","FHZB","FHZB" ))
clust_Abbrecher <- FindReplace(data = clust_Abbrecher, Var = "hzb_art", replaceData = Replaces,
                               from = "from", to = "to", exact = TRUE)

# fachgebundene Hochschulreife
Replaces <- data.frame(from = c("48", "49", "51", "59"),
                       to = c("FGHZB", "FGHZB", "FGHZB","FGHZB"))
clust_Abbrecher <- FindReplace(data = clust_Abbrecher, Var = "hzb_art", replaceData = Replaces,
                               from = "from", to = "to", exact = TRUE)

# sonstige Zugangsberechtigung (beruflich Qualifizierte, Eignungspr?fung, Ausland)

Replaces <- data.frame(from = c("33", "34", "37", "39", "52", "77", "78", "81", "82"),
                       to = c("SHZB", "SHZB", "SHZB","SHZB", "SHZB", "SHZB", "SHZB","SHZB","SHZB"))
clust_Abbrecher <- FindReplace(data = clust_Abbrecher, Var = "hzb_art", replaceData = Replaces,
                               from = "from", to = "to", exact = TRUE)

# 3.2. Gr?nde f?r Abbruch aggregieren

Replaces2 <- data.frame(from = c("08", "02", "04", "06", "07", "09", "11", "12", "13"),
                       to = c("Pr?fung NB", "Sonstiges", "Sonstiges","Sonstiges", "Sonstiges", 
                              "Sonstiges", "Sonstiges","Sonstiges","Sonstiges"))
clust_Abbrecher <- FindReplace(data = clust_Abbrecher, Var = "exmatrikulationsgrund", replaceData = Replaces2,
                               from = "from", to = "to", exact = TRUE)



# 3.3. Jahreszahlen der Semester extrahieren

as.character(clust_Abbrecher$immatrikulationssemester)
clust_Abbrecher$immatrikulationssemester<- substring(clust_Abbrecher$immatrikulationssemester, 1,4)
clust_Abbrecher$immatrikulationssemester<-as.numeric(clust_Abbrecher$immatrikulationssemester)
str(clust_Abbrecher)

as.character(clust_Abbrecher$exmatrikulationssemester)
clust_Abbrecher$exmatrikulationssemester<- substring(clust_Abbrecher$exmatrikulationssemester, 1,4)
clust_Abbrecher$exmatrikulationssemester<-as.numeric(clust_Abbrecher$exmatrikulationssemester)
str(clust_Abbrecher)

# 3.4. Alter bei Exmatrikulation anf?gen

alter_bei_exma<- (clust_Abbrecher$exmatrikulationssemester) - (clust_Abbrecher$geburtsjahr)
clust_Abbrecher<- cbind(clust_Abbrecher, alter_bei_exma)

# 3.5. Alter bei Abschluss der HZB anf?gen

alter_bei_hzb<- (clust_Abbrecher$hzb_jahr) - (clust_Abbrecher$geburtsjahr)
clust_Abbrecher<- cbind(clust_Abbrecher, alter_bei_hzb)
str(clust_Abbrecher)

# 4. Korrelation und Variablenreduktion

# 4.1 Strings faktorisieren
clust_Abbrecher[  ,c(2,4,5,8,9,13,15)] <- lapply(clust_Abbrecher[  ,c(2,4,5,8,9,13,15)], as.factor)
str(clust_Abbrecher)

# 4.2. Korrelation der numerischen Variablen
cor_num_clust<- as.table(cor(clust_Abbrecher[  ,c(3,6,7,10,11,12,14,16,17,18)], use = "complete.obs", method = "pearson"))
View(cor_num_clust) # Identifikation: imma_jahr, exma_jahr, geburtsjahr, hzb_jahr zzgl. Matrikelnummer

# kategorische Variablen: staat, dienst entfernen

# 4.3 Entferne Variablen

clust_Abbrecher<- clust_Abbrecher[  ,-c(1,3,4,7,8,11,12)]
str(clust_Abbrecher) # Datensatz besteht nunmehr aus 11 Variablen



# 5. Auf Outlier pr?fen
# 5.1. Datensatz visualisieren

plot(clust_Abbrecher[  ,c(3,5,6,8,10,11)]) # auff?llig: hzb_note, HS-Semester vor akt. Studium, 
# fachsemester, alter_bei_exma, alter_bei HZB

# 5.2 auff?llige Variablen genauer betrachten
boxplot(clust_Abbrecher$hzb_note)
boxplot(clust_Abbrecher$hochschulsemester_vor_akt_studium) # >= 25
boxplot(clust_Abbrecher$alter_bei_hzb) 
boxplot(clust_Abbrecher$alter_bei_exma) # >50 (# 766)

subset(clust_Abbrecher, alter_bei_exma > 50) # 766
subset(clust_Abbrecher, hochschulsemester_vor_akt_studium >= 25) # 840

# 5.3 Outlier entfernen
clust_Abbrecher<- clust_Abbrecher[-c(766, 840),  ]
str(clust_Abbrecher) # 1030 VObservations
clust_A<- clust_Abbrecher[c(1,2,4,6,8,3,5,7,9,10,11)]
str(clust_A)
head(clust_A)
summary(clust_A)

# 6. auf fehlende Werte pr?fen
any(is.na(clust_A)) # False


# 7. Distanzmatrix erstellen

gower_dist <- daisy(clust_A, metric = "gower", type = list() )
summary(gower_dist)
gower_mat <- as.matrix(gower_dist)


gower_mat <- as.matrix(gower_dist)

# 7.2. Output most similar pair

clust_A[
  which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
        arr.ind = TRUE)[1, ], ]

# 7.3. Output most dissimilar pair

clust_A[
  which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),
        arr.ind = TRUE)[1, ], ]


# 8. Anzahl geeigneter Cluster ermitteln


sil_width <- c(NA)
for(i in 2:10){
  pam_fit <- pam(gower_dist,
                 diss = TRUE,
                 k = i)
  sil_width[i] <- pam_fit$silinfo$avg.width
}

# Plot sihouette width

plot(1:10, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:10, sil_width)



# 9. hierarchisches Clustering
hc_A<- agnes(gower_dist, method = "complete")
as.dendrogram(hc_A)
plot(hc_A)
pltree(hc_A, cex = 0.6, hang = -1, main = "Cluster Abbrecher (complete)")


# 9.1. k=3
clusters_k3 <- cutree(hc_A, k =3)

k3_abbrecher<- mutate(clust_A, cluster = clusters_k3)
head(k3_abbrecher)
count(k3_abbrecher, "cluster")
freq_k3<-t(count(k3_abbrecher, "cluster"))
freq_k3<-freq_k3[2,  ]
freq_k3

sum1_k3<-k3_abbrecher[  ,6:12]%>%
  group_by(cluster) %>%
  summarise_all(funs(mean(.)))

head(sum1_k3)
sum_k3<-t(sum1_k3)
colnames(sum_k3)<- c("cluster 1", "cluster 2", "cluster 3")
sum_k3

crosstab1<-table(k3_abbrecher$geschlecht, k3_abbrecher$cluster)
prob_g<- round(prop.table(crosstab1,2), digits = 2)
prob_g

crosstab2<-table(k3_abbrecher$hzb_art, k3_abbrecher$cluster)
prob_hzb<- round(prop.table(crosstab2,2), digits = 2)
prob_hzb

crosstab3<-table(k3_abbrecher$berufsausbildung, k3_abbrecher$cluster)
prob_beruf<- round(prop.table(crosstab3,2), digits = 2)
prob_beruf

crosstab4<-table(k3_abbrecher$exmatrikulationsgrund, k3_abbrecher$cluster)
prob_grund<- round(prop.table(crosstab4,2), digits = 2)
prob_grund

crosstab5<-table(k3_abbrecher$fachbereich, k3_abbrecher$cluster)
prob_fb<- round(prop.table(crosstab5,2), digits = 2)
prob_fb

sum_clust_k3<- round(rbind(freq_k3, sum_k3[2:7,  ],prob_g, prob_hzb, prob_beruf, prob_grund, prob_fb ), digits =2)
sum_clust_k3

# 9.2. k=4
clusters_k4 <- cutree(hc_A, k =4)

k4_abbrecher<- mutate(clust_A, cluster = clusters_k4)
head(k4_abbrecher)
count(k4_abbrecher, "cluster")
freq_k4<-t(count(k4_abbrecher, "cluster"))
freq_k4<-freq_k4[2,  ]


sum1_k4<-k4_abbrecher[  ,6:12]%>%
  group_by(cluster) %>%
  summarise_all(funs(mean(.)))

head(sum1_k4)
sum_k4<-t(sum1_k4)
colnames(sum_k4)<- c("cluster 1", "cluster 2", "cluster 3", "cluster 4")
sum_k4

crosstab1<-table(k4_abbrecher$geschlecht, k4_abbrecher$cluster)
prob_g<- round(prop.table(crosstab1,2), digits = 2)
prob_g

crosstab2<-table(k4_abbrecher$hzb_art, k4_abbrecher$cluster)
prob_hzb<- round(prop.table(crosstab2,2), digits = 2)
prob_hzb

crosstab3<-table(k4_abbrecher$berufsausbildung, k4_abbrecher$cluster)
prob_beruf<- round(prop.table(crosstab3,2), digits = 2)
prob_beruf

crosstab4<-table(k4_abbrecher$exmatrikulationsgrund, k4_abbrecher$cluster)
prob_grund<- round(prop.table(crosstab4,2), digits = 2)
prob_grund

crosstab5<-table(k4_abbrecher$fachbereich, k4_abbrecher$cluster)
prob_fb<- round(prop.table(crosstab5,2), digits = 2)
prob_fb

sum_clust_k4<- round(rbind(freq_k4, sum_k4[2:7,  ],prob_g, prob_hzb, prob_beruf, prob_grund, prob_fb ), digits =2)
sum_clust_k4


# 9.3. k=6
clusters_k6 <- cutree(hc_A, k =6)

k6_abbrecher<- mutate(clust_A, cluster = clusters_k6)
head(k6_abbrecher)
count(k6_abbrecher, "cluster")
freq_k6<-t(count(k6_abbrecher, "cluster"))
freq_k6<-freq_k6[2,  ]
freq_k6

sum1_k6<-k6_abbrecher[  ,6:12]%>%
  group_by(cluster) %>%
  summarise_all(funs(mean(.)))

head(sum1_k6)
sum_k6<-t(sum1_k6)
colnames(sum_k6)<- c("cluster 1", "cluster 2", "cluster 3", "cluster 4", "cluster 5", "cluster 6")
sum_k6

crosstab1<-table(k6_abbrecher$geschlecht, k6_abbrecher$cluster)
prob_g<- round(prop.table(crosstab1,2), digits = 2)
prob_g

crosstab2<-table(k6_abbrecher$hzb_art, k6_abbrecher$cluster)
prob_hzb<- round(prop.table(crosstab2,2), digits = 2)
prob_hzb

crosstab3<-table(k6_abbrecher$berufsausbildung, k6_abbrecher$cluster)
prob_beruf<- round(prop.table(crosstab3,2), digits = 2)
prob_beruf

crosstab4<-table(k6_abbrecher$exmatrikulationsgrund, k6_abbrecher$cluster)
prob_grund<- round(prop.table(crosstab4,2), digits = 2)
prob_grund

crosstab5<-table(k6_abbrecher$fachbereich, k6_abbrecher$cluster)
prob_fb<- round(prop.table(crosstab5,2), digits = 2)
prob_fb

sum_clust_k6<- round(rbind(freq_k6, sum_k6[2:7,  ],prob_g, prob_hzb, prob_beruf, prob_grund, prob_fb ), digits =2)
sum_clust_k6

# k=10

clusters_k10 <- cutree(hc_A, k =10)

k10_abbrecher<- mutate(clust_A, cluster = clusters_k10)
head(k10_abbrecher)
count(k10_abbrecher, "cluster")
freq_k10<-t(count(k10_abbrecher, "cluster"))
freq_k10<-freq_k10[2,  ]
freq_k10


sum1_k10<-k10_abbrecher[  ,6:12]%>%
  group_by(cluster) %>%
  summarise_all(funs(mean(.)))

head(sum1_k10)
sum_k10<-t(sum1_k10)
colnames(sum_k10)<- c("cluster 1", "cluster 2", "cluster 3", "cluster 4", "cluster 5", "cluster 6", "cluster 7", 
                      "cluster 8", "cluster 9", "cluster 10")
sum_k10

crosstab1<-table(k10_abbrecher$geschlecht, k10_abbrecher$cluster)
prob_g<- round(prop.table(crosstab1,2), digits = 2)
prob_g

crosstab2<-table(k10_abbrecher$hzb_art, k10_abbrecher$cluster)
prob_hzb<- round(prop.table(crosstab2,2), digits = 2)
prob_hzb

crosstab3<-table(k10_abbrecher$berufsausbildung, k10_abbrecher$cluster)
prob_beruf<- round(prop.table(crosstab3,2), digits = 2)
prob_beruf

crosstab4<-table(k10_abbrecher$exmatrikulationsgrund, k10_abbrecher$cluster)
prob_grund<- round(prop.table(crosstab4,2), digits = 2)
prob_grund

crosstab5<-table(k10_abbrecher$fachbereich, k10_abbrecher$cluster)
prob_fb<- round(prop.table(crosstab5,2), digits = 2)
prob_fb

sum_clust_k10<- round(rbind(freq_k10, sum_k10[2:7,  ],prob_g, prob_hzb, prob_beruf, prob_grund, prob_fb ), digits =2)
sum_clust_k10


# 10. nach Excel exportieren


write.xlsx(sum_clust_k3, file="cluster_Abbrecher01.xlsx",
           sheetName="k3")
write.xlsx(sum_clust_k4, file="cluster_Abbrecher01.xlsx",
           sheetName="k4", append = TRUE)
write.xlsx(sum_clust_k6, file="cluster_Abbrecher01.xlsx",
           sheetName="k6", append = TRUE)
write.xlsx(sum_clust_k10, file="cluster_Abbrecher01.xlsx",
           sheetName="k10", append = TRUE)


