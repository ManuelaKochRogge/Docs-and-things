##Prognose Studienabbruch an der HS Harz

#################################
library(mlr)
library(ParamHelpers)
library(openxlsx)
library(class)

#################################################################
# 1. Daten vorbereiten

# Daten einlesen
sta_all<- read_excel("Daten_all_ml.xlsx", col_names = TRUE)

# Untersuchung Datensatz
str(sta_all)
sta_all<-sta_all[  ,2:9] # entferne Matrikelnummer
sta_all[  ,c(1,2,5,6,8)] <- lapply(sta_all[  ,c(1,2,5,6,8)], as.factor) # Faktorisierung der nominalen Variablen

# Prädiktoren identifizieren mit "Cluster" als abhängiger Variable
pred_model <- glm(cluster ~.,
                  data = sta_all, family = "binomial")
pred_model <- glm(cluster ~ geschlecht + hzb_art + hzb_note + hzb_jahr+
                    dienst + berufsausbildung + hochschulsemester_vor_akt_studium,
                  data = sta_all, family = "binomial")

# Untersuchung Ergebnisse
summary(pred_model)

# *** Geschlecht, hzb_art, hzb_note
# ** berufsausbildung
# nicht signifikant: hzb_jahr, dienst, hs_semester_vor_aktuellem Studium

# Entferne nicht-signifikante Variablen

sta<-sta_all[  ,c(1,2,3,6,8)]
sta<-as.data.frame(sta)
str(sta) # noch 5 Variablen
summary(sta)
table(sta$cluster) # 0 = kein StA, 1 = StA

################################################################

## 2. Auswahl des "besten Prognosemodells" anhand Benchmarking

# Erzeugen von zwei im wesentlichen identische Pseudo-Tasks 
# (Unterscheidung um auf jedes einzelne davon eine separate Resampling-Methode anzuwenden)
# Definition von StA als positve Klasse
task1 = makeClassifTask(data = sta, target= "cluster", id = "CV", positive = "1") 
task2 = makeClassifTask(data = sta, target = "cluster", id = "Bootstrap", positive = "1")

# Definition von Learnern fuer den Vergleich in der Benchmark-Studie
lrn.rpart = makeLearner("classif.rpart")
lrn.rf = makeLearner("classif.randomForest", par.vals = list(ntree = 30L))
lrn.svm = makeLearner("classif.ksvm")
lrn.log = makeLearner("classif.logreg")


# instanziiere je eine 10-fache CV und ein Bootstrap-Sample mit 100 Iterationen
rinst.cv = makeResampleInstance("CV", iters = 10L, task = task1)
rinst.boot = makeResampleInstance("Bootstrap", iters = 100L, task = task2)

## Benchmark der Verfahren
bench = benchmark(
  learners = list(lrn.rpart, lrn.rf, lrn.svm, lrn.log),
  tasks = list(task1, task2),
  resamplings = list(rinst.cv, rinst.boot),
  measures = list(mmce, acc, tpr, fpr, fnr, tnr)
)

#################################################################

## 3. Vergleich der Resultate der Benchmark-Studie
bench
tab.bench<-getBMRAggrPerformances(bench, as.df=TRUE)
str(tab.bench)
summary(tab.bench) # Identifikation kvsm mit 10facher Kreuzvalidierung

write.xlsx(tab.bench, file = "Benchmark_learner_ml02.xlsx",
           sheetName = "notTuned", append= TRUE)

# Visualisierung der Benchmark-Resultate mit Hilfe von Boxplots
plotBMRBoxplots(bench)

#################################################################
## 4. Durchführung Klassifizierung mit kvsm und 10facher CV

# Kreuzvalidierung (CV) mit 10 Iterationen
rd3 = makeResampleDesc(method = "CV", iters = 10)

# Erstelle Learner (mit Anzeige "prob")
lrn.svm.p = makeLearner("classif.kvsm", predict.type = "prob")
res = resample(learner = lrn.svm.p, task = task1, resampling = rd3)
res
pred.t<-res$pred

# Ermittlung der Performance Kennzahlen
performance(pred.t, measures = list(mmce, acc, fpr, tpr, tnr, fnr,f1))


# Ermittlung "optimaler Treshold"
d <- generateThreshVsPerfData(pred.t, measures = list(tpr, tnr, mmce))
plotThreshVsPerf(d) # Treshold von 0.35 scheint vertretbar

#################################################################
## 5. Tuning (Treshold anpassen)
# Learner anpassen
lrn.rpart.p = makeLearner("classif.rpart", predict.type = "prob")
lrn.rf.p = makeLearner("classif.randomForest", par.vals = list(ntree = 30L), predict.type = "prob")
lrn.svm.p = makeLearner("classif.ksvm", predict.type = "prob")
lrn.log.p = makeLearner("classif.logreg", predict.type = "prob")

tuned.lrn.rpart <- setPredictThreshold(lrn.rpart.p, 0.3)
tuned.lrn.rf<- setPredictThreshold(lrn.rf.p, 0.3)
tuned.lrn.svm <- setPredictThreshold(lrn.svm.p, 0.3)
tuned.lrn.log<<-setPredictThreshold(lrn.log.p, 0.3)

#Benchmark der Learner mit Treshold 0.30
bench.tuned_all = benchmark(
  learners = list(tuned.lrn.rpart, tuned.lrn.rf, tuned.lrn.svm, tuned.lrn.log),
  tasks = list(task1, task2),
  resamplings = list(rinst.cv, rinst.boot),
  measures = list(mmce, acc, tpr, fpr, fnr, tnr)
)

bench.tuned_all
tab.bench.tuned_all<-getBMRAggrPerformances(bench.tuned_all, as.df=TRUE)
str(tab.bench.tuned_all)
summary(tab.bench.tuned_all)
write.xlsx(tab.bench.tuned_all, file = "Benchmark_learner_ml03.xlsx",
           sheetName = "Treshold_30_", append = TRUE)

plotBMRBoxplots(bench.tuned_all)
################################################################
# 6. Ermittlung Performance und  ROC Curves 

# ksvm (Treshold 0.5)
res.05 = resample(learner = lrn.svm.p, task = task1, resampling = rd3)
res.05
pred.05<-res.05$pred
performance(pred.05, measures = list(mmce, acc, fpr, tpr, tnr, fnr, f1))

d1 <- generateThreshVsPerfData(pred.05, measures = list(fpr, tpr, mmce))
plotThreshVsPerf(d1) 
plotROCCurves(d1)
performance(pred.05, mlr::auc) #0.636

# ksvm (Treshold 0.3)
res.03 = resample(learner = tuned.lrn.svm, task = task1, resampling = rd3)
res.03
pred.03<-res.03$pred
performance(pred.03, measures = list(mmce, acc, fpr, tpr, tnr, fnr, f1))

d2 <- generateThreshVsPerfData(pred.03, measures = list(fpr, tpr, mmce))
plotThreshVsPerf(d2) 
plotROCCurves(d2)
performance(pred.03, mlr::auc) # 0.645

# log Regression (Treshold 0.5)
res.log.50 = resample(learner = lrn.log.p, task = task1, resampling = rd3)
res.log.50
pred.log.50<-res.log.50$pred
performance(pred.log.50, measures = list(mmce, acc, fpr, tpr, tnr, fnr,f1))

d4 <- generateThreshVsPerfData(pred.log.50, measures = list(fpr, tpr, mmce))
plotThreshVsPerf(d4) 
plotROCCurves(d4)
performance(pred.log.50, mlr::auc) # 0.688


# log Regression (Treshold 0.3)
res.log.30 = resample(learner = tuned.lrn.log, task = task1, resampling = rd3)
res.log.30
pred.log.30<-res.log.30$pred
performance(pred.log.30, measures = list(mmce, acc, fpr, tpr, tnr, fnr, f1))

d3 <- generateThreshVsPerfData(pred.log.30, measures = list(fpr, tpr, mmce))
plotThreshVsPerf(d3) 
plotROCCurves(d3)
performance(pred.log.30, mlr::auc) # 0.688

#################################################################

## 7. ROC Curves mit mehreren Algorithmen

# 2 Kurven (ksvm 0.5 und ksvm 0.35)
df = generateThreshVsPerfData(list(kvsm = pred.05, kvsm_tuned = pred.03), measures = list(fpr, tpr))
plotROCCurves(df)

# 2 Kurven (kvsm 0.3 und logreg 0.3)
df1 = generateThreshVsPerfData(list(kvsm_tuned = pred.03, 
       log.reg_tuned  = pred.025), measures = list(fpr, tpr))
plotROCCurves(df1)

################################################################




