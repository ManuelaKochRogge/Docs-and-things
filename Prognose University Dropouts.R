##Prognose Studienabbruch an der HS Harz

#################################
library(mlr)
library(ParamHelpers)
library(openxlsx)
library(class)
library(dplyr)
library(caret)


getwd()
#################################################################


# read data

sta_all <- read.xlsx('Leistungsdaten Sem3.xlsx', colNames = TRUE)


# Untersuchung Datensatz
str(sta_all)
sta_all<-sta_all[  ,2:12] # entferne Matrikelnummer
sta_all[  ,c(1,5,6,8,10)] <- lapply(sta_all[  ,c(1,5,6,8,10)], as.factor) # Faktorisierung der nominalen Variablen
str(sta_all)

# prediction model with "cluster" as dependent variable
pred_model <- glm(cluster ~.,
                  data = sta_all, family = "binomial")

pred_model2 <- glm(cluster ~ geschlecht + hzb_art + hzb_note + berufsausbildung, data = sta_all, family = binomial)

# model summaries
summary(pred_model)
summary(pred_model2)

# importance of variables
importance<-varImp(pred_model, Scale = FALSE)
print(importance)
sort(print(importance, decreasing = TRUE))


sink("pred_model.txt")
print(summary(pred_model))
sink()

# *** note, versuch, geburtsjahr, hzb_note
# . berufsausbildung
# not significant: hzb_jahr, dienst, hs_semester_vor_aktuellem Studium, Geschlecht, FAchbereich

# remove non-significant variables

sta<-sta_all[  ,c(1,3,4,5,7,10,11)]
sta<-as.data.frame(sta)
str(sta) # noch 5 Variablen
summary(sta)
table(sta$cluster) # 0 = kein StA, 1 = StA

################################################################

## 2. benchmark algorithms

# Create two identical pseudo-tasks
# (applying a distinct resampling strategy for each)
# university dropout = positive class
task1 = makeClassifTask(data = sta, target= "cluster", id = "CV", positive = "1") 
task2 = makeClassifTask(data = sta, target = "cluster", id = "Bootstrap", positive = "1")

# Definition of learners
lrn.rpart = makeLearner("classif.rpart", predict.type = "prob")
lrn.rf = makeLearner("classif.randomForest", predict.type = "prob", par.vals = list(ntree = 30L))
lrn.svm = makeLearner("classif.ksvm", predict.type = "prob")
lrn.log = makeLearner("classif.logreg", predict.type = "prob")


# initiate 10-times CV und Bootstrap-Sample mit 100 Iterations
rinst.cv = makeResampleInstance("CV", iters = 10L, task = task1)
rinst.boot = makeResampleInstance("Bootstrap", iters = 100L, task = task2)

## Benchmark 

bench = benchmark(
  learners = list(lrn.rpart, lrn.rf, lrn.svm, lrn.log),
  tasks = list(task1, task2),
  resamplings = list(rinst.cv, rinst.boot),
  measures = list(mmce, acc, tpr, fpr, fnr, tnr,f1,auc)
)

#################################################################

## 3. compare benchmark results
  

tab.bench<-getBMRAggrPerformances(bench, as.df=TRUE)
str(tab.bench)
summary(tab.bench) #  kvsm mit 10fold CV

write.xlsx(tab.bench, file = "Benchmark_learner_leistung_sem3.xlsx",
           sheetName = "notTuned", append= TRUE)

# Visualize benchmark results with Boxplots
plotBMRBoxplots(bench)

#################################################################
## 4. Classification with  kvsm und 10fold CV

# CV with 10 Iterations
rd3 = makeResampleDesc(method = "CV", iters = 10)

# Create learner (indicating "prob")

lrn.svm.p<-makeLearner("classif.ksvm", predict.type = "prob")
res = resample(learner = lrn.svm.p, task = task1, resampling = rd3)
res
pred.t<-res$pred

# Performance Parameters
performance(pred.t, measures = list(mmce, acc, fpr, tpr, tnr, fnr,f1,auc))


# Determine  "optimale threshold"
d <- generateThreshVsPerfData(pred.t, measures = list(tpr, tnr, mmce))
plotThreshVsPerf(d) # Threshold of 0.35 seems reasonable

#################################################################
## 5. Tuning 
# tune learner
lrn.rpart.p = makeLearner("classif.rpart", predict.type = "prob")
lrn.rf.p = makeLearner("classif.randomForest", par.vals = list(ntree = 30L), predict.type = "prob")
lrn.svm.p = makeLearner("classif.ksvm", predict.type = "prob")
lrn.log.p = makeLearner("classif.logreg", predict.type = "prob")

tuned.lrn.rpart <- setPredictThreshold(lrn.rpart.p, 0.3)
tuned.lrn.rf<- setPredictThreshold(lrn.rf.p, 0.3)
tuned.lrn.svm <- setPredictThreshold(lrn.svm.p, 0.3)
tuned.lrn.log<<-setPredictThreshold(lrn.log.p, 0.3)

#Benchmark learner with threshold 0.30
bench.tuned_all = benchmark(
  learners = list(tuned.lrn.rpart, tuned.lrn.rf, tuned.lrn.svm, tuned.lrn.log),
  tasks = list(task1, task2),
  resamplings = list(rinst.cv, rinst.boot),
  measures = list(mmce, acc, tpr, fpr, fnr, tnr,f1,auc)
)

bench.tuned_all
tab.bench.tuned_all<-getBMRAggrPerformances(bench.tuned_all, as.df=TRUE)
str(tab.bench.tuned_all)
summary(tab.bench.tuned_all)
write.xlsx(tab.bench.tuned_all, file = "Benchmark_learner_leistung sem3.xlsx",
           sheetName = "Treshold_30_", append = TRUE)

plotBMRBoxplots(bench.tuned_all)
################################################################
# 6. Performance and  ROC Curves 

# ksvm (Threshold 0.5)
res.05 = resample(learner = lrn.svm.p, task = task1, resampling = rd3)
res.05
pred.05<-res.05$pred
performance(pred.05, measures = list(mmce, acc, fpr, tpr, tnr, fnr, f1))

d1 <- generateThreshVsPerfData(pred.05, measures = list(fpr, tpr, mmce))
plotThreshVsPerf(d1) 
plotROCCurves(d1)
performance(pred.05, mlr::auc) #0.893

# ksvm (Threshold 0.3)
res.03 = resample(learner = tuned.lrn.svm, task = task1, resampling = rd3)
res.03
pred.03<-res.03$pred
performance(pred.03, measures = list(mmce, acc, fpr, tpr, tnr, fnr, f1))

d2 <- generateThreshVsPerfData(pred.03, measures = list(fpr, tpr, mmce))
plotThreshVsPerf(d2) 
plotROCCurves(d2)
performance(pred.03, mlr::auc) # 0.895

# log Regression (Threshold 0.5)
res.log.50 = resample(learner = lrn.log.p, task = task1, resampling = rd3)
res.log.50
pred.log.50<-res.log.50$pred
performance(pred.log.50, measures = list(mmce, acc, fpr, tpr, tnr, fnr,f1))

d4 <- generateThreshVsPerfData(pred.log.50, measures = list(fpr, tpr, mmce))
plotThreshVsPerf(d4) 
plotROCCurves(d4)
performance(pred.log.50, mlr::auc) # 0.881


# log Regression (Threshold 0.3)
res.log.30 = resample(learner = tuned.lrn.log, task = task1, resampling = rd3)
res.log.30
pred.log.30<-res.log.30$pred
performance(pred.log.30, measures = list(mmce, acc, fpr, tpr, tnr, fnr, f1))

d3 <- generateThreshVsPerfData(pred.log.30, measures = list(fpr, tpr, mmce))
plotThreshVsPerf(d3) 
plotROCCurves(d3)
performance(pred.log.30, mlr::auc) # 0.8779

#################################################################

## 7. ROC Curves 

#  (ksvm 0.5 und ksvm 0.35)
df = generateThreshVsPerfData(list(kvsm = pred.05, kvsm_tuned = pred.03), measures = list(fpr, tpr))
plotROCCurves(df)

# (kvsm 0.3 und logreg 0.3)
df1 = generateThreshVsPerfData(list(kvsm_tuned = pred.03, 
       log.reg_tuned  = pred.log.30), measures = list(fpr, tpr))
plotROCCurves(df1)

################################################################




