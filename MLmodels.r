setwd("C:/Users/DMTM1/Desktop/mRMR_IFS_RF/SVM_RF_MRMRE/ML_Validation")
Data1<-read.csv("3k_nomissingdata.csv", header=TRUE)
Data2<-read.csv("726features.csv",header=TRUE)
Data3<-read.csv("38Features.csv",header=TRUE)
Data4<-read.csv("15Features.csv",header=TRUE)

library(randomForest)
library(caret)
#library(e1071)
#library(klaR)
library(RRF)
library(pROC)

Data1$Data<-factor(Data1$Data, ordered = TRUE)
levels(Data1$Data)<-make.names(levels(Data1$Data))

Data2$PANID<-factor(Data2$PANID, ordered = TRUE)
levels(Data2$PANID)<-make.names(levels(Data2$PANID))

Data3$PANID<-factor(Data3$PANID, ordered = TRUE)
levels(Data3$PANID)<-make.names(levels(Data3$PANID))

Data4$PANID<-factor(Data4$PANID, ordered = TRUE)
levels(Data4$PANID)<-make.names(levels(Data4$PANID))

#RandomForest

seed<-45
ntree=51
#setting seeds in train control for reproducible results

#For LOOCV length="nrow(training)+1"
#For CV, length = "(n_repeats*nresampling)+1", n_repeats=repeats parameter in trainControl,nresampling = "k" in k-fold CV
seeds<-vector(mode="list",length=38)

#For first "length-1" models, for (i in 1:length-1) seeds[[i]]<-sample.int(1000,tuneLength)
for (i in 1:37) seeds [[i]]<-sample.int(1000,1) 
#For last model-
seeds[[38]]<-sample.int(1000, 1)



trcontrol <- trainControl(method="LOOCV", number = 10, seeds=seeds, search="grid",classProbs=TRUE, savePredictions=TRUE)
set.seed<-seed
model1 <- train(Data1[-1],Data1$Data, method="rf", metric= "Accuracy", tuneGrid=expand.grid(mtry=c(round(sqrt(ncol(Data1[-1]))))), trControl=trcontrol, nodesize=1, maxnodes=15, ntree=ntree, tuneLength = 1)
set.seed<-seed
model2 <- train(Data2[-1],Data2$PANID, method="rf", metric= "Accuracy", tuneGrid=expand.grid(mtry=c(round(sqrt(ncol(Data2[-1]))))), trControl=trcontrol, nodesize=1, maxnodes=15, ntree=ntree, tuneLength = 1)
set.seed<-seed
model3 <- train(Data3[-1],Data3$PANID, method="rf", metric= "Accuracy", tuneGrid=expand.grid(mtry=c(round(sqrt(ncol(Data3[-1]))))), trControl=trcontrol, nodesize=1, maxnodes=15, ntree=ntree, tuneLength = 1)
set.seed<-seed
model4 <- train(Data4[-1],Data4$PANID, method="rf", metric= "Accuracy", tuneGrid=expand.grid(mtry=c(round(sqrt(ncol(Data4[-1]))))), trControl=trcontrol, nodesize=1, maxnodes=15, ntree=ntree, tuneLength = 1)

tab1<-confusionMatrix(model1$pred$pred, model1$pred$obs)
tab2<-confusionMatrix(model2$pred$pred, model2$pred$obs)
tab3<-confusionMatrix(model3$pred$pred, model3$pred$obs)
tab4<-confusionMatrix(model4$pred$pred, model4$pred$obs)


tab1<-tab1$table
tab2<-tab2$table
tab3<-tab3$table
tab4<-tab4$table


accu1=(tab1[1,1]+tab1[2,2])/(tab1[1,1]+tab1[1,2]+tab1[2,1]+tab1[2,2])
recall1=(tab1[1,1])/(tab1[1,1]+tab1[2,1])
specificity1=(tab1[2,2])/(tab1[1,2]+tab1[2,2])
prec1=(tab1[1,1])/(tab1[1,1]+tab1[1,2])
MCC1=((tab1[1,1]*tab1[2,2])-(tab1[1,2]*tab1[2,1]))/(sqrt((tab1 [1,1]+tab1 [1,2])* (tab1 [1,1]+tab1 [2,1])*(tab1 [2,2]+tab1 [1,2])*(tab1 [2,2]+tab1[2,1])))


accu2=(tab2[1,1]+tab2[2,2])/(tab2[1,1]+tab2[1,2]+tab2[2,1]+tab2[2,2])
recall2=(tab2[1,1])/(tab2[1,1]+tab2[2,1])
specificity2=(tab2[2,2])/(tab2[1,2]+tab2[2,2])
prec2=(tab2[1,1])/(tab2[1,1]+tab2[1,2])
MCC2=((tab2[1,1]*tab2[2,2])-(tab2[1,2]*tab2[2,1]))/(sqrt((tab2 [1,1]+tab2 [1,2])* (tab2 [1,1]+tab2 [2,1])*(tab2 [2,2]+tab2 [1,2])*(tab2 [2,2]+tab2[2,1])))

accu3=(tab3[1,1]+tab3[2,2])/(tab3[1,1]+tab3[1,2]+tab3[2,1]+tab3[2,2])
recall3=(tab3[1,1])/(tab3[1,1]+tab3[2,1])
specificity3=(tab3[2,2])/(tab3[1,2]+tab3[2,2])
prec3=(tab3[1,1])/(tab3[1,1]+tab3[1,2])
MCC3=((tab3[1,1]*tab3[2,2])-(tab3[1,2]*tab3[2,1]))/(sqrt((tab3 [1,1]+tab3 [1,2])* (tab3 [1,1]+tab3 [2,1])*(tab3 [2,2]+tab3 [1,2])*(tab3 [2,2]+tab3[2,1])))

accu4=(tab4[1,1]+tab4[2,2])/(tab4[1,1]+tab4[1,2]+tab4[2,1]+tab4[2,2])
recall4=(tab4[1,1])/(tab4[1,1]+tab4[2,1])
specificity4=(tab4[2,2])/(tab4[1,2]+tab4[2,2])
prec4=(tab4[1,1])/(tab4[1,1]+tab4[1,2])
MCC4=((tab4[1,1]*tab4[2,2])-(tab4[1,2]*tab4[2,1]))/(sqrt((tab4 [1,1]+tab4 [1,2])* (tab4 [1,1]+tab4 [2,1])*(tab4 [2,2]+tab4 [1,2])*(tab4 [2,2]+tab4[2,1])))


res_RF<-data.frame(matrix(c(accu1,recall1,specificity1,prec1,MCC1)))
rownames(res_RF)<-c("Accuracy","Recall","Specificity","Preicision","MCC")
colnames(res_RF)<-c("Before")

res_RF[,2]<-c(accu2,recall2,specificity2,prec2,MCC2)
res_RF[,3]<-c(accu3,recall3,specificity3,prec3,MCC3)
res_RF[,4]<-c(accu4,recall4,specificity4,prec4,MCC4)

write.csv(res_RF,"ResultTables_RF.csv", row.names=FALSE)

#X11()
#plot(roc(predictor=model1$pred$pred, response=model1$pred$obs))
#X11()
#plot(roc(predictor=model2$pred$pred, response=model2$pred$obs))
#X11()
#plot(roc(predictor=model3$pred$pred, response=model3$pred$obs))
#X11()
#plot(roc(predictor=model4$pred$pred, response=model4$pred$obs))

############################################################################


#SVM

train(Finaldata[-1],Finaldata$Data, method="svmLinearWeights", metric= "Accuracy", trControl=trcontrol, tuneLength = 1)


####################################################################################################

#RRF



seed<-9999
ntree=31





trcontrol <- trainControl(method="LOOCV", number = 1, seeds=NA, search="grid",classProbs=TRUE, savePredictions=TRUE)
set.seed<-seed
model1 <- train(Data1[-1],Data1$Data, method="RRF", metric= "Accuracy", tuneGrid=expand.grid(mtry=c(round(sqrt(ncol(Data1[-1])))),coefReg=c(0),coefImp=c(1)), trControl=trcontrol, nodesize=30, maxnodes=30, ntree=ntree, tuneLength = 1)
set.seed<-seed
model2 <- train(Data2[-1],Data2$PANID, method="RRF", metric= "Accuracy", tuneGrid=expand.grid(mtry=c(round(sqrt(ncol(Data2[-1])))),coefReg=c(0),coefImp=c(1)), trControl=trcontrol, nodesize=30, maxnodes=30, ntree=ntree, tuneLength = 1)
set.seed<-seed
model3 <- train(Data3[-1],Data3$PANID, method="RRF", metric= "Accuracy", tuneGrid=expand.grid(mtry=c(round(sqrt(ncol(Data3[-1])))),coefReg=c(0),coefImp=c(1)), trControl=trcontrol, nodesize=30, maxnodes=30, ntree=ntree, tuneLength = 1)
set.seed<-seed
model4 <- train(Data4[-1],Data4$PANID, method="RRF", metric= "Accuracy", tuneGrid=expand.grid(mtry=c(round(sqrt(ncol(Data4[-1])))),coefReg=c(0),coefImp=c(1)), trControl=trcontrol, nodesize=30, maxnodes=30, ntree=ntree, tuneLength = 1)

tab1<-confusionMatrix(model1$pred$pred, model1$pred$obs)
tab2<-confusionMatrix(model2$pred$pred, model2$pred$obs)
tab3<-confusionMatrix(model3$pred$pred, model3$pred$obs)
tab4<-confusionMatrix(model4$pred$pred, model4$pred$obs)


tab1<-tab1$table
tab2<-tab2$table
tab3<-tab3$table
tab4<-tab4$table


accu1=(tab1[1,1]+tab1[2,2])/(tab1[1,1]+tab1[1,2]+tab1[2,1]+tab1[2,2])
recall1=(tab1[1,1])/(tab1[1,1]+tab1[2,1])
specificity1=(tab1[2,2])/(tab1[1,2]+tab1[2,2])
prec1=(tab1[1,1])/(tab1[1,1]+tab1[1,2])
MCC1=((tab1[1,1]*tab1[2,2])-(tab1[1,2]*tab1[2,1]))/(sqrt((tab1 [1,1]+tab1 [1,2])* (tab1 [1,1]+tab1 [2,1])*(tab1 [2,2]+tab1 [1,2])*(tab1 [2,2]+tab1[2,1])))


accu2=(tab2[1,1]+tab2[2,2])/(tab2[1,1]+tab2[1,2]+tab2[2,1]+tab2[2,2])
recall2=(tab2[1,1])/(tab2[1,1]+tab2[2,1])
specificity2=(tab2[2,2])/(tab2[1,2]+tab2[2,2])
prec2=(tab2[1,1])/(tab2[1,1]+tab2[1,2])
MCC2=((tab2[1,1]*tab2[2,2])-(tab2[1,2]*tab2[2,1]))/(sqrt((tab2 [1,1]+tab2 [1,2])* (tab2 [1,1]+tab2 [2,1])*(tab2 [2,2]+tab2 [1,2])*(tab2 [2,2]+tab2[2,1])))

accu3=(tab3[1,1]+tab3[2,2])/(tab3[1,1]+tab3[1,2]+tab3[2,1]+tab3[2,2])
recall3=(tab3[1,1])/(tab3[1,1]+tab3[2,1])
specificity3=(tab3[2,2])/(tab3[1,2]+tab3[2,2])
prec3=(tab3[1,1])/(tab3[1,1]+tab3[1,2])
MCC3=((tab3[1,1]*tab3[2,2])-(tab3[1,2]*tab3[2,1]))/(sqrt((tab3 [1,1]+tab3 [1,2])* (tab3 [1,1]+tab3 [2,1])*(tab3 [2,2]+tab3 [1,2])*(tab3 [2,2]+tab3[2,1])))

accu4=(tab4[1,1]+tab4[2,2])/(tab4[1,1]+tab4[1,2]+tab4[2,1]+tab4[2,2])
recall4=(tab4[1,1])/(tab4[1,1]+tab4[2,1])
specificity4=(tab4[2,2])/(tab4[1,2]+tab4[2,2])
prec4=(tab4[1,1])/(tab4[1,1]+tab4[1,2])
MCC4=((tab4[1,1]*tab4[2,2])-(tab4[1,2]*tab4[2,1]))/(sqrt((tab4 [1,1]+tab4 [1,2])* (tab4 [1,1]+tab4 [2,1])*(tab4 [2,2]+tab4 [1,2])*(tab4 [2,2]+tab4[2,1])))


res_RRF<-data.frame(matrix(c(accu1,recall1,specificity1,prec1,MCC1)))
rownames(res_RF)<-c("Accuracy","Recall","Specificity","Preicision","MCC")
colnames(res_RF)<-c("Before")

res_RRF[,2]<-c(accu2,recall2,specificity2,prec2,MCC2)
res_RRF[,3]<-c(accu3,recall3,specificity3,prec3,MCC3)
res_RRF[,4]<-c(accu4,recall4,specificity4,prec4,MCC4)

write.csv(res_RRF,"ResultTables_RRF.csv", row.names=FALSE)

################################################################################################################


#knn

train(Finaldata[-1],Finaldata$Data, method="knn", metric= "Accuracy", trControl=trcontrol, tuneLength = 1)

####################################################################################################

#NaiveBayes

set.seed<-seed
model_before_NB <- train(Finaldata[-1],Finaldata$Data, method="nb", metric= "Accuracy", trControl=trcontrol, tuneLength = 1)


#########################################################################################################
