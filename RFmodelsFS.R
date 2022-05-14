setwd("/home/devika/Desktop/ValidationByML/Datsets_TenFSResults_ML")
Data_Chi<-read.csv("Chi.csv", header=TRUE)
Data_GR<-read.csv("GR.csv",header=TRUE)
Data_IG<-read.csv("IG.csv",header=TRUE)
Data_mRMRe<-read.csv("mRMRe.csv",header=TRUE)
Data_OneR<-read.csv("OneR.csv",header=TRUE)
Data_Rank<-read.csv("Rank.csv",header=TRUE)
Data_RF<-read.csv("RF.csv",header=TRUE)
Data_RRelief<-read.csv("RRelief.csv",header=TRUE)
Data_SVMRFE<-read.csv("SVM-RFE.csv",header=TRUE)
Data_Uncertainty<-read.csv("Uncertainty.csv",header=TRUE)
Data_DE<-read.csv("DEgenes_27.csv",header=TRUE)

Data_Chi$PANID<-factor(Data_Chi$PANID, ordered = TRUE)
levels(Data_Chi$PANID)<-make.names(levels(Data_Chi$PANID))

Data_GR$PANID<-factor(Data_GR$PANID, ordered = TRUE)
levels(Data_GR$PANID)<-make.names(levels(Data_GR$PANID))

Data_IG$PANID<-factor(Data_IG$PANID, ordered = TRUE)
levels(Data_IG$PANID)<-make.names(levels(Data_IG$PANID))

Data_mRMRe$PANID<-factor(Data_mRMRe$PANID, ordered = TRUE)
levels(Data_mRMRe$PANID)<-make.names(levels(Data_mRMRe$PANID))

Data_OneR$PANID<-factor(Data_OneR$PANID, ordered = TRUE)
levels(Data_OneR$PANID)<-make.names(levels(Data_OneR$PANID))

Data_Rank$PANID<-factor(Data_Rank$PANID, ordered = TRUE)
levels(Data_Rank$PANID)<-make.names(levels(Data_Rank$PANID))

Data_RF$PANID<-factor(Data_RF$PANID, ordered = TRUE)
levels(Data_RF$PANID)<-make.names(levels(Data_RF$PANID))

Data_RRelief$PANID<-factor(Data_RRelief$PANID, ordered = TRUE)
levels(Data_RRelief$PANID)<-make.names(levels(Data_RRelief$PANID))

Data_SVMRFE$PANID<-factor(Data_SVMRFE$PANID, ordered = TRUE)
levels(Data_SVMRFE$PANID)<-make.names(levels(Data_SVMRFE$PANID))

Data_Uncertainty$PANID<-factor(Data_Uncertainty$PANID, ordered = TRUE)
levels(Data_Uncertainty$PANID)<-make.names(levels(Data_Uncertainty$PANID))

Data_DE$PANID<-factor(Data_DE$PANID, ordered = TRUE)
levels(Data_DE$PANID)<-make.names(levels(Data_DE$PANID))


library(randomForest)
library(caret)
library(pROC)


##############################################
###RandomForest
seed<-19
ntree=10

seeds<-as.list(seq(from = 268, by = 103, length.out = 38))


trcontrol <- trainControl(method="LOOCV", number=0, seeds=seeds, search="grid",classProbs=TRUE, savePredictions=TRUE)


model_Chi <- train(Data_Chi[-1],Data_Chi$PANID, method="rf", metric= "Accuracy", tuneGrid=expand.grid(mtry=c(48)), trControl=trcontrol, nodesize=1, maxnodes=15, ntree=ntree, tuneLength = 1)
tab_Chi<-confusionMatrix(model_Chi$pred$pred, model_Chi$pred$obs)
tab_Chi<-tab_Chi$table
accu_Chi=(tab_Chi[1,1]+tab_Chi[2,2])/(tab_Chi[1,1]+tab_Chi[1,2]+tab_Chi[2,1]+tab_Chi[2,2])
recall_Chi=(tab_Chi[1,1])/(tab_Chi[1,1]+tab_Chi[2,1])
specificity_Chi=(tab_Chi[2,2])/(tab_Chi[1,2]+tab_Chi[2,2])
prec_Chi=(tab_Chi[1,1])/(tab_Chi[1,1]+tab_Chi[1,2])
MCC_Chi=((tab_Chi[1,1]*tab_Chi[2,2])-(tab_Chi[1,2]*tab_Chi[2,1]))/(sqrt((tab_Chi [1,1]+tab_Chi [1,2])* (tab_Chi [1,1]+tab_Chi [2,1])*(tab_Chi [2,2]+tab_Chi [1,2])*(tab_Chi [2,2]+tab_Chi[2,1])))
auc_Chi<-roc(predictor=model_Chi$pred$pred, response=model_Chi$pred$obs)

model_GR <- train(Data_GR[-1],Data_GR$PANID, method="rf", metric= "Accuracy", tuneGrid=expand.grid(mtry=c(48)), trControl=trcontrol, nodesize=1, maxnodes=15, ntree=ntree, tuneLength = 1)
tab_GR<-confusionMatrix(model_GR$pred$pred, model_GR$pred$obs)
tab_GR<-tab_GR$table
accu_GR=(tab_GR[1,1]+tab_GR[2,2])/(tab_GR[1,1]+tab_GR[1,2]+tab_GR[2,1]+tab_GR[2,2])
recall_GR=(tab_GR[1,1])/(tab_GR[1,1]+tab_GR[2,1])
specificity_GR=(tab_GR[2,2])/(tab_GR[1,2]+tab_GR[2,2])
prec_GR=(tab_GR[1,1])/(tab_GR[1,1]+tab_GR[1,2])
MCC_GR=((tab_GR[1,1]*tab_GR[2,2])-(tab_GR[1,2]*tab_GR[2,1]))/(sqrt((tab_GR [1,1]+tab_GR [1,2])* (tab_GR [1,1]+tab_GR [2,1])*(tab_GR [2,2]+tab_GR [1,2])*(tab_GR [2,2]+tab_GR[2,1])))
auc_GR<-roc(predictor=model_GR$pred$pred, response=model_GR$pred$obs)

model_IG <- train(Data_IG[-1],Data_IG$PANID, method="rf", metric= "Accuracy", tuneGrid=expand.grid(mtry=c(48)), trControl=trcontrol, nodesize=1, maxnodes=15, ntree=ntree, tuneLength = 1)
tab_IG<-confusionMatrix(model_IG$pred$pred, model_IG$pred$obs)
tab_IG<-tab_IG$table
accu_IG=(tab_IG[1,1]+tab_IG[2,2])/(tab_IG[1,1]+tab_IG[1,2]+tab_IG[2,1]+tab_IG[2,2])
recall_IG=(tab_IG[1,1])/(tab_IG[1,1]+tab_IG[2,1])
specificity_IG=(tab_IG[2,2])/(tab_IG[1,2]+tab_IG[2,2])
prec_IG=(tab_IG[1,1])/(tab_IG[1,1]+tab_IG[1,2])
MCC_IG=((tab_IG[1,1]*tab_IG[2,2])-(tab_IG[1,2]*tab_IG[2,1]))/(sqrt((tab_IG [1,1]+tab_IG [1,2])* (tab_IG [1,1]+tab_IG [2,1])*(tab_IG [2,2]+tab_IG [1,2])*(tab_IG [2,2]+tab_IG[2,1])))
auc_IG<-roc(predictor=model_IG$pred$pred, response=model_IG$pred$obs)


model_mRMRe <- train(Data_mRMRe[-1],Data_mRMRe$PANID, method="rf", metric= "Accuracy", tuneGrid=expand.grid(mtry=c(48)), trControl=trcontrol, nodesize=1, maxnodes=15, ntree=ntree, tuneLength = 1)
tab_mRMRe<-confusionMatrix(model_mRMRe$pred$pred, model_mRMRe$pred$obs)
tab_mRMRe<-tab_mRMRe$table
accu_mRMRe=(tab_mRMRe[1,1]+tab_mRMRe[2,2])/(tab_mRMRe[1,1]+tab_mRMRe[1,2]+tab_mRMRe[2,1]+tab_mRMRe[2,2])
recall_mRMRe=(tab_mRMRe[1,1])/(tab_mRMRe[1,1]+tab_mRMRe[2,1])
specificity_mRMRe=(tab_mRMRe[2,2])/(tab_mRMRe[1,2]+tab_mRMRe[2,2])
prec_mRMRe=(tab_mRMRe[1,1])/(tab_mRMRe[1,1]+tab_mRMRe[1,2])
MCC_mRMRe=((tab_mRMRe[1,1]*tab_mRMRe[2,2])-(tab_mRMRe[1,2]*tab_mRMRe[2,1]))/(sqrt((tab_mRMRe [1,1]+tab_mRMRe [1,2])* (tab_mRMRe [1,1]+tab_mRMRe [2,1])*(tab_mRMRe [2,2]+tab_mRMRe [1,2])*(tab_mRMRe [2,2]+tab_mRMRe[2,1])))
auc_mRMRe<-roc(predictor=model_mRMRe$pred$pred, response=model_mRMRe$pred$obs)


model_OneR <- train(Data_OneR[-1],Data_OneR$PANID, method="rf", metric= "Accuracy", tuneGrid=expand.grid(mtry=c(48)), trControl=trcontrol, nodesize=1, maxnodes=15, ntree=ntree, tuneLength = 1)
tab_OneR<-confusionMatrix(model_OneR$pred$pred, model_OneR$pred$obs)
tab_OneR<-tab_OneR$table
accu_OneR=(tab_OneR[1,1]+tab_OneR[2,2])/(tab_OneR[1,1]+tab_OneR[1,2]+tab_OneR[2,1]+tab_OneR[2,2])
recall_OneR=(tab_OneR[1,1])/(tab_OneR[1,1]+tab_OneR[2,1])
specificity_OneR=(tab_OneR[2,2])/(tab_OneR[1,2]+tab_OneR[2,2])
prec_OneR=(tab_OneR[1,1])/(tab_OneR[1,1]+tab_OneR[1,2])
MCC_OneR=((tab_OneR[1,1]*tab_OneR[2,2])-(tab_OneR[1,2]*tab_OneR[2,1]))/(sqrt((tab_OneR [1,1]+tab_OneR [1,2])* (tab_OneR [1,1]+tab_OneR [2,1])*(tab_OneR [2,2]+tab_OneR [1,2])*(tab_OneR [2,2]+tab_OneR[2,1])))
auc_OneR<-roc(predictor=model_OneR$pred$pred, response=model_OneR$pred$obs)

model_Rank <- train(Data_Rank[-1],Data_Rank$PANID, method="rf", metric= "Accuracy", tuneGrid=expand.grid(mtry=c(48)), trControl=trcontrol, nodesize=1, maxnodes=15, ntree=ntree, tuneLength = 1)
tab_Rank<-confusionMatrix(model_Rank$pred$pred, model_Rank$pred$obs)
tab_Rank<-tab_Rank$table
accu_Rank=(tab_Rank[1,1]+tab_Rank[2,2])/(tab_Rank[1,1]+tab_Rank[1,2]+tab_Rank[2,1]+tab_Rank[2,2])
recall_Rank=(tab_Rank[1,1])/(tab_Rank[1,1]+tab_Rank[2,1])
specificity_Rank=(tab_Rank[2,2])/(tab_Rank[1,2]+tab_Rank[2,2])
prec_Rank=(tab_Rank[1,1])/(tab_Rank[1,1]+tab_Rank[1,2])
MCC_Rank=((tab_Rank[1,1]*tab_Rank[2,2])-(tab_Rank[1,2]*tab_Rank[2,1]))/(sqrt((tab_Rank [1,1]+tab_Rank [1,2])* (tab_Rank [1,1]+tab_Rank [2,1])*(tab_Rank [2,2]+tab_Rank [1,2])*(tab_Rank [2,2]+tab_Rank[2,1])))
auc_Rank<-roc(predictor=model_Rank$pred$pred, response=model_Rank$pred$obs)

model_RF <- train(Data_RF[-1],Data_RF$PANID, method="rf", metric= "Accuracy", tuneGrid=expand.grid(mtry=c(48)), trControl=trcontrol, nodesize=1, maxnodes=15, ntree=ntree, tuneLength = 1)
tab_RF<-confusionMatrix(model_RF$pred$pred, model_RF$pred$obs)
tab_RF<-tab_RF$table
accu_RF=(tab_RF[1,1]+tab_RF[2,2])/(tab_RF[1,1]+tab_RF[1,2]+tab_RF[2,1]+tab_RF[2,2])
recall_RF=(tab_RF[1,1])/(tab_RF[1,1]+tab_RF[2,1])
specificity_RF=(tab_RF[2,2])/(tab_RF[1,2]+tab_RF[2,2])
prec_RF=(tab_RF[1,1])/(tab_RF[1,1]+tab_RF[1,2])
MCC_RF=((tab_RF[1,1]*tab_RF[2,2])-(tab_RF[1,2]*tab_RF[2,1]))/(sqrt((tab_RF [1,1]+tab_RF [1,2])* (tab_RF [1,1]+tab_RF [2,1])*(tab_RF [2,2]+tab_RF [1,2])*(tab_RF [2,2]+tab_RF[2,1])))
auc_RF<-roc(predictor=model_RF$pred$pred, response=model_RF$pred$obs)

model_RRelief <- train(Data_RRelief[-1],Data_RRelief$PANID, method="rf", metric= "Accuracy", tuneGrid=expand.grid(mtry=c(48)), trControl=trcontrol, nodesize=1, maxnodes=15, ntree=ntree, tuneLength = 1)
tab_RRelief<-confusionMatrix(model_RRelief$pred$pred, model_RRelief$pred$obs)
tab_RRelief<-tab_RRelief$table
accu_RRelief=(tab_RRelief[1,1]+tab_RRelief[2,2])/(tab_RRelief[1,1]+tab_RRelief[1,2]+tab_RRelief[2,1]+tab_RRelief[2,2])
recall_RRelief=(tab_RRelief[1,1])/(tab_RRelief[1,1]+tab_RRelief[2,1])
specificity_RRelief=(tab_RRelief[2,2])/(tab_RRelief[1,2]+tab_RRelief[2,2])
prec_RRelief=(tab_RRelief[1,1])/(tab_RRelief[1,1]+tab_RRelief[1,2])
MCC_RRelief=((tab_RRelief[1,1]*tab_RRelief[2,2])-(tab_RRelief[1,2]*tab_RRelief[2,1]))/(sqrt((tab_RRelief [1,1]+tab_RRelief [1,2])* (tab_RRelief [1,1]+tab_RRelief [2,1])*(tab_RRelief [2,2]+tab_RRelief [1,2])*(tab_RRelief [2,2]+tab_RRelief[2,1])))
auc_RRelief<-roc(predictor=model_RRelief$pred$pred, response=model_RRelief$pred$obs)

model_SVMRFE <- train(Data_SVMRFE[-1],Data_SVMRFE$PANID, method="rf", metric= "Accuracy", tuneGrid=expand.grid(mtry=c(48)), trControl=trcontrol, nodesize=1, maxnodes=15, ntree=ntree, tuneLength = 1)
tab_SVMRFE<-confusionMatrix(model_SVMRFE$pred$pred, model_SVMRFE$pred$obs)
tab_SVMRFE<-tab_SVMRFE$table
accu_SVMRFE=(tab_SVMRFE[1,1]+tab_SVMRFE[2,2])/(tab_SVMRFE[1,1]+tab_SVMRFE[1,2]+tab_SVMRFE[2,1]+tab_SVMRFE[2,2])
recall_SVMRFE=(tab_SVMRFE[1,1])/(tab_SVMRFE[1,1]+tab_SVMRFE[2,1])
specificity_SVMRFE=(tab_SVMRFE[2,2])/(tab_SVMRFE[1,2]+tab_SVMRFE[2,2])
prec_SVMRFE=(tab_SVMRFE[1,1])/(tab_SVMRFE[1,1]+tab_SVMRFE[1,2])
MCC_SVMRFE=((tab_SVMRFE[1,1]*tab_SVMRFE[2,2])-(tab_SVMRFE[1,2]*tab_SVMRFE[2,1]))/(sqrt((tab_SVMRFE [1,1]+tab_SVMRFE [1,2])* (tab_SVMRFE [1,1]+tab_SVMRFE [2,1])*(tab_SVMRFE [2,2]+tab_SVMRFE [1,2])*(tab_SVMRFE [2,2]+tab_SVMRFE[2,1])))
auc_SVMRFE<-roc(predictor=model_SVMRFE$pred$pred, response=model_SVMRFE$pred$obs)

model_Uncertainty <- train(Data_Uncertainty[-1],Data_Uncertainty$PANID, method="rf", metric= "Accuracy", tuneGrid=expand.grid(mtry=c(48)), trControl=trcontrol, nodesize=1, maxnodes=15, ntree=ntree, tuneLength = 1)
tab_Uncertainty<-confusionMatrix(model_Uncertainty$pred$pred, model_Uncertainty$pred$obs)
tab_Uncertainty<-tab_Uncertainty$table
accu_Uncertainty=(tab_Uncertainty[1,1]+tab_Uncertainty[2,2])/(tab_Uncertainty[1,1]+tab_Uncertainty[1,2]+tab_Uncertainty[2,1]+tab_Uncertainty[2,2])
recall_Uncertainty=(tab_Uncertainty[1,1])/(tab_Uncertainty[1,1]+tab_Uncertainty[2,1])
specificity_Uncertainty=(tab_Uncertainty[2,2])/(tab_Uncertainty[1,2]+tab_Uncertainty[2,2])
prec_Uncertainty=(tab_Uncertainty[1,1])/(tab_Uncertainty[1,1]+tab_Uncertainty[1,2])
MCC_Uncertainty=((tab_Uncertainty[1,1]*tab_Uncertainty[2,2])-(tab_Uncertainty[1,2]*tab_Uncertainty[2,1]))/(sqrt((tab_Uncertainty [1,1]+tab_Uncertainty [1,2])* (tab_Uncertainty [1,1]+tab_Uncertainty [2,1])*(tab_Uncertainty [2,2]+tab_Uncertainty [1,2])*(tab_Uncertainty [2,2]+tab_Uncertainty[2,1])))
auc_Uncertainty<-roc(predictor=model_Uncertainty$pred$pred, response=model_Uncertainty$pred$obs)


model_DE <- train(Data_DE[-1],Data_DE$PANID, method="rf", metric= "Accuracy", tuneGrid=expand.grid(mtry=c(48)), trControl=trcontrol, nodesize=1, maxnodes=15, ntree=ntree, tuneLength = 1)
tab_DE<-confusionMatrix(model_DE$pred$pred, model_DE$pred$obs)
tab_DE<-tab_DE$table
accu_DE=(tab_DE[1,1]+tab_DE[2,2])/(tab_DE[1,1]+tab_DE[1,2]+tab_DE[2,1]+tab_DE[2,2])
recall_DE=(tab_DE[1,1])/(tab_DE[1,1]+tab_DE[2,1])
specificity_DE=(tab_DE[2,2])/(tab_DE[1,2]+tab_DE[2,2])
prec_DE=(tab_DE[1,1])/(tab_DE[1,1]+tab_DE[1,2])
MCC_DE=((tab_DE[1,1]*tab_DE[2,2])-(tab_DE[1,2]*tab_DE[2,1]))/(sqrt((tab_DE [1,1]+tab_DE [1,2])* (tab_DE [1,1]+tab_DE [2,1])*(tab_DE [2,2]+tab_DE [1,2])*(tab_DE [2,2]+tab_DE[2,1])))
auc_DE<-roc(predictor=model_DE$pred$pred, response=model_DE$pred$obs)



res_RF_FS<-data.frame(matrix(c(accu_Chi,recall_Chi,specificity_Chi,prec_Chi,MCC_Chi,auc_Chi$auc)))
rownames(res_RF_FS)<-c("Accuracy","Recall","Specificity","Preicision","MCC","AUC")
colnames(res_RF_FS)<-c("Chi")

res_RF_FS[,2]<-c(accu_GR,recall_GR,specificity_GR,prec_GR,MCC_GR,auc_GR$auc)
res_RF_FS[,3]<-c(accu_IG,recall_IG,specificity_IG,prec_IG,MCC_IG,auc_IG$auc)
res_RF_FS[,4]<-c(accu_mRMRe,recall_mRMRe,specificity_mRMRe,prec_mRMRe,MCC_mRMRe,auc_mRMRe$auc)
res_RF_FS[,5]<-c(accu_OneR,recall_OneR,specificity_OneR,prec_OneR,MCC_OneR,auc_OneR$auc)
res_RF_FS[,6]<-c(accu_Rank,recall_Rank,specificity_Rank,prec_Rank,MCC_Rank,auc_Rank$auc)
res_RF_FS[,7]<-c(accu_RF,recall_RF,specificity_RF,prec_RF,MCC_RF,auc_RF$auc)
res_RF_FS[,8]<-c(accu_RRelief,recall_RRelief,specificity_RRelief,prec_RRelief,MCC_RRelief,auc_RRelief$auc)
res_RF_FS[,9]<-c(accu_SVMRFE,recall_SVMRFE,specificity_SVMRFE,prec_SVMRFE,MCC_SVMRFE,auc_SVMRFE$auc)
res_RF_FS[,10]<-c(accu_Uncertainty,recall_Uncertainty,specificity_Uncertainty,prec_Uncertainty,MCC_Uncertainty,auc_Uncertainty$auc)
res_RF_FS[,11]<-c(accu_DE,recall_DE,specificity_DE,prec_DE,MCC_DE,auc_DE$auc)


colnames(res_RF_FS)<-c("Chi","GR","IG","mRMRe","OneR","Rank","RF","RRelief","SVMRFE","Uncertainty","DE")

write.csv(res_RF_FS,"ResultTables_RF_FS10.csv", row.names=TRUE, col.names=TRUE)


pdf(file="roc_RF_Chi.pdf")
plot(roc(predictor=model_Chi$pred$pred, response=model_Chi$pred$obs))
dev.off()


pdf(file="roc_RF_GR.pdf")
plot(roc(predictor=model_GR$pred$pred, response=model_GR$pred$obs))
dev.off()


pdf(file="roc_RF_IG.pdf")
plot(roc(predictor=model_IG$pred$pred, response=model_IG$pred$obs))
dev.off()


pdf(file="roc_RF_mRMRe.pdf")
plot(roc(predictor=model_mRMRe$pred$pred, response=model_mRMRe$pred$obs))
dev.off()


pdf(file="roc_RF_OneR.pdf")
plot(roc(predictor=model_OneR$pred$pred, response=model_OneR$pred$obs))
dev.off()


pdf(file="roc_RF_RF.pdf")
plot(roc(predictor=model_RF$pred$pred, response=model_RF$pred$obs))
dev.off()


pdf(file="roc_RF_Rank.pdf")
plot(roc(predictor=model_Rank$pred$pred, response=model_Rank$pred$obs))
dev.off()


pdf(file="roc_RF_RRelief.pdf")
plot(roc(predictor=model_RRelief$pred$pred, response=model_RRelief$pred$obs))
dev.off()


pdf(file="roc_RF_SVMRFE.pdf")
plot(roc(predictor=model_SVMRFE$pred$pred, response=model_SVMRFE$pred$obs))
dev.off()


pdf(file="roc_RF_Uncertainty.pdf")
plot(roc(predictor=model_Uncertainty$pred$pred, response=model_Uncertainty$pred$obs))
dev.off()


pdf(file="roc_RF_DE.pdf")
plot(roc(predictor=model_DE$pred$pred, response=model_DE$pred$obs))
dev.off()
