#FeatureSelection by mRMRe using 10 solution sets
setwd("C:/Users/DMTM1/Desktop/mRMR_IFS_RF/Final")
FSdata= read.csv("NonImputedData.csv", header=TRUE)
FSdata<-t(FSdata)
FSdata<-na.omit(FSdata)
FSdata<-t(FSdata)
FSdata<-as.data.frame(FSdata)
write.csv(FSdata,"726features.csv", row.names=FALSE)

library(mRMRe)
library(randomForest)
library(caret)
library(e1071)
library(klaR)
library(RRF)


#FSdata=read.csv("726features.csv", header=TRUE)
FSdata$PANID<-factor(FSdata$PANID, ordered = TRUE)
levels(FSdata$PANID)<-make.names(levels(FSdata$PANID))

#mRMRe module
dd<-mRMR.data(FSdata)
n<-10
res<-mRMR.ensemble(data = dd, target_indices = c(1),solution_count = n, feature_count = 725)
final<-data.frame(res@filters$`1`,res@scores$`1`)


#Aggregate and sorting
Final_Sorted<-data.frame(matrix(c(2:726)))
colnames(Final_Sorted)<-c("FeatureNumber")

for (i in 1:n) {

Sol<-data.frame(final[i],final[i+n])
Sol_sorted<-Sol[order(Sol[1],decreasing=FALSE),]
Final_Sorted[i+1]<-Sol_sorted[2]

}


Final_Sorted$Mean<-rowMeans(as.matrix(Final_Sorted[,-1]))
Final_Sorted<-Final_Sorted[order(Final_Sorted$Mean,decreasing = TRUE),]


a<-as.matrix(Final_Sorted[1])
b<-c(1,a[1:nrow(a)])
Finaldata<-FSdata[,b]

f<-as.vector(colnames(Finaldata[-1]))
Final_Sorted$featurename<-f

#FeatureScoring and Ranking
write.csv(Final_Sorted,"Scoredfeatures.csv",row.names=FALSE)
#ExpressionProfile According to Rank
write.csv(Finaldata,"Selectedfeatures.csv", row.names=FALSE)

############################################################################################
#Finaldata will be used for IFS
###############################################################################################
#Finaldata<-read.csv("Selectedfeatures.csv",header=TRUE)
#levels(Finaldata$PANID)<-make.names(levels(Finaldata$PANID))
################################################################################################

#RandomForest-IFS

seed<-123
ntree=31
trcontrol <- trainControl(method="LOOCV", number = 1, seeds=NA, search="grid",classProbs=TRUE, savePredictions=TRUE)
set.seed<-seed
model_before_RF <- train(Finaldata[-1],Finaldata$PANID, method="rf", metric= "Accuracy", tuneGrid=expand.grid(mtry=c(round(sqrt(ncol(Finaldata[-1]))))), trControl=trcontrol, nodesize=0, maxnodes=100, ntree=ntree, tuneLength = 1)


tab1<-confusionMatrix(model_before_RF$pred$pred, model_before_RF$pred$obs)
tab1<-tab1$table
Confusion_RF<-list(matrix(c(tab1[1:2,1:2]),nrow=2,ncol=2))
accu1=(tab1[1,1]+tab1[2,2])/(tab1[1,1]+tab1[1,2]+tab1[2,1]+tab1[2,2])
recall1=(tab1[1,1])/(tab1[1,1]+tab1[2,1])
specificity1=(tab1[2,2])/(tab1[1,2]+tab1[2,2])
prec1=(tab1[1,1])/(tab1[1,1]+tab1[1,2])
MCC1=((tab1[1,1]*tab1[2,2])-(tab1[1,2]*tab1[2,1]))/(sqrt((tab1 [1,1]+tab1 [1,2])* (tab1 [1,1]+tab1 [2,1])*(tab1 [2,2]+tab1 [1,2])*(tab1 [2,2]+tab1[2,1])))

print(paste("Accuracy:",round(accu1,3),"Sensitivity/Recall:",round(recall1,3),"Precision:",round(prec1,3),"Specificity:",round(specificity1,3),"MCC:",round(MCC1,3)))


res_RF<-data.frame(matrix(c(accu1,recall1,specificity1,prec1,MCC1)))
rownames(res_RF)<-c("Accuracy","Recall","Specificity","Preicision","MCC")
colnames(res_RF)<-c("Before")


for (i in 2:ncol(Finaldata)) {
set.seed<-seed
model_after_RF <- train(Finaldata[2:i],Finaldata$PANID, method="rf", metric= "Accuracy", trControl=trcontrol, tuneGrid=expand.grid(mtry=c(round(sqrt(i)))), ntree=ntree, nodesize=0, maxnodes=100, tuneLength = 1)

#tab2<- as.matrix(model_after$finalModel$confusion)
tab2<-confusionMatrix(model_after_RF$pred$pred, model_after_RF$pred$obs)
tab2<-tab2$table
accu2=(tab2[1,1]+tab2[2,2])/(tab2[1,1]+tab2[1,2]+tab2[2,1]+tab2[2,2])
recall2=(tab2[1,1])/(tab2[1,1]+tab2[2,1])
specificity2=(tab2[2,2])/(tab2[1,2]+tab2[2,2])
prec2=(tab2[1,1])/(tab2[1,1]+tab2[1,2])
MCC2=((tab2[1,1]*tab2[2,2])-(tab2[1,2]*tab2[2,1]))/(sqrt((tab2 [1,1]+tab2 [1,2])* (tab2 [1,1]+tab2 [2,1])*(tab2 [2,2]+tab2 [1,2])*(tab2 [2,2]+tab2[2,1])))
res_RF[,i]<-c(accu2,recall2,specificity2,prec2,MCC2)
Confusion_RF[i]<-list(matrix(c(tab2[1:2,1:2]),nrow=2,ncol=2))
}


write.csv(res_RF,"ResultTables_RF_ntree31.csv", row.names=FALSE)
write.csv(Confusion_RF,"Confusion_RF_ntree31.csv", row.names=FALSE)



############################################################################


#SVM-IFS

seed<-123
W<-c(0.35,0.35,0.65,0.65,0.35,0.35,0.35,0.35,0.35,0.35,0.35,0.35,0.35,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.65,0.35,0.35,0.35,0.35,0.35,0.35,0.35,0.35,0.35,0.35,0.35,0.65,0.65,0.35,0.35)
trcontrol <- trainControl(method="LOOCV", number = 1, seeds=NA, search="grid", classProbs=TRUE, savePredictions=TRUE)
#CLASSIFICATION FORMULA
set.seed<-seed
model_before_SVM <- train(Finaldata[-1],Finaldata$PANID, method="svmLinearWeights", metric= "Accuracy", trControl=trcontrol, tuneLength = 1, weights=W)


tab3<-confusionMatrix(model_before_SVM$pred$pred, model_before_SVM$pred$obs)
tab3<-tab3$table
Confusion_SVM<-list(matrix(c(tab3[1:2,1:2]),nrow=2,ncol=2))

accu3=(tab3[1,1]+tab3[2,2])/(tab3[1,1]+tab3[1,2]+tab3[2,1]+tab3[2,2])
recall3=(tab3[1,1])/(tab3[1,1]+tab3[2,1])
specificity3=(tab3[2,2])/(tab3[1,2]+tab3[2,2])
prec3=(tab3[1,1])/(tab3[1,1]+tab3[1,2])
MCC3=((tab3[1,1]*tab3[2,2])-(tab3[1,2]*tab3[2,1]))/(sqrt((tab3 [1,1]+tab3 [1,2])* (tab3 [1,1]+tab3 [2,1])*(tab3 [2,2]+tab3 [1,2])*(tab3 [2,2]+tab3[2,1])))

print(paste("Accuracy:",round(accu3,3),"Sensitivity/Recall:",round(recall3,3),"Precision:",round(prec3,3),"Specificity:",round(specificity3,3),"MCC:",round(MCC3,3)))

res_SVM<-data.frame(matrix(c(accu3,recall3,specificity3,prec3,MCC3)))
rownames(res_SVM)<-c("Accuracy","Recall","Specificity","Preicision","MCC")
colnames(res_SVM)<-c("Before")

for (i in 2:ncol(Finaldata)) {
set.seed<-seed
model_after_SVM <- train(Finaldata[2:i],Finaldata$PANID, method="svmLinearWeights", metric= "Accuracy", trControl=trcontrol, tuneLength = 1, weights=W)
tab4<-confusionMatrix(model_after_SVM$pred$pred, model_after_SVM$pred$obs)
tab4<-tab4$table
accu4=(tab4[1,1]+tab4[2,2])/(tab4[1,1]+tab4[1,2]+tab4[2,1]+tab4[2,2])
recall4=(tab4[1,1])/(tab4[1,1]+tab4[2,1])
specificity4=(tab4[2,2])/(tab4[1,2]+tab4[2,2])
prec4=(tab4[1,1])/(tab4[1,1]+tab4[1,2])
MCC4=((tab4[1,1]*tab4[2,2])-(tab4[1,2]*tab4[2,1]))/(sqrt((tab4 [1,1]+tab4 [1,2])* (tab4 [1,1]+tab4 [2,1])*(tab4 [2,2]+tab4 [1,2])*(tab4 [2,2]+tab4[2,1])))
res_SVM[,i]<-c(accu4,recall4,specificity4,prec4,MCC4)
Confusion_SVM[i]<-list(matrix(c(tab4[1:2,1:2]),nrow=2,ncol=2))
}

write.csv(res_SVM,"ResultTables_SVM.csv", row.names=FALSE)
write.csv(Confusion_SVM,"Confusion_SVM.csv", row.names=FALSE)


#Try LDAfeatureselectionStepwise and KNN also, Sigfeature,


####################################################################################################

#RRF-IFS

seed<-123
ntree=21
trcontrol <- trainControl(method="LOOCV", number = 1, seeds=NA, search="grid",classProbs=TRUE, savePredictions=TRUE)
#CLASSIFICATION FORMULA
set.seed<-seed
model_before_RRF <- train(Finaldata[-1],Finaldata$PANID, method="RRF", metric= "Accuracy", trControl=trcontrol, tuneGrid=expand.grid(mtry=c(27),coefReg=c(1),coefImp=c(1)),nodesize=0, maxnodes=100,ntree=ntree)
	
tab7<-confusionMatrix(model_before_RRF$pred$pred, model_before_RRF$pred$obs)
tab7<-tab7$table
Confusion_RRF<-list(matrix(c(tab7[1:2,1:2]),nrow=2,ncol=2))
accu7=(tab7[1,1]+tab7[2,2])/(tab7[1,1]+tab7[1,2]+tab7[2,1]+tab7[2,2])
recall7=(tab7[1,1])/(tab7[1,1]+tab7[2,1])
specificity7=(tab7[2,2])/(tab7[1,2]+tab7[2,2])
prec7=(tab7[1,1])/(tab7[1,1]+tab7[1,2])
MCC7=((tab7[1,1]*tab7[2,2])-(tab7[1,2]*tab7[2,1]))/(sqrt((tab7 [1,1]+tab7 [1,2])* (tab7 [1,1]+tab7 [2,1])*(tab7 [2,2]+tab7 [1,2])*(tab7 [2,2]+tab7[2,1])))

print(paste("Accuracy:",round(accu7,3),"Sensitivity/Recall:",round(recall7,3),"Precision:",round(prec7,3),"Specificity:",round(specificity7,3),"MCC:",round(MCC7,3)))

res_RRF<-data.frame(matrix(c(accu7,recall7,specificity7,prec7,MCC7)))
rownames(res_RRF)<-c("Accuracy","Recall","Specificity","Preicision","MCC")
colnames(res_RRF)<-c("Before")

for (i in 2:ncol(Finaldata)) {
set.seed<-seed
model_after_RRF <- train(Finaldata[2:i],Finaldata$PANID, method="RRF", metric= "Accuracy", trControl=trcontrol,tuneGrid=expand.grid(mtry=c(round(sqrt(i))),coefReg=c(1),coefImp=c(1)),nodesize=0, maxnodes=100, ntree=ntree)
tab8<-confusionMatrix(model_after_RRF$pred$pred, model_after_RRF$pred$obs)
tab8<-tab8$table
accu8=(tab8[1,1]+tab8[2,2])/(tab8[1,1]+tab8[1,2]+tab8[2,1]+tab8[2,2])
recall8=(tab8[1,1])/(tab8[1,1]+tab8[2,1])
specificity8=(tab8[2,2])/(tab8[1,2]+tab8[2,2])
prec8=(tab8[1,1])/(tab8[1,1]+tab8[1,2])
MCC8=((tab8[1,1]*tab8[2,2])-(tab8[1,2]*tab8[2,1]))/(sqrt((tab8 [1,1]+tab8 [1,2])* (tab8 [1,1]+tab8 [2,1])*(tab8 [2,2]+tab8 [1,2])*(tab8 [2,2]+tab8[2,1])))
res_RRF[,i]<-c(accu8,recall8,specificity8,prec8,MCC8)
Confusion_RRF[i]<-list(matrix(c(tab8[1:2,1:2]),nrow=2,ncol=2))
}

write.csv(res_RRF,"ResultTables_RRF2.csv", row.names=FALSE)
write.csv(Confusion_RRF,"Confusion_RRF2.csv", row.names=FALSE)


####################################################################################################

#NaiveBayes-IFS

set.seed<-seed
model_before_NB <- train(Finaldata[-1],Finaldata$PANID, method="nb", metric= "Accuracy", trControl=trcontrol, tuneLength = 1)


tab5<-confusionMatrix(model_before_NB$pred$pred, model_before_NB$pred$obs)
tab5<-tab5$table
Confusion_NB<-list(matrix(c(tab5[1:2,1:2]),nrow=2,ncol=2))

accu5=(tab5[1,1]+tab5[2,2])/(tab5[1,1]+tab5[1,2]+tab5[2,1]+tab5[2,2])
recall5=(tab5[1,1])/(tab5[1,1]+tab5[2,1])
specificity5=(tab5[2,2])/(tab5[1,2]+tab5[2,2])
prec5=(tab5[1,1])/(tab5[1,1]+tab5[1,2])
MCC5=((tab5[1,1]*tab5[2,2])-(tab5[1,2]*tab5[2,1]))/(sqrt((tab5 [1,1]+tab5 [1,2])* (tab5 [1,1]+tab5 [2,1])*(tab5 [2,2]+tab5 [1,2])*(tab5 [2,2]+tab5[2,1])))

print(paste("Accuracy:",round(accu5,3),"Sensitivity/Recall:",round(recall5,3),"Precision:",round(prec5,3),"Specificity:",round(specificity5,3),"MCC:",round(MCC5,3)))

res_NB<-data.frame(matrix(c(accu5,recall5,specificity5,prec5,MCC5)))
rownames(res_NB)<-c("Accuracy","Recall","Specificity","Preicision","MCC")
colnames(res_NB)<-c("Before")

for (i in 2:ncol(Finaldata)) {
set.seed<-seed
model_after_NB <- train(Finaldata[2:i],Finaldata$PANID, method="nb", metric= "Accuracy", trControl=trcontrol, tuneLength = 1)
tab6<-confusionMatrix(model_after_NB$pred$pred, model_after_NB$pred$obs)
tab6<-tab6$table
accu6=(tab6[1,1]+tab6[2,2])/(tab6[1,1]+tab6[1,2]+tab6[2,1]+tab6[2,2])
recall6=(tab6[1,1])/(tab6[1,1]+tab6[2,1])
specificity6=(tab6[2,2])/(tab6[1,2]+tab6[2,2])
prec6=(tab6[1,1])/(tab6[1,1]+tab6[1,2])
MCC6=((tab6[1,1]*tab6[2,2])-(tab6[1,2]*tab6[2,1]))/(sqrt((tab6 [1,1]+tab6 [1,2])* (tab6 [1,1]+tab6 [2,1])*(tab6 [2,2]+tab6 [1,2])*(tab6 [2,2]+tab6[2,1])))
res_NB[,i]<-c(accu6,recall6,specificity6,prec6,MCC6)
Confusion_NB[i]<-list(matrix(c(tab6[1:2,1:2]),nrow=2,ncol=2))
}

write.csv(res_NB,"ResultTables_NB.csv", row.names=FALSE)
write.csv(Confusion_NB,"Confusion_NB.csv", row.names=FALSE)

################################################################################################################


#knn-IFS

trcontrol <- trainControl(method="LOOCV", number = 1, seeds=NA, search="grid",classProbs=TRUE, savePredictions=TRUE)
set.seed<-seed
model_before_knn <- train(Finaldata[-1],Finaldata$PANID, method="knn", metric= "Accuracy", trControl=trcontrol, tuneLength = 1)
tab9<-confusionMatrix(model_before_knn$pred$pred, model_before_knn$pred$obs)
tab9<-tab9$table
Confusion_knn<-list(matrix(c(tab9[1:2,1:2]),nrow=2,ncol=2))

accu9=(tab9[1,1]+tab9[2,2])/(tab9[1,1]+tab9[1,2]+tab9[2,1]+tab9[2,2])
recall9=(tab9[1,1])/(tab9[1,1]+tab9[2,1])
specificity9=(tab9[2,2])/(tab9[1,2]+tab9[2,2])
prec9=(tab9[1,1])/(tab9[1,1]+tab9[1,2])
MCC9=((tab9[1,1]*tab9[2,2])-(tab9[1,2]*tab9[2,1]))/(sqrt((tab9 [1,1]+tab9 [1,2])* (tab9 [1,1]+tab9 [2,1])*(tab9 [2,2]+tab9 [1,2])*(tab9 [2,2]+tab9[2,1])))

print(paste("Accuracy:",round(accu9,3),"Sensitivity/Recall:",round(recall9,3),"Precision:",round(prec9,3),"Specificity:",round(specificity9,3),"MCC:",round(MCC9,3)))

res_knn<-data.frame(matrix(c(accu9,recall9,specificity9,prec9,MCC9)))
rownames(res_knn)<-c("Accuracy","Recall","Specificity","Preicision","MCC")
colnames(res_knn)<-c("Before")

for (i in 2:ncol(Finaldata)) {
set.seed<-seed
model_after_knn <- train(Finaldata[2:i],Finaldata$PANID, method="knn", metric= "Accuracy", trControl=trcontrol, tuneLength = 1)
tab10<-confusionMatrix(model_after_knn$pred$pred, model_after_knn$pred$obs)
tab10<-tab10$table
accu10=(tab10[1,1]+tab10[2,2])/(tab10[1,1]+tab10[1,2]+tab10[2,1]+tab10[2,2])
recall10=(tab10[1,1])/(tab10[1,1]+tab10[2,1])
specificity10=(tab10[2,2])/(tab10[1,2]+tab10[2,2])
prec10=(tab10[1,1])/(tab10[1,1]+tab10[1,2])
MCC10=((tab10[1,1]*tab10[2,2])-(tab10[1,2]*tab10[2,1]))/(sqrt((tab10 [1,1]+tab10 [1,2])* (tab10 [1,1]+tab10 [2,1])*(tab10 [2,2]+tab10 [1,2])*(tab10 [2,2]+tab10[2,1])))
res_knn[,i]<-c(accu10,recall10,specificity10,prec10,MCC10)
Confusion_knn[i]<-list(matrix(c(tab10[1:2,1:2]),nrow=2,ncol=2))
}

write.csv(res_knn,"ResultTables_knn.csv", row.names=FALSE)
write.csv(Confusion_knn,"Confusion_knn.csv", row.names=FALSE)



