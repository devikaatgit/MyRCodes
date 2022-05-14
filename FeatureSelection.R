setwd("C:/Users/DMTM1/Desktop/mRMR_IFS_RF/SVM_RF_MRMRE")
Data<-read.csv("726features.csv",header=T)
Data$PANID<-factor(Data$PANID, ordered = TRUE)

library(sigFeature)

Res<-svmrfeFeatureRanking(Data[-1],Data$PANID)
write.csv(Res,file = "SVM_RFE.csv")
Res1<-svmrfeFeatureRanking(Data[-1],Data$PANID)
Res2<-svmrfeFeatureRanking(Data[-1],Data$PANID)
Res3<-svmrfeFeatureRanking(Data[-1],Data$PANID)
Res<-data.frame(Res1,Res2,Res3)
write.csv(Res,file = "SVM_RFE.csv")

library(FSelector)
RF1<-random.forest.importance(PANID~., Data, importance.type = 1)
RF2<-random.forest.importance(PANID~., Data, importance.type = 1)
RF3<-random.forest.importance(PANID~., Data, importance.type = 1)
RF<-data.frame(RF1,RF2,RF3)
#a<-RF$attr_importance
#b<-RF$attr_importance.1
#c<-RF$attr_importance.2
#cor.test(a,c, method = "spearm", alternative = "g")
write.csv(RF,file = "RandomForest_Fselctor.csv")

RRelief1<-relief(PANID~., Data)
RRelief2<-relief(PANID~., Data)
RRelief3<-relief(PANID~., Data)
RRelief<-data.frame(RRelief1,RRelief2,RRelief3)
write.csv(RRelief,file = "RRelief_Fselctor.csv")

OneR1<-oneR(PANID~., Data)
OneR2<-oneR(PANID~., Data)
OneR3<-oneR(PANID~., Data)
OneR<-data.frame(OneR1,OneR2,OneR3)
write.csv(OneR,file = "OneR_Fselctor.csv")

IG1<-information.gain(PANID~., Data)
IG2<-information.gain(PANID~., Data)
IG3<-information.gain(PANID~., Data)
IG<-data.frame(IG1,IG2,IG3)
write.csv(IG,file = "InfGain_Fselctor.csv")

GR1<-gain.ratio(PANID~., Data)
GR2<-gain.ratio(PANID~., Data)
GR3<-gain.ratio(PANID~., Data)
GR<-data.frame(GR1,GR2,GR3)
write.csv(GR,file = "GainRatio_Fselctor.csv")


Uncertainity1<-symmetrical.uncertainty(PANID~., Data)
Uncertainity2<-symmetrical.uncertainty(PANID~., Data)
Uncertainity3<-symmetrical.uncertainty(PANID~., Data)
Uncertainity<-data.frame(Uncertainity1,Uncertainity2,Uncertainity3)
write.csv(Uncertainity,file = "Uncertainity_Fselctor.csv")

Chi1<-chi.squared(PANID~., Data)
Chi2<-chi.squared(PANID~., Data)
Chi3<-chi.squared(PANID~., Data)
Chi<-data.frame(Chi1,Chi2,Chi3)
write.csv(Chi,file = "Chi_Fselctor.csv")

Data$PANID<-as.numeric(Data$PANID)
Rank1<-rank.correlation(PANID~., Data)
Rank2<-rank.correlation(PANID~., Data)
Rank3<-rank.correlation(PANID~., Data)
Rank<-data.frame(Rank1,Rank2,Rank3)
write.csv(Rank,file = "rank_Fselctor.csv")

#cor.test(a,b, method = "spearm", alternative = "g")
library(mRMRe)


dd<-mRMR.data(Data)
res<-mRMR.ensemble(data = dd, target_indices = 1,solution_count = 5, feature_count = 50)
final<-data.frame(res@filters$`1`,res@scores$`1`)
#Sum and sort in excel

#library(OrderedList)
#list1<-list$RandomForest
#list2<-list$Rrelief

#list1<-list[[1]]
#list2<-list[[2]]
#x <- compareLists(list1,list2,two.sided=F,invar.q=0.9)
#getOverlap(x, max.rank = NULL, percent = 1)


#Consoildated data in ordered list for all techniques - columnwise saved as Ranks.csv
library(RobustRankAggreg)
list<-read.csv("Ranks.csv",header=T,stringsAsFactors=F)
list<-as.list(list)
Rank<-aggregateRanks(glist =list, full=TRUE, exact=TRUE)
write.csv(Rank,"FinalRankedList_RRA.csv")
