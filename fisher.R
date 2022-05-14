#setwd() and load data file path/name correctly
dfID<-read.csv("positive4.csv",header=T)
dfwoID<-dfID[,c(2,3,4,5)]
arr<-array(t(dfwoID),dim=c(2,2,nrow(dfwoID)))
p<-apply(arr,3,function(x)fisher.test(x, alternative="greater" ))
t<-0
i<-1
for(i in i:length(p)){t[i]<-(p[[i]]$p.value)}
write.csv(t,file.choose())
