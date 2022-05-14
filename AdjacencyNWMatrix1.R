#To produce an adjacency matrix data from an edgelist data of a network (Node1,node2,weight)
#Exclusive node slection where node 1 and node 2 lists are not same and the variables can be alphanumeric

df<-read.csv("Rawdata.csv", header=T)
colnames(df)<-c("Y","CODE","W")
df$Y<-as.factor(df$Y)
df$CODE<-as.factor(df$CODE)

mat<-matrix(0,length(levels(df$Y)),length (levels(df$CODE)))
rownames(mat)<-(levels(df$Y))
colnames(mat)<-(levels(df$CODE))
df2<-data.frame(mat)

for
 (i in 1:length(levels(df$Y)))
{
for (j in 1:length(levels(df$CODE)))

if(isTRUE (df$W[df$Y==levels(df$Y)[i]&df$CODE==levels(df$CODE)[j]]>=0))

{df2[i,j]<-df$W[df$Y==levels(df$Y)[i]&df$CODE==levels(df$CODE)[j]]}

}

write.csv(df2,file="ADJmatrix.csv")
