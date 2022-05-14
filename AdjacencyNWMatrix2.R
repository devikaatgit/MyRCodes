#To construct square matrix with all nodes to nodes connections - including self connections:

df<-read.csv("Rawdata.csv", header=T)
colnames(df)<-c("Y","CODE","W")
df$Y<-as.factor(df$Y)
df$CODE<-as.factor(df$CODE)

Newnodes<-c(levels(df$Y),levels(df$CODE))
mat<-matrix(0,length(Newnodes),length (Newnodes))
rownames(mat)<-Newnodes
colnames(mat)<-Newnodes
df2<-data.frame(mat)

for
 (i in 1:length(Newnodes))
{
for (j in 1:length(Newnodes))

if(isTRUE (df$W[df$Y==Newnodes[i]&df$CODE==Newnodes[j]]>=0))

{df2[i,j]<-df$W[df$Y==Newnodes[i]&df$CODE==Newnodes[j]]}

}

write.csv(df2,file="ADJmatrix.csv")
