#Code to convert a network data in adjacency matrix format to an edgelist tabular form data

setwd("F:/Move/DBT-RA/GalaxyMothurFiles/UseGalaxyOrg_Gurusamy_OTU/NetCoMi/Final")
df1<-read.csv("HUCdisc_DiffNet.csv", header=F)
m<-as.matrix(df1[-1,-1])
rownames(m)<-df1[2:nrow(df1),1]
colnames(m)<-df1[1,2:ncol(df1)]

dfr=0
(
for(r in 1:nrow(m))
(for(c in 1:ncol(m))
if(m[r,c]<1&&m[r,c]>0)
{dfr<-dfr+1}
)
)

mat = matrix(ncol = 3, nrow = dfr)
df=data.frame(mat)
colnames(df)<-c("Node1","Node2","Weight")

n=1

for(r in 1:nrow(m))
{(for(c in 1:ncol(m))
if (m[r,c]>0&&m[r,c]<1)
{df$Node1[n]<-rownames(m)[r]
df$Node2[n]<-colnames(m)[c]
df$Weight[n]<-m[r,c]
n=n+1}
)}

write.csv(df,file="DNetworkHUC.csv")
