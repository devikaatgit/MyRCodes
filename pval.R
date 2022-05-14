#converting z to p-value_doublesided
setwd("C:/Users/DMTM1/Documents/Docs/Final work/Effect size analysis/p-value_doublesided")
zval<-read.csv("zval.csv", header=T)
p<-apply(zval,1,function(x)2*pnorm(-abs(x)))
zval$p<-p #Adding pval column

hist(zval$p) #histogram (freq-dist of p) # Take care it is not U-shaped.


#q-value package install
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install("qvalue", version = "3.8")

library(qvalue)

qval <- qvalue(zval$p)

zval$q<-qval$qvalues

#to analyse structure and identify qvalue heads

summary(qval)

str(qval)

write.csv(zval,file="zanalysis_results.csv")

