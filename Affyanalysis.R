#Calling libraries
  library(simpleaffy)
  library(multtest)
  library(cluster)
  library(IRanges)
  library(biomaRt)
  library(affyPLM)

#Calling libraries
  library(hgu133a.db)
  library(hgu133acdf)

#Set a vector for normalization algorithm either rma or mas5, so everywhere the settings change on changing value here
  probe_summary_algorithm = "rma"
#Set a vector of headings of groups to be checked for DE
  test_between=c("control","treatment") 


  
#read data in current directory
#see covdesc.txt format-CEL file names and their category
#read.affy function takes the names and categories from txtfile and reads all corresponding CEL files
  raw.data<-read.affy("covdesc.txt")
#raw.data is a complex vector,dont attempt to open. to see give str(raw.data)


#grouping the two groups to be differentially expressed
  #xcont <- column numbers of control
  #xtreat <- column numbers of treatment
xcont=which(raw.data@phenoData@data$treatment==test_between[1])
xtreat=which(raw.data@phenoData@data$treatment==test_between[2])  
  


#plotting the images of some CEL files
#par function
  par(mfrow=c(2,2))
    image(raw.data[,1])
    image(raw.data[,2])
    image(raw.data[,3])
    image(raw.data[,4])

#Retreiving simple probe level data
  #Retrieves PM (perfect match) probes
    mypm<-pm(raw.data)
  #Retrieves MM (mismatch) probes
    mymm<-mm(raw.data)
  #Retrieve affy IDs
    myaffyids<-probeNames(raw.data)
  #combines all above to a data frame
    probe.result<-data.frame(myaffyids, mypm,mymm)

X11() #To open the graphics in a separate new window

#Plots histogram of PM intensities for all the arrays.
#jpeg("PM_probe_intensities.jpeg")
  hist(raw.data[,1:21],main="PM intensity distribution for the arrays")
#If required,remove the samples which are completely different from others or wait till normalization

X11()

#Boxplot of unnormalized data
#jpeg("Box_plot_unnorm_data.jpg")
  boxplot(raw.data)

#Normalization of data using RMA declared on the top
#function call.exprs() generateslog2 expression values
#'sc'and NA method parameter takes effect for mas5 algorithm  only
  normalized_data<-call.exprs(raw.data,probe_summary_algorithm,sc=500,method = NA)

#normalized_data is a data frame with samples in columns and genes in rows with respective expression values in the cells
  hist(normalized_data) #If required, subset number of columns [,1:21]for seeing hist of 1 or more samples separately
#Remove the outlying sample if any. in the current data, there are not any.
  
#Normalized data is in log2 scale,so later to do linear analysis, 2 power the value needs to be taken.


#Set vectors of values and headings   
  expval<-exprs(normalized_data) #exprs() retrieves expression values
  prbnames<-rownames(normalized_data)
  clnames<-colnames(expval)


#set as data frame
  expval_frame<-data.frame(expval)
  probval_frame<-data.frame(prbnames)
  combined_frame<-data.frame(expval_frame,probval_frame)
  

#Do t test for all rows of a matrix (all genes)
#expval are log, convert to linear before doing ttest
  M<-as.matrix(data.frame(2^expval)) #exaggerated form of M<-2^expval
#Define a function for row-t-test
  row_t_test<-function(x){t.test(x[xcont],x[xtreat],paired = FALSE, var.equal = FALSE)}
#apply row-t-test to all rows
  res<-apply(M,1,row_t_test)
  pval<-0
  for(i in 1:length(res)){pval[i]<-(res[[i]]$p.value)}

#Finding mean (sum/length) of all control and treatment per gene

 #Finding sum of all control/treatment
	#creating empty vectors of length equals number of rows in expval.
	
	treatment_data=rep(0,nrow(expval))
	control_data=rep(0,nrow(expval)) #Replicating 0 for nrow times
 
	#Finding sum (of linear values) by adding values successively to the empty vector with a loop
	
	for(i in xcont) {control_data<-control_data + 2^expval[,i] }
	for(i in xtreat) {treatment_data<-treatment_data + 2^expval[,i] } #For each ith column, all row values are added together

	#Finding mean in linear
	
	control_mean<-control_data/length(xcont)
	treatment_mean<-treatment_data/length(xtreat)

#Finding FC in linear and converting to log
#FC=log(x(linear)/y(linear) OR log(log(xlinear)/log(ylinear)) #!= log(xlinear)/log(ylinear)
FC_linear<-(treatment_mean/control_mean)
FC_log<-log2(FC_linear)

#Annotation/Mapping of probe IDs to gene names and characteristics with help of chip library details(here hgu133a.db)
#To know what details present in db >keytypes(hgu133a.db)

#Copy required columns to a dataframe for annotation
#First define all columns separately and then make a data frame

#ACCNUM<-sapply(contents(hgu133aACCNUM),paste)
#SYMBOL<-sapply(contents(hgu133aSYMBOL),paste)
#DESCRIPTION<-sapply(contents(hgu133aGENENAME),paste)
#annot<-data.frame(ACCNUM,SYMBOL,DESCRIPTION)

annot <- data.frame(ACCNUM=sapply(contents(hgu133aACCNUM), paste, collapse=", "), SYMBOL=sapply(contents(hgu133aSYMBOL), paste, collapse=","), DESC=sapply(contents(hgu133aGENENAME), paste, collapse=", "), ACHROMOSOME=sapply(contents(hgu133aCHR), paste, collapse=", "))

#Reordering according to the IDs in rawdata
myannot<-annot[prbnames,]
final.dat<-data.frame(prbnames,2^expval_frame,FC_linear,FC_log,pval,myannot)


write.table(final.dat,file="hgu133a_analysis_all.txt", sep="\t", row.names=FALSE)
#row.names=FALSE because prbnames(first column) same as rownames - otherwise it will be duplicated.

#Volcano plot
plot(FC_log,log10(pval),col="blue",cex=0.3)


#Selecting genes whose pval<0.05 only
