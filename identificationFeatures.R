# Definition of Libraries----------------------------------------
#You may need to change/include the path of your working directory 
#Import the dataset into R Studio. 
rm(list = ls()) # Cleaning environment
library(tidyverse)
library(ggplot2)
library(ggpubr)
# Calling library dplyr for the use of filter
library(dplyr)
#install.packages("moments"); #De-comment to install the package
library(moments) #Load the package
#install.packages("scales"); #De-comment to install the package
library(scales) #For more graphic scales options
library(factoextra) # Library used to graph PCA Biplot

## Taking 500 random samples ----
dat <- read.csv("Path Here", na.strings=NA, stringsAsFactors=TRUE)

set.seed("StudentIDNumberHere") 
#Randomly select 500 rows 
selected.rows <- sample(1:nrow(dat),size=500,replace=FALSE) 

#Your sub-sample of 500 observations 
mydata <- dat[selected.rows,] 

dim(mydata) #check the dimension of your sub-sample


# Definition of Functions----------------------------------------

# Function to graph histograms
graph_histogram <- function(dataset, feature) {
  #Visualization Histogram
  g <- ggplot(dataset,aes(x=feature)) +
    geom_histogram(bins = 30, colour="white",fill="steelblue") 
  #Clean up the plot
  g.clean <- g +
    ylab("Frequency") +
    theme_pubclean(base_size=14); 
  g.clean
}

#Function used to fin lower fencer depending on Quantile 1 (25%)
lowerfencer <- function(feature) {
  iqr = IQR(feature) 
  Q1 = quantile(feature, 0.25)
  LowerFencer = Q1 - 1.5*iqr
  return(LowerFencer)
}

#Function used to fin upper fencer depending on Quantile 3 (75%)
upperfencer <- function(feature) {
  iqr = IQR(feature)
  Q3 = quantile(feature, 0.75)
  UpperFencer = Q3 + 1.5*iqr
  return(UpperFencer)
}


# Part 1 (i) ---------------------------------------------------------

##Determining the number (%) of instances for each of categorical or binary variables for Table 1--------
Port.freq <- table(mydata$Port); Port.freq 
#Total number of observations in Port
Port.prop <- prop.table(Port.freq); Port.prop*100
#Proportions of observations for each instance in Port

Protocol.freq <- table(mydata$Protocol); Protocol.freq 
#Total number of observations in Protocol
Protocol.prop <- prop.table(Protocol.freq); Protocol.prop*100
#Proportions of observations for each instance in Protocol

Target.Honeypot.Server.OS.freq <- table(mydata$Target.Honeypot.Server.OS); Target.Honeypot.Server.OS.freq 
#Total number of observations in Target.Honeypot.Server.OS
Target.Honeypot.Server.OS.prop <- prop.table(Target.Honeypot.Server.OS.freq); Target.Honeypot.Server.OS.prop*100
#Proportions of observations for each instance in Target.Honeypot.Server.OS

Source.OS.Detected.freq <- table(mydata$Source.OS.Detected); Source.OS.Detected.freq 
#Total number of observations in Source.OS.Detected
Source.OS.Detected.prop <- prop.table(Source.OS.Detected.freq); Source.OS.Detected.prop*100
#Proportions of observations for each instance in Source.OS.Detected

Source.Port.Range.freq <- table(mydata$Source.Port.Range); Source.Port.Range.freq 
#Total number of observations in Source.Port.Range
Source.Port.Range.prop <- prop.table(Source.Port.Range.freq); Source.Port.Range.prop*100
#Proportions of observations for each instance in Source.Port.Range

Source.IP.Type.Detected.freq <- table(mydata$Source.IP.Type.Detected); Source.IP.Type.Detected.freq 
#Total number of observations in Source.IP.Type.Detected
Source.IP.Type.Detected.prop <- prop.table(Source.IP.Type.Detected.freq); Source.IP.Type.Detected.prop*100
#Proportions of observations for each instance in Source.IP.Type.Detected

APT.freq <- table(mydata$APT); APT.freq 
#Total number of observations in APT
APT.prop <- prop.table(APT.freq); APT.prop*100
#Proportions of observations for each instance in APT


# Part 1 (ii) ---------------------------------------------------------

## Finding measure of centre for Table 2 -----

###Measures of centre for Hits -----
mean(mydata$Hits) #Mean (i.e. average) Hits
median(mydata$Hits) #Median age
range(mydata$Hits) #Range of age, i.e. minimum and maximum
skewness(mydata$Hits, na.rm = TRUE) #Measures of shape

###Measures of centre for Average.Request.Size.Bytes -----
mean(mydata$Average.Request.Size.Bytes) #Mean (i.e. average) Average.Request.Size.Bytes
median(mydata$Average.Request.Size.Bytes) #Median age
range(mydata$Average.Request.Size.Bytes) #Range of age, i.e. minimum and maximum
skewness(mydata$Average.Request.Size.Bytes, na.rm = TRUE) #Measures of shape

###Measures of centre for Attack.Window.Seconds -----
mean(mydata$Attack.Window.Seconds) #Mean (i.e. average) Attack.Window.Seconds
median(mydata$Attack.Window.Seconds) #Median age
range(mydata$Attack.Window.Seconds) #Range of age, i.e. minimum and maximum
skewness(mydata$Attack.Window.Seconds, na.rm = TRUE) #Measures of shape

###Measures of centre for Average.Attacker.Payload.Entropy.Bits-----
mean(mydata$Average.Attacker.Payload.Entropy.Bits) #Mean (i.e. average) Average.Attacker.Payload.Entropy.Bits
median(mydata$Average.Attacker.Payload.Entropy.Bits) #Median age
range(mydata$Average.Attacker.Payload.Entropy.Bits) #Range of age, i.e. minimum and maximum
skewness(mydata$Average.Attacker.Payload.Entropy.Bits, na.rm = TRUE) #Measures of shape

###Measures of centre for Attack.Source.IP.Address.Count-----
mean(mydata$Attack.Source.IP.Address.Count) #Mean (i.e. average) Attack.Source.IP.Address.Count
median(mydata$Attack.Source.IP.Address.Count) #Median age
range(mydata$Attack.Source.IP.Address.Count) #Range of age, i.e. minimum and maximum
skewness(mydata$Attack.Source.IP.Address.Count, na.rm = TRUE) #Measures of shape

###Measures of centre for Average.ping.to.attacking.IP.milliseconds-----
mean(mydata$Average.ping.to.attacking.IP.milliseconds) #Mean (i.e. average) Average.ping.to.attacking.IP.milliseconds
median(mydata$Average.ping.to.attacking.IP.milliseconds) #Median age
range(mydata$Average.ping.to.attacking.IP.milliseconds) #Range of age, i.e. minimum and maximum
skewness(mydata$Average.ping.to.attacking.IP.milliseconds, na.rm = TRUE) #Measures of shape

###Measures of centre for Average.ping.variability-----
mean(mydata$Average.ping.variability) #Mean (i.e. average) Average.ping.variability
median(mydata$Average.ping.variability) #Median age
range(mydata$Average.ping.variability) #Range of age, i.e. minimum and maximum
skewness(mydata$Average.ping.variability, na.rm = TRUE) #Measures of shape

###Measures of centre for Individual.URLs.requested-----
mean(mydata$Individual.URLs.requested) #Mean (i.e. average) Individual.URLs.requested
median(mydata$Individual.URLs.requested) #Median age
range(mydata$Individual.URLs.requested) #Range of age, i.e. minimum and maximum
skewness(mydata$Individual.URLs.requested, na.rm = TRUE) #Measures of shape

###Measures of centre for IP.Range.Trust.Score-----
mean(mydata$IP.Range.Trust.Score) #Mean (i.e. average) IP.Range.Trust.Score
median(mydata$IP.Range.Trust.Score) #Median age
range(mydata$IP.Range.Trust.Score) #Range of age, i.e. minimum and maximum
skewness(mydata$IP.Range.Trust.Score, na.rm = TRUE) #Measures of shape


(colMeans(is.na(mydata)))*100 # Percentage of missing observations


# Part 1 (iii) ---------------------------------------------------------

##Replacing categorical invalid values on Source OS Detected -----------------------------------------------------
#Inserting NA as missing values on ??? observations
mydata$Source.OS.Detected[mydata$Source.OS.Detected == "???"] <- NA

#Validating ??? was replaced
sum(mydata$Source.OS.Detected == "???")

##Handling missing values Source Port Range Feature-------------------
# Delete Source.Port.Range Feature
mydata$Source.Port.Range <- NULL 

##Detecting outliers Hits-----------------------------------------------------
# Hits Histogram
fig1 <- graph_histogram(mydata, mydata$Hits)
fig1 <- fig1 + xlab("Hits"); fig1
# Generate another Column with outliers equal 0
mydata$outlier <- ifelse(mydata$Hits > lowerfencer(mydata$Hits) & mydata$Hits < upperfencer(mydata$Hits),1,0)
#View new column
view(mydata)
#Calculation of number and percentage of outliers
res <- sum(mydata$outlier == 0)
sprintf("There are %s outliers", res)
res <- res*100/sum(mydata$outlier)
res <-format(round(res, 2), nsmall = 2)
sprintf("Percentage of Outlier is %s", res)


##Detecting outliers Average Request Size -------------------------------
# Average Request Size Histogram
fig2 <- graph_histogram(mydata, mydata$Average.Request.Size.Bytes)
fig2 <- fig2 + xlab("Average Request Size"); fig2
# Generate another Column with outliers equal 0
mydata$outlier <- ifelse(mydata$Average.Request.Size.Bytes > lowerfencer(mydata$Average.Request.Size.Bytes) & mydata$Average.Request.Size.Bytes < upperfencer(mydata$Average.Request.Size.Bytes),1,0)
#View new column
view(mydata)
#Calculation of number and percentage of outliers
res <- sum(mydata$outlier == 0)
sprintf("There are %s outliers", res)
res <- res*100/sum(mydata$outlier)
res <-format(round(res, 2), nsmall = 2)
sprintf("Percentage of Outlier is %s", res)


##Detecting outliers Attack Window -------------------------------
# Attack Window Histogram
fig3 <- graph_histogram(mydata, mydata$Attack.Window.Seconds)
fig3 <- fig3 + xlab("Attack Window (seconds)"); fig3
# Generate another Column with outliers equal 0
mydata$outlier <- ifelse(mydata$Attack.Window.Seconds > lowerfencer(mydata$Attack.Window.Seconds) & mydata$Attack.Window.Seconds < upperfencer(mydata$Attack.Window.Seconds),1,0)
#View new column
view(mydata)
#Calculation of number and percentage of outliers
res <- sum(mydata$outlier == 0)
sprintf("There are %s outliers", res)
res <- res*100/sum(mydata$outlier)
res <-format(round(res, 2), nsmall = 2)
sprintf("Percentage of Outlier is %s", res)


##Detecting outliers Average Attacker Payload Entropy -------------------------------
# Average Attacker Payload Entropy Histogram
fig4 <- graph_histogram(mydata, mydata$Average.Attacker.Payload.Entropy.Bits)
fig4 <- fig4 + xlab("Average Attacker Payload Entropy (Bits)"); fig4
# Generate another Column with outliers equal 0
mydata$outlier <- ifelse(mydata$Average.Attacker.Payload.Entropy.Bits > lowerfencer(mydata$Average.Attacker.Payload.Entropy.Bits) & mydata$Average.Attacker.Payload.Entropy.Bits < upperfencer(mydata$Average.Attacker.Payload.Entropy.Bits),1,0)
#View new column
view(mydata)
#Calculation of number and percentage of outliers
res <- sum(mydata$outlier == 0)
sprintf("There are %s outliers", res)
res <- res*100/sum(mydata$outlier)
res <-format(round(res, 2), nsmall = 2)
sprintf("Percentage of Outlier is %s", res)


##Detecting outliers Attack Source IP Address Count ---------------------
# Attack Source IP Address Count Histogram
fig5 <- graph_histogram(mydata, mydata$Attack.Source.IP.Address.Count)
fig5 <- fig5 + xlab("Attack Source IP Address Count"); fig5
# Generate another Column with outliers equal 0
mydata$outlier <- ifelse(mydata$Attack.Source.IP.Address.Count > lowerfencer(mydata$Attack.Source.IP.Address.Count) & mydata$Attack.Source.IP.Address.Count < upperfencer(mydata$Attack.Source.IP.Address.Count),1,0)
#View new column
view(mydata)
#Calculation of number and percentage of outliers
res <- sum(mydata$outlier == 0)
sprintf("There are %s outliers", res)
res <- res*100/sum(mydata$outlier)
res <-format(round(res, 2), nsmall = 2)
sprintf("Percentage of Outlier is %s", res)


##Detecting outliers Average ping to attacking IP (milliseconds)-------------------

# Average ping to attacking IP Histogram
fig6 <- graph_histogram(mydata, mydata$Average.ping.to.attacking.IP.milliseconds)
fig6 <- fig6 + xlab("Average ping to attacking IP (milliseconds)"); fig6

# Detecting 99999 values percentage
res <- sum(mydata$Average.ping.to.attacking.IP.milliseconds == 99999)
sprintf("There are %s cases of 99999 values", res)

# Generate another Column with outliers equal 0
mydata$outlier <- ifelse(mydata$Average.ping.to.attacking.IP.milliseconds > lowerfencer(mydata$Average.ping.to.attacking.IP.milliseconds) & mydata$Average.ping.to.attacking.IP.milliseconds < upperfencer(mydata$Average.ping.to.attacking.IP.milliseconds),1,0)
#View new column
view(mydata)
#Calculation of number and percentage of outliers
res <- sum(mydata$outlier == 0)
sprintf("There are %s outliers", res)
res <- res*100/sum(mydata$outlier)
res <-format(round(res, 2), nsmall = 2)
sprintf("Percentage of Outlier is %s", res)



##Detecting outliers Average ping variability-------------------
# Average ping variability Histogram
fig7 <- graph_histogram(mydata, mydata$Average.ping.variability)
fig7 <- fig7 + xlab(" Average ping variability (st.dev)"); fig7
# Generate another Column with outliers equal 0
mydata$outlier <- ifelse(mydata$Average.ping.variability > lowerfencer(mydata$Average.ping.variability) & mydata$Average.ping.variability < upperfencer(mydata$Average.ping.variability),1,0)
#View new column
view(mydata)
#Calculation of number and percentage of outliers
res <- sum(mydata$outlier == 0)
sprintf("There are %s outliers", res)
res <- res*100/sum(mydata$outlier)
res <-format(round(res, 2), nsmall = 2)
sprintf("Percentage of Outlier is %s", res)


##Detecting outliers Individual URLs requested-------------------
# Individual.URLs.requested Histogram
fig8 <- graph_histogram(mydata, mydata$Individual.URLs.requested)
fig8 <- fig8 + xlab("Individual URLs requested"); fig8
# Generate another Column with outliers equal 0
mydata$outlier <- ifelse(mydata$Individual.URLs.requested > lowerfencer(mydata$Individual.URLs.requested) & mydata$Individual.URLs.requested < upperfencer(mydata$Individual.URLs.requested),1,0)
#View new column
view(mydata)
#Calculation of number and percentage of outliers
res <- sum(mydata$outlier == 0)
sprintf("There are %s outliers", res)
res <- res*100/sum(mydata$outlier)
res <-format(round(res, 2), nsmall = 2)
sprintf("Percentage of Outlier is %s", res)

sum(mydata$Individual.URLs.requested == 0)# Detecting total of zeros in Individual.URLs.requested
##Detecting missing values IP Range Trust Score-------------------
# Find missing values in IP.Range.Trust.Score
sum(is.na(mydata$IP.Range.Trust.Score))


# Part 2 (i) ---------------------------------------------------------

##Handling outliers Hits-----------------------------------------------------
# Generate another Column with outliers equal 0

mydata$outlier <- ifelse(mydata$Hits > lowerfencer(mydata$Hits) & mydata$Hits < upperfencer(mydata$Hits),1,0)
# Generate clean data filtering the outliers marked with 0
mydata = filter(mydata, outlier != 0)
# Delete outlier column
mydata$outlier <- NULL 

#Calling function to graph feature after Handling Outliers
fig11 <- graph_histogram(mydata, mydata$Hits)
fig11 <- fig11 + xlab("Hits"); fig11


##Handling outliers Average Request Size -------------------------------
# Generate another Column with outliers equal 0
mydata$outlier <- ifelse(mydata$Average.Request.Size.Bytes > lowerfencer(mydata$Average.Request.Size.Bytes) & mydata$Average.Request.Size.Bytes < upperfencer(mydata$Average.Request.Size.Bytes),1,0)
# Generate clean data filtering the outliers marked with 0
mydata = filter(mydata, outlier != 0)
# Delete outlier column
mydata$outlier <- NULL 

#Calling function to graph feature after Handling Outliers
fig21 <- graph_histogram(mydata, mydata$Average.Request.Size.Bytes)
fig21 <- fig21 + xlab("Average Request Size"); fig21


##Handling outliers Attack Window -------------------------------
# Generate another Column with outliers equal 0
mydata$outlier <- ifelse(mydata$Attack.Window.Seconds > lowerfencer(mydata$Attack.Window.Seconds) & mydata$Attack.Window.Seconds < upperfencer(mydata$Attack.Window.Seconds),1,0)
# Generate clean data filtering the outliers marked with 0
mydata = filter(mydata, outlier != 0)
# Delete outlier column
mydata$outlier <- NULL 

#Calling function to graph feature after Handling Outliers
fig31 <- graph_histogram(mydata, mydata$Attack.Window.Seconds)
fig31 <- fig31 + xlab("Attack Window (seconds)"); fig31

##Handling outliers Average Attacker Payload Entropy -------------------------------
# Generate another Column with outliers equal 0
mydata$outlier <- ifelse(mydata$Average.Attacker.Payload.Entropy.Bits > lowerfencer(mydata$Average.Attacker.Payload.Entropy.Bits) & mydata$Average.Attacker.Payload.Entropy.Bits < upperfencer(mydata$Average.Attacker.Payload.Entropy.Bits),1,0)
# Generate clean data filtering the outliers marked with 0
mydata = filter(mydata, outlier != 0)
# Delete outlier column
mydata$outlier <- NULL 

#Calling function to graph feature after Handling Outliers
fig41 <- graph_histogram(mydata, mydata$Average.Attacker.Payload.Entropy.Bits)
fig41 <- fig41 + xlab("Average Attacker Payload Entropy (Bits)"); fig41


##Handling Attack Source IP Address Count ---------------------
# Generate another Column with outliers equal 0
mydata$outlier <- ifelse(mydata$Attack.Source.IP.Address.Count > lowerfencer(mydata$Attack.Source.IP.Address.Count) & mydata$Attack.Source.IP.Address.Count < upperfencer(mydata$Attack.Source.IP.Address.Count),1,0)
# Generate clean data filtering the outliers marked with 0
mydata = filter(mydata, outlier != 0)
# Delete outlier column
mydata$outlier <- NULL 

#Calling function to graph feature after Handling Outliers
fig51 <- graph_histogram(mydata, mydata$Attack.Source.IP.Address.Count)
fig51 <- fig51 + xlab("Attack Source IP Address Count"); fig51

##Handling outliers Average ping to attacking IP (milliseconds)-------------------

##Inserting NA as missing values on 99999 observations
mydata$Average.ping.to.attacking.IP.milliseconds[mydata$Average.ping.to.attacking.IP.milliseconds == 99999] <- NA

#Calling function to graph feature after Handling Outliers
fig61 <- graph_histogram(mydata, mydata$Average.ping.to.attacking.IP.milliseconds)
fig61 <- fig61 + xlab("Average ping to attacking IP (milliseconds)"); fig6

# # Generate another Column with outliers equal 0
# mydata$outlier <- ifelse(mydata$Average.ping.to.attacking.IP.milliseconds > lowerfencer(mydata$Average.ping.to.attacking.IP.milliseconds) & mydata$Average.ping.to.attacking.IP.milliseconds < upperfencer(mydata$Average.ping.to.attacking.IP.milliseconds),1,0)
# # Generate clean data filtering the outliers marked with 0
# mydata = filter(mydata, outlier != 0)
# # Delete outlier column
# mydata$outlier <- NULL 


##Handling outliers Average ping variability-------------------
# Generate another Column with outliers equal 0

#Normalization of the data with log-transformation
mydata$Average.ping.variability <- sqrt(mydata$Average.ping.variability)

#Checking skewness after transformation
skewness(mydata$Average.ping.variability, na.rm = TRUE)

#Calling function to graph feature after Handling Outliers
fig71 <- graph_histogram(mydata, mydata$Average.ping.variability)
fig71 <- fig71 + xlab(" Average ping variability (st.dev)"); fig71


##Handling outliers Individual URLs requested-------------------
#Normalization of the data with log-transformation
mydata$Individual.URLs.requested <- sqrt(mydata$Individual.URLs.requested)

#Checking skewness after transformation
skewness(mydata$Individual.URLs.requested, na.rm = TRUE)

#Calling function to graph feature after Handling Outliers
fig81 <- graph_histogram(mydata, mydata$Individual.URLs.requested)
fig81 <- fig81 + xlab("Individual URLs requested"); fig81

##Handling missing values Range Trust Score Feature-------------------
# Delete Range.Trust.Score Feature
mydata$IP.Range.Trust.Score <- NULL 


# Part 2 (ii) ---------------------------------------------------------


##Saving Data after the cleaning ------
write.csv(mydata, #Name of the data frame/tibble to be exported
          "C:/Users/juans/OneDrive - Edith Cowan University/MAT6206 - Data Analysis and Visualisation/Assessments/Assignment 1/ML_dataset_updated.csv" #Name of the new CSV file to write the data to.
)


# Part 2 (iii) ---------------------------------------------------------

## Extracting numeric features with APT  ---------------------------------------------------------

mydata <- data.frame(mydata$Hits,mydata$Average.Request.Size.Bytes,mydata$Attack.Window.Seconds,mydata$Average.Attacker.Payload.Entropy.Bits,mydata$Attack.Source.IP.Address.Count,mydata$Average.ping.to.attacking.IP.milliseconds, mydata$Average.ping.variability,mydata$Individual.URLs.requested,mydata$APT);


##Removing the incomplete cases  -----------------------------------

#Check there is not invalid values before start the PCA
pimcomplete = sum(is.na(mydata))*100/count(mydata)
sprintf("Percentage of incomplete cases to remove is %s", pimcomplete)
#Removing incomplete cases
mydata <- na.omit(mydata)

##Showing the bivariate relationship ----

#Scatter plot matrix
pairs(mydata[,1:8],
      pch=23, #Shape of the points
      #Colour of the outline of the points
      col=as.numeric(mydata$mydata.APT)+1,
      #Fill colour of the points with reduced colour intensity
      bg=alpha(as.numeric(mydata$mydata.APT)+1,0.4),
      cex=1.5, #Size of the points
      upper.panel=NULL, #Do not display the the upper panel
      #Substitution the full stop in the feature names with a space
      labels=gsub("[[:punct:]]"," ",colnames(mydata[,1:8])))

##Execution of PCA only on the numeric features-----
pca.mydata <- prcomp(mydata[,1:8],scale=TRUE)
###Showing individual and cumulative proportion of variance -----
summary(pca.mydata)
###Showing Principal component Coefficients -----

###Determining how many components retain ----

varexp.mydata <- summary(pca.mydata)$importance; varexp.mydata

df <- data.frame(Variance=varexp.mydata[1,]^2,PC=1:length(pca.mydata$sdev));
ggplot(df,aes(PC,Variance))+
  geom_line(colour="steelblue",size=1.5,linetype=2)+
  geom_point(size=5)+
  theme_minimal(base_size=14)+
  xlab("Principal Component")+
  ylab("Variance")+
  scale_x_discrete(limits=paste("PC",1:length(pca.mydata$sdev),sep=""))+
  annotate("text",x=c(1:8)+0.15,y=varexp.mydata[1,]^2+0.3,
           label=paste(round(varexp.mydata[2,]*100,1),"%",sep=""))

# Part 2 (iv) ---------------------------------------------------------
##Create a PCA - Biplot-----

### PC1 vs PC2-----
fviz_pca_biplot(pca.mydata,
                axes = c(1,2), #Specifying the PCs to be plotted.
                #Parameters for samples
                col.ind=mydata$mydata.APT, #Outline colour of the shape
                fill.ind=mydata$mydata.APT, #fill colour of the shape
                alpha=0.5, #transparency of the fill colour
                pointsize=4, #Size of the shape
                pointshape=21, #Type of Shape
                #Parameter for variables
                col.var="red", #Colour of the variable labels
                label="var", #Show the labels for the variables only
                repel=TRUE, #Avoid label overplotting
                addEllipses=TRUE, #Add ellipses to the plot
                legend.title=list(colour="APT",fill="APT",alpha="APT"))
### PC1 vs PC3-----
fviz_pca_biplot(pca.mydata,
                axes = c(1,3), #Specifying the PCs to be plotted.
                #Parameters for samples
                col.ind=mydata$mydata.APT, #Outline colour of the shape
                fill.ind=mydata$mydata.APT, #fill colour of the shape
                alpha=0.5, #transparency of the fill colour
                pointsize=4, #Size of the shape
                pointshape=21, #Type of Shape
                #Parameter for variables
                col.var="red", #Colour of the variable labels
                label="var", #Show the labels for the variables only
                repel=TRUE, #Avoid label overplotting
                addEllipses=TRUE, #Add ellipses to the plot
                legend.title=list(colour="APT",fill="APT",alpha="APT"))
### PC2 vs PC3-----
fviz_pca_biplot(pca.mydata,
                axes = c(2,3), #Specifying the PCs to be plotted.
                #Parameters for samples
                col.ind=mydata$mydata.APT, #Outline colour of the shape
                fill.ind=mydata$mydata.APT, #fill colour of the shape
                alpha=0.5, #transparency of the fill colour
                pointsize=4, #Size of the shape
                pointshape=21, #Type of Shape
                #Parameter for variables
                col.var="red", #Colour of the variable labels
                label="var", #Show the labels for the variables only
                repel=TRUE, #Avoid label overplotting
                addEllipses=TRUE, #Add ellipses to the plot
                legend.title=list(colour="APT",fill="APT",alpha="APT"))
##Confirmation of Correlation----
view(cor(mydata[,1:8]))
view(mydata)
# Part 2 (v) ----
##Data classification ----
##Create a biplot for PC1 vs PC2-----

df <- data.frame(pca.mydata$x, #PCA scores
                 APT=mydata$mydata.APT);
ggplot(df,aes(x=PC1,y=PC2))+
  geom_point(aes(colour=APT),alpha=0.8,size=4)+
  theme_minimal(base_size=14)+
  theme(legend.position = "top")+
  xlab("PC1")+
  ylab("PC2");

##Create a biplot for PC1 vs PC3-----

ggplot(df,aes(x=PC1,y=PC3))+
  geom_point(aes(colour=APT),alpha=0.8,size=4)+
  theme_minimal(base_size=14)+
  theme(legend.position = "top")+
  xlab("PC1")+
  ylab("PC3");

##Create a biplot for PC2 vs PC3-----

ggplot(df,aes(x=PC2,y=PC3))+
  geom_point(aes(colour=APT),alpha=0.8,size=4)+
  theme_minimal(base_size=14)+
  theme(legend.position = "top")+
  xlab("PC2")+
  ylab("PC3")


