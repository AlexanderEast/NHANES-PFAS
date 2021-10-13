rm(list=ls())
library(nhanesA)
library(stringr)
library(plyr)
library(rio)


# 1. Load NHANES Data  using nhanesa.
PFAS2018   <- nhanes('PFAS_J')
PFAS2016   <- nhanes('PFAS_I')
PFAS2014   <- nhanes('PFAS_H')
PFAS2012   <- nhanes('PFC_G')

DEMO2018   <- nhanes('DEMO_J')       
DEMO2016   <- nhanes('DEMO_I')  
DEMO2014   <- nhanes('DEMO_H')  
DEMO2012   <- nhanes('DEMO_G')  

PFAS<-rbind.fill(PFAS2012,PFAS2014,PFAS2016,PFAS2018)
DEMO<-rbind.fill(DEMO2012,DEMO2014,DEMO2016,DEMO2018)


data <- merge(PFAS,DEMO, by = 'SEQN')
rm(list=setdiff(ls(), "data"))


# Cycle
colnames(data)[colnames(data) == "SDDSRVYR"] <- "Cycle"
data$Cycle[data$Cycle == 7] <- '2011-2012'
data$Cycle[data$Cycle == 8] <- '2013-2014'
data$Cycle[data$Cycle == 9] <- '2015-2016'
data$Cycle[data$Cycle == 10] <- '2017-2018'


# Gender
data$RIAGENDR[data$RIAGENDR == 1] <- 'Male'
data$RIAGENDR[data$RIAGENDR == 2] <- 'Female'
colnames(data)[colnames(data) == "RIAGENDR"] <- "Sex"

# Age
# Age is 12+, capped at 80 (all values over 80 are coded as 80)
colnames(data)[colnames(data) == "RIDAGEYR"] <- "Age (years)"


# Ethnicity
data$RIDRETH3[data$RIDRETH3 == 1]   <- 'Mexican American'
data$RIDRETH3[data$RIDRETH3 == 2]   <- 'Other Hispanic'
data$RIDRETH3[data$RIDRETH3 == 3]   <- 'White'
data$RIDRETH3[data$RIDRETH3 == 4]   <- "Black"
data$RIDRETH3[data$RIDRETH3 == 6]   <- "Asian"
data$RIDRETH3[data$RIDRETH3 == 7]   <- "Other/Biracial"
data$RIDRETH3[data$RIDRETH3 == "."] <- "Missing"
colnames(data)[colnames(data) == "RIDRETH3"] <- "Ethnicity"

# Born
data$DMDBORN4[data$DMDBORN4 == 1] <- 'US'
data$DMDBORN4[data$DMDBORN4 == 2] <- 'Abroad'
data$DMDBORN4[data$DMDBORN4 == 77] <- 'Refused'
data$DMDBORN4[data$DMDBORN4 == 99] <- "Dont know"
colnames(data)[colnames(data) == "DMDBORN4"] <- "Birthplace"



# Column select
y<- import("LODs.xlsx")[2]
data <-data[c("Cycle","Sex","Age (years)",
              "Ethnicity","Birthplace",y$ID)]
rm(y)

export(data,"NHANES Variables Data.xlsx")

# Name Chemicals

# PFOA, PFOS, combining years and isomers.
AS <- data[names(data) %in% 
               c("LBXNFOS","LBXMFOS","LBXNFOA",
                 "LBXBFOA","LBXPFOS","LBXPFOA")]
AS[is.na(AS)] <- 0

AS$PFOA <- AS$LBXPFOA+AS$LBXBFOA+AS$LBXNFOA
AS$PFOS <- AS$LBXPFOA+AS$LBXNFOS+AS$LBXPFOS

AS[AS == 0] <- NA

data <- data[!names(data) %in% 
               c("LBXNFOS","LBXMFOS","LBXNFOA",
                 "LBXBFOA","LBXPFOS","LBXPFOA")]
data$PFOA<- AS$PFOA
data$PFOS<- AS$PFOS
rm(AS)

# PFDA
colnames(data)[colnames(data) == "LBXPFDE"] <- "PFDA"
# PFHxS
colnames(data)[colnames(data) == "LBXPFHS"] <- "PFHxS"
# PFNA
colnames(data)[colnames(data) == "LBXPFNA"] <- "PFNA"
# Me-PFOSA-AcOH
colnames(data)[colnames(data) == "LBXMPAH"] <- "Me-PFOSA-AcOH"
# PFUA
colnames(data)[colnames(data) == "LBXPFUA"] <- "PFUA"
# PFDoA
colnames(data)[colnames(data) == "LBXPFDO"] <- "PFDoA"
# PFBS
colnames(data)[colnames(data) == "LBXPFBS"] <- "PFBS"
# PFHP
colnames(data)[colnames(data) == "LBXPFHP"] <- "PFHP"
# PFSA
colnames(data)[colnames(data) == "LBXPFSA"] <- "PFSA"
# 2-(N-Ethyl-perfluorooctane sulfonamido) acetic acid
colnames(data)[colnames(data) == "LBXEPAH"] <- "Et-PFOSA-AcOH"

export(data,"Alex Variables Data.xlsx")

# Summary Statistics
data<- split(data,data$Cycle)


sumpfas <- function(x){
mysumstats<- function(z){
m <- mean(z,na.rm = TRUE)
y <- c(m,quantile(z,c(.1,.5,.75,.95,1),na.rm = TRUE))
y<-data.frame(t(y))
colnames(y)<-c("Mean","10th%","Median","75th%","95th","Max")
y$Mean<- signif(y$Mean,3)
return(y)
}

mysummary <- t(sapply(x[6:ncol(x)],mysumstats))

done <- cbind(rownames(mysummary,),x$Cycle[1:nrow(mysummary)],mysummary)
colnames(done)[1:2]<- c("Analyte","Cycle")
return(data.frame(done))
}

finished <- rbind.fill(lapply(data,sumpfas))
colnames(finished)[3:ncol(finished)]<-c("Mean","10th%","Median","75th%","95th","Max")
finished<- finished[!is.nan(as.numeric(finished$Mean)),]
rm(data)

export(finished,"PFAS Summary.xlsx")


