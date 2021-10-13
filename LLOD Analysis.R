# Counts by year of PFAS

# 
library(nhanesA)
library(rio)
library(plyr)
#
rm(list=ls())
data <- import_list("NHANES PFAS 2011-2018 AE.xlsx")
data <- data$`Limits of Detection`

x <- split(data,data$`Item Id`)

tpose <- function(y){
z <- t(data.frame(y$LLOD))
z <- data.frame("Analyte" = y$Analyte[nrow(y)],
  "ID" = unique(y$`Item Id`), z)
colnames(z)[3:length(z)]<- y$Year

return(z)
}

x <- rbind.fill(lapply(x,tpose))
rm(tpose)


# Counts across 4 Cycles
x$Count <- length(x)-rowSums(is.na(x))-2
length(x)

# Clean for export and export
x <- x[,c(1,2,7,5,6,3,4)]
x <- x[order(-x$Count),]
#export(x,"LODs.xlsx")
