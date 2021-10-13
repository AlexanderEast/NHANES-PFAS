# AE
# PFBA, PFBS, PFDA, PFHxA, PFHxS, PFNA, PFOA, PFOS

rm(list=ls())
library(nhanesA)
library(ggplot2)
library(stringr)
library(plyr)
library(tidyr)
library(rio)
# 1. Load 2018 NHANES Data (Published Nov 2020) Data using nhanesa.
PFAS   <- nhanes('PFAS_J')
DEMO   <- nhanes('DEMO_J')       
PFAS2  <- nhanes('SSPFAS_J')       

data <- merge(PFAS,PFAS2, by = 'SEQN')
data <- merge(data,DEMO, by = 'SEQN')
rm(PFAS,PFAS2,DEMO)

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
data$DMDBORN4[data$DMDBORN4 == 77] <- "Dont know"
colnames(data)[colnames(data) == "DMDBORN4"] <- "Birthplace"

# Naming PFAS
# PFDA
colnames(data)[colnames(data) == "LBXPFDE"] <- "PFDA"
# PFHxA
colnames(data)[colnames(data) == "SSPFHA"] <- "PFHxA"
# PFHxS
colnames(data)[colnames(data) == "LBXPFHS"] <- "PFHxS"
# PFNA
colnames(data)[colnames(data) == "LBXPFNA"] <- "PFNA"
# PFOA
colnames(data)[colnames(data) == "LBXNFOA"] <- "PFOA"
colnames(data)[colnames(data) == "LBXBFOA"] <- "PFOA Isomers"
# PFOS
colnames(data)[colnames(data) == "LBXNFOS"] <- "PFOS"
colnames(data)[colnames(data) == "LBXMFOS"] <- "PFOS Isomers"
# PFBA
# Not included in 2018 NHANES release.
# PFBS
# Not included in 2018 NHANES release. 


# Clean Data

# Select columns
data <-data[c("Sex","Age (years)","Ethnicity","Birthplace",
              "PFDA","PFHxA","PFHxS","PFNA","PFOA","PFOA Isomers",
              "PFOS","PFOS Isomers")]

# Change class
ccols  <- c("Sex","Ethnicity","Birthplace")
ncols <- colnames(data)[!colnames(data) %in% ccols]
data[ccols] <- lapply(data[ccols], as.character)
data[ncols] <- lapply(data[ncols], as.numeric)

# Add units
data$Units <- "ng/mL"

# Below LOD
belowlod <- function(x){
  Count <- sum(x < .1)
  Percentage <- str_c(signif(Count/length(x)*100,2),"%")
  LOD <- "0.1 ng/mL"
  Above <- length(x)-Count
  return(data.frame(LOD,Count,Percentage,Above))
}

belowlod <- rbind.fill(lapply(data[ncols[2:9]],belowlod))
belowlod$Chemical<-ncols[2:9]
belowlod<-belowlod[c("Chemical","LOD","Count","Percentage","Above")]
colnames(belowlod)[colnames(belowlod) == "Above"] <- "Points Above LOD"

# Summary Statistics 


x<-data$PFDA

sumstats <- t(sapply(data[ncols[2:9]],quantile,c(0,.10,.5,.75,.95,1)))
mymean   <- sapply(data[ncols[2:9]],mean)
sumstats <- cbind(mymean,sumstats)
colnames(sumstats)<-c("Mean","Min","10th%","Median","75th%","95th","Max")
sumstats



# Plot Data

# Format
plot <- gather(data[ncols[2:9]],key = Chemical, value ="Value")


# Boxplots
ggplot(plot, aes(y=Value,x=Chemical, fill = Chemical))+
  geom_boxplot()+
  scale_y_continuous( trans = "log", breaks = c(0.00001,.0001,.001,.01,.1,1,10,100))+
  labs(title = "2017-2018 NHANES Serum Concentrations among all Individuals",
       x= "Chemical",
       y= "Serum concentration (ng/mL)",
       caption = 
         "Values below 0.1 LOD are entered as 0.07 ng/mL (0.71 for PFHxA) in NHANES and are included in this plot.")


# Probability Density
plot <- plot[!plot$Value < .1,]

ggplot(plot, aes(x = Value, fill = Chemical)) + 
  geom_density(alpha = 1) + xlim(0,10)+
  facet_wrap(~Chemical,nrow=4,scales = "free")+
  labs(title = "Probability Density Functions of each PFAS in NHANES Serum Measurements 2017-2018 ",
       x= "Probability",
       y= "Serum concentration (ng/mL)",
       caption = "Plots represent all serum concentrations >10 ng/mL and above 0.1 ng/mL LOD. 
                  Across all PFAS, 225 measurements exceeded 10 ng/mL and are not shown.")

done <- list(Data = data,Detection = belowlod,Summary = sumstats)
export(done,"NHANES 20172018 6 Chem AE2.xlsx")
