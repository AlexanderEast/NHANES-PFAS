# Data vis

# Packages
library(rio)
library(tidyr)
library(ggplot2)
library(dplyr)
library(lemon)
# Import
rm(list =ls())
data <- import("Alex Variables Data.xlsx")

# Tidy
colnames(data)
demo <- data[,1:5]

conc <- data[,6:ncol(data)]
conc <- gather(conc)

demo <- rbind(demo,demo,demo,demo,demo,demo,
             demo,demo,demo,demo,demo,demo)

data<- cbind(demo,conc)
rm(conc,demo)

colnames(data)[colnames(data) == "key"] <- "Analyte"
colnames(data)[colnames(data) == "value"] <- "Concentration"
data$Concentration <-as.numeric(data$Concentration)

ggplot(data, aes(x = Concentration, fill = Analyte)) + 
  geom_density(alpha = 1) + xlim(0,10)+
  facet_wrap(~Analyte,nrow=4,scales = "free")+
  labs(title = "PFAS in NHANES Serum Measurements, 2011-2018",
       x= "Probability",
       y= "Serum concentration (ng/mL)",
       caption = "Plots represent all serum concentrations >10 ng/mL. 
       Values below LOD are included and coded as LOD/sqrt(2). ")

ggplot(data, aes(y=Concentration,x=Cycle, fill = Cycle))+
  geom_boxplot()+
  scale_y_continuous( trans = "log", breaks = c(0.00001,.0001,.001,.01,.1,1,10,100))+
  facet_rep_wrap(~Analyte,repeat.tick.labels = TRUE,nrow=3)+
  labs(title = "NHANES Serum Concentrations for All Individuals, 2011-2018 Cycles",
       x= "Chemical",
       y= "Serum concentration (ng/mL)",
       caption = 
         "Values below LOD are included in this plot and coded as LOD/sqrt(2).")


ggplot(data, aes(y=Concentration,x=Cycle, fill = Sex))+
  geom_boxplot()+
  scale_y_continuous( trans = "log", breaks = c(0.00001,.0001,.001,.01,.1,1,10,100))+
  facet_rep_wrap(~Analyte,repeat.tick.labels = TRUE,nrow=3)+
  labs(title = "NHANES Serum Concentrations by Sex, 2011-2018 Cycles",
       x= "Chemical",
       y= "Serum concentration (ng/mL)",
       caption = 
         "Values below LOD are included in this plot and coded as LOD/sqrt(2).")


ggplot(data, aes(y=Concentration,x=Cycle, fill = Ethnicity))+
  geom_boxplot()+
  scale_y_continuous( trans = "log", breaks = c(0.00001,.0001,.001,.01,.1,1,10,100))+
  facet_rep_wrap(~Analyte,repeat.tick.labels = TRUE,nrow=3)+
  labs(title = "NHANES Serum Concentrations by Ethnicity, 2011-2018 Cycles",
       x= "Chemical",
       y= "Serum concentration (ng/mL)",
       caption = 
         "Values below LOD are included in this plot and coded as LOD/sqrt(2).")



data<- data[data$Birthplace!= 'Dont know',]
data<- data[data$Birthplace!= 'Refused',]

ggplot(data, aes(y=Concentration,x=Cycle, fill = Birthplace))+
  geom_boxplot()+
  scale_y_continuous( trans = "log", breaks = c(0.00001,.0001,.001,.01,.1,1,10,100))+
  facet_rep_wrap(~Analyte,repeat.tick.labels = TRUE,nrow=3)+
  labs(title = "NHANES Serum Concentrations by Birthplace, 2011-2018 Cycles",
       x= "Chemical",
       y= "Serum concentration (ng/mL)",
       caption = 
         "Values below LOD are included in this plot and coded as LOD/sqrt(2).")


ggplot(data, aes(x = Concentration, fill = Sex)) + 
  geom_density(alpha = .5) + xlim(0,10)+
  facet_wrap(~Analyte,nrow=4,scales = "free")+
  labs(title = "PFAS in NHANES Serum Measurements, 2011-2018 by Sex",
       x= "Probability",
       y= "Serum concentration (ng/mL)",
       caption = "Plots represent all serum concentrations >10 ng/mL. 
       Values below LOD are included and coded as LOD/sqrt(2). ")
