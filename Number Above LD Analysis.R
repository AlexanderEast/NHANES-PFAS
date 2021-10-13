# % Below ND

rm(list=ls())
library(stringr)
library('rio')
library(nhanesA)
library(plyr)
PFAS2018   <- nhanes('PFAS_J')
PFAS2016   <- nhanes('PFAS_I')
PFAS2014   <- nhanes('PFAS_H')
PFAS2012   <- nhanes('PFC_G')


PFAS2012 <- PFAS2012[str_detect(colnames(PFAS2012),"LBD")]
PFAS2014 <- PFAS2014[str_detect(colnames(PFAS2014),"LBD")]
PFAS2016 <- PFAS2016[str_detect(colnames(PFAS2016),"LBD")]
PFAS2018 <- PFAS2018[str_detect(colnames(PFAS2018),"LBD")]



countabovelod <- function(x){
  above <- as.numeric(table(x)[1])
  return(above)
}


PFAS2012 <- data.frame(t(sapply(PFAS2012,countabovelod)))
PFAS2014 <- data.frame(t(sapply(PFAS2014,countabovelod)))
PFAS2016 <- data.frame(t(sapply(PFAS2016,countabovelod)))
PFAS2018 <- data.frame(t(sapply(PFAS2018,countabovelod)))


x <- rbind.fill(PFAS2012,PFAS2014,PFAS2016,PFAS2018)
rm(list=setdiff(ls(), "x"))


colnames(x) <- str_sub(colnames(x),4,-2)
colnames(x) <- str_c("LBX",colnames(x))

x<- data.frame(t(x))
colnames(x) <- c("2011-2012","2013-2014","2015-2016","2017-2018")
x$ID<- row.names(x)

# Bind Analyte Titles

y<- import("LODs.xlsx")[1:2]


z <- merge(y,x,"ID", sort = F)
export(z,"Count Above LODs.xlsx")
