
# rm(list=ls())
## Install required packages
# install.packages("dplyr")
# install.packages("dbplyr")
# install.packages("tidyverse")
# install.packages("survey")
# install.packages("convey")

# Download data for two countries and three years

#countries <- c('UK', 'DE')
#years <- c(2011,2012,2013)

# P file
#silc.data <- silc.data <- tbl(pg, 'pp') %>%
#  filter(pb010 %in% years, 
#         pb020 %in% countries) %>%
#  dplyr::select(pb010, pb020, pb210, pb030, pb040, pb140, pb150, 
#                pe040, pl040, pl060, pl100, 
#                py010g, py020g, px020, px030, px050, 
#                ph010) %>% collect(n = Inf)

#pb010: year
#pb020: country
#pb030: personal_id, 
#pb140: year of birth
#pb150: sex
#pe040: education
#pl040: status in main emp.
#pl060: work hours main job
#pl100: work hours other jobs
#py010g: employee income
#py020g: non cash employee income
#px030: household id
#ph010: general health

# R file
#silc.r <- tbl(pg, 'rr') %>% 
#  filter(rb020 %in% countries,
#         rb010 %in% years) %>%
#  dplyr::select(rb010, rb020, rb030, rb050, rb090, rx010, rx030) %>%
#  collect(n = Inf)

#rb010: year
#rb020: country
#rb030: personal id
#rb050: weight (personal cross-sectional weight)
#rb090: 
#rx010: 
#rx030: household id

# D file
#silc.d <- tbl(pg, 'dd') %>%
#  filter(db020 %in% countries, 
#         db010 %in% years) %>%
#  dplyr::select(db010,db020, db030, db040, db090, db100) %>% collect(n = Inf)

#db010: 
#db020: country
#db030: household_id
#db040: region
#db090: household weight
#db100: degree of urbanisation


# H file
#silc.h <- tbl(pg, 'hh') %>%
#  filter(hb020 %in% countries, 
#         hb010 %in% years) %>%
#  dplyr::select(hb010,hb020, hb030, hh010, hh070, 
#                hy010, hy020, hy030g, hy090g,
#                hx040, hx050, hx060, hx080, hx090) %>% collect(n = Inf)

#hh010: dwelling type
#hh070: total housing cost
#hy010: household gross income
#hy020: disposable household income
#hy030: imputed rent 
#hy090g: income from rental of prporty
#hx090: equivalised disposable household income
#hx080: poverty indicator
#hx050: equivalised household size
#hx040: household size




## Load required packages

library(dplyr)
library(dbplyr)
library(tidyverse)
library(survey)
library(convey)




################## Initial data prep



## Retrieve the dataset
load(file="dataUKDE.RData")
load(file="dataUKDEd.RData")
load(file="dataUKDEh.RData")
load(file="dataUKDEr.RData")



# Create unique IDs taking into account country, year and ID
# Some IDs may be used by several countries or in several years

# for households
silc.data$id_h <- paste(silc.data$pb020, silc.data$pb010, silc.data$px030, sep="")

silc.h$id_h <- paste(silc.h$hb020, silc.h$hb010, silc.h$hb030, sep="")
silc.r$id_h <- paste(silc.r$rb020, silc.r$rb010, silc.r$rx030, sep="")
silc.d$id_h <- paste(silc.d$db020, silc.d$db010, silc.d$db030, sep="")

# for individuals
silc.data$id_p <- paste(silc.data$pb020, silc.data$pb010, silc.data$pb030, sep ="")
silc.r$id_p <- paste(silc.r$rb020, silc.r$rb010, silc.r$rb030, sep ="")

# Merge household-level data 
silc.hh <- dplyr::right_join(silc.d, silc.h, by="id_h")

# Merge individual data
silc.indiv <- dplyr::right_join(silc.data, silc.r, 
                                by = c("id_p" = "id_p", 
                                       "id_h" = "id_h"))

# Merge all data
silc.dataset <- right_join(silc.indiv, silc.hh, 
                        by="id_h")
# "right_join" used due to different numbers of observations for individuals and households


# Store data as data.frame in data directory


data <- as.data.frame(silc.dataset)

# Store data containing only household information
data.hh <- as.data.frame(silc.hh)

# Store data containing only personal information
data.p <- as.data.frame(silc.indiv)

#save(file="SILCData.RData", data)
#save(file="SILCDataHH.RData", data.hh)
#save(file="dataUKDE.RData", silc.d) 
#save(file="dataUKDEd.RData", silc.data) 
#save(file="dataUKDEh.RData", silc.h) 
#save(file="dataUKDEr.RData", silc.r) 




################## Wrangling


# PERSONAL LEVEL



## Setting our survey design

silc.mig.svy <- svydesign(ids = ~ id_p, # ID of the individual 
                         strata = ~db040, nest = TRUE, # region-based strata
                         weights = ~rb050, # personal cross-sectional weights
                         data = data) %>% # our data
  convey_prep()




# Defining migrants



## let's pick people whose country of birth is 
## not the same as end country to be "migrants" (this definition can be enhanced further for better results)

silc.migrantsDE <- subset(silc.mig.svy, pb020 == "DE" & pb210 == "OTH")
silc.migrantsUK <- subset(silc.mig.svy, pb020 == "UK" & pb210 == "OTH")

## Don't forget the natives

silc.nativesDE <- subset(silc.mig.svy, pb020 == "DE" & pb210 == "LOC")
silc.nativesUK <- subset(silc.mig.svy, pb020 == "UK" & pb210 == "LOC")

## And a total

silc.DE <- subset(silc.mig.svy, pb020 == "DE")
silc.UK <- subset(silc.mig.svy, pb020 == "UK")




################### SOME GENERAL METRICS (via convey)


## Let's check At Risk Of Poverty rates for starters

atRiskTbl <- cbind(svyarpr(~hx090, design=silc.UK), #total arpr UK
                svyarpr(~hx090, design=silc.DE), #total arpr DE
                svyarpr(~hx090, design=silc.migrantsDE), # migrant arpr DE
                svyarpr(~hx090, design=silc.migrantsUK), # migrant arpr UK
                svyarpr(~hx090, design=silc.nativesDE), # native arpr DE
                svyarpr(~hx090, design=silc.nativesUK)  # native arpr UK
                
)

barplot(atRiskTbl, col = c("blue", "black"), names.arg=c("Tot UK","Tot DE","Mig DE","Mig UK","Nat DE","Nat UK")) 



## Quintile Share Ratio

qShareRatio <- cbind(svyqsr(~hx090, design=silc.UK, alpha1= .20), #total qShareRatio UK
                   svyqsr(~hx090, design=silc.DE, alpha1= .20), #total qShareRatio DE
                   svyqsr(~hx090, design=silc.migrantsDE, alpha1= .20), # migrant qShareRatio DE
                   svyqsr(~hx090, design=silc.migrantsUK, alpha1= .20), # migrant qShareRatio UK
                   svyqsr(~hx090, design=silc.nativesDE, alpha1= .20), # native qShareRatio DE
                   svyqsr(~hx090, design=silc.nativesUK, alpha1= .20)  # native qShareRatio UK
                   
)

barplot(qShareRatio, col = c("blue", "black"), names.arg=c("Tot UK","Tot DE","Mig DE","Mig UK","Nat DE","Nat UK")) 


## GINIS!


theGini <- cbind(svygini( ~ hx090 , design = silc.UK , na.rm = TRUE ), #total Gini UK
                 svygini( ~ hx090 , design = silc.DE , na.rm = TRUE ), #total Gini DE
                 svygini( ~ hx090 , design = silc.migrantsDE , na.rm = TRUE ), # migrant Gini DE
                 svygini( ~ hx090 , design = silc.migrantsUK , na.rm = TRUE ), # migrant Gini UK
                 svygini( ~ hx090 , design = silc.nativesDE , na.rm = TRUE ), # native Gini DE
                 svygini( ~ hx090 , design = silc.nativesUK , na.rm = TRUE )  # native Gini UK
                     
)
barplot(theGini, col = c("blue", "black"), names.arg=c("Tot UK","Tot DE","Mig DE","Mig UK","Nat DE","Nat UK")) 


theGini

## Atkinson


theAtkinson <- cbind(svyatk( ~ hx090 , subset( silc.UK , hx090 > 0 ) , epsilon = 1.5 ), #total Atkinson UK
                     svyatk( ~ hx090 , subset( silc.DE , hx090 > 0 ) , epsilon = 1.5 ), #total Atkinson DE
                     svyatk( ~ hx090 , subset( silc.migrantsDE , hx090 > 0 ) , epsilon = 1.5 ), # migrant Atkinson DE
                     svyatk( ~ hx090 , subset( silc.migrantsUK , hx090 > 0 ) , epsilon = 1.5 ), # migrant Atkinson UK
                     svyatk( ~ hx090 , subset( silc.nativesDE , hx090 > 0 ) , epsilon = 1.5 ), # native Atkinson DE
                     svyatk( ~ hx090 , subset( silc.nativesUK , hx090 > 0 ) , epsilon = 1.5 )  # native Atkinson UK
                 
)
barplot(theAtkinson, col = c("blue", "black"), names.arg=c("Tot UK","Tot DE","Mig DE","Mig UK","Nat DE","Nat UK")) 

theAtkinson





################# Decomposition (via oaxaca)



## Oaxaca-Blinder Decomposition

#install.packages("oaxaca")

library(oaxaca)


## Get dataframes for year 2012, migrants

df.migrantsDE <- subset(data, pb020 == "DE" & pb210 == "OTH" & pb010 == 2012)
df.migrantsUK <- subset(data, pb020 == "UK" & pb210 == "OTH" & pb010 == 2012)

## Don't forget the natives

df.nativesDE <- subset(data, pb020 == "DE" & pb210 == "LOC" & pb010 == 2012)
df.nativesUK <- subset(data, pb020 == "UK" & pb210 == "LOC" & pb010 == 2012)

## And a total

df.DE <- subset(data, pb020 == "DE" & pb010 == 2012)
df.UK <- subset(data, pb020 == "UK" & pb010 == 2012)


## py010g - gross wage 
## salaries -- gross employee cash and near cash income for wage-earners only
## rx010 - age at the time of interview

df.DE$female <- (as.integer(as.logical(df.DE$pb150 == 2)))
df.DE$dropout <- (as.integer(as.logical(df.DE$pe040 == 1 | df.DE$pe040 == 2)))
df.DE$highschool <- (as.integer(as.logical(df.DE$pe040 == 3)))
df.DE$college <- (as.integer(as.logical(df.DE$pe040 == 5)))
df.DE$migrant <- (as.integer(as.logical(df.DE$pb210 == "OTH")))



## Germany for the whole population (wage earners only)

df.DE.wage.earners <- subset(df.DE, df.DE$py010g > 0)


oaxacaResults <- oaxaca(formula = py010g ~ rx010 + female + dropout + highschool + college | migrant | dropout + highschool + college,
                        data = df.DE.wage.earners, R = 1000)



oaxacaResults$n
oaxacaResults$y
oaxacaResults$threefold$overall
oaxacaResults$twofold$overall



plot(oaxacaResults, components = c("endowments","coefficients"))



## UK for the whole population (wage earners only)


df.UK$female <- (as.integer(as.logical(df.UK$pb150 == 2)))
df.UK$dropout <- (as.integer(as.logical(df.UK$pe040 == 2)))
df.UK$highschool <- (as.integer(as.logical(df.UK$pe040 == 3)))
df.UK$college <- (as.integer(as.logical(df.UK$pe040 == 5)))
## Different definition of a migrant because SILC data is different
df.UK$migrant <- (as.integer(as.logical(df.UK$pb210 == "OTH" | df.UK$pb210 == "EU")))

## Remove NA's
df.UK[is.na(df.UK)] <- 0

## Check mean wages of wage earners UK

df.UK.wage.earners <- subset(df.UK, df.UK$py010g > 0)

  df.UK.migrants2 <- subset(df.UK.wage.earners, (df.UK.wage.earners$pb210 == "EU" | df.UK.wage.earners$pb210 == "OTH"))
  df.UK.natives2 <- subset(df.UK.wage.earners, (df.UK.wage.earners$pb210 == "LOC"))
  
  df.UK.migrants.order <- df.UK.migrants2[order(df.UK.migrants2$py010g), ]
  df.UK.natives.order <- df.UK.natives2[order(df.UK.natives2$py010g), ]
  
  
  
  ### Not sure what's the problem, let's break it down
  
#  df.UK.dropout <- subset(df.UK.wage.earners, (df.UK.wage.earners$pe040 == 2))
# df.UK.highschool <- subset(df.UK.wage.earners, (df.UK.wage.earners$pe040 == 3))
#  df.UK.college <- subset(df.UK.wage.earners, (df.UK.wage.earners$pe040 == 5))
  
#  df.UK.migrant.dropout <- subset(df.UK.dropout, (df.UK.dropout$pb210 == "OTH" | df.UK.dropout$pb210 == "EU"))
#  df.UK.migrant.HS <- subset(df.UK.highschool, (df.UK.highschool$pb210 == "OTH" | df.UK.highschool$pb210 == "EU"))
#  df.UK.migrant.college <- subset(df.UK.college, (df.UK.college$pb210 == "OTH" | df.UK.college$pb210 == "EU"))
  
  
#  df.UK.full.migrants <- rbind(df.UK.migrant.dropout, df.UK.migrant.HS, df.UK.migrant.college)
#  migrantWages <- df.UK.full.migrants[order(df.UK.full.migrants$py010g), ]
  
#  df.UK.natives.dropout <- subset(df.UK.dropout, (df.UK.dropout$pb210 == "LOC"))
#  df.UK.natives.HS <- subset(df.UK.highschool, (df.UK.highschool$pb210 == "LOC"))
#  df.UK.natives.college <- subset(df.UK.college, (df.UK.college$pb210 == "LOC"))
  
#  df.UK.full.natives <- rbind(df.UK.natives.dropout, df.UK.natives.HS, df.UK.natives.college)
#  nativeWages <- df.UK.full.natives[order(df.UK.full.natives$py010g), ]

  
  
#  mean(head(df.UK.full.migrants$py010g, n=100), trim = 0.01)
#  mean(head(df.UK.full.natives$py010g, n=100), trim = 0.01)
  
    
#  mean(df.UK.dropout$py010g, trim = 0.01)
#  mean(df.UK.highschool$py010g, trim = 0.01)
#  mean(df.UK.college$py010g, trim = 0.01)
  
#  mean(df.UK.migrants2$py010g, trim = 0.01)
#  mean(df.UK.natives2$py010g, trim = 0.01)
  
#  mean(df.UK.migrant.dropout$py010g, trim = 0.01)
#  mean(df.UK.migrant.HS$py010g, trim = 0.01)
#  mean(df.UK.migrant.college$py010g, trim = 0.01)

  
#  mean(df.UK.natives.dropout$py010g, trim = 0.01)
#  mean(df.UK.natives.HS$py010g, trim = 0.01)
#  mean(df.UK.natives.college$py010g, trim = 0.01)
  
  
#  install.packages("ineq")
  library(ineq)
  
  nativeUKLC <- Lc(df.UK.natives2$py010g, plot = TRUE)
  
  migrantUKLC <- Lc(df.UK.migrants2$py010g)
  lines(migrantUKLC, col="blue")
  
  
## Same for DE
  
  
  df.DE.migrants2 <- subset(df.DE.wage.earners, (df.DE.wage.earners$pb210 == "EU" | df.DE.wage.earners$pb210 == "OTH"))
  df.DE.natives2 <- subset(df.DE.wage.earners, (df.DE.wage.earners$pb210 == "LOC"))

#  df.DE.dropout <- subset(df.DE.wage.earners, (df.DE.wage.earners$pe040 == 2))
#  df.DE.highschool <- subset(df.DE.wage.earners, (df.DE.wage.earners$pe040 == 3))
#  df.DE.college <- subset(df.DE.wage.earners, (df.DE.wage.earners$pe040 == 5))
  

#  mean(df.DE.dropout$py010g, trim = 0.01)
#  mean(df.DE.highschool$py010g, trim = 0.01)
#  mean(df.DE.college$py010g, trim = 0.01)
  
#  mean(df.DE.migrants2$py010g, trim = 0.01)
#  mean(df.DE.natives2$py010g, trim = 0.01)
 
  migrantDELC <- Lc(df.DE.migrants2$py010g)
  
  nativeDELC <- Lc(df.DE.natives2$py010g)
  lines(migrantDELC, col="black")
  lines(nativeDELC, col="green")

##Inconsistent with public sources...

oaxacaResultsUK <- oaxaca(formula = py010g ~ rx010 + female + dropout + highschool + college | migrant | dropout + highschool + college,
                        data = df.UK.wage.earners, R = 1000)


oaxacaResultsUK$n
oaxacaResultsUK$y
oaxacaResultsUK$threefold$overall
oaxacaResultsUK$twofold$overall



plot(oaxacaResultsUK, components = c("endowments","coefficients"))




## UK within migrants and within natives (wage earners only)


df.migrantsUK$female <- (as.integer(as.logical(df.migrantsUK$pb150 == 2)))
df.migrantsUK$highschool <- (as.integer(as.logical(df.migrantsUK$pe040 == 3)))
df.migrantsUK$college <- (as.integer(as.logical(df.migrantsUK$pe040 == 5)))
df.migrantsUK$dropout <- (as.integer(as.logical(df.migrantsUK$pe040 == 2)))
df.migrantsUK$migrant <- (as.integer(as.logical(df.migrantsUK$pb210 == "OTH")))
df.migrantsUK$wage_earner <- (as.integer(as.logical(df.migrantsUK$py010g > 0)))


df.migrants.UK.wage_earners <- subset(df.migrantsUK, df.migrantsUK$py010g > 0)

df.migrants.UK.wage_earners[is.na(df.migrants.UK.wage_earners)] <- 0

oaxacaResults.migrantsUK <- oaxaca(formula = py010g ~ rx010 + female + highschool + dropout | college ,
                          data = df.migrants.UK.wage_earners, R = 1000)


oaxacaResults.migrantsUK$n
oaxacaResults.migrantsUK$y
oaxacaResults.migrantsUK$threefold$overall
oaxacaResults.migrantsUK$twofold$overall



plot(oaxacaResults.migrantsUK, components = c("endowments","coefficients"))



