#### Load packages ####
library(dplyr)
library(ranger)
library(tidyverse)
library(stringr)
library(forcats)
library(writexl)
library(readxl)
library(faircause)
library(fairadapt)
library(ggplot2)
library(haven)
library(psych)
library(factoextra)
library(formattable)
library(reshape2)

#### To include in model based on ANES own aggregated variables  ####

## Create outcome variable
# "V201022" - PRE: ALREADY VOTED IN GENERAL ELECTION
# "V201023" - PRE: CONFIRMATION VOTED (EARLY) IN NOVEMBER 3 ELECTION
#	"V201028"	- PRE: Did R vote for President (election through mail)
#	"V201029"	- PRE: For whom did R vote for President
#	"V202066"	- POST: Did R vote in November 2020 election
#	"V202072"	- POST: Did R vote for President
#	"V202073"	- POST: For whom did R vote for President

## Create protected attribute around race
#	"V201546"	- PRE: R: Are you Spanish, Hispanic, or Latino
#	"V201547a"	- PRE: Race of R: White [mention]
#	"V201547b"	- PRE: Race of R: Black or African‐American [mention]
#	"V201547c"	- PRE: Race of R: Asian [mention]
#	"V201547d"	- PRE: Race of R: Native Hawaiian or Pacific Islander [mention]
#	"V201547e"	- PRE: Race of R: Native American or Alaska Native [mention]
#	"V201547z"	- PRE: Race of R: other specify
#	"V201549x"	- PRE: SUMMARY: R self‐identified race/ethnicity

## Create confounders
#	"V201507x"	- PRE: SUMMARY: Respondent age
#	"V201508"	- PRE: Marital status
#	"V201600"	- PRE: What is your (R) sex? [revised] 
#	"V201601"	- PRE: Sexual orientation of R [revised]

## Create mediator variable for religion or create index on religiousness?
# "V201433" - PRE: IS RELIGION IMPORTANT PART OF R LIFE
#	"V201435"	- PRE: What is present religion of R
#	"V201436"	- PRE: If R has no particular religion does R mean atheist or agnostic
#	"V201452"	- PRE: Ever attend church or religious services -- REDUNDANT
#	"V201453"	- PRE: Attend religious services how often
#	"V201454"	- PRE: ATTEND CHURCH MORE OFTEN THAN ONCE A WEEK -- REDUNDANT

## Create Mediators
# "V201511x" - PRE: SUMMARY: Respondent 5 Category level of education
# "V201534x" - PRE: SUMMARY: R occupation status 1 digit
#	"V201617x"	- PRE: SUMMARY: Total (family) income

## Create subindex for Party ID PTYID
# "V201231x" - PRE: SUMMARY: PARTY ID -- COMBINES VARIABLES V201228, V201229, and V201230

## Create index for political engagement

## Create subindex for Engagement - Mobilization MOBILPO
#	"V202013"	- POST: R attend online political meetings, rallies, speeches, fundraisers -- REST OF MOBILPO VARIABLES ARE NOT RELEVANT
#	"V202014"	- POST: R go to any political meetings, rallies, speeches, dinners -- PART OF MOBILPO
#	"V202015"	- POST: R wear campaign button or post sign or bumper sticker -- PART OF MOBILPO
#	"V202016"	- POST: R do any (other) work for party or candidate -- PART OF MOBILPO
#	"V202017"	- POST: R contribute money to individual candidate running for public office -- PART OF MOBILPO
#	"V202019"	- POST: R contribute money to political party during this election year -- PART OF MOBILPO
#	"V202021"	- POST: R contribute to any other group that supported or opposed candidates -- PART OF MOBILPO

## Create subindex for Engagement - Discussion DISCUSS
#	"V202022"	- POST: R ever discuss politics with family or friends -- PART OF DISCUSS
#	"V202024"	- POST: Has R in past 12 months: gotten into a political argument -- PART OF DISCUSS

## Create subindex for Engagement - Involvement - INVOLV
#	"V202025"	- POST: Has R in past 12 months: joined a protest march, rally, or demonstration -- PART OF INVOLV
#	"V202026"	- POST: Has R in past 12 months: sign internet or paper petition -- PART OF INVOLV
#	"V202027"	- POST: Has R in past 12 months: given money to religious organization -- PART OF INVOLV
#	"V202028"	- POST: Has R in past 12 months: given money to other organization -- PART OF INVOLV
#	"V202029"	- POST: Has R in past 12 months: posted comment online about political issue -- PART OF INVOLV
#	"V202030"	- POST: Has R in past 12 months: contacted member of US Senate or House of Rep -- PART OF INVOLV
#	"V202031"	- POST: Has R in past 12 months: worked w/others to deal w/issue facing community -- PART OF INVOLV
#	"V202032"	- POST: Has R in past 12 months: attend mtg about issue facing community/schools -- PART OF INVOLV
#	"V202033"	- POST: Has R in past 12 months: done any volunteer work -- PART OF INVOLV

## Create index for LR attitudes
## Create subindex based on Issues 1
#	"V201200"	- PRE: 7pt scale liberal‐conservative self‐placement
#	"V201246"	- PRE: 7pt scale spending & services: self‐placement
#	"V201252"	- PRE: 7pt scale gov‐private medical insurance scale: self‐placement
#	"V201255"	- PRE: 7pt scale guaranteed job‐income scale: self‐placement
#	"V201258"	- PRE: 7pt scale gov assistance to blacks scale: self‐placement
#	"V201262"	- PRE: 7pt scale environment‐business tradeoff: self‐placement

## Create index for trust in institutions
# "V201233" - PRE: How often trust government in Washington to do what is right [revised]
# "V201234" - PRE: Government run by a few big interests or for benefit of al
# "V201235" - PRE: Does government waste much tax money
# "V201236" - PRE: How many in government are corrupt

## Create index for attitude towards government role
## Create subindex for Services & spending SPSRVPR
# "V201302x" - PRE: SUMMARY: Federal Budget Spending: Social Security
# "V201305x" - PRE: SUMMARY: Federal Budget Spending: public schools
# "V201308x" - PRE: SUMMARY: Federal Budget Spending: Tightening border security
# "V201311x" - PRE: SUMMARY: Federal Budget Spending: dealing with crime
# "V201314x" - PRE: SUMMARY: Federal Budget Spending: welfare programs
# "V201317x" - PRE: SUMMARY: Federal Budget Spending: building and repairing highways
# "V201320x" - PRE: SUMMARY: Federal Budget Spending: aid to the poor
# "V201323x" - PRE: SUMMARY: Federal Budget Spending: protecting the environment

## Create index for Democratic Norms
# "V201366" - PRE: How important that news organizations free to criticize
#	"V201367"	- PRE: How important branches of government keep one another from too much power
#	"V201368"	- PRE: How important elected officials face serious consequences for misconduct
#	"V201369"	- PRE: How important that people agree on basic facts
# "V201372x" -  PRE: SUMMARY: Helpful/harmful if Pres didn't have to worry about congress/courts
# "V201375x" - PRE: SUMMARY: Favor or oppose restricting journalist access
# "V201376" - PRE: How concerned government might undermine media
# "V201377" - PRE: How much trust in news media
# "V201378" - PRE: Appropriate/inappropriate Pres ask foreign countries to investigate rivals

## Create index for Engagement in campaigns
# "V201005" -  PRE: How often does R pay attention to politics and elections
# "V201006" - PRE: How interested in following campaigns

## Create index for closeness of election
# "V201218" - PRE: WILL PRESIDENTIAL RACE BE CLOSE OR WILL (WINNER) WIN BY A LOT
# "V201220" - PRE: WILL PRESIDENTIAL RACE BE CLOSE IN STATE

setwd("~/Documents/PhD at Data Analytics Lab/06. Articles/03. Causal Fairness Explainability/02. Data")
anes.data <- read.csv("anes_timeseries_2020_csv_20220210.csv",quote = "",sep=",") 

voter.turnout.data <- anes.data %>% select(c("V201022","V201023","V202066","V201028","V202072",
                                             "V201029","V202073","V202109x","V201549x","V201600",
                                             "V201507x","V201508","V201601","V201433","V201435",
                                             "V202355","V201617x","V201511x","V201534x","V201231x",
                                             "V202013","V202014","V202015","V202016","V202017",
                                             "V202019","V202021","V202022","V202024","V202025",
                                             "V202026","V202028","V202029","V202030","V202027",
                                             "V202031","V202032","V202033","V201200","V201246",
                                             "V201252","V201255","V201258","V201262","V201233",
                                             "V201234","V201235","V201236","V201302x","V201305x",
                                             "V201314x","V201320x","V201323x","V201308x","V201311x",
                                             "V201317x","V201366","V201372x","V201375x","V201376",
                                             "V201377","V201378","V201367","V201368","V201369",
                                             "V201005","V201006","V201218","V201220")) %>% data.frame



str(voter.turnout.data)
#### Set Outcome Y ####

# voter.turnout #
voter.turnout.data$V201022[voter.turnout.data$V201022==2] <- 0
voter.turnout.data$V201023[voter.turnout.data$V201023==2] <- 0
voter.turnout.data$V202066[voter.turnout.data$V202066 %in% c(1,2,3)] <- 0
voter.turnout.data$V202066[voter.turnout.data$V202066==4] <- 1
voter.turnout.data$pre.voted <- ifelse(voter.turnout.data$V201023 == 1,1,ifelse(voter.turnout.data$V201023 == 0,0,voter.turnout.data$V201022))
voter.turnout.data$voted <- ifelse(voter.turnout.data$V202066 == 1,1,ifelse(voter.turnout.data$V202066 == 0,0,voter.turnout.data$pre.voted))
voter.turnout.data$V201022[voter.turnout.data$V201022 %in% c(-9)] <- NA
voter.turnout.data$V201023[voter.turnout.data$V201023 %in% c(-9,-1)] <- NA
voter.turnout.data$V202066[voter.turnout.data$V202066 %in% c(-9,-7,-6,-1)] <- NA
voter.turnout.data$pre.voted[voter.turnout.data$pre.voted %in% c(-9,-7,-6,-1)] <- NA
voter.turnout.data$voted[voter.turnout.data$voted %in% c(-9,-7,-6,-1)] <- NA

voter.turnout.data$V201028[voter.turnout.data$V201028==2] <- 0
voter.turnout.data$V202072[voter.turnout.data$V202072==2] <- 0
voter.turnout.data$pre.voted.president <- ifelse(voter.turnout.data$pre.voted == 0,0,voter.turnout.data$V201028)
voter.turnout.data$voted.president <-  ifelse(voter.turnout.data$voted == 0,0,ifelse(voter.turnout.data$V202072 == 1,1,ifelse(voter.turnout.data$pre.voted == 1,voter.turnout.data$pre.voted.president,voter.turnout.data$V202072)))
voter.turnout.data$V201028[voter.turnout.data$V201028 %in% c(-9,-1)] <- NA
voter.turnout.data$V202072[voter.turnout.data$V202072 %in% c(-9,-7,-6,-1)] <- NA
voter.turnout.data$pre.voted.president[voter.turnout.data$pre.voted.president %in% c(-9,-7,-6,-1)] <- NA
voter.turnout.data$voted.president[voter.turnout.data$voted.president %in% c(-9,-7,-6,-1)] <- NA

voter.turnout.data$V201029[voter.turnout.data$V201029 %in% c(-9,-1)] <- NA
voter.turnout.data$V202073[voter.turnout.data$V202073 %in% c(-9,-8,-7,-6,-1)] <- NA
voter.turnout.data$vote <- ifelse(voter.turnout.data$V201029 %in% c(1,2,3,4,5,12),voter.turnout.data$V201029,ifelse(voter.turnout.data$V202073 %in% c(1,2,3,4,5,7,8,11,12),voter.turnout.data$V202073,NA))

voter.turnout.data$V202109x[voter.turnout.data$V202109x %in% c(-2)] <- NA
voter.turnout.data$voterturnout <- voter.turnout.data$V202109x


#### Set Protected Attributes X #### 

# race #
voter.turnout.data$V201549x[voter.turnout.data$V201549x %in% c(-9,-8)] <- NA
voter.turnout.data[,'V201549x'] <- as.factor(voter.turnout.data[,'V201549x'])
voter.turnout.data$race.binary <- ifelse(voter.turnout.data$V201549x == 1,1,0)
names(voter.turnout.data)[names(voter.turnout.data) == 'V201549x'] <- 'race'
voter.turnout.data[,'race.binary'] <- as.factor(voter.turnout.data[,'race.binary'])

# sex #
voter.turnout.data$V201600[voter.turnout.data$V201600==-9] <- NA
voter.turnout.data[,'V201600'] <- as.factor(ifelse(voter.turnout.data$V201600 == 1,1,0))
names(voter.turnout.data)[names(voter.turnout.data) == 'V201600'] <- 'gender'

#### Set Confounders Z ####

# age #
voter.turnout.data$V201507x[voter.turnout.data$V201507x==-9] <- NA
names(voter.turnout.data)[names(voter.turnout.data) == 'V201507x'] <- 'age'

# marital status #
voter.turnout.data$V201508[voter.turnout.data$V201508 %in% c(-9,-8)] <- NA
voter.turnout.data[,'V201508'] <- as.factor(voter.turnout.data[,'V201508'])
names(voter.turnout.data)[names(voter.turnout.data) == 'V201508'] <- 'marital.status'

# sexual orientation #
voter.turnout.data$V201601[voter.turnout.data$V201601 %in% c(-9,-5)] <- NA
voter.turnout.data[,'V201601'] <- as.factor(voter.turnout.data[,'V201601'])
names(voter.turnout.data)[names(voter.turnout.data) == 'V201601'] <- 'sexual.orientation'

#### Set Mediators W ####

# religiousness #
voter.turnout.data$V201433[voter.turnout.data$V201433 %in% c(-9,-8)] <- NA # 1 to 5, flip it
voter.turnout.data$V201433 <- sapply(voter.turnout.data$V201433, function(x) ifelse(x == 5, 0, ifelse(x == 4, 0.25, ifelse(x == 3, 0.5, ifelse(x == 2, 0.75, ifelse(x == 1, 1, 0))))))
names(voter.turnout.data)[names(voter.turnout.data) == 'V201433'] <- 'religiousness'

# religion #
voter.turnout.data$V201435[voter.turnout.data$V201435 %in% c(-9,-8)] <- NA
voter.turnout.data[,'V201435'] <- as.factor(voter.turnout.data[,'V201435'])
names(voter.turnout.data)[names(voter.turnout.data) == 'V201435'] <- 'religion'

# rural.urban #
voter.turnout.data$V202355[voter.turnout.data$V202355 %in% c(-9,-8,-7,-6,-5)] <- NA
voter.turnout.data[,'V202355'] <- as.factor(voter.turnout.data[,'V202355'])
voter.turnout.data$rural.urban.binary <- ifelse(voter.turnout.data$V202355 == 4,1,0)
names(voter.turnout.data)[names(voter.turnout.data) == 'V202355'] <- 'rural.urban'

#income #
voter.turnout.data$V201617x[voter.turnout.data$V201617x %in% c(-9,-5)] <- NA
voter.turnout.data[,'V201617x'] <- factor(voter.turnout.data[,'V201617x'], levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22), ordered = TRUE)
names(voter.turnout.data)[names(voter.turnout.data) == 'V201617x'] <- 'income'

# education #
voter.turnout.data$V201511x[voter.turnout.data$V201511x %in% c(-9,-8,-2)] <- NA
voter.turnout.data[,'V201511x'] <- factor(voter.turnout.data[,'V201511x'], levels = c(1,2,3,4,5), ordered = TRUE)
names(voter.turnout.data)[names(voter.turnout.data) == 'V201511x'] <- 'education'

# employment / occupational status #
voter.turnout.data$V201534x[voter.turnout.data$V201534x %in% c(-2)] <- NA
voter.turnout.data[,'V201534x'] <- as.factor(voter.turnout.data[,'V201534x'])
names(voter.turnout.data)[names(voter.turnout.data) == 'V201534x'] <- 'employment'

# party identification
voter.turnout.data$V201231x[voter.turnout.data$V201231x %in% c(-9,-8)] <- NA
voter.turnout.data[,'V201231x'] <- as.factor(voter.turnout.data[,'V201231x'])
names(voter.turnout.data)[names(voter.turnout.data) == 'V201231x'] <- 'party.identification'

# political engagement - mobilization
voter.turnout.data$V202013[voter.turnout.data$V202013 %in% c(-7,-6)] <- NA
voter.turnout.data$V202014[voter.turnout.data$V202014 %in% c(-9,-7,-6)] <- NA
voter.turnout.data$V202015[voter.turnout.data$V202015 %in% c(-9,-7,-6)] <- NA
voter.turnout.data$V202016[voter.turnout.data$V202016 %in% c(-7,-6)] <- NA
voter.turnout.data$V202017[voter.turnout.data$V202017 %in% c(-7,-6)] <- NA
voter.turnout.data$V202019[voter.turnout.data$V202019 %in% c(-9,-8,-7,-6)] <- NA
voter.turnout.data$V202021[voter.turnout.data$V202021 %in% c(-9,-8,-7,-6)] <- NA
voter.turnout.data[c("V202013","V202014","V202015","V202016","V202017","V202019","V202021")] <- sapply(voter.turnout.data[c("V202013","V202014","V202015","V202016","V202017","V202019","V202021")], function(x) ifelse(x == 1, 1,0))

# Perform standard PCA 
pca.variables_political.engagement.mobilization <- c("V202013","V202014","V202015","V202016","V202017","V202019","V202021")
pca_political.engagement.mobilization <- voter.turnout.data %>%
  select(all_of(pca.variables_political.engagement.mobilization)) %>%
  drop_na %>%
  scale %>%
  prcomp
pca_political.engagement.mobilization
summary(pca_political.engagement.mobilization)
fviz_eig(pca_political.engagement.mobilization)

# Perform PCA with Varimax rotation
pca_political.engagement.mobilization_varimax <- voter.turnout.data %>%
  select(all_of(pca.variables_political.engagement.mobilization)) %>%
  drop_na %>%
  scale %>%
  principal(nfactors = 2, rotate = "varimax", scores = TRUE)
pca_political.engagement.mobilization_varimax$loadings

voter.turnout.data$political.engagement.mobilization <- voter.turnout.data %>% 
  select(c("V202013","V202014","V202015","V202016","V202017","V202019","V202021")) %>% 
  apply(., 1, function(x) ifelse(sum(is.na(x)) <= 2,mean(x,na.rm = TRUE),NA))

# political engagement - discussion
voter.turnout.data$V202022[voter.turnout.data$V202022 %in% c(-9,-7,-6)] <- NA
voter.turnout.data$V202024[voter.turnout.data$V202024 %in% c(-9,-7,-6)] <- NA
voter.turnout.data$V202022 <- sapply(voter.turnout.data$V202022, function(x) ifelse(x == 1, 1,0))
voter.turnout.data$V202024 <- sapply(voter.turnout.data$V202024, function(x) ifelse(x == 1, 1,0))

# Perform standard PCA 
pca.variables_political.engagement.discussion <- c("V202022","V202024")
pca_political.engagement.discussion <- voter.turnout.data %>%
  select(all_of(pca.variables_political.engagement.discussion)) %>%
  drop_na %>%
  scale %>%
  prcomp
pca_political.engagement.discussion
summary(pca_political.engagement.discussion)
fviz_eig(pca_political.engagement.discussion)

# Perform PCA with Varimax rotation
pca_political.engagement.discussion_varimax <- voter.turnout.data %>%
  select(all_of(pca.variables_political.engagement.discussion)) %>%
  drop_na %>%
  scale %>%
  principal(nfactors = 2, rotate = "varimax", scores = TRUE)
pca_political.engagement.discussion_varimax$loadings

voter.turnout.data$political.engagement.discussion <- voter.turnout.data %>% 
  select(c("V202022","V202024")) %>% 
  apply(., 1, function(x) ifelse(sum(is.na(x)) <= 2,mean(x,na.rm = TRUE),NA))

# political engagement - involvement
voter.turnout.data$V202025[voter.turnout.data$V202025 %in% c(-9,-7,-6)] <- NA
voter.turnout.data$V202026[voter.turnout.data$V202026 %in% c(-9,-7,-6)] <- NA
voter.turnout.data$V202027[voter.turnout.data$V202027 %in% c(-9,-8,-7,-6)] <- NA
voter.turnout.data$V202028[voter.turnout.data$V202028 %in% c(-9,-7,-6)] <- NA
voter.turnout.data$V202029[voter.turnout.data$V202029 %in% c(-9,-8,-7,-6)] <- NA
voter.turnout.data$V202030[voter.turnout.data$V202030 %in% c(-9,-8,-7,-6)] <- NA
voter.turnout.data$V202031[voter.turnout.data$V202031 %in% c(-9,-7,-6)] <- NA
voter.turnout.data$V202032[voter.turnout.data$V202032 %in% c(-9,-7,-6)] <- NA
voter.turnout.data$V202033[voter.turnout.data$V202033 %in% c(-7,-6)] <- NA
voter.turnout.data[c("V202025","V202026","V202027","V202028","V202029","V202030","V202031","V202032","V202033")] <- sapply(voter.turnout.data[c("V202025","V202026","V202027","V202028","V202029","V202030","V202031","V202032","V202033")], function(x) ifelse(x == 1, 1,0))

# Perform standard PCA
pca.variables_political.engagement.involvement <- c("V202025","V202026","V202027","V202028","V202029","V202030","V202031","V202032","V202033")
pca_political.engagement.involvement<- voter.turnout.data %>%
  select(all_of(pca.variables_political.engagement.involvement)) %>%
  drop_na %>%
  scale %>%
  prcomp
pca_political.engagement.involvement
summary(pca_political.engagement.involvement)
fviz_eig(pca_political.engagement.involvement)

# Perform PCA with Varimax rotation
pca_political.engagement.involvement_varimax <- voter.turnout.data %>%
  select(all_of(pca.variables_political.engagement.involvement)) %>%
  drop_na %>%
  scale %>%
  principal(nfactors = 2, rotate = "varimax", scores = TRUE)
pca_political.engagement.involvement_varimax$loadings

voter.turnout.data$political.engagement.involvement <- voter.turnout.data %>% 
  select(c("V202025","V202026","V202027","V202028","V202029","V202030","V202031","V202032","V202033")) %>% 
  apply(., 1, function(x) ifelse(sum(is.na(x)) <= 2,mean(x,na.rm = TRUE),NA))

# Split in 2 variables based on PCA
voter.turnout.data$political.engagement.involvement.political <- voter.turnout.data %>% 
  select(c("V202025","V202026","V202028","V202029","V202030")) %>% 
  apply(., 1, function(x) ifelse(sum(is.na(x)) <= 2,mean(x,na.rm = TRUE),NA))

voter.turnout.data$political.engagement.involvement.community <- voter.turnout.data %>% 
  select(c("V202027","V202031","V202032","V202033")) %>% 
  apply(., 1, function(x) ifelse(sum(is.na(x)) <= 2,mean(x,na.rm = TRUE),NA))

# left-right attitude
voter.turnout.data$V201200[voter.turnout.data$V201200 %in% c(-9,-8,99)] <- NA # 1: liberal; 7: conservative
voter.turnout.data$V201246[voter.turnout.data$V201246 %in% c(-9,99)] <- NA #1: liberal; 7: conservative
voter.turnout.data$V201252[voter.turnout.data$V201252 %in% c(-9,-8,99)] <- NA # 1: liberal; 7: conservative
voter.turnout.data$V201255[voter.turnout.data$V201255 %in% c(-9,-8,99)] <- NA # 1: liberal; 7: conservative
voter.turnout.data$V201258[voter.turnout.data$V201258 %in% c(-9,-8,99)] <- NA # 1: liberal; 7: conservative
voter.turnout.data$V201262[voter.turnout.data$V201262 %in% c(-9,-8,99)] <- NA # 1: liberal; 7: conservative

# Perform standard PCA
pca.variables_LR.attitudes <- c("V201200","V201246","V201252","V201255","V201258","V201262")
pca_LR.attitudes<- voter.turnout.data %>%
  select(all_of(pca.variables_LR.attitudes)) %>%
  drop_na %>%
  scale %>%
  prcomp
pca_LR.attitudes
summary(pca_LR.attitudes)
fviz_eig(pca_LR.attitudes)

# Perform PCA with Varimax rotation
pca_LR.attitudes_varimax <- voter.turnout.data %>%
  select(all_of(pca.variables_LR.attitudes)) %>%
  drop_na %>%
  scale %>%
  principal(nfactors = 1, rotate = "varimax", scores = TRUE)
pca_LR.attitudes_varimax$loadings

voter.turnout.data$LR.attitudes <- voter.turnout.data %>% 
  select(c("V201200","V201246","V201252","V201255","V201258","V201262")) %>% 
  apply(., 1, function(x) ifelse(sum(is.na(x)) <= 2,mean(x,na.rm = TRUE),NA))

# trust in politics and institutions
voter.turnout.data$V201233[voter.turnout.data$V201233 %in% c(-9,-8)] <- NA # 1: trust, 5: no trust
voter.turnout.data$V201234[voter.turnout.data$V201234 %in% c(-9,-8)] <- NA # 1: no trust 2:trust
voter.turnout.data$V201235[voter.turnout.data$V201235 %in% c(-9,-8)] <- NA # 1: no trust 3: trust
voter.turnout.data$V201236[voter.turnout.data$V201236 %in% c(-9,-8)] <- NA # 1: no trust 5:trust
voter.turnout.data$V201233 <- sapply(voter.turnout.data$V201233, function(x) ifelse(x == 5, 0, ifelse(x == 4, 0.25, ifelse(x == 3, 0.5, ifelse(x == 2, 0.75, ifelse(x == 1, 1, 0))))))
voter.turnout.data$V201234 <- sapply(voter.turnout.data$V201234, function(x) ifelse(x == 1, 0, 1))
voter.turnout.data$V201235 <- sapply(voter.turnout.data$V201235, function(x) ifelse(x == 3, 1, ifelse(x == 2, 0.5, ifelse(x == 1, 0, 0))))
voter.turnout.data$V201236 <- sapply(voter.turnout.data$V201236, function(x) ifelse(x == 5, 1, ifelse(x == 4, 0.75, ifelse(x == 3, 0.5, ifelse(x == 2, 0.25, ifelse(x == 1, 0, 0))))))

# Perform standard PCA
pca.variables_trust.politics <- c("V201233","V201234","V201235","V201236")
pca_trust.politics <- voter.turnout.data %>%
  select(all_of(pca.variables_trust.politics)) %>%
  drop_na %>%
  scale %>%
  prcomp
pca_trust.politics
summary(pca_trust.politics)
fviz_eig(pca_trust.politics)

# Perform PCA with Varimax rotation
pca_trust.politics_varimax <- voter.turnout.data %>%
  select(all_of(pca.variables_trust.politics)) %>%
  drop_na %>%
  scale %>%
  principal(nfactors = 1, rotate = "varimax", scores = TRUE)
pca_trust.politics_varimax$loadings

voter.turnout.data$trust.politics <- voter.turnout.data %>% 
  select(c("V201233","V201234","V201235","V201236")) %>% 
  apply(., 1, function(x) ifelse(sum(is.na(x)) <= 2,mean(x,na.rm = TRUE),NA))

# attitude towards government role & regulation
voter.turnout.data$V201302x[voter.turnout.data$V201302x %in% c(-2)] <- NA
voter.turnout.data$V201305x[voter.turnout.data$V201305x %in% c(-2)] <- NA
voter.turnout.data$V201308x[voter.turnout.data$V201308x %in% c(-2)] <- NA
voter.turnout.data$V201311x[voter.turnout.data$V201311x %in% c(-2)] <- NA
voter.turnout.data$V201314x[voter.turnout.data$V201314x %in% c(-2)] <- NA 
voter.turnout.data$V201317x[voter.turnout.data$V201317x %in% c(-2)] <- NA
voter.turnout.data$V201320x[voter.turnout.data$V201320x %in% c(-2)] <- NA
voter.turnout.data$V201323x[voter.turnout.data$V201323x %in% c(-2)] <- NA
voter.turnout.data[c("V201302x","V201305x","V201308x","V201311x","V201314x","V201317x","V201320x","V201323x")] <- 6 -voter.turnout.data[c("V201302x","V201305x","V201308x","V201311x","V201314x","V201317x","V201320x","V201323x")]

# Perform standard PCA
pca.variables_government.role <- c("V201302x","V201305x","V201308x","V201311x","V201314x","V201317x","V201320x","V201323x")
pca_government.role <- voter.turnout.data %>%
  select(all_of(pca.variables_government.role)) %>%
  drop_na %>%
  scale %>%
  prcomp
pca_government.role
summary(pca_government.role)
fviz_eig(pca_government.role)

# Perform PCA with Varimax rotation
pca_government.role_varimax <- voter.turnout.data %>%
  select(all_of(pca.variables_government.role)) %>%
  drop_na %>%
  scale %>%
  principal(nfactors = 2, rotate = "varimax", scores = TRUE)
pca_government.role_varimax$loadings

voter.turnout.data$government.role <- voter.turnout.data %>% 
  select(c("V201302x","V201305x","V201308x","V201311x","V201314x","V201317x","V201320x","V201323x")) %>% 
  apply(., 1, function(x) ifelse(sum(is.na(x)) <= 2,mean(x,na.rm = TRUE),NA))

# Splitting based on PCA
voter.turnout.data$government.role.social <- voter.turnout.data %>% 
  select(c("V201302x","V201305x","V201314x","V201320x","V201323x")) %>% 
  apply(., 1, function(x) ifelse(sum(is.na(x)) <= 2,mean(x,na.rm = TRUE),NA))

voter.turnout.data$government.role.security <- voter.turnout.data %>% 
  select(c("V201308x","V201311x","V201317x",)) %>% 
  apply(., 1, function(x) ifelse(sum(is.na(x)) <= 2,mean(x,na.rm = TRUE),NA))

## Create index for Democratic Norms
voter.turnout.data$V201366[voter.turnout.data$V201366 %in% c(-9,-8)] <- NA # 1: not free ; 5: free
voter.turnout.data$V201367[voter.turnout.data$V201367 %in% c(-9,-8)] <- NA # 1: not free ; 5: free
voter.turnout.data$V201368[voter.turnout.data$V201368 %in% c(-9,-8)] <- NA # 1: not free ; 5: free
voter.turnout.data$V201369[voter.turnout.data$V201369 %in% c(-9,-8)] <- NA # 1: not free ; 5: free
voter.turnout.data$V201372x[voter.turnout.data$V201372x %in% c(-2)] <- NA  # 1: not free ; 7: free
voter.turnout.data$V201375x[voter.turnout.data$V201375x %in% c(-2)] <- NA  # 1: not free ; 7: free
voter.turnout.data$V201376[voter.turnout.data$V201376 %in% c(-9,-8)] <- NA # 1: not free ; 5: free
voter.turnout.data$V201377[voter.turnout.data$V201377 %in% c(-9,-8)] <- NA # 1: not free ; 5: free
voter.turnout.data$V201378[voter.turnout.data$V201378 %in% c(-9,-8)] <- NA # 1: not free ; 5: free
voter.turnout.data$V201372x <- sapply(voter.turnout.data$V201372x, function(x) ifelse(x == 1, 0.714, ifelse(x == 2, 1.429, ifelse(x == 3, 2.143, ifelse(x == 4, 2.857, ifelse(x == 5, 3.571, ifelse(x == 6, 4.286, 5)))))))
voter.turnout.data$V201375x <- sapply(voter.turnout.data$V201375x, function(x) ifelse(x == 1, 0.714, ifelse(x == 2, 1.429, ifelse(x == 3, 2.143, ifelse(x == 4, 2.857, ifelse(x == 5, 3.571, ifelse(x == 6, 4.286, 5)))))))

pca.variables_democratic.norms <- c("V201366","V201367","V201368","V201369","V201372x","V201375x","V201376","V201377","V201378")
pca_democratic.norms <- voter.turnout.data %>%
  select(all_of(pca.variables_democratic.norms)) %>%
  drop_na %>%
  scale %>%
  prcomp
pca_democratic.norms
summary(pca_democratic.norms)
fviz_eig(pca_democratic.norms)

# Perform PCA with Varimax rotation
pca_democratic.norms_varimax <- voter.turnout.data %>%
  select(all_of(pca.variables_democratic.norms)) %>%
  drop_na %>%
  scale %>%
  principal(nfactors = 2, rotate = "varimax", scores = TRUE)
pca_democratic.norms_varimax$loadings

voter.turnout.data$democratic.norms <- voter.turnout.data %>% 
  select(c("V201366","V201367","V201368","V201369","V201372x","V201375x","V201376","V201377","V201378")) %>% 
  apply(., 1, function(x) ifelse(sum(is.na(x)) <= 2,mean(x,na.rm = TRUE),NA))

# Split based on PCA

voter.turnout.data$democratic.norms.1 <- voter.turnout.data %>% 
  select(c("V201366","V201372x","V201375x","V201376","V201377","V201378")) %>% 
  apply(., 1, function(x) ifelse(sum(is.na(x)) <= 2,mean(x,na.rm = TRUE),NA))

voter.turnout.data$democratic.norms.2 <- voter.turnout.data %>% 
  select(c("V201367","V201368","V201369")) %>% 
  apply(., 1, function(x) ifelse(sum(is.na(x)) <= 2,mean(x,na.rm = TRUE),NA))

# engagement in campaigns
voter.turnout.data$V201005[voter.turnout.data$V201005 %in% c(-9)] <- NA
voter.turnout.data$V201006[voter.turnout.data$V201006 %in% c(-9)] <- NA
voter.turnout.data$V201005 <- 6 - voter.turnout.data$V201005
voter.turnout.data$V201006 <- 4 - voter.turnout.data$V201006
voter.turnout.data$V201006 <- sapply(voter.turnout.data$V201006, function(x) (0.6*x))

pca.variables_engagement <- c("V201005","V201006")
pca_engagement <- voter.turnout.data %>%
  select(all_of(pca.variables_engagement)) %>%
  drop_na %>%
  prcomp
summary(pca_engagement)

voter.turnout.data$engagement.campaigns <- voter.turnout.data %>% 
  select(c("V201005","V201006")) %>% 
  apply(., 1, function(x) ifelse(sum(is.na(x)) <= 2,mean(x,na.rm = TRUE),NA))

# closeness of election
voter.turnout.data$V201218[voter.turnout.data$V201218 %in% c(-9,-8,-1)] <- NA
voter.turnout.data$V201220[voter.turnout.data$V201220 %in% c(-9,-8,-1)] <- NA
voter.turnout.data[c("V201218","V201220")] <- sapply(voter.turnout.data[c("V201218","V201220")], function(x) ifelse(x == 1, 1,0))

pca.variables_closeness <- c("V201218","V201220")
pca_closeness <- voter.turnout.data %>%
  select(all_of(pca.variables_closeness)) %>%
  drop_na %>%
  prcomp
summary(pca_closeness)

voter.turnout.data$closeness.election <- voter.turnout.data %>% 
  select(c("V201218","V201220")) %>% 
  apply(., 1, function(x) ifelse(sum(is.na(x)) <= 2,mean(x,na.rm = TRUE),NA))

str(voter.turnout.data)
nrow(voter.turnout.data)

write_xlsx(voter.turnout.data,"Voter Turnout data - ANES 2020.xlsx")

#### RUN MODEL - select variables ####

# Subset variables from dataset
selected.variables <- c("voted","voted.president","race","race.binary","gender","age","marital.status","sexual.orientation",
                        "religiousness","religion","rural.urban","rural.urban.binary","income","education","employment","party.identification",
                        "political.engagement.mobilization","political.engagement.discussion","political.engagement.involvement",
                        "political.engagement.involvement.political","political.engagement.involvement.community",
                        "LR.attitudes","trust.politics","government.role","government.role.social","government.role.security",
                        "democratic.norms","democratic.norms.1","democratic.norms.2","engagement.campaigns","closeness.election")

voter.turnout.data.scope <- voter.turnout.data %>% select(all_of(selected.variables))
voter.turnout.data.scope <- voter.turnout.data.scope[complete.cases(voter.turnout.data.scope[,selected.variables]),]
str(voter.turnout.data.scope)
nrow(voter.turnout.data.scope)

#### Calculate total variance ####

TV_gender <- mean(voter.turnout.data.scope$voted[voter.turnout.data.scope$gender == 1],na.rm=TRUE) -
  mean(voter.turnout.data.scope$voted[voter.turnout.data.scope$gender == 0],na.rm=TRUE)
TV_gender

TV_race <- mean(voter.turnout.data.scope$voted[voter.turnout.data.scope$race.binary == 1],na.rm=TRUE)  -
  mean(voter.turnout.data.scope$voted[voter.turnout.data.scope$race.binary == 0],na.rm=TRUE) 
TV_race

#### RUN MODEL - Set X, Y, Z and W with all mediators ####
Y <- "voted" # outcome
X <- "race.binary" # protected attribute
Z <- c("gender","age","marital.status","sexual.orientation") # confounders
W <-  c("education","employment","income","rural.urban.binary","religion","religiousness",
        "political.engagement.involvement.community","political.engagement.involvement.political",
        "political.engagement.mobilization","political.engagement.discussion","LR.attitudes",
        "party.identification","democratic.norms.1","democratic.norms.2","trust.politics",
        "government.role.social","government.role.security","engagement.campaigns","closeness.election") # mediators

set.seed(2024)
tvd.voter.turnout <- fairness_cookbook(data = voter.turnout.data.scope, X = X, W = W, Z = Z, Y = Y, x0 = 0, x1 = 1,method = "medDML")
voter.turnout.meas <- summary(tvd.voter.turnout)$measures
voter.turnout.meas

autoplot(tvd.voter.turnout, decompose = "xspec",dataset = "Voter Turnout Data")

voter.turnout.meas_individual.IE <- data.frame(
  col1 = character(),
  col2 = numeric(),
  col3 = numeric()
)

for (i in 1:length(W)) {
  set.seed(2025)
  W_temp <- W[i]
  tvd.voter.turnout_temp <- fairness_cookbook(data = voter.turnout.data.scope, X = X, W = W_temp, Z = Z, Y = Y, x0 = 0, x1 = 1,method = "medDML")
  voter.turnout.meas_temp <- summary(tvd.voter.turnout_temp)$measures
  voter.turnout.meas_individual.IE <- rbind(voter.turnout.meas_individual.IE,c(W_temp,abs(round(voter.turnout.meas_temp$value[2],4)),round(voter.turnout.meas_temp$sd[2],4)))
}
colnames(voter.turnout.meas_individual.IE) <- c("Mediator","Ctf-IE Value","Ctf-IE SD")

### MEASURES USED FOR TABLE 3###
voter.turnout.meas_individual.IE %>% arrange("Ctf-IE Value",decreasing=T)

#### RUN MODEL - Set X, Y, Z and all significant mediators ####
Y <- "voted" # outcome
X <- "race.binary" # protected attribute
Z <- c("gender","age","marital.status","sexual.orientation") # confounders
W <-  c("education","employment","income","rural.urban.binary","religiousness",
        "political.engagement.involvement.community","political.engagement.involvement.political",
        "political.engagement.mobilization","political.engagement.discussion","LR.attitudes",
        "democratic.norms.2","trust.politics","government.role.social","government.role.security","closeness.election") # mediators

set.seed(2025)
tvd.voter.turnout_significant.W <- fairness_cookbook(data = voter.turnout.data.scope, X = X, W = W, Z = Z, Y = Y, x0 = 0, x1 = 1,method = "medDML")
voter.turnout.meas_significant.W <- summary(tvd.voter.turnout_significant.W)$measures 

### MEASURES USED FOR TABLE 4###
voter.turnout.meas_significant.W
write_xlsx(voter.turnout.meas_significant.W,"voter.turnout.meas significant.W.xlsx")

### PLOT USED FOR FIGURE 4###
autoplot(tvd.voter.turnout_significant.W, decompose = "xspec",dataset = "Voter Turnout Data")



#### RUN MODEL - Set X, Y, Z with build up of all significant mediators ####
Y <- "voted" # outcome
X <- "race.binary" # protected attribute
Z <- c("gender","age","marital.status","sexual.orientation") # confounders
W <-  c("education","employment","income","rural.urban.binary","religiousness",#"religion",
        "political.engagement.involvement.community","political.engagement.involvement.political",
        "political.engagement.mobilization","political.engagement.discussion","LR.attitudes",
        "democratic.norms.2","trust.politics","government.role.social","government.role.security","closeness.election") # mediators
W.label <- c("Education","Employment","Income","Rural/urban","Religiousness","Political engagement - community involvement",
             "Political engagement - political involvement","Political engagement - mobilization","Political engagement - discussion",
             "Left-right attitudes","Democratic norms","Trust in politics","Government role - social","Government role - security",
             "Closeness of election")

voter.turnout.meas_split.significant.W <- as.data.frame(voter.turnout.meas_significant.W)
tvd.voter.turnout.significant.W <- as.data.frame(tvd.voter.turnout_significant.W$measures)
voter.turnout.meas_split.significant.W$mediator <- "All"
tvd.voter.turnout.significant.W$mediator <- "All"


for (i in 1:length(W)) {
  set.seed(2025)
  W_temp <- W[1:i]
  tvd.voter.turnout_temp <- fairness_cookbook(data = voter.turnout.data.scope, X = X, W = W_temp, Z = Z, Y = Y, x0 = 0, x1 = 1,method = "medDML")
  voter.turnout.meas_temp <- summary(tvd.voter.turnout_temp)$measures
  tvd.voter.turnout_boot <- tvd.voter.turnout_temp$measures
  voter.turnout.meas_temp$mediator <- W.label[i]
  tvd.voter.turnout_boot$mediator <- W.label[i]
  voter.turnout.meas_split.significant.W <- rbind(voter.turnout.meas_split.significant.W,voter.turnout.meas_temp)
  tvd.voter.turnout.significant.W <- rbind(tvd.voter.turnout.significant.W,tvd.voter.turnout_boot)
  print(autoplot(tvd.voter.turnout_temp, decompose = "xspec",dataset = W[i]))
}




voter.turnout.meas_split.significant.W$mediator <- factor(voter.turnout.meas_split.significant.W$mediator, levels=c("Education","Employment","Income","Rural/urban","Religiousness","Political engagement - community involvement",
                                                                                                                    "Political engagement - political involvement","Political engagement - mobilization","Political engagement - discussion",
                                                                                                                    "Left-right attitudes","Democratic norms","Trust in politics","Government role - social","Government role - security",
                                                                                                                    "Closeness of election","All"))
tvd.voter.turnout.significant.W$mediator <- factor(voter.turnout.meas_split.significant.W$mediator, levels=c("Education","Employment","Income","Rural/urban","Religiousness","Political engagement - community involvement",
                                                                                                             "Political engagement - political involvement","Political engagement - mobilization","Political engagement - discussion",
                                                                                                             "Left-right attitudes","Democratic norms","Trust in politics","Government role - social","Government role - security",
                                                                                                             "Closeness of election","All"))
tvd.voter.turnout.bootstrap <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(tvd.voter.turnout.bootstrap) <- c("Mediator", "Measure", "Value", "SD")

tvd.voter.turnout.bootstrap[1,"Mediator"] <- W.label[1]
tvd.voter.turnout.bootstrap[1,"Measure"] <- "ctfie"
tvd.voter.turnout.bootstrap[1,"Value"] <- mean(tvd.voter.turnout.significant.W[tvd.voter.turnout.significant.W$mediator == "Education" & tvd.voter.turnout.significant.W$measure == "ctfie",]$value)


for (i in 2:(length( W.label))) {
  tvd.voter.turnout.bootstrap[i,"Mediator"] <- W.label[i]
  tvd.voter.turnout.bootstrap[i,"Measure"] <- "ctfie"
  tvd.voter.turnout.bootstrap[i,"Value"] <- mean(tvd.voter.turnout.significant.W[tvd.voter.turnout.significant.W$mediator == W.label[i] &
                                                                                   tvd.voter.turnout.significant.W$measure == "ctfie",]$value -
                                                   tvd.voter.turnout.significant.W[tvd.voter.turnout.significant.W$mediator == W.label[i-1] &
                                                                                     tvd.voter.turnout.significant.W$measure == "ctfie",]$value)
}

voter.turnout.meas_split.significant.W <- voter.turnout.meas_split.significant.W %>% filter(mediator != "All")

### PLOT USED FOR FIGURE 5###
# Line plot with error bars
ggplot(voter.turnout.meas_split.significant.W[voter.turnout.meas_split.significant.W$measure %in% c("ctfde","ctfie","ctfse","tv"),], aes(x = mediator, y = abs(value), color = measure, group = measure)) +
  geom_line(data = subset(voter.turnout.meas_split.significant.W, measure %in% c("ctfde", "ctfie")), size = 1.06) +
  geom_line(data = subset(voter.turnout.meas_split.significant.W, measure %in% c("ctfse", "tv")), size = 0.53) +
  geom_point() +  # Points on the lines
  geom_errorbar(aes(ymin = abs(value) - 2*sd, ymax = abs(value) + 2*sd), width = 0.3) +  # Error bars
  labs( x = "Mediator", y = "Effect Measure") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_color_manual(
    values = c(
      "ctfde" = "#7CAE00",
      "ctfie" = "#3FBFC4",
      "ctfse" = "#C87CFF",
      "tv"    = "#F7766C"
    ),
    labels = c(
      "ctfde" = "Ctf-DE",
      "ctfie" = "Ctf-IE",
      "ctfse" = "Ctf-SE",
      "tv"    = "TV"
    )
  ) +
  theme_minimal()


# Stacked barplot 
ggplot(
  voter.turnout.meas_split.significant.W[
    voter.turnout.meas_split.significant.W$measure %in% c("ctfde", "ctfie", "ctfse"),
  ],
  aes(x = mediator, y = abs(value), fill = measure)
) +
  geom_bar(stat = "identity", position = "stack") +  # Stacked bars
  scale_fill_manual(
    values = c(
      "ctfde" = "#7CAE00",
      "ctfie" = "#3FBFC4",
      "ctfse" = "#C87CFF"
    )
  ) +
  labs(x = "Mediator", y = "Effect Measure") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  theme_minimal()

write_xlsx(voter.turnout.meas_split.significant.W,"voter.turnout.meas split significant.W.xlsx")

#### Distribution of confounders in function of race ####

### PLOT USED FOR FIGURE 6A###
# Age vs race
ggplot(voter.turnout.data.scope, aes(x = age, fill = race.binary)) +
  geom_density(alpha = 0.4) + theme_bw() +
  theme(
    legend.position = c(0.8, 0.8),
    legend.box.background = element_rect()
  ) + 
  labs(fill = "Race/ethnicity") +
  scale_fill_manual(values = c("0" =  "#F7766C", "1" ="#3FBFC4"),
                    labels = c("Non-white", "White")) 

### PLOT USED FOR FIGURE 6B###
# Test age independent of race. Hypothesis is rejected (-values ). However, possible confounders of this relationship are not measured in the corresponding dataset.
wilcox.test(voter.turnout.data.scope[voter.turnout.data.scope$race.binary == 1, ]$age, 
            voter.turnout.data.scope[voter.turnout.data.scope$race.binary == 0, ]$age)

# gender vs race
ggplot(voter.turnout.data.scope, aes(x = gender, fill = race.binary)) +
  geom_bar(position = "fill") +
  geom_text(
    aes(label = percent(round(after_stat(count)/tapply(after_stat(count), after_stat(x), sum)[after_stat(x)], 4))),
    stat = "count", position = position_fill(0.5)
  ) +
  scale_y_continuous(labels = percent) + ylab("proportion") +
  labs(fill = "Race/ethnicity") +
  scale_fill_manual(values = c("0" =  "#F7766C", "1" ="#3FBFC4"),
                    labels = c("Non-white", "White")) +
  theme_minimal() 

# Test gender independent of race. Hypothesis is failed to be rejected (-values ).
wilcox.test(as.numeric(voter.turnout.data.scope[voter.turnout.data.scope$race.binary == 1, ]$gender),
            as.numeric(voter.turnout.data.scope[voter.turnout.data.scope$race.binary == 0, ]$gender))

TV_race.gender.1 <- mean(voter.turnout.data.scope$voted[voter.turnout.data.scope$gender == 1 & voter.turnout.data.scope$race.binary == 1],na.rm=TRUE) -
  mean(voter.turnout.data.scope$voted[voter.turnout.data.scope$gender == 1 & voter.turnout.data.scope$race.binary == 0],na.rm=TRUE)
TV_race.gender.1

TV_race.gender.0 <- mean(voter.turnout.data.scope$voted[voter.turnout.data.scope$gender == 0 & voter.turnout.data.scope$race.binary == 1],na.rm=TRUE) -
  mean(voter.turnout.data.scope$voted[voter.turnout.data.scope$gender == 0 & voter.turnout.data.scope$race.binary == 0],na.rm=TRUE)
TV_race.gender.0

fair_pred <- fair_predictions(voter.turnout.data.scope, X, Z, W, Y, x0 = "Non-white", x1 = "White")





#### RUN MODEL - Set X, Y, Z and add education --> significant standalone IE ####
W <- c("education") # mediators

set.seed(2025)
tvd.voter.turnout_education <- fairness_cookbook(data = voter.turnout.data.scope, X = X, W = W, Z = Z, Y = Y, x0 = 0, x1 = 1,method = "medDML")
voter.turnout.meas_education <- summary(tvd.voter.turnout_education)$measures
voter.turnout.meas_education

write_xlsx(voter.turnout.meas_education,"voter.turnout.meas education.xlsx")

autoplot(tvd.voter.turnout_education, decompose = "xspec",dataset = "Voter Turnout Data - add education")

#### RUN MODEL - Set X, Y, Z and add employment --> significant standalone IE ####
W <- c("education","employment") # mediators

set.seed(2025)
tvd.voter.turnout_employment <- fairness_cookbook(data = voter.turnout.data.scope, X = X, W = W, Z = Z, Y = Y, x0 = 0, x1 = 1,method = "medDML")
voter.turnout.meas_employment <- summary(tvd.voter.turnout_employment)$measures
voter.turnout.meas_employment

write_xlsx(voter.turnout.meas_employment,"voter.turnout.meas employment.xlsx")

autoplot(tvd.voter.turnout_employment, decompose = "xspec",dataset = "Voter Turnout Data - add employment")

#### RUN MODEL - Set X, Y, Z and add income --> significant standalone IE ####
W <- c("education","employment","income") # mediators

set.seed(2025)
tvd.voter.turnout_income <- fairness_cookbook(data = voter.turnout.data.scope, X = X, W = W, Z = Z, Y = Y, x0 = 0, x1 = 1,method = "medDML")
voter.turnout.meas_income <- summary(tvd.voter.turnout_income)$measures
voter.turnout.meas_income

write_xlsx(voter.turnout.meas_income,"voter.turnout.meas income.xlsx")

autoplot(tvd.voter.turnout_income, decompose = "xspec",dataset = "Voter Turnout Data - add income")
#### RUN MODEL - Set X, Y, Z and add rural.urban --> significant standalone IE ####
W <- c("education","employment","income","rural.urban.binary") # mediators

set.seed(2025)
tvd.voter.turnout_rural.urban <- fairness_cookbook(data = voter.turnout.data.scope, X = X, W = W, Z = Z, Y = Y, x0 = 0, x1 = 1,method = "medDML")
voter.turnout.meas_rural.urban <- summary(tvd.voter.turnout_rural.urban)$measures
voter.turnout.meas_rural.urban

write_xlsx(voter.turnout.meas_rural.urban,"voter.turnout.meas rural.urban.xlsx")

autoplot(tvd.voter.turnout_rural.urban, decompose = "xspec",dataset = "Voter Turnout Data - add rural.urban")

#### RUN MODEL - Set X, Y, Z and add religion --> significant standalone IE ####
W <- c("education","employment","income","rural.urban.binary","religion") # mediators

set.seed(2025)
tvd.voter.turnout_religion <- fairness_cookbook(data = voter.turnout.data.scope, X = X, W = W, Z = Z, Y = Y, x0 = 0, x1 = 1,method = "medDML")
voter.turnout.meas_religion <- summary(tvd.voter.turnout_religion)$measures
voter.turnout.meas_religion

write_xlsx(voter.turnout.meas_religion,"voter.turnout.meas religion.xlsx")

autoplot(tvd.voter.turnout_religion, decompose = "xspec",dataset = "Voter Turnout Data - add religion")

#### RUN MODEL - Set X, Y, Z and add religiousness --> significant standalone IE ####

W <- c("education","employment","income","rural.urban.binary","religion","religiousness") # mediators

set.seed(2025)
tvd.voter.turnout_religiousness <- fairness_cookbook(data = voter.turnout.data.scope, X = X, W = W, Z = Z, Y = Y, x0 = 0, x1 = 1,method = "medDML")
voter.turnout.meas_religiousness <- summary(tvd.voter.turnout_religiousness)$measures
voter.turnout.meas_religiousness

write_xlsx(voter.turnout.meas_religiousness,"voter.turnout.meas religiousness.xlsx")

autoplot(tvd.voter.turnout_religiousness, decompose = "xspec",dataset = "Voter Turnout Data - religiousness")

#### RUN MODEL - Set X, Y, Z and add political.engagement.involvement.community --> significant standalone IE ####
W <- c("education","employment","income","rural.urban.binary","religion","religiousness",
       "political.engagement.involvement.community") # mediators

set.seed(2025)
tvd.voter.turnout_political.engagement.involvement.community <- fairness_cookbook(data = voter.turnout.data.scope, X = X, W = W, Z = Z, Y = Y, x0 = 0, x1 = 1,method = "medDML")
voter.turnout.meas_political.engagement.involvement.community <- summary(tvd.voter.turnout_political.engagement.involvement.community)$measures
voter.turnout.meas_political.engagement.involvement.community

write_xlsx(voter.turnout.meas_political.engagement.involvement.community,"voter.turnout.meas political.engagement.involvement.community.xlsx")

autoplot(tvd.voter.turnout_political.engagement.involvement.community, decompose = "xspec",dataset = "Voter Turnout Data - add political.engagement.involvement.community")

#### RUN MODEL - Set X, Y, Z and add political.engagement.involvement.political --> significant standalone IE ####
W <- c("education","employment","income","rural.urban.binary","religion","religiousness",
       "political.engagement.involvement.community","political.engagement.involvement.political") # mediators

set.seed(2025)
tvd.voter.turnout_political.engagement.involvement.political <- fairness_cookbook(data = voter.turnout.data.scope, X = X, W = W, Z = Z, Y = Y, x0 = 0, x1 = 1,method = "medDML")
voter.turnout.meas_political.engagement.involvement.political <- summary(tvd.voter.turnout_political.engagement.involvement.political)$measures
voter.turnout.meas_political.engagement.involvement.political

write_xlsx(voter.turnout.meas_political.engagement.involvement.political,"voter.turnout.meas political.engagement.involvement.political.xlsx")

autoplot(tvd.voter.turnout_political.engagement.involvement.political, decompose = "xspec",dataset = "Voter Turnout Data - add political.engagement.involvement.political")

#### RUN MODEL - Set X, Y, Z and add political.engagement.mobilization --> significant standalone IE ####
W <- c("education","employment","income","rural.urban.binary","religion","religiousness",
       "political.engagement.involvement.community","political.engagement.involvement.political",
       "political.engagement.mobilization") # mediators

set.seed(2025)
tvd.voter.turnout_political.engagement.mobilization <- fairness_cookbook(data = voter.turnout.data.scope, X = X, W = W, Z = Z, Y = Y, x0 = 0, x1 = 1,method = "medDML")
voter.turnout.meas_political.engagement.mobilization <- summary(tvd.voter.turnout_political.engagement.mobilization)$measures
voter.turnout.meas_political.engagement.mobilization

write_xlsx(voter.turnout.meas_political.engagement.mobilization,"voter.turnout.meas political.engagement.mobilization.xlsx")

autoplot(tvd.voter.turnout_political.engagement.mobilization, decompose = "xspec",dataset = "Voter Turnout Data - add political.engagement.mobilization")

#### RUN MODEL - Set X, Y, Z and add political.engagement.discussion --> significant standalone IE ####
W <- c("education","employment","income","rural.urban.binary","religion","religiousness",
       "political.engagement.involvement.community","political.engagement.involvement.political",
       "political.engagement.mobilization","political.engagement.discussion") # mediators

set.seed(2025)
tvd.voter.turnout_political.engagement.discussion <- fairness_cookbook(data = voter.turnout.data.scope, X = X, W = W, Z = Z, Y = Y, x0 = 0, x1 = 1,method = "medDML")
voter.turnout.meas_political.engagement.discussion <- summary(tvd.voter.turnout_political.engagement.discussion)$measures
voter.turnout.meas_political.engagement.discussion

write_xlsx(voter.turnout.meas_political.engagement.discussion,"voter.turnout.meas political.engagement.discussion.xlsx")

autoplot(tvd.voter.turnout_political.engagement.discussion, decompose = "xspec",dataset = "Voter Turnout Data - add political.engagement.discussion")

#### RUN MODEL - Set X, Y, Z and add LR.attitudes --> significant standalone IE ####
W <- c("education","employment","income","rural.urban.binary","religion","religiousness",
       "political.engagement.involvement.community","political.engagement.involvement.political",
       "political.engagement.mobilization","political.engagement.discussion","LR.attitudes") # mediators

set.seed(2025)
tvd.voter.turnout_LR.attitudes <- fairness_cookbook(data = voter.turnout.data.scope, X = X, W = W, Z = Z, Y = Y, x0 = 0, x1 = 1,method = "medDML")
voter.turnout.meas_LR.attitudes <- summary(tvd.voter.turnout_LR.attitudes)$measures
voter.turnout.meas_LR.attitudes

write_xlsx(voter.turnout.meas_LR.attitudes,"voter.turnout.meas LR.attitudes.xlsx")

autoplot(tvd.voter.turnout_LR.attitudes, decompose = "xspec",dataset = "Voter Turnout Data - add LR.attitudes")

#### RUN MODEL - Set X, Y, Z and add party.identification --> not significant standalone IE ####
W <- c("education","employment","income","rural.urban.binary","religion","religiousness",
       "political.engagement.involvement.community","political.engagement.involvement.political",
       "political.engagement.mobilization","political.engagement.discussion","LR.attitudes",
       "party.identification") # mediators

set.seed(2025)
tvd.voter.turnout_party.identification <- fairness_cookbook(data = voter.turnout.data.scope, X = X, W = W, Z = Z, Y = Y, x0 = 0, x1 = 1,method = "medDML")
voter.turnout.meas_party.identification <- summary(tvd.voter.turnout_party.identification)$measures
voter.turnout.meas_party.identification

write_xlsx(voter.turnout.meas_party.identification,"voter.turnout.meas party.identification.xlsx")

autoplot(tvd.voter.turnout_party.identification, decompose = "xspec",dataset = "Voter Turnout Data - add party.identification")

#### RUN MODEL - Set X, Y, Z and add democratic.norms.1 (Media) --> not significant standalone IE, but democratic.norms.2 is significant ####
W <- c("education","employment","income","rural.urban.binary","religion","religiousness",
       "political.engagement.involvement.community","political.engagement.involvement.political",
       "political.engagement.mobilization","political.engagement.discussion","LR.attitudes",
       "party.identification","democratic.norms.1") # mediators

set.seed(2025)
tvd.voter.turnout_democratic.norms.1 <- fairness_cookbook(data = voter.turnout.data.scope, X = X, W = W, Z = Z, Y = Y, x0 = 0, x1 = 1,method = "medDML")
voter.turnout.meas_democratic.norms.1 <- summary(tvd.voter.turnout_democratic.norms.1)$measures
voter.turnout.meas_democratic.norms.1

write_xlsx(voter.turnout.meas_democratic.norms.1,"voter.turnout.meas democratic.norms.1.xlsx")

autoplot(tvd.voter.turnout_democratic.norms.1, decompose = "xspec",dataset = "Voter Turnout Data - add democratic.norms.1")

#### RUN MODEL - Set X, Y, Z and add democratic.norms.2 (Government) --> significant standalone IE ####
W <- c("education","employment","income","rural.urban.binary","religion","religiousness",
       "political.engagement.involvement.community","political.engagement.involvement.political",
       "political.engagement.mobilization","political.engagement.discussion","LR.attitudes",
       "party.identification","democratic.norms.1","democratic.norms.2") # mediators

set.seed(2025)
tvd.voter.turnout_democratic.norms.2 <- fairness_cookbook(data = voter.turnout.data.scope, X = X, W = W, Z = Z, Y = Y, x0 = 0, x1 = 1,method = "medDML")
voter.turnout.meas_democratic.norms.2 <- summary(tvd.voter.turnout_democratic.norms.2)$measures
voter.turnout.meas_democratic.norms.2

write_xlsx(voter.turnout.meas_democratic.norms.2,"voter.turnout.meas democratic.norms.2.xlsx")

autoplot(tvd.voter.turnout_democratic.norms.2, decompose = "xspec",dataset = "Voter Turnout Data - add democratic.norms.2")

#### RUN MODEL - Set X, Y, Z and add trust.politics --> significant standalone IE ####
W <- c("education","employment","income","rural.urban.binary","religion","religiousness",
       "political.engagement.involvement.community","political.engagement.involvement.political",
       "political.engagement.mobilization","political.engagement.discussion","LR.attitudes",
       "party.identification","democratic.norms.1","democratic.norms.2","trust.politics") # mediators

set.seed(2025)
tvd.voter.turnout_trust.politics <- fairness_cookbook(data = voter.turnout.data.scope, X = X, W = W, Z = Z, Y = Y, x0 = 0, x1 = 1,method = "medDML")
voter.turnout.meas_trust.politics <- summary(tvd.voter.turnout_trust.politics)$measures
voter.turnout.meas_trust.politics

write_xlsx(voter.turnout.meas_trust.politics,"voter.turnout.meas trust.politics.xlsx")

autoplot(tvd.voter.turnout_trust.politics, decompose = "xspec",dataset = "Voter Turnout Data - add trust.politics")

#### RUN MODEL - Set X, Y, Z and add government.role.social --> significant standalone IE ####
W <-  c(
  "government.role.social") # mediators

set.seed(2025)
tvd.voter.turnout_government.role.social <- fairness_cookbook(data = voter.turnout.data.scope, X = X, W = W, Z = Z, Y = Y, x0 = 0, x1 = 1,method = "medDML")
voter.turnout.meas_government.role.social <- summary(tvd.voter.turnout_government.role.social)$measures
voter.turnout.meas_government.role.social

write_xlsx(voter.turnout.meas_government.role.social,"voter.turnout.meas government.role.social.xlsx")

autoplot(tvd.voter.turnout_government.role.social, decompose = "xspec",dataset = "Voter Turnout Data - add government.role.social")

#### RUN MODEL - Set X, Y, Z and add government.role.security --> significant standalone IE ####
W <-  c("education","employment","income","rural.urban.binary","religion","religiousness",
        "political.engagement.involvement.community","political.engagement.involvement.political",
        "political.engagement.mobilization","political.engagement.discussion","LR.attitudes",
        "party.identification","democratic.norms.1","democratic.norms.2","trust.politics",
        "government.role.social","government.role.security") # mediators

set.seed(2025)
tvd.voter.turnout_government.role.security <- fairness_cookbook(data = voter.turnout.data.scope, X = X, W = W, Z = Z, Y = Y, x0 = 0, x1 = 1,method = "medDML")
voter.turnout.meas_government.role.security <- summary(tvd.voter.turnout_government.role.security)$measures
voter.turnout.meas_government.role.security

write_xlsx(voter.turnout.meas_government.role.security,"voter.turnout.meas government.role.security.xlsx")

autoplot(tvd.voter.turnout_government.role.security, decompose = "xspec",dataset = "Voter Turnout Data - add government.role.security")

#### RUN MODEL - Set X, Y, Z and add engagement.campaigns --> not significant standalone IE ####
W <-  c("education","employment","income","rural.urban.binary","religion","religiousness",
        "political.engagement.involvement.community","political.engagement.involvement.political",
        "political.engagement.mobilization","political.engagement.discussion","LR.attitudes",
        "party.identification","democratic.norms.1","democratic.norms.2","trust.politics",
        "government.role.social","government.role.security","engagement.campaigns") # mediators

set.seed(2025)
tvd.voter.turnout_engagement.campaigns <- fairness_cookbook(data = voter.turnout.data.scope, X = X, W = W, Z = Z, Y = Y, x0 = 0, x1 = 1,method = "medDML")
voter.turnout.meas_engagement.campaigns <- summary(tvd.voter.turnout_engagement.campaigns)$measures
voter.turnout.meas_engagement.campaigns

write_xlsx(voter.turnout.meas_engagement.campaigns,"voter.turnout.meas engagement.campaigns.xlsx")

autoplot(tvd.voter.turnout_engagement.campaigns, decompose = "xspec",dataset = "Voter Turnout Data - add engagement.campaigns")

#### RUN MODEL - Set X, Y, Z and add closeness.election --> significant standalone IE ####
W <-  c("education","employment","income","rural.urban.binary","religion","religiousness",
        "political.engagement.involvement.community","political.engagement.involvement.political",
        "political.engagement.mobilization","political.engagement.discussion","LR.attitudes",
        "party.identification","democratic.norms.1","democratic.norms.2","trust.politics",
        "government.role.social","government.role.security","engagement.campaigns","closeness.election") # mediators

set.seed(2025)
tvd.voter.turnout_closeness.election <- fairness_cookbook(data = voter.turnout.data.scope, X = X, W = W, Z = Z, Y = Y, x0 = 0, x1 = 1,method = "medDML")
voter.turnout.meas_closeness.election <- summary(tvd.voter.turnout_closeness.election)$measures
voter.turnout.meas_closeness.election

write_xlsx(voter.turnout.meas_engagement.campaigns,"voter.turnout.meas engagement.campaigns.xlsx")

autoplot(tvd.voter.turnout_closeness.election, decompose = "xspec",dataset = "Voter Turnout Data - add closeness.election")

#### RUN MODEL - Set X, Y, Z and W with all mediators; compare white with black respondents ####
Y <- "voted" # outcome
X <- "race" # protected attribute
Z <- c("gender","age","marital.status","sexual.orientation") # confounders
W <- c("religiousness","religion","income","education","employment","political.engagement.mobilization","political.engagement.discussion",
       "political.engagement.involvement","LR.attitudes","trust.politics","government.role","closeness.election")  # mediators

set.seed(2025)
tvd.voter.turnout_black <- fairness_cookbook(data = voter.turnout.data.scope[voter.turnout.data.scope$race %in% c(1,2),], X = X, W = W, Z = Z, Y = Y, x0 = 2, x1 = 1,method = "medDML")
voter.turnout.meas_black  <- summary(tvd.voter.turnout_black )$measures
voter.turnout.meas_black 

autoplot(tvd.voter.turnout_black , decompose = "xspec",dataset = "Voter Turnout Data - White vs Black")

#### RUN MODEL - Set X, Y, Z and W with all mediators; compare white with hispanic respondents ####
Y <- "voted" # outcome
X <- "race" # protected attribute
Z <- c("gender","age","marital.status","sexual.orientation") # confounders
W <- c("religiousness","religion","income","education","employment","political.engagement.mobilization","political.engagement.discussion",
       "political.engagement.involvement","LR.attitudes","trust.politics","government.role","closeness.election") # mediators

set.seed(2025)
tvd.voter.turnout_hispanic <- fairness_cookbook(data = voter.turnout.data.scope[voter.turnout.data.scope$race %in% c(1,3),], X = X, W = W, Z = Z, Y = Y, x0 = 3, x1 = 1,method = "medDML")
voter.turnout.meas_hispanic  <- summary(tvd.voter.turnout_hispanic)$measures
voter.turnout.meas_hispanic 

autoplot(tvd.voter.turnout_hispanic , decompose = "xspec",dataset = "Voter Turnout Data - White vs Hispanic")

#### RUN MODEL - Set X, Y, Z and W with all mediators; compare white with asian respondents ####
Y <- "voted" # outcome
X <- "race" # protected attribute
Z <- c("gender","age","marital.status","sexual.orientation") # confounders
W <- c("religiousness","religion","income","education","employment","political.engagement.mobilization","political.engagement.discussion",
       "political.engagement.involvement","LR.attitudes","trust.politics","government.role","closeness.election") # mediators

set.seed(2025)
tvd.voter.turnout_asian <- fairness_cookbook(data = voter.turnout.data.scope[voter.turnout.data.scope$race %in% c(1,4),], X = X, W = W, Z = Z, Y = Y, x0 = 4, x1 = 1,method = "medDML")
voter.turnout.meas_asian  <- summary(tvd.voter.turnout_asian)$measures
voter.turnout.meas_asian

autoplot(tvd.voter.turnout_asian , decompose = "xspec",dataset = "Voter Turnout Data - White vs Asian")

#### RUN MODEL - Set X, Y, Z and W with all mediators; compare white with native respondents ####
Y <- "voted" # outcome
X <- "race" # protected attribute
Z <- c("gender","age","marital.status","sexual.orientation") # confounders
W <- c("religiousness","religion","income","education","employment","political.engagement.mobilization","political.engagement.discussion",
       "political.engagement.involvement","LR.attitudes","trust.politics","government.role","closeness.election") # mediators

set.seed(2025)
tvd.voter.turnout_native<- fairness_cookbook(data = voter.turnout.data.scope[voter.turnout.data.scope$race %in% c(1,5),], X = X, W = W, Z = Z, Y = Y, x0 = 5, x1 = 1,method = "medDML")
voter.turnout.meas_native  <- summary(tvd.voter.turnout_native)$measures
voter.turnout.meas_native

autoplot(tvd.voter.turnout_native , decompose = "xspec",dataset = "Voter Turnout Data - White vs Native American")

#### RUN MODEL - Set X, Y, Z and W with all mediators; compare white with respondents with multiple races ####
Y <- "voted" # outcome
X <- "race" # protected attribute
Z <- c("gender","age","marital.status","sexual.orientation") # confounders
W <- c("religiousness","religion","income","education","employment","political.engagement.mobilization","political.engagement.discussion",
       "political.engagement.involvement","LR.attitudes","trust.politics","government.role","closeness.election") # mediators

set.seed(2025)
tvd.voter.turnout_multiple<- fairness_cookbook(data = voter.turnout.data.scope[voter.turnout.data.scope$race %in% c(1,6),], X = X, W = W, Z = Z, Y = Y, x0 = 6, x1 = 1,method = "medDML")
voter.turnout.meas_multiple  <- summary(tvd.voter.turnout_multiple)$measures
voter.turnout.meas_multiple

autoplot(tvd.voter.turnout_multiple , decompose = "xspec",dataset = "Voter Turnout Data - White vs Multiple races")




#### Appendix ####

TV_race_whitevsblack <- mean(voter.turnout.data.scope$voted[voter.turnout.data.scope$race == 1],na.rm=TRUE)  -
  mean(voter.turnout.data.scope$voted[voter.turnout.data.scope$race == 2],na.rm=TRUE) 
TV_race_whitevsblack

TV_race_whitevshispanic<- mean(voter.turnout.data.scope$voted[voter.turnout.data.scope$race == 1],na.rm=TRUE)  -
  mean(voter.turnout.data.scope$voted[voter.turnout.data.scope$race == 3],na.rm=TRUE) 
TV_race_whitevshispanic

TV_race_whitevsasian<- mean(voter.turnout.data.scope$voted[voter.turnout.data.scope$race == 1],na.rm=TRUE)  -
  mean(voter.turnout.data.scope$voted[voter.turnout.data.scope$race == 4],na.rm=TRUE) 
TV_race_whitevsasian

TV_race_whitevsnative<- mean(voter.turnout.data.scope$voted[voter.turnout.data.scope$race == 1],na.rm=TRUE)  -
  mean(voter.turnout.data.scope$voted[voter.turnout.data.scope$race == 5],na.rm=TRUE) 
TV_race_whitevsnative

TV_race_whitevsmultiple<- mean(voter.turnout.data.scope$voted[voter.turnout.data.scope$race == 1],na.rm=TRUE)  -
  mean(voter.turnout.data.scope$voted[voter.turnout.data.scope$race == 6],na.rm=TRUE) 
TV_race_whitevsmultiple