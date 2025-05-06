
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
#	"V203402"	- PRE: IWR DESCR: race/ethnicity

## Create confounders
#	"V201507x"	- PRE: SUMMARY: Respondent age
#	"V203401"	- PRE: IWR DESCR: age -- REDUNDANT
#	"V202638"	- POST: IWR OBS: respondent's estimated age -- REDUNDANT
#	"V203411"	- POST: IWR DESCR: age -- REDUNDANT
#	"V201508"	- PRE: Marital status
#	"V201509"	- PRE: Domestic partnership status -- REDUNDANT
#	"V201600"	- PRE: What is your (R) sex? [revised] 
#	"V202637"	- POST: IWR OBS: respondent's gender -- REDUNDANT
#	"V203403"	- PRE: IWR DESCR: gender -- REDUNDANT
#	"V201601"	- PRE: Sexual orientation of R [revised]

## Create confounding variable for religion or create index on religiousness?
# "V201433" - PRE: IS RELIGION IMPORTANT PART OF R LIFE
#	"V201435"	- PRE: What is present religion of R
#	"V201436"	- PRE: If R has no particular religion does R mean atheist or agnostic
#	"V201452"	- PRE: Ever attend church or religious services -- REDUNDANT
#	"V201453"	- PRE: Attend religious services how often
#	"V201454"	- PRE: ATTEND CHURCH MORE OFTEN THAN ONCE A WEEK -- REDUNDANT
#	"V201462"	- PRE: Religious identification -- REDUNDANT

## Create Mediators
#	"V201510"	- PRE: Highest level of Education
# "V201511x" - PRE: SUMMARY: Respondent 5 Category level of education
#	"V203404"	- PRE: IWR DESCR: education -- REDUNDANT?
#	"V201529"	- PRE: Describe R's employment
# "V201534x" - PRE: SUMMARY: R occupation status 1 digit
#	"V201530"	- RESTRICTED: PRE: What kind of business/industry
#	"V201532"	- RESTRICTED: PRE: R's most important activities or duties
#	"V201533x"	- PRE: SUMMARY: R occupation status 2 digit
#	"V201607"	- RESTRICTED: PRE: Total income amount ‐ revised
#	"V201617x"	- PRE: SUMMARY: Total (family) income
#	"V202458"	- RESTRICTED: POST: Total income amount
#	"V202355"	- POST: Does R currently live in a rural or urban area

## Create subindex for Party ID PTYID
# "V201231x" - PRE: SUMMARY: PARTY ID -- COMBINES VARIABLES V201228, V201229, and V201230
#	"V201228"	- PRE: Party ID: Does R think of self as Democrat, Republican, or Independent -- REDUNDANT
#	"V201229"	- PRE: PARTY IDENTIFICATION STRONG - DEMOCRAT REPUBLICAN -- REDUNDANT
#	"V201230"	- PRE: NO PARTY IDENTIFICATION - CLOSER TO DEMOCRATIC PARTY OR REPUBLICAN PARTY -- REDUNDANT
#	"V201232"	- PRE: Party identity importance -- REDUNDANT

## Create index for political engagement
## Create subindex for Election expectations to win PRESWIN
#	"V201221"	- PRE: VERSION 1A Does R consider voting a duty or choice -- REST OF PRESWIN VARIABLES ARE NOT RELEVANT
#	"V201222"	- PRE: VERSION 1B Does R consider voting a choice or duty
#	"V201223"	- PRE: How strongly does R feel that voting is a duty
#	"V201224"	- PRE: How strongly does R feel that voting is a choice
#	"V201225x"	- PRE: SUMMARY: VOTING AS DUTY OR CHOICE -- COMBINES VARIABLES V201221, V201222, V201223, and V201224
## Create subindex for Engagement - Mobilization MOBILPO
#	"V202013"	- POST: R attend online political meetings, rallies, speeches, fundraisers -- REST OF MOBILPO VARIABLES ARE NOT RELEVANT
#	"V202014"	- POST: R go to any political meetings, rallies, speeches, dinners -- PART OF MOBILPO
#	"V202015"	- POST: R wear campaign button or post sign or bumper sticker -- PART OF MOBILPO
#	"V202016"	- POST: R do any (other) work for party or candidate -- PART OF MOBILPO
#	"V202017"	- POST: R contribute money to individual candidate running for public office -- PART OF MOBILPO
#	"V202018"	- RESTRICTED: POST: Party of candidate for whom R contributed money -- PART OF MOBILPO -- REDUNDANT
#	"V202019"	- POST: R contribute money to political party during this election year -- PART OF MOBILPO
#	"V202020"	- RESTRICTED: POST: Party to which R contributed -- PART OF MOBILPO -- REDUNDANT
#	"V202021"	- POST: R contribute to any other group that supported or opposed candidates -- PART OF MOBILPO
## Create subindex for Engagement - Discussion DISCUSS
#	"V202022"	- POST: R ever discuss politics with family or friends -- PART OF DISCUSS
#	"V202023"	- POST: How many days in past week discussed politics with family or friends -- PART OF DISCUSS
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
#	"V202034"	- POST: Has R in past 12 months: contacted federal elected official -- PART OF INVOLV
# "V202036" - POST: Has R in past 12 months: contacted non‐elected official in federal govt -- PART OF INVOLV
# "V202038" - POST: Has R in past 12 months: contacted elected official on state/local level -- PART OF INVOLV
# "V202040" - POST: Has R in past 12 months: contacted non‐elected official in state/local gov -- PART OF INVOLV

## Create index for LR attitudes
## Create subindex based on Issues 1
#	"V201200"	- PRE: 7pt scale liberal‐conservative self‐placement
#	"V201246"	- PRE: 7pt scale spending & services: self‐placement
#	"V201252"	- PRE: 7pt scale gov‐private medical insurance scale: self‐placement
#	"V201255"	- PRE: 7pt scale guaranteed job‐income scale: self‐placement
#	"V201258"	- PRE: 7pt scale gov assistance to blacks scale: self‐placement
#	"V201262"	- PRE: 7pt scale environment‐business tradeoff: self‐placement
#	"V201201"	- PRE: If R had to choose liberal or conservative self‐placemt -- REDUNDANT?

## Create index for barriers to voting
#	"V202119"	- POST: How difficult was it for R to vote
#	"V202121"	- POST: How long was wait time at polling place
#	"V202122"	- POST: How long does it take to get to polling place

## Create index for trust in institutions
# "V201233" - PRE: How often trust government in Washington to do what is right [revised]
# "V201234" - PRE: Government run by a few big interests or for benefit of al
# "V201235" - PRE: Does government waste much tax money
# "V201236" - PRE: How many in government are corrupt

#	"V201351"	- PRE: Votes counted accurately -- REDUNDANT?
#	"V201352"	- PRE: Trust election officials -- REDUNDANT?
#	"V201373"	- PRE: Favor or oppose restricting journalist access -- REDUNDANT?
#	"V201376"	- PRE: How concerned government might undermine media -- REDUNDANT?
#	"V201377"	- PRE: How much trust in news media -- REDUNDANT?
#	"V201379"	- PRE: Prefer government official who compromises or sticks to principles -- REDUNDANT?
#	"V202312"	- POST: Much of what people hear in schools and media are lies by those in power -- REDUNDANT?
#	"V202576"	- POST: GSS: CSES5‐ Q21: Satisfaction with democratic process -- REDUNDANT?
#	"V202577"	- POST: GSS: REV How often trust government in Washington to do what is right -- REDUNDANT?
#	"V202634"	- POST: GSS: How confident is R in medical institutions -- REDUNDANT?
#	"V202635"	- POST: GSS: How confident is R in scientific community -- REDUNDANT?
#	"V202425"	- POST: CSES5‐Q07: How widespread is corruption among politicians in US -- REDUNDANT?
#	"V202427"	- POST: CSES5‐Q09: How good/bad a job has government done in last 4 years -- REDUNDANT?
#	"V201366"	- PRE: How important that news organizations free to criticize -- REDUNDANT?

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
#	"V202256"	- POST: Would it be good for society to have more or less government regulation -- REDUNDANT
#	"V202257"	- POST: Favor or oppose government trying to reduce income inequality -- REDUNDANT
#	"V202274"	- POST: People in rural areas get more or less from government -- REDUNDANT
#	"V202348"	- POST: Should federal government do more or less about opioid drug addiction -- REDUNDANT
#	"V202377"	- POST: Should the minimum wage be raised, kept the same, or lowered -- REDUNDANT
#	"V202378"	- POST: Increase/decrease government spending to help people pay for health care -- REDUNDANT
#	"V202334"	- POST: Favor or oppose increased regulation on greenhouse emissions -- REDUNDANT

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

## Create index for Union Membership
# "V201544" - PRE: Anyone in HH belong to labor union
# "V201545" - PRE: Who in HH belongs to union


setwd("~/Documents/PhD at Data Analytics Lab/06. Articles/03. Causal Fairness Explainability/02. Data")
anes.data <- read.csv("anes_timeseries_2020_csv_20220210.csv",quote = "",sep=",") 
#anes.data <- read_excel('ANES data.xlsx')

voter.turnout.data <- anes.data %>% select(c("V202066","V203402","V201546","V201547a","V201547b","V201547c",
                                             "V201547d","V201547e","V201547z","V201549x","V203403",
                                             "V203401","V203411","V201028","V201029","V201019","V201020","V201021",
                                             "V201100","V201102","V201216","V201223","V201224","V201032","V201033",
                                             "V201200","V201201","V201221","V201222","V201228",
                                             "V201232","V202017","V202018","V202019","V202020",
                                             "V202021","V202022","V202023","V202024","V202025",
                                             "V202026","V202027","V202028","V202029","V202030",
                                             "V202031","V202032","V202034","V202407","V201511x",
                                             "V202442","V202443","V202444","V202119","V201534x",
                                             "V202121","V202122","V202013","V202014","V202015",
                                             "V202016","V201617x","V201510","V203404",
                                             "V201436","V201462","V202355","V201012","V201013a",
                                             "V201529","V201526","V201527","V201528","V201530",
                                             "V201533x","V201532","V201600","V202408","V202441",
                                             "V201601","V201239","V201240","V201241","V201242",
                                             "V201243","V201244","V201246","V201252","V201255",
                                             "V201258","V201262","V201300","V201303","V201306",
                                             "V201312","V201315","V201318","V201321","V201337",
                                             "V201340","V201343","V201351","V201352","V201366",
                                             "V201367","V201368","V201369","V201373","V201376",
                                             "V201377","V201379","V201429","V201435","V201452",
                                             "V201453","V201462","V201507x","V201508","V201509",
                                             "V201517","V201594","V201607","V201620","V202033",
                                             "V201628","V201629a","V201629b","V201629c","V201629d",
                                             "V201629e","V201641","V202072","V202073","V202143",
                                             "V202144","V202156","V202157","V202160","V202256",
                                             "V202257","V202274","V202291","V202292","V202312",
                                             "V202334","V202338","V202348","V202377","V202378",
                                             "V202406","V202425","V202427","V202429","V202439",
                                             "V202452","V202458","V202475","V202477","V202478",
                                             "V202479","V202480","V202481","V202482","V202527",
                                             "V202528","V202529","V202530","V202531","V202532",
                                             "V202533","V202534","V202535","V202536","V202537",
                                             "V202538","V202575","V202576","V202577","V202634",
                                             "V202635","V202637","V202638","V201218","V201220",
                                             "V201233","V201234","V201235","V201236","V201372x",
                                             "V201375x","V201376","V201377","V201378","V201005",
                                             "V201006","V201544","V201545","V201433","V201231x",
                                             "V202036","V202038","V202040","V201302x","V201305x",
                                             "V201308x","V201314x","V201317x","V201320x","V201323x",
                                             "V201311x","V201022","V201023","V202109x")) %>% data.frame

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

# table(voter.turnout.data$voted.president)
# barplot(table(voter.turnout.data$voted.president))


#### Set Protected Attributes X #### 

# race #
voter.turnout.data$V201549x[voter.turnout.data$V201549x %in% c(-9,-8)] <- NA
voter.turnout.data[,'V201549x'] <- as.factor(voter.turnout.data[,'V201549x'])
voter.turnout.data$race.binary <- ifelse(voter.turnout.data$V201549x == 1,1,0)
#voter.turnout.data$race.binary<- ifelse(voter.turnout.data$V201549x == 1,"White","Non-white") #consider race as numerical value
names(voter.turnout.data)[names(voter.turnout.data) == 'V201549x'] <- 'race'
voter.turnout.data[,'race.binary'] <- as.factor(voter.turnout.data[,'race.binary']) #consider race as factor

# sex #
voter.turnout.data$V201600[voter.turnout.data$V201600==-9] <- NA # select this one
voter.turnout.data[,'V201600'] <- as.factor(ifelse(voter.turnout.data$V201600 == 1,1,0))
names(voter.turnout.data)[names(voter.turnout.data) == 'V201600'] <- 'gender'
#voter.turnout.data$V202637[voter.turnout.data$V202637 %in% c(-8,-7,-6,-5,-1)] <- NA
#voter.turnout.data$V203403[voter.turnout.data$V203403 %in% c(-2,-1)] <- NA

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
voter.turnout.data$V201453[voter.turnout.data$V201453 %in% c(-9,-8)] <- NA # 1 to 5, flip it, -1 is inapplicable so this should be 0 value as well
voter.turnout.data$V201433 <- sapply(voter.turnout.data$V201433, function(x) ifelse(x == 5, 0, ifelse(x == 4, 0.25, ifelse(x == 3, 0.5, ifelse(x == 2, 0.75, ifelse(x == 1, 1, 0))))))
voter.turnout.data$V201453 <- sapply(voter.turnout.data$V201453, function(x) ifelse(x == 5, 0, ifelse(x == 4, 0.25, ifelse(x == 3, 0.5, ifelse(x == 2, 0.75, ifelse(x == 1, 1, 0))))))
names(voter.turnout.data)[names(voter.turnout.data) == 'V201433'] <- 'religiousness'

#voter.turnout.data$religiousness <- voter.turnout.data %>% 
#  select(c("V201433","V201453")) %>% 
#  apply(., 1, function(x) ifelse(sum(is.na(x)) <= 2,mean(x,na.rm = TRUE),NA))

# religion #
voter.turnout.data$V201435[voter.turnout.data$V201435 %in% c(-9,-8)] <- NA
voter.turnout.data[,'V201435'] <- as.factor(voter.turnout.data[,'V201435'])
names(voter.turnout.data)[names(voter.turnout.data) == 'V201435'] <- 'religion'
#voter.turnout.data$V201436[voter.turnout.data$V201436 %in% c(-9,-1)] <- NA

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
voter.turnout.data$V202023[voter.turnout.data$V202023 %in% c(-9,-7,-6,-1)] <- NA # drop this variable
voter.turnout.data$V202024[voter.turnout.data$V202024 %in% c(-9,-7,-6)] <- NA
voter.turnout.data$V202022 <- sapply(voter.turnout.data$V202022, function(x) ifelse(x == 1, 1,0))
voter.turnout.data$V202023 <- sapply(voter.turnout.data$V202023, function(x) (0.143*x))
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
#	following are about contacting an official
voter.turnout.data$V202034[voter.turnout.data$V202034 %in% c(-8,-7,-6)] <- NA
voter.turnout.data$V202036[voter.turnout.data$V202036 %in% c(-9,-8,-7,-6)] <- NA
voter.turnout.data$V202038[voter.turnout.data$V202038 %in% c(-9,-8,-7,-6)] <- NA
voter.turnout.data$V202040[voter.turnout.data$V202040 %in% c(-9,-7,-6)] <- NA
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
#pca.variables_democratic.norms <- c("V201366","V201367","V201368","V201369")
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

#### RUN MODEL - Set X, Y, Z and W with all mediators ####
Y <- "voted" # outcome
X <- "race.binary" # protected attribute
Z <- c("gender","age","marital.status","sexual.orientation") # confounders
W <-  c("education","employment","income","rural.urban.binary","religion","religiousness",
        "political.engagement.involvement.community","political.engagement.involvement.political",
        "political.engagement.mobilization","political.engagement.discussion","LR.attitudes",
        "party.identification","democratic.norms.1","democratic.norms.2","trust.politics",
        "government.role.social","government.role.security","engagement.campaigns","closeness.election") # mediators
# W <-  c("religiousness","religion","rural.urban","income","education","employment","party.identification",
#          "political.engagement.mobilization","political.engagement.discussion","political.engagement.involvement",
#          "LR.attitudes","trust.politics","government.role","democratic.norms","engagement.campaigns","closeness.election") # mediators

set.seed(2024)
tvd.voter.turnout <- fairness_cookbook(data = voter.turnout.data.scope, X = X, W = W, Z = Z, Y = Y, x0 = 0, x1 = 1,method = "medDML")
voter.turnout.meas <- summary(tvd.voter.turnout)$measures
voter.turnout.meas

autoplot(tvd.voter.turnout, decompose = "xspec",dataset = "Voter Turnout Data")

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
#### RUN MODEL - Set X, Y, Z and add rural.urban --> not significant standalone IE ####
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

#### RUN MODEL - Set X, Y, Z and add democratic.norms.2 (Government) --> not significant standalone IE, but democratic.norms.2 is significant ####
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

#### RUN MODEL - Set X, Y, Z and all significant mediators ####
Y <- "voted" # outcome
X <- "race.binary" # protected attribute
Z <- c("gender","age","marital.status","sexual.orientation") # confounders
# W <- c("religiousness","religion","income","education","employment","political.engagement.mobilization","political.engagement.discussion",
#        "political.engagement.involvement","LR.attitudes","trust.politics","government.role","closeness.election")  # mediators
W <-  c("education","employment","income","rural.urban.binary","religiousness",#"religion",
        "political.engagement.involvement.community","political.engagement.involvement.political",
        "political.engagement.mobilization","political.engagement.discussion","LR.attitudes",
        "democratic.norms.2","trust.politics","government.role.social","government.role.security","closeness.election") # mediators


# set.seed(2025)
# voter.turnout.data.scope2 <- voter.turnout.data.scope
# voter.turnout.data.scope2$race.binary <- ifelse(voter.turnout.data.scope2$race.binary=="White",1,0)
# voter.turnout.data.scope2 <- data.frame(apply(voter.turnout.data.scope2, 2, function(x) as.numeric(as.character(x))))
# tvd.voter.turnout_significant.W.CF <- fairness_cookbook(data = voter.turnout.data.scope2, X = X, W = W, Z = Z, Y = Y, x0 = 0, x1 = 1,method="causal_forest")
# voter.turnout.meas_significant.W.CF <- summary(tvd.voter.turnout_significant.W.CF)$measures
# voter.turnout.meas_significant.W.CF
# autoplot(tvd.voter.turnout_significant.W.CF, decompose = "xspec",dataset = "Voter Turnout Data - causal forest - all significant W")

set.seed(2023)
tvd.voter.turnout_significant.W <- fairness_cookbook(data = voter.turnout.data.scope, X = X, W = W, Z = Z, Y = Y, x0 = 0, x1 = 1,method = "medDML")
voter.turnout.meas_significant.W <- summary(tvd.voter.turnout_significant.W)$measures 
voter.turnout.meas_significant.W
write_xlsx(voter.turnout.meas_significant.W,"voter.turnout.meas significant.W.xlsx")

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




#### Distribution of confounders in function of race ####

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

# Marital status vs race
ggplot(voter.turnout.data.scope, aes(x = marital.status, fill = race.binary)) +
  geom_bar(position = "fill") +
  geom_text(
    aes(label = percent(round(after_stat(count)/tapply(after_stat(count), after_stat(x), sum)[after_stat(x)], 4))),
    stat = "count", position = position_fill(0.5)
  ) +
  scale_y_continuous(labels = percent) + ylab("proportion") +
  theme_minimal() + ggtitle("Race independent of marital status rejected (p < 0.001)")
# Test marital status independent of race. Hypothesis is rejected (-values ). However, possible confounders of this relationship are not measured in the corresponding dataset.
wilcox.test(as.numeric(voter.turnout.data.scope[voter.turnout.data.scope$race.binary == 1, ]$marital.status),
                       as.numeric(voter.turnout.data.scope[voter.turnout.data.scope$race.binary == 0, ]$marital.status))

# Sexual orientation vs race
ggplot(voter.turnout.data.scope, aes(x = sexual.orientation, fill = race.binary)) +
  geom_bar(position = "fill") +
  geom_text(
    aes(label = percent(round(after_stat(count)/tapply(after_stat(count), after_stat(x), sum)[after_stat(x)], 4))),
    stat = "count", position = position_fill(0.5)
  ) +
  scale_y_continuous(labels = percent) + ylab("proportion") +
  theme_minimal() + ggtitle("Sexual orientation status independent of race rejected (p < 0.001)")

# Income vs race
ggplot(voter.turnout.data.scope, aes(x = as.numeric(income), fill = race.binary)) +
  geom_density(alpha = 0.4) + theme_bw() +
  theme(
    legend.position = c(0.8, 0.8),
    legend.box.background = element_rect()
  ) + ggtitle("income independent of race rejected (p < 0.001)")
# Test Income independent of race. Hypothesis is rejected (-values ). However, possible confounders of this relationship are not measured in the corresponding dataset.
wilcox.test(as.numeric(voter.turnout.data.scope[voter.turnout.data.scope$race.binary == 1, ]$income),
            as.numeric(voter.turnout.data.scope[voter.turnout.data.scope$race.binary == 0, ]$income))

# Education vs race
ggplot(voter.turnout.data.scope, aes(x = education, fill = race.binary)) +
  geom_bar(position = "fill") +
  geom_text(
    aes(label = percent(round(after_stat(count)/tapply(after_stat(count), after_stat(x), sum)[after_stat(x)], 4))),
    stat = "count", position = position_fill(0.5)
  ) +
  scale_y_continuous(labels = percent) + ylab("proportion") +
  theme_minimal() + ggtitle("Education independent of race rejected (p < 0.001)")
# Test Education independent of race. Hypothesis is rejected (-values ). However, possible confounders of this relationship are not measured in the corresponding dataset.
wilcox.test(as.numeric(voter.turnout.data.scope[voter.turnout.data.scope$race.binary == 1, ]$education),
            as.numeric(voter.turnout.data.scope[voter.turnout.data.scope$race.binary == 0, ]$education))

# Employment vs race
ggplot(voter.turnout.data.scope, aes(x = employment, fill = race.binary)) +
  geom_bar(position = "fill") +
  geom_text(
    aes(label = percent(round(after_stat(count)/tapply(after_stat(count), after_stat(x), sum)[after_stat(x)], 4))),
    stat = "count", position = position_fill(0.5)
  ) +
  scale_y_continuous(labels = percent) + ylab("proportion") +
  theme_minimal() + ggtitle("employment independent of race rejected (p < 0.001)")
# Test Employment independent of race. Hypothesis is rejected (-values ). However, possible confounders of this relationship are not measured in the corresponding dataset.
wilcox.test(as.numeric(voter.turnout.data.scope[voter.turnout.data.scope$race.binary == 1, ]$employment),
            as.numeric(voter.turnout.data.scope[voter.turnout.data.scope$race.binary == 0, ]$employment))

# LR.attitudes vs race
ggplot(voter.turnout.data.scope, aes(x = as.numeric(LR.attitudes), fill = race.binary)) +
  geom_density(alpha = 0.4) + theme_bw() +
  theme(
    legend.position = c(0.8, 0.8),
    legend.box.background = element_rect()
  ) + ggtitle("LR attitudes independent of race rejected (p < 0.001)")
# Test LR.attitudes independent of race. Hypothesis is rejected (-values ). However, possible confounders of this relationship are not measured in the corresponding dataset.
wilcox.test(as.numeric(voter.turnout.data.scope[voter.turnout.data.scope$race.binary == 1, ]$LR.attitudes),
            as.numeric(voter.turnout.data.scope[voter.turnout.data.scope$race.binary == 0, ]$LR.attitudes))

# government.role vs race
ggplot(voter.turnout.data.scope, aes(x = as.numeric(government.role), fill = race.binary)) +
  geom_density(alpha = 0.4) + theme_bw() +
  theme(
    legend.position = c(0.8, 0.8),
    legend.box.background = element_rect()
  ) + ggtitle("Trust politics independent of race rejected (p < 0.001)")
# Test Trust politics independent of race. Hypothesis is rejected (-values ). However, possible confounders of this relationship are not measured in the corresponding dataset.
wilcox.test(as.numeric(voter.turnout.data.scope[voter.turnout.data.scope$race.binary == "White", ]$government.role),
            as.numeric(voter.turnout.data.scope[voter.turnout.data.scope$race.binary == "Non-white", ]$government.role))

# Closeness election vs race
ggplot(voter.turnout.data.scope, aes(x = closeness.election, fill = race.binary)) +
  geom_bar(position = "fill") +
  geom_text(
    aes(label = percent(round(after_stat(count)/tapply(after_stat(count), after_stat(x), sum)[after_stat(x)], 4))),
    stat = "count", position = position_fill(0.5)
  ) +
  scale_y_continuous(labels = percent) + ylab("proportion") +
  theme_minimal() + ggtitle("Closeness of election independent of race rejected (p < 0.001)")
# Test Trust closeness election independent of race. Hypothesis is rejected (-values ). However, possible confounders of this relationship are not measured in the corresponding dataset.
wilcox.test(as.numeric(voter.turnout.data.scope[voter.turnout.data.scope$race.binary == "White", ]$closeness.election),
            as.numeric(voter.turnout.data.scope[voter.turnout.data.scope$race.binary == "Non-white", ]$closeness.election))


fair_pred <- fair_predictions(voter.turnout.data.scope, X, Z, W, Y, x0 = "Non-white", x1 = "White")




#### Extra test ####

# Set X, Y, Z and W with all mediators
Y <- "voted" # outcome
X <- "race.binary" # protected attribute
Z <- c("age","marital.status","sexual.orientation","gender") # confounders
W <-  c("closeness.election") # mediators
# Z <- c("gender","age","marital.status","sexual.orientation") # confounders
# W <-  c("religiousness","religion","rural.urban","income","education","employment","party.identification",
#         "political.engagement.mobilization","political.engagement.discussion",
#         "political.engagement.involvement.political","political.engagement.involvement.community",
#         "LR.attitudes","trust.politics","government.role.social","government.role.security",
#         "democratic.norms.1","democratic.norms.2","engagement.campaigns","closeness.election") # mediators

set.seed(2025)
tvd.voter.turnout <- fairness_cookbook(data = voter.turnout.data.scope, X = X, W = W, Z = Z, Y = Y, x0 = "Non-white", x1 = "White")
voter.turnout.meas <- summary(tvd.voter.turnout)$measures
voter.turnout.meas

autoplot(tvd.voter.turnout, decompose = "xspec",dataset = "Voter Turnout Data")

write_xlsx(voter.turnout.meas,"voter.turnout.meas closeness.election.xlsx")



#### Reproducing (Bergh, 2007) results ####

# Variables #
# V961081: did you vote? 
# V961082: for whom did you vote? 1: Clinton, 2: Dole
# V960066: gender? 1: male, 2: female
# V960702: own income?
# V960623: public sector? 1: yes, other: no
# Feminist consciousness
# # V961300: Looking at page 8 of the booklet, here is a list of groups. Please read over the list and tell me the number for those groups you feel particularly close to – people who are most like you in their ideas and interests and feelings about things. Feminists”;
# # V960503: Which one of the opinions on this page best agrees with your view? You can just tell me the number of the opinion you choose. 1. By law, abortion should never be permitted. 2. The law should permit abortion only in case of rape, incest or when the woman’s life is in danger. 3. The law should permit abortion for reasons other than rape, incest or danger to the woman’s life, but only after the need for the abortion has been clearly established. 4. By law, a woman should always been able to obtain an abortion as a matter of personal choice”
# # V960543: Recently there has been a lot of talk about women’s rights. Some people feel that women should have an equal role with men in running business, industry, and government. (Suppose these people are at one end of a scale, at point 1.) Others feel that a woman’s place is in the home. (Suppose these people are at the other end, at point 7.) And, of course, some other people have opinions somewhere in between, at points 2, 3, 4, 5 or 6. Where would you place yourself on this scale, or haven’t you thought much about this
# # V961039: I’d like to get your feelings toward some of our political leaders and other people who are in the news these days. I’ll read the name of a person and I’d like you to rate that person using something we call the feeling thermometer. Ratings between 50 degrees and 100 degrees mean that you feel favorable and warm toward the person. Ratings between 0 degrees and 50 degrees mean that you don’t feel favorable toward that person and that you don’t care too much for that person. You would rate the person at the 50 degree mark if you don’t feel particular warm or cold toward the person. The women’s movement
# Left-right attitudes
# # V961484: In politics people sometimes talk of left and right. Where would you place yourself on a scale from 0 to 10 where 0 means the left and 10 means the right?”
# # V960450: Some people think the government should provide fewer services even in areas such as health and education in order to reduce spending. Suppose these people are at one end of a scale, at point 1. Other people feel it is important for the government to provide many more services even if it means an increase in spending. Suppose these people are at the other end, at point 7. And, of course, some other people have opinions somewhere in between, at points 2, 3, 4, 5 or 6. Where would you place yourself on this scale, or haven’t you thought much about this?”
# # V960479: There is much concern about the rapid rise in medical and hospital costs. Some people feel there should be a government insurance plan which would cover all medical and hospital expenses for everyone. (Suppose these people are at one end of a scale, at point 1.) Others feel that all medical expenses should be paid by individuals and through private insurance plans like Blue Cross or some other company paid plans. (Suppose these people are at the other end, at point 7.) And, of course, some other people have opinions somewhere in between at points 2, 3, 4, 5, or 6. Where would you place yourself on this scale, or haven’t you thought much about this?”
# # V960483: Some people feel the government in Washington should see to it that every person has a job and a good standard of living (Suppose these people are at one end of a scale, at point 1.) Others think the government should just let each person get ahead on their own. (Suppose these people are at the other end, at point 7.) And, of course, some other people have opinions somewhere in between, at points 2, 3, 4, 5, or 6. Where would you place yourself on this scale, or haven’t you thought much about this?”
# # V960487: Some people feel that the government in Washington should make every effort to improve the social and economic position of blacks. (Suppose these people area one end of a scale, at point 1.) Others feel that the government should not make any special effort to help blacks because they should help themselves. (Suppose these people are at the other end, at point 7.) And, of course, some other people have opinions somewhere in between, at points 2, 3, 4, 5, or 6. Where would you place yourself on this scale, or haven’t you thought much about this?”
# # V960537: Some people think we need much tougher government regulations on business in order to protect the environment. (Suppose these people are at one end of a scale, at point 1.) Others think that current regulation to protect the environment are already too much of a burden on business. (Suppose these people are at the other end of the scale, a point number 7.) And, of course, some other people have opinions somewhere in between at points 2, 3, 4, 5 or 6. Where would you place yourself on this scale, or haven’t you thought much about this?”


anes1996.data <- read_dta('nes96.dta')
str(anes1996.data)
#write_xlsx(anes1996.data,"ANES 1996 data.xlsx")

anes1996.scope.data <- anes1996.data %>% 
  select(c("V961081","V961082","V960066","V960702","V960623","V961300","V960503","V960543","V961039","V961484","V960450","V960479","V960483","V960487","V960537")) %>% data.frame
str(anes1996.scope.data)

anes1996.scope.data <- anes1996.scope.data %>% 
  filter(V961081==1) %>%
  filter(V961082 %in% c(1,2)) %>%
  filter(V960702 < 25) %>%
  filter(V960503 %in% c(1,2,3,4)) %>%
  filter(V960543 %in% c(1,2,3,4,5,6,7)) %>%
  filter(V961039 < 101)

## Outcome Y, vote ##  
  
# anes1996.scope.data[,'V961082'] <- as.factor(anes1996.scope.data[,'V961082'])

## Protected Attributes X, gender ##  

anes1996.scope.data$V960066 <- as.character(anes1996.scope.data$V960066)
anes1996.scope.data$V960066[anes1996.scope.data$V960066==1] <- "Male"
anes1996.scope.data$V960066[anes1996.scope.data$V960066==2] <- "Female"
anes1996.scope.data[,'V960066'] <- as.factor(anes1996.scope.data[,'V960066'])

## Confounders Z, none ##


## Mediators W, Income, Public sector, L–R attitudes, Feminist consc., ##

#income #
anes1996.scope.data[,'V960702'] <- factor(anes1996.scope.data[,'V960702'], levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24), ordered = TRUE)

#public sector#
anes1996.scope.data$V960623[anes1996.scope.data$V960623==5] <- 0
anes1996.scope.data[,'V960623'] <- factor(anes1996.scope.data[,'V960623'], levels = c(0,1))

# Total variation in function of gender#

TV_gender <- mean(anes1996.scope.data$V961082[anes1996.scope.data$V960066 == "Male"]) -
  mean(anes1996.scope.data$V961082[anes1996.scope.data$V960066 == "Female"])
TV_gender

anes1996.scope.data[,'V961082'] <- as.factor(anes1996.scope.data[,'V961082'])

X <- "V960066" # protected attribute
W <- c("V960702","V960623") # mediators
Z <- "V960066"
Y <- "V961082" # outcome

set.seed(2024)
tvd.gender.gap <- fairness_cookbook(data = anes1996.scope.data, X = X, W = W, Z = Z, Y = Y, x0 = "Male", x1 = "Female")

fairness_cookbook()
tvd.gender.gap.meas <- summary(tvd.gender.gap)$measures
tvd.gender.gap.meas

autoplot(tvd.gender.gap, decompose = "xspec",dataset = "Voting Gender Gap")

ggplot(anes1996.scope.data, aes(x = V960702, fill = V960066)) +
  geom_density(alpha = 0.4) + theme_bw() +
  theme(
    legend.position = c(0.8, 0.8),
    legend.box.background = element_rect()
  ) + ggtitle("income independent of gender rejected (p < 0.001)")








#### Appendix ####

voter.turnout.data %>% group_by(race) %>% summarise(total = n())
voter.turnout.data %>% group_by(race2) %>% summarise(total = n())
voter.turnout.data %>% group_by(latino) %>% summarise(total = n())
voter.turnout.data %>% group_by(white) %>% summarise(total = n())
voter.turnout.data %>% group_by(black) %>% summarise(total = n())
voter.turnout.data %>% group_by(asian) %>% summarise(total = n())
voter.turnout.data %>% group_by(native.hawaian) %>% summarise(total = n())
voter.turnout.data %>% group_by(native.american) %>% summarise(total = n())
voter.turnout.data %>% group_by(other) %>% summarise(total = n())

voter.turnout.data %>% group_by(age) %>% summarise(total = n())
voter.turnout.data %>% group_by(age2) %>% summarise(total = n())
voter.turnout.data %>% group_by(gender) %>% summarise(total = n())
voter.turnout.data %>% group_by(gender2) %>% summarise(total = n())
voter.turnout.data %>% group_by(sex) %>% summarise(total = n())
voter.turnout.data %>% group_by(voter.turnout) %>% summarise(total = n())

voter.turnout.data %>% group_by(vote.wait.time) %>% summarise(total = n())
voter.turnout.data %>% group_by(pol.time.distance) %>% summarise(total = n())
age.summary <- voter.turnout.data %>% group_by(age) %>% summarise(total = n())

voter.turnout.data <- voter.turnout.data[complete.cases(voter.turnout.data[,c("race2","age","religion","rural.urban","sexual.orientation","primary.vote","income","education","voter.turnout")]),]
voter.turnout.data <- voter.turnout.data[complete.cases(voter.turnout.data$education),]
voter.turnout.data <- voter.turnout.data %>% select(c("race2","age","religion","rural.urban","sexual.orientation","primary.vote","income","education","voter.turnout"))

#### To include in model based on own selection##

## Create outcome variable
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
#	"V201509"	- PRE: Domestic partnership status
#	"V201510"	- PRE: Highest level of Education
#	"V201529"	- PRE: Describe R's employment
#	"V201530"	- RESTRICTED: PRE: What kind of business/industry
#	"V201532"	- RESTRICTED: PRE: R's most important activities or duties
#	"V201533x"	- PRE: SUMMARY: R occupation status 2 digit
#	"V201600"	- PRE: What is your (R) sex? [revised]
#	"V201601"	- PRE: Sexual orientation of R [revised]
#	"V201607"	- RESTRICTED: PRE: Total income amount ‐ revised
#	"V201617x"	- PRE: SUMMARY: Total (family) income
#	"V202458"	- RESTRICTED: POST: Total income amount
#	"V202355"	- POST: Does R currently live in a rural or urban area
#	"V202637"	- POST: IWR OBS: respondent's gender
#	"V202638"	- POST: IWR OBS: respondent's estimated age
#	"V203401"	- PRE: IWR DESCR: age
#	"V203402"	- PRE: IWR DESCR: race/ethnicity
#	"V203403"	- PRE: IWR DESCR: gender
#	"V203404"	- PRE: IWR DESCR: education
#	"V203411"	- POST: IWR DESCR: age

## Create confounding variable for religion or create index on religiousness?
#	"V201435"	- PRE: What is present religion of R
#	"V201436"	- PRE: If R has no particular religion does R mean atheist or agnostic
#	"V201452"	- PRE: Ever attend church or religious services
#	"V201453"	- PRE: Attend religious services how often
#	"V201462"	- PRE: Religious identification

## Create index for LR attitudes
#	"V201200"	- PRE: 7pt scale liberal‐conservative self‐placement
#	"V201246"	- PRE: 7pt scale spending & services: self‐placement
#	"V201252"	- PRE: 7pt scale gov‐private medical insurance scale: self‐placement
#	"V201255"	- PRE: 7pt scale guaranteed job‐income scale: self‐placement
#	"V201258"	- PRE: 7pt scale gov assistance to blacks scale: self‐placement
#	"V201262"	- PRE: 7pt scale environment‐business tradeoff: self‐placement
#	"V201201"	- PRE: If R had to choose liberal or conservative self‐placemt -- REDUNDANT?

## Create index for party identity
#	"V201228"	- PRE: Party ID: Does R think of self as Democrat, Republican, or Independent
#	"V202439"	- POST: CSES5‐Q18: Left‐right‐self
#	"V202441"	- POST: CSES5‐Q22a: Is R close to any political party
#	"V202442"	- POST: CSES5‐Q22b: Does R feel closer to one party
#	"V202443"	- POST: CSES5‐Q22c: Which party does R feel closest to
#	"V202444"	- POST: CSES5‐Q22d: Degree of closeness to this party
#	"V202429"	- POST: CSES5‐Q10b: Party that represents R's views best

## Create index for political engagement
#	"V201232"	- PRE: Party identity importance
#	"V201221"	- PRE: VERSION 1A Does R consider voting a duty or choice
#	"V201222"	- PRE: VERSION 1B Does R consider voting a choice or duty
#	"V201223"	- PRE: How strongly does R feel that voting is a duty
#	"V201224"	- PRE: How strongly does R feel that voting is a choice
#	"V201629a"	- PRE: Media sources R used to follow presidential campaign: tv programs
#	"V201629b"	- PRE: Media sources R used to follow presidential campaign: newspapers
#	"V201629c"	- PRE: Media sources R used to follow presidential campaign: internet sites
#	"V201629d"	- PRE: Media sources R used to follow presidential campaign: radio news
#	"V201629e"	- PRE: Media sources R used to follow presidential campaign: none
#	"V202013"	- POST: R attend online political meetings, rallies, speeches, fundraisers
#	"V202014"	- POST: R go to any political meetings, rallies, speeches, dinners
#	"V202015"	- POST: R wear campaign button or post sign or bumper sticker
#	"V202016"	- POST: R do any (other) work for party or candidate
#	"V202017"	- POST: R contribute money to individual candidate running for public office
#	"V202018"	- RESTRICTED: POST: Party of candidate for whom R contributed money
#	"V202019"	- POST: R contribute money to political party during this election year
#	"V202020"	- RESTRICTED: POST: Party to which R contributed
#	"V202021"	- POST: R contribute to any other group that supported or opposed candidates
#	"V202022"	- POST: R ever discuss politics with family or friends
#	"V202023"	- POST: How many days in past week discussed politics with family or friends
#	"V202024"	- POST: Has R in past 12 months: gotten into a political argument
#	"V202025"	- POST: Has R in past 12 months: joined a protest march, rally, or demonstration
#	"V202026"	- POST: Has R in past 12 months: sign internet or paper petition
#	"V202027"	- POST: Has R in past 12 months: given money to religious organization
#	"V202028"	- POST: Has R in past 12 months: given money to other organization
#	"V202029"	- POST: Has R in past 12 months: posted comment online about political issue
#	"V202030"	- POST: Has R in past 12 months: contacted member of US Senate or House of Rep
#	"V202031"	- POST: Has R in past 12 months: worked w/others to deal w/issue facing community
#	"V202032"	- POST: Has R in past 12 months: attend mtg about issue facing community/schools
#	"V202033"	- POST: Has R in past 12 months: done any volunteer work
#	"V202034"	- POST: Has R in past 12 months: contacted federal elected official
#	"V202406"	- POST: CSES5‐Q01: How interested in politics is R
#	"V202407"	- POST: CSES5‐Q02: How closely does R follow politics in media
#	"V202408"	- POST: CSES5‐Q03: Agree/disagree: R understands most important political issues
#	"V202575"	-POST: GSS: How often does R pay attention to politics and elections

## Create index for attitude towards government role
#	"V201246"	- PRE: 7pt scale spending & services: self‐placement
#	"V201252"	- PRE: 7pt scale gov‐private medical insurance scale: self‐placement
#	"V201255"	- PRE: 7pt scale guaranteed job‐income scale: self‐placement
#	"V201258"	- PRE: 7pt scale gov assistance to blacks scale: self‐placement
#	"V201262"	- PRE: 7pt scale environment‐business tradeoff: self‐placement
#	"V201300"	- PRE: Federal Budget Spending: Social Security
#	"V201303"	- PRE: Federal Budget Spending: public schools
#	"V201306"	- PRE: Federal Budget Spending: Tightening border security
#	"V201312"	- PRE: Federal Budget Spending: welfare programs
#	"V201315"	- PRE: Federal Budget Spending: building and repairing highways
#	"V201318"	- PRE: Federal Budget Spending: aid to the poor
#	"V201321"	- PRE: Federal Budget Spending: protecting the environment
#	"V202256"	- POST: Would it be good for society to have more or less government regulation
#	"V202257"	- POST: Favor or oppose government trying to reduce income inequality
#	"V202274"	- POST: People in rural areas get more or less from government
#	"V202348"	- POST: Should federal government do more or less about opioid drug addiction
#	"V202377"	- POST: Should the minimum wage be raised, kept the same, or lowered
#	"V202378"	- POST: Increase/decrease government spending to help people pay for health care
#	"V202334"	- POST: Favor or oppose increased regulation on greenhouse emissions

## Create index for trust in institutions
#	"V201351"	- PRE: Votes counted accurately
#	"V201352"	- PRE: Trust election officials
#	"V201373"	- PRE: Favor or oppose restricting journalist access
#	"V201376"	- PRE: How concerned government might undermine media
#	"V201377"	- PRE: How much trust in news media
#	"V201379"	- PRE: Prefer government official who compromises or sticks to principles
#	"V202312"	- POST: Much of what people hear in schools and media are lies by those in power
#	"V202576"	- POST: GSS: CSES5‐ Q21: Satisfaction with democratic process
#	"V202577"	- POST: GSS: REV How often trust government in Washington to do what is right
#	"V202634"	- POST: GSS: How confident is R in medical institutions
#	"V202635"	- POST: GSS: How confident is R in scientific community
#	"V202425"	- POST: CSES5‐Q07: How widespread is corruption among politicians in US
#	"V202427"	- POST: CSES5‐Q09: How good/bad a job has government done in last 4 years
#	"V201366"	- PRE: How important that news organizations free to criticize

## Create index for barriers to voting
#	"V202119"	- POST: How difficult was it for R to vote
#	"V202121"	- POST: How long was wait time at polling place
#	"V202122"	- POST: How long does it take to get to polling place

## Create index for closeness of election
# "V201218" - PRE: WILL PRESIDENTIAL RACE BE CLOSE OR WILL (WINNER) WIN BY A LOT
# "V201220" - PRE: WILL PRESIDENTIAL RACE BE CLOSE IN STATE


#### Variables to consider in model ##
#	"V201012" - PRE: City included in address of registration (not registered at samp addr)
#	"V201013a"	- PRE: State of registration (not registered at samp addr; given at REG)
#	"V201019"	- PRE: Does R intend to register to vote
#	"V201020"	- PRE: Did R vote in a Presidential primary or caucus
#	"V201021"	- PRE: For which candidate did R vote in Presidential primary

#	"V201032"	- PRE: Does R intend to vote for President
#	"V201033"	- PRE: For whom does R intend to vote for President
#	"V201100"	- PRE: How likely is it that R will vote in November
#	"V201102"	- PRE: Did R vote for President in 2016
#	"V201216"	- PRE: How much R cares who wins Presidential Election [revised]

#	"V201239"	- PRE: Which party better: handling nations economy
#	"V201240"	- PRE: Which party better: handling health care
#	"V201241"	- PRE: Which party better: handling immigration
#	"V201242"	- PRE: Which party better: handling taxes
#	"V201243"	- PRE: Which party better: handling environment
#	"V201244"	- PRE: Which party better: handling COVID‐19

#	"V201337"	- PRE: Importance of abortion issue to R
#	"V201340"	- PRE: Abortion rights Supreme Court
#	"V202475"	- POST: Does R consider themself a feminist or anti‐feminist
#	"V202160"	- POST: Feeling thermometer: feminists
#	"V202291"	- POST: Do women demanding equality seek special favors
#	"V202292"	- POST: Do women complaining about discrimination cause more problems

#	"V201343"	- PRE: R favor or oppose death penalty

#	"V201367"	- PRE: How important branches of government keep one another from too much power
#	"V201368"	- PRE: How important elected officials face serious consequences for misconduct
#	"V201369"	- PRE: How important that people agree on basic facts

#	"V201429"	- PRE: Best way to deal with urban unrest

#	"V201517"	- PRE: R worked for pay last week
#	"V201526"	- PRE: How many weeks R worked past 12 months
#	"V201527"	- PRE: How many hours R worked per week
#	"V201528"	- PRE: Hours worked per week past 12 months ok

#	"V201594"	- PRE: How worried is R about current financial situation

#	"V201620"	- PRE: Does R have health insurance

#	"V201628"	- PRE: How many Guns owned
#	"V202338"	- POST: How important is issue of gun access to R

#	"V201641"	- PRE: Political knowledge intro
#	"V202143"	- POST: Feeling thermometer: Democratic Presidential candidate: Joe Biden
#	"V202144"	- POST: Feeling thermometer: Republican Presidential candidate: Donald Trump
#	"V202156"	- POST: Feeling thermometer: Democratic Vice Presidential candidate: Kamala Harris
#	"V202157"	- POST: Feeling thermometer: Republican Vice Presidential candidate: Mike Pence

#	"V202452"	- POST: How often does R have concerned feelings for other racial/ethnic groups
#	"V202477"	- POST: Feeling thermometer: Asian‐Americans
#	"V202478"	- POST: Feeling thermometer: Asians
#	"V202479"	- POST: Feeling thermometer: Hispanics
#	"V202480"	- POST: Feeling thermometer: blacks
#	"V202481"	- POST: Feeling thermometer: illegal immigrants
#	"V202482"	- POST: Feeling thermometer: whites
#	"V202527"	- POST: Discrimination in the US against blacks
#	"V202528"	- POST: Discrimination in the US against Hispanics
#	"V202529"	- POST: Discrimination in the US against Asians
#	"V202530"	- POST: Discrimination in the US against whites
#	"V202531"	- POST: Discrimination in the US against Muslims
#	"V202532"	- POST: Discrimination in the US against Christians
#	"V202533"	- POST: Discrimination in the US against Gays and Lesbians
#	"V202534"	- POST: Discrimination in the US against women
#	"V202535"	- POST: Discrimination in the US against men
#	"V202536"	- POST: Discrimination in the US against transgender people
#	"V202537"	- POST: How much discrimination has R faced personally because or race/ethnicity
#	"V202538"	- POST: How much discrimination has R faced because of gender


# access to voting resources/ barriers to voting -- Not relevant in fact
# voter.turnout.data$V202119[voter.turnout.data$V202119 %in% c(-9,-7,-6,-5,-1)] <- NA 
# voter.turnout.data$V202121[voter.turnout.data$V202121 %in% c(-7,-6,-5,-1)] <- NA
# voter.turnout.data$V202122[voter.turnout.data$V202122 %in% c(-7,-6,-5,-1)] <- NA




# Load necessary library
library(ggplot2)
library(reshape2)  # For reshaping data

# Sample data (Replace this with your actual data)
set.seed(123)
df_values <- data.frame(
  X = 1:19,
  A = runif(19, 5, 15),
  B = runif(19, 10, 20),
  C = runif(19, 15, 25)
)

df_sd <- data.frame(
  X = 1:19,
  A = runif(19, 0.5, 2),
  B = runif(19, 0.5, 2),
  C = runif(19, 0.5, 2)
)

# Reshape data for ggplot
df_long <- melt(df_values, id.vars = "X", variable.name = "Group", value.name = "Value")
df_sd_long <- melt(df_sd, id.vars = "X", variable.name = "Group", value.name = "SD")

# Merge values and standard deviations
df_plot <- merge(df_long, df_sd_long, by = c("X", "Group"))

# Plot with error bars
ggplot(df_plot, aes(x = X, y = Value, color = Group, group = Group)) +
  geom_line() +  # Line plot
  geom_point() +  # Points on the lines
  geom_errorbar(aes(ymin = Value - SD, ymax = Value + SD), width = 0.3) +  # Error bars
  labs(title = "Line Plot with Error Bars", x = "X-axis", y = "Value") +
  theme_minimal()