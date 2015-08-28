#############################################################
#Data Analysis
#############################################################
#0. Creating datasets for observational, vignette, and conjoint
#1. Recoding variables for analyses
#3. Data Quality
#2. Conjoint Analysis
#2.1 Design Tests
#3. Vignette
#3.1 Design Tests
#4. Observational
#############################################################
#############################################################

rm(list=ls())

library(foreign)
library(cjoint)
library(plyr)
library(dgof)
library(xtable)
library(plotrix)

#Function

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}


######################################################

#Analysis of Pilot data

#Reading data from Qualtrics
#Not made available at this moment 
#data <- read.csv("~/Dropbox/Dissertation/survey_experiment/Qualtrics Data/soft_launch_final.csv",
                  #header=T, as.is=T)

data <- read.csv("~/Dropbox/Dissertation/survey_experiment/Qualtrics Data/soft_launch_final.csv",
                  header=T, as.is=T)

#Eliminating those who did not make it pass filter
data <- data[-1,]
data <- data[data$gc==1,]


#Eliminating unnecessary variables
#pilot <- pilot[,-c(2:7, 191:201)] #random order tracking, comments

#Separating Vignette and Observational data

obs <- data[ ,c("V1", "rocha","santa", "check_pref", "check_ngos", "check_sc", "check_APAE",  
                "fund_NGO", "Fund_hosp", "Fund_local", "Fund_APAE", "gender", "age", "uf_1", 
                "religion", "MW",
                "education", "vote2014", "party")]

vignette <- data[, c("V1", "cred_nsp", "resp_cnsp", "rate_cnsp_1", "cred_pref", "resp_cpref",     
                    "rate_cpref_1",  "cred_nsp2",  "resp_cnsp2", "rate_cnsp2_1", "cred_pref2",   
                    "resp_cpre2", "rate_cpre2_1", "blame_nsp", "resp_bnsp", "rate_bnsp_1",  
                   "blame_pref", "resp_bpref", "rate_bpref_1",  "blame_pre2",  "resp_bpre2",    
                   "rate_bpre2_1", "blame_nsp2", "resp_bnsp2", "rate_bnsp2_1", 
                   "gender", "age", "uf_1", "religion", "MW", 
                   "education", "vote2014", "party")]

####### Checking randomization in Conjoint

#Row randomization
table(data$Row1)
table(data$Row2)
table(data$Row3)


#Making Conjoint data in individual-profile choice format as row:

#conjoint <- conjoint[,-c(104:106)]

# For each respondent
RespId <- unique(data$V1) 
list_c <- list()
for (i in 1:length(RespId)){
  temp <- data[data$V1==RespId[i],]
  col_attr <- as.matrix(temp[,c("Row1", "Row2", "Row3")])
  
  profile1 <- temp[,c("V1", "Row1_A", "Row2_A", "Row3_A", "rate1_1", "forced1", "rate1_p_1",
                      "gender", "age", "uf_1", "religion", "MW", 
                      "education", "vote2014", "party")]
  profile1$panel <- "A" 
  profile1$task <- 1
  names(profile1) <- c("respID", col_attr[1], col_attr[2], col_attr[3],
                       "rate", "choice", "rate_fed", "gender", "age", "uf_1", "religion", "MW", 
                       "education", "vote2014", "party", "panel", "task")
  
  profile2 <- temp[,c("V1", "Row1_B", "Row2_B", "Row3_B", "rate1_2", "forced1", "rate1_p_1",
                      "gender", "age", "uf_1", "religion", "MW", 
                      "education", "vote2014", "party")]
  profile2$panel <- "B" 
  profile2$task <- 1
  names(profile2) <- c("respID", col_attr[1], col_attr[2], col_attr[3],
                       "rate", "choice", "rate_fed", "gender", "age", "uf_1", "religion", "MW", 
                       "education", "vote2014", "party", "panel", "task")
  
  profile3 <- temp[,c("V1", "Row1_2_A", "Row2_2_A", "Row3_2_A", "rate2_1", "forced2", "rate2_p_1",
                      "gender", "age", "uf_1", "religion", "MW", 
                      "education", "vote2014", "party")]
  profile3$panel <- "A" 
  profile3$task <- 2
  names(profile3) <- c("respID", col_attr[1], col_attr[2], col_attr[3],
                       "rate", "choice", "rate_fed",
                       "gender", "age", "uf_1", "religion", "MW", 
                       "education", "vote2014", "party","panel", "task")
  
  profile4 <- temp[,c("V1", "Row1_2_B", "Row2_2_B", "Row3_2_B", "rate2_2", "forced2", "rate2_p_1",
                      "gender", "age", "uf_1", "religion", "MW", 
                      "education", "vote2014", "party")]
  profile4$panel <- "B" 
  profile4$task <- 2
  names(profile4) <- c("respID", col_attr[1], col_attr[2], col_attr[3],
                       "rate", "choice", "rate_fed",
                       "gender", "age", "uf_1", "religion", "MW", 
                       "education", "vote2014", "party", "panel", "task")
  
  profile5 <- temp[,c("V1", "Row1_3_A", "Row2_3_A", "Row3_3_A", "rate3_1", "forced3", "rate3_p_1",
                      "gender", "age", "uf_1", "religion", "MW", 
                      "education", "vote2014", "party")]
  profile5$panel <- "A" 
  profile5$task <- 3
  names(profile5) <- c("respID", col_attr[1], col_attr[2], col_attr[3],
                       "rate", "choice", "rate_fed", "gender", "age", "uf_1", "religion", "MW", 
                       "education", "vote2014", "party", "panel", "task")
  
  profile6 <- temp[,c("V1", "Row1_3_B", "Row2_3_B", "Row3_3_B", 
                      "rate3_2", "forced3", "rate3_p_1",
                      "gender", "age", "uf_1", "religion", "MW", 
                      "education", "vote2014", "party")]
  profile6$panel <- "B" 
  profile6$task <- 3
  names(profile6) <- c("respID", col_attr[1], col_attr[2], col_attr[3],
                       "rate", "choice", "rate_fed",
                       "gender", "age", "uf_1", "religion", "MW", 
                       "education", "vote2014", "party", "panel", "task")
  
  profile7 <- temp[,c("V1", "Row1_4_A", "Row2_4_A", "Row3_4_A", "rate4_1", "forced4", "rate4_p_1",
                      "gender", "age", "uf_1", "religion", "MW", 
                      "education", "vote2014", "party")]
  profile7$panel <- "A" 
  profile7$task <- 4
  names(profile7) <- c("respID", col_attr[1], col_attr[2], col_attr[3], 
                       "rate", "choice", "rate_fed", "gender", "age", "uf_1", "religion", "MW", 
                       "education", "vote2014", "party", "panel", "task")
  
  profile8 <- temp[,c("V1", "Row1_4_B", "Row2_4_B", "Row3_4_B", "rate4_2", "forced4", "rate4_p_1",
                      "gender", "age", "uf_1", "religion", "MW", 
                      "education", "vote2014", "party")]
  profile8$panel <- "B" 
  profile8$task <- 4
  names(profile8) <- c("respID", col_attr[1], col_attr[2], col_attr[3],
                       "rate", "choice", "rate_fed", "gender", "age", "uf_1", "religion", "MW", 
                        "education", "vote2014", "party", "panel", "task")
  
  resp <- rbind(profile1, profile2, profile3, profile4, profile5, profile6, profile7, profile8)
  

  
  list_c[[i]] <- resp 
  
}

conjointf <- rbind.fill(list_c)
conjointf$d_choice <- ifelse(substrRight(as.character(conjointf$choice), 1)==conjointf$panel, 1, 0)
names(conjointf)


#Checking Feature randomization
table(conjointf$partisanship) 
table(conjointf$federal)
table(conjointf$nsp_city) 


#Checking for identical profiles

RespId <- unique(conjointf$respID) 
list_temp <- list()
for (i in 1:length(RespId)){
  temp <- conjointf[conjointf$respID==RespId[i],]
  list_t <- list()
    for (j in 1:4){
      t.temp <- temp[temp$task==j,]
      if (t.temp$partisanship[1]==t.temp$partisanship[2] &
          t.temp$nsp_city[1]==t.temp$nsp_city[2] &
          t.temp$federal[1]==t.temp$federal[2]){
      t.temp$equal <- 1
      } 
      else{
        t.temp$equal <- 0
      }
      list_t[[j]] <- t.temp 
    }
  taskt <- rbind.fill(list_t)
  list_temp[[i]] <- taskt 
}


conjoint.f1 <- rbind.fill(list_temp)

table(conjoint.f1$equal)

#Eliminating those with any equal set of columns
conjoint.f <- conjoint.f1[conjoint.f1$equal==0,]

#Creating variables

#Outcome rate
conjoint.f$ratef[conjoint.f$rate=="Péssimo"] <- 1
conjoint.f$ratef[conjoint.f$rate=="Muito ruim"] <- 2
conjoint.f$ratef[conjoint.f$rate=="Ruim"] <- 3
conjoint.f$ratef[conjoint.f$rate=="Nem ruim nem bom"] <- 4
conjoint.f$ratef[conjoint.f$rate=="Bom"] <- 5
conjoint.f$ratef[conjoint.f$rate=="Muito bom"] <- 6
conjoint.f$ratef[conjoint.f$rate=="Ótimo"] <- 7

#Outcome federal rate
conjoint.f$ratefedf[conjoint.f$rate_fed=="Péssimo"] <- 1
conjoint.f$ratefedf[conjoint.f$rate_fed=="Muito ruim"] <- 2
conjoint.f$ratefedf[conjoint.f$rate_fed=="Ruim"] <- 3
conjoint.f$ratefedf[conjoint.f$rate_fed=="Nem ruim nem bom"] <- 4
conjoint.f$ratefedf[conjoint.f$rate_fed=="Bom"] <- 5
conjoint.f$ratefedf[conjoint.f$rate_fed=="Muito bom"] <- 6
conjoint.f$ratefedf[conjoint.f$rate_fed=="Ótimo"] <- 7


#Treats
conjoint.f$partisanshipf <- as.factor(conjoint.f$partisanship)
conjoint.f$federalf <- as.factor(conjoint.f$federal)
conjoint.f$nsp_cityf <- as.factor(conjoint.f$nsp_city)

table(conjoint.f$partisanship)
table(conjoint.f$federal)
table(conjoint.f$nsp_city)


#pre-treat attributes
conjoint.f$genderf <- ifelse(conjoint.f$gender=="Feminino", 1, 0)

conjoint.f$religionf[conjoint.f$religion=="Católica"] <- 1
conjoint.f$religionf[conjoint.f$religion=="Espírita"] <- 2
conjoint.f$religionf[conjoint.f$religion=="Evangélica"] <- 3
conjoint.f$religionf[conjoint.f$religion=="Não tenho religião"] <- 4
conjoint.f$religionf[conjoint.f$religion=="Outra"] <- 5
conjoint.f$religionf[conjoint.f$religion=="Protestant"] <- 6


conjoint.f$schoolingf[conjoint.f$education=="Ensino médio completo e superior incompleto (colegial completo e superior incompleto)"] <- 1
conjoint.f$schoolingf[conjoint.f$education=="Ensino superior completo"] <- 2
conjoint.f$schoolingf[conjoint.f$education=="Pós-graduação, mestrado ou doutorado "] <- 3


conjoint.f$vote2014f[conjoint.f$vote2014=="Aécio Neves (PSDB, 45)"] <- 1
conjoint.f$vote2014f[conjoint.f$vote2014=="Branco/Nulo"] <- 2
conjoint.f$vote2014f[conjoint.f$vote2014=="Dilma Rousseff (PT, 13)"] <- 3

conjoint.f$MWf[conjoint.f$MW=="0 a 2 salários mínimos (0 a 1.244,00 reais)"] <- 1
conjoint.f$MWf[conjoint.f$MW=="2 a 5 salários mínimos (1.245 a 3.110,00 reais)"] <- 2
conjoint.f$MWf[conjoint.f$MW=="5 a 8 salários mínimos (3.111,00 a 4.976,00 reais)"] <- 3
conjoint.f$MWf[conjoint.f$MW=="8 a 15 salários mínimos (4.976,00 a 9.330,00 reais)"] <- 4
conjoint.f$MWf[conjoint.f$MW=="Mais de 15 salários mínimos (mais de 9.331,00 reais)"] <- 5


table(conjoint.f$gender)
table(conjoint.f$religion)
table(conjoint.f$vote2014)
table(conjoint.f$education)
table(conjoint.f$MW)

#Blame recoding
vignette$treat_b[vignette$blame_nsp==1] <- "treatb_nsp"
vignette$treat_b[vignette$blame_pref==1] <- "treatb_pref"
vignette$treat_b[vignette$blame_nsp2==1] <- "treatb_nsp2"
vignette$treat_b[vignette$blame_pre2==1] <- "treatb_pref2"

#Credit recoding
vignette$treat_c[vignette$cred_nsp==1] <- "treatc_nsp"
vignette$treat_c[vignette$cred_nsp2==1] <- "treatc_nsp2"
vignette$treat_c[vignette$cred_pref==1] <- "treatc_pref"
vignette$treat_c[vignette$cred_pref2==1] <- "treatc_pref2"


table(vignette$treat_b)
table(vignette$treat_c)

#Outcome Blame
vignette$outcome_blame[vignette$treat_b=="treatb_nsp"] <- vignette$resp_bnsp[vignette$treat_b=="treatb_nsp"]
vignette$outcome_blame[vignette$treat_b=="treatb_pref"] <- vignette$resp_bpref[vignette$treat_b=="treatb_pref"]
vignette$outcome_blame[vignette$treat_b=="treatb_nsp2"] <- vignette$resp_bnsp2[vignette$treat_b=="treatb_nsp2"]
vignette$outcome_blame[vignette$treat_b=="treatb_pref2"] <- vignette$resp_bpre2[vignette$treat_b=="treatb_pref2"]

#Outcome Blame Rate
vignette$outcome_blamer[vignette$treat_b=="treatb_nsp"] <- vignette$rate_bnsp_1[vignette$treat_b=="treatb_nsp"]
vignette$outcome_blamer[vignette$treat_b=="treatb_pref"] <- vignette$rate_bpref_1[vignette$treat_b=="treatb_pref"]
vignette$outcome_blamer[vignette$treat_b=="treatb_nsp2"] <- vignette$rate_bnsp2_1[vignette$treat_b=="treatb_nsp2"]
vignette$outcome_blamer[vignette$treat_b=="treatb_pref2"] <- vignette$rate_bpre2_1[vignette$treat_b=="treatb_pref2"]

#Numeric rate
vignette$outcome_blamerf[vignette$outcome_blamer=="Péssimo"] <- 1
vignette$outcome_blamerf[vignette$outcome_blamer=="Muito ruim"] <- 2
vignette$outcome_blamerf[vignette$outcome_blamer=="Ruim"] <- 3
vignette$outcome_blamerf[vignette$outcome_blamer=="Nem bom nem ruim"] <- 4
vignette$outcome_blamerf[vignette$outcome_blamer=="Bom"] <- 5
vignette$outcome_blamerf[vignette$outcome_blamer=="Muito bom"] <- 6
vignette$outcome_blamerf[vignette$outcome_blamer=="Ótimo"] <- 7 


#Outcome Credit
vignette$outcome_credit[vignette$treat_c=="treatc_nsp"] <- vignette$resp_cnsp[vignette$treat_c=="treatc_nsp"]
vignette$outcome_credit[vignette$treat_c=="treatc_pref"] <- vignette$resp_cpref[vignette$treat_c=="treatc_pref"]
vignette$outcome_credit[vignette$treat_c=="treatc_nsp2"] <- vignette$resp_cnsp2[vignette$treat_c=="treatc_nsp2"]
vignette$outcome_credit[vignette$treat_c=="treatc_pref2"] <- vignette$resp_cpre2[vignette$treat_c=="treatc_pref2"]

#Outcome Credit Rate
vignette$outcome_creditr[vignette$treat_c=="treatc_nsp"] <- vignette$rate_cnsp_1[vignette$treat_c=="treatc_nsp"]
vignette$outcome_creditr[vignette$treat_c=="treatc_pref"] <- vignette$rate_cpref_1[vignette$treat_c=="treatc_pref"]
vignette$outcome_creditr[vignette$treat_c=="treatc_nsp2"] <- vignette$rate_cnsp2_1[vignette$treat_c=="treatc_nsp2"]
vignette$outcome_creditr[vignette$treat_c=="treatc_pref2"] <- vignette$rate_cpre2_1[vignette$treat_c=="treatc_pref2"]

#Numeric rate
vignette$outcome_creditrf[vignette$outcome_creditr=="Péssimo"] <- 1
vignette$outcome_creditrf[vignette$outcome_creditr=="Muito ruim"] <- 2
vignette$outcome_creditrf[vignette$outcome_creditr=="Ruim"] <- 3
vignette$outcome_creditrf[vignette$outcome_creditr=="Nem bom nem ruim"] <- 4
vignette$outcome_creditrf[vignette$outcome_creditr=="Bom"] <- 5
vignette$outcome_creditrf[vignette$outcome_creditr=="Muito bom"] <- 6
vignette$outcome_creditrf[vignette$outcome_creditr=="Ótimo"] <- 7 

#Recoding
vignette$genderf <- ifelse(vignette$gender=="Feminino", 1, 0)

vignette$religionf[vignette$religion=="Católica"] <- 1
vignette$religionf[vignette$religion=="Espírita"] <- 2
vignette$religionf[vignette$religion=="Evangélica"] <- 3
vignette$religionf[vignette$religion=="Não tenho religião"] <- 4
vignette$religionf[vignette$religion=="Outra"] <- 5
vignette$religionf[vignette$religion=="Protestant"] <- 6


vignette$schoolingf[vignette$schooling=="Ensino médio completo e superior incompleto (colegial completo e superior incompleto)"] <- 1
vignette$schoolingf[vignette$schooling=="Ensino superior completo"] <- 2
vignette$schoolingf[vignette$schooling=="Pós-graduação, mestrado ou doutorado "] <- 3


vignette$vote2014f[vignette$vote2014=="Aécio Neves (PSDB, 45)"] <- 1
vignette$vote2014f[vignette$vote2014=="Branco/Nulo"] <- 2
vignette$vote2014f[vignette$vote2014=="Dilma Rousseff (PT, 13)"] <- 3

vignette$MWf[vignette$MW=="0 a 2 salários mínimos (0 a 1.244,00 reais)"] <- 1
vignette$MWf[vignette$MW=="2 a 5 salários mínimos (1.245 a 3.110,00 reais)"] <- 2
vignette$MWf[vignette$MW=="5 a 8 salários mínimos (3.111,00 a 4.976,00 reais)"] <- 3
vignette$MWf[vignette$MW=="8 a 15 salários mínimos (4.976,00 a 9.330,00 reais)"] <- 4
vignette$MWf[vignette$MW=="Mais de 15 salários mínimos (mais de 9.331,00 reais)"] <- 5


##############################################################################################################################
# Check quotas --> OK
# check screen outs --> OK
# check randomization --> OK
# check identifical columns --> OK
# check straight liners
# check time to complete survey
##############################################################################################################################

# Quotas
prop.table(table(obs$education))
prop.table(table(obs$gender))

# Screen outs
names(data)
table(data$consent)
table(data$age18)
table(data$vote)
table(data$filter_1)
table(data$filter_2)
table(data$filter_3) #verde
table(data$filter_4)
table(data$filter_5)
table(data$filter_6)
table(data$filter_7)
table(data$filter_8) #amarelo
table(data$filter_9)
table(data$test2)

#Randomization

#Checking Feature randomization
table(conjointf$partisanship) 
table(conjointf$federal)
table(conjointf$nsp_city) 

#Checking Vignette

table(vignette$treat_b)
table(vignette$treat_c)

#Checking identical columns

table(conjoint.f1$equal)
equalr <- conjoint.f1[conjoint.f1$equal==1,]
length(unique(equalr$respID)) #103

#Straight-liners

order.obs <- data[c("DO.Q.santa", "DO.Q.rocha", "DO.Q.party", "DO.Q.Fund_local",    
                    "DO.Q.Fund_hosp", "DO.Q.fund_NGO", "DO.Q.Fund_APAE", "DO.Q.check_pref",    
                    "DO.Q.check_sc", "DO.Q.check_APAE", "DO.Q.check_ngos", "DO.Q.resp_bnsp",     
                    "DO.Q.resp_bnsp2", "DO.Q.resp_bpref", "DO.Q.resp_bpre2", "DO.Q.resp_cpref",    
                    "DO.Q.resp_cnsp", "DO.Q.resp_cnsp2","DO.Q.resp_cpre2")]


#Straightliners in Conjoint Rate
ids <- unique(conjointf$respID)
list_s <- list()
for (i in 1:length(ids)){
  temp <- conjointf[conjointf$respID==ids[i],]
  if (temp$rate[1]==temp$rate[2] &
        temp$rate[2]==temp$rate[3] &
        temp$rate[3]==temp$rate[4] &
        temp$rate[4]==temp$rate[5] &  
        temp$rate[5]==temp$rate[6] &
        temp$rate[6]==temp$rate[7] &
        temp$rate[7]==temp$rate[8]){
    temp$straight.rate <- 1
  } 
  else{
    temp$straight.rate <- 0
  }
  list_s[[i]] <- temp 
}

data.straight <- rbind.fill(list_s)
table(data.straight$straight.rate)
data.straightr <- data.straight[data.straight$straight.rate==1, ]
length(unique(data.straightr$respID))
#96 cases


#Straightliners in Conjoint Choice
ids <- unique(conjointf$respID)
list_c <- list()
for (i in 1:length(ids)){
  temp <- conjointf[conjointf$respID==ids[i],]
  if (temp$choice[1]==temp$choice[2] &
        temp$choice[2]==temp$choice[3] &
        temp$choice[3]==temp$choice[4] &
        temp$choice[4]==temp$choice[5] &  
        temp$choice[5]==temp$choice[6] &
        temp$choice[6]==temp$choice[7] &
        temp$choice[7]==temp$choice[8]){
    temp$straight.choice <- 1
  } 
  else{
    temp$straight.choice <- 0
  }
  list_c[[i]] <- temp 
}

data.straightc <- rbind.fill(list_c)
table(data.straightc$straight.choice)
data.straightcr <- data.straightc[data.straightc$straight.choice==1, ]
length(unique(data.straightcr$respID))
#201 cases

#Merge both
var_choice <- data.straightc$straight.choice
data.straightf <- cbind(data.straight, var_choice)

table(data.straightf$var_choice, data.straightf$straight.rate)

data.straightf$rep <- ifelse(data.straightf$var_choice==1 & data.straightf$straight.rate==1, 1, 0)
table(data.straightf$rep)

rep.unique <- data.straightf[data.straightf$rep==1, ]

length(unique(rep.unique$respID))

#Time to complete survey
data$Q_TotalDuration1 <- as.numeric(data$Q_TotalDuration)
summary(data$Q_TotalDuration1)
median(data$Q_TotalDuration1)/60
quantile(data$Q_TotalDuration1, probs=c(0, .5, .75, .8, .9, .95, .99))

#Exclude rep.unique and identifical panel

straight_liners <- unique(rep.unique$respID)
conjointff <- conjoint.f[!(conjoint.f$respID %in% straight_liners), ]


straight_liners <- unique(rep.unique$respID)
vignette.f <- vignette[!(vignette$V1 %in% straight_liners), ]
vignetteff <- vignette.f

save(conjointff, file="~/Dropbox/PreAnalysisPlans/conjointff.Rda") # this data is available
save(vignetteff, file="~/Dropbox/PreAnalysisPlans/conjointff.Rda") # this data is available


###########################################################################
# ANALYSIS


#Checking randomization (examples)

b.gender <- amce(genderf ~ partisanshipf + federalf + nsp_cityf,
                data=conjointff,
                cluster=TRUE, respondent.id="respID")

conjointff$vote2014f <- as.numeric(conjointff$vote2014) #see this warning
b.vote <- amce(vote2014f ~ partisanshipf + federalf + nsp_cityf,
                 data=conjointff,
                 cluster=TRUE, respondent.id="respID")

b.MW <- amce(MWf ~ partisanshipf + federalf + nsp_cityf,
               data=conjointff,
               cluster=TRUE, respondent.id="respID")

#Main Analysis

#Choice
results.choice <- amce(d_choice ~ partisanshipf + federalf + nsp_cityf,
                data=conjointff,
                cluster=TRUE, respondent.id="respID")

#Rate
results.rate <- amce(ratef ~ partisanshipf + federalf + nsp_cityf,
                     data=conjointff,
                     cluster=TRUE, respondent.id="respID")

#Rate Federal
results.ratefed <- amce(ratefedf ~ partisanshipf + federalf + nsp_cityf,
                     data=conjointff,
                     cluster=TRUE, respondent.id="respID")

level_names <- list()
level_names[["partisanshipf"]][1] <- "PT"
level_names[["partisanshipf"]][2] <- "PSDB"
level_names[["federalf"]][1] <- "No partnership"
level_names[["federalf"]][2] <- "Yes partnership" 
level_names[["nsp_cityf"]][1] <- "City Hall"
level_names[["nsp_cityf"]][2] <- "City Hall+Fed"
level_names[["nsp_cityf"]][3] <- "NSP"
level_names[["nsp_cityf"]][4] <- "NSP+Fed"

pdf("~/Dropbox/Dissertation/Plans/Survey Chapter/forced_conjoint.pdf", width=8, height=8)
plot(results.choice, xlab="Change in Pr(Preferred Mayor)",
     ylim=c(-1,1), breaks=c(-.2, 0, .2, .4, .6, .8), labels=c("-.2","0",".2", ".4", ".6", ".8"), text.size=15, 
     attribute_names=c("Partisanship", "Fed. Partnership", "Delegation"), 
     level_names= level_names, label.baseline=TRUE, point.size=1)
dev.off()

pdf("~/Dropbox/Dissertation/Plans/Survey Chapter/rate_conjoint.pdf", width=8, height=8)
plot(results.rate, xlab="Change in Mayoral Ratings",
     ylim=c(-1,1), breaks=c(-.2, 0, .2, .4, .6, .8), labels=c("-.2","0",".2", ".4", ".6", ".8"), text.size=15, 
     attribute_names=c("Partisanship", "Fed. Partnership", "City vs NSP"), 
     level_names= level_names, label.baseline=TRUE, point.size=1)
dev.off()

pdf("~/Dropbox/Dissertation/Plans/Survey Chapter/rate_conjoint_fed.pdf", width=8, height=8)
plot(results.ratefed, xlab="Change in Federal Government Ratings",
     ylim=c(-1,1), breaks=c(-.2, 0, .2, .4, .6, .8), labels=c("-.2","0",".2", ".4", ".6", ".8"), text.size=15, 
     attribute_names=c("Partisanship", "Fed. Partnership", "City vs NSP"), 
     level_names= level_names, label.baseline=TRUE, point.size=1)
dev.off()


#######################Vignette survey

###################### Checking randomization in Vignette


vignetteff$treat_prov_fed[vignetteff$treat_b== "treatb_nsp" | vignetteff$treat_b== "treatb_pref" |
                          vignetteff$treat_c== "treatc_nsp" | vignetteff$treat_c== "treatc_pref"] <- 1
vignetteff$treat_prov_fed[vignetteff$treat_b== "treatb_nsp2" | vignetteff$treat_b== "treatb_pref2" |
                          vignetteff$treat_c== "treatc_nsp2" | vignetteff$treat_c== "treatc_pref2" ] <- 0

#Credit 
#Recode outcome_credit
vignetteff$outcome_credit[vignetteff$outcome_credit== "O diretor da Associação Social Santa Maria" | 
                          vignetteff$outcome_credit== "O diretor da casa da Associação Social Santa Maria Clara" |
                          vignetteff$outcome_credit== "O diretor da casa de cuidado ao idoso"] <- "Director"


prop.table(table(vignetteff$treat_c, vignetteff$outcome_credit), 2)*100
tab.credit <- table(vignetteff$treat_c, vignetteff$outcome_credit)
chisq.test(tab.credit, correct = TRUE)
xtable(prop.table(tab.credit, 1)*100)
  
#Blame
#Recode outcome_blame
vignetteff$outcome_blame[vignetteff$outcome_blame== "O diretor da creche Associação Comunitária Meninos de Paula" | 
                          vignetteff$outcome_blame== "O diretor da creche da Prefeitura" ] <- "Director"

prop.table(table(vignetteff$treat_b, vignetteff$outcome_blame), 2)*100

#Chisquare
tab.blame <- table(vignetteff$treat_b, vignetteff$outcome_blame)
chisq.test(tab.blame, correct = TRUE)
xtable(prop.table(tab.blame, 1)*100)

###########Mayoral Ratings

#Credit 
prop.table(table(vignetteff$treat_c, vignetteff$outcome_creditr), 2)*100
tab.creditr <- table(vignetteff$treat_c, vignetteff$outcome_creditr)
chisq.test(tab.creditr, correct = TRUE)
xtable(prop.table(tab.creditr, 1)*100)

#Blame
prop.table(table(vignetteff$treat_b, vignetteff$outcome_blamer), 2)*100

#Chisqure
tab.blamer <- table(vignetteff$treat_b, vignetteff$outcome_blamer)
chisq.test(tab.blamer, correct = TRUE)
xtable(prop.table(tab.blamer, 1)*100)

#Credit Means
table <- matrix(NA, 3, 3)

#First cell
mean_ate1 <- round(mean(vignetteff[vignetteff$treat_c=="treatc_nsp",]$outcome_creditrf),2)
se_ate1 <- round(std.error(vignetteff[vignetteff$treat_c=="treatc_nsp",]$outcome_creditrf),2)
table[1, 1] <- c(paste(mean_ate1, se_ate1, sep=","))

#Second cell
mean_ate2 <- round(mean(vignetteff[vignetteff$treat_c=="treatc_nsp2",]$outcome_creditrf),2)
se_ate2 <- round(std.error(vignetteff[vignetteff$treat_c=="treatc_nsp2",]$outcome_creditrf),2)
table[1, 2] <- c(paste(mean_ate2, se_ate2, sep=","))

#Third cell
mean_ate3 <- round(mean(vignetteff[vignetteff$treat_c=="treatc_pref",]$outcome_creditrf),2)
se_ate3 <- round(std.error(vignetteff[vignetteff$treat_c=="treatc_pref",]$outcome_creditrf),2)
table[2, 1] <- c(paste(mean_ate3, se_ate3, sep=","))

#Fourth cell
mean_ate4 <- round(mean(vignetteff[vignetteff$treat_c=="treatc_pref2",]$outcome_creditrf),2)
se_ate4 <- round(std.error(vignetteff[vignetteff$treat_c=="treatc_pref2",]$outcome_creditrf),2)
table[2, 2] <- c(paste(mean_ate4, se_ate4, sep=","))

#Bottom cells (differences across type of provision)

#first cell 
mean_ate <- mean_ate1 - mean_ate3

#Standard error of difference of means
var_t <- var(vignetteff[vignetteff$treat_c=="treatc_nsp",]$outcome_creditrf)/nrow(vignetteff[vignetteff$treat_c=="treatc_nsp",])
var_c <- var(vignetteff[vignetteff$treat_c=="treatc_pref",]$outcome_creditrf)/nrow(vignetteff[vignetteff$treat_c=="treatc_pref",])
  
se_diff <- sqrt(var_t + var_c)

table[3, 1] <- c(paste(round(mean_ate, 2), round(se_diff, 2), sep=","))

#second cell
mean_ate <- mean_ate2 - mean_ate4

#Standard error of difference of means
var_t <- var(vignetteff[vignetteff$treat_c=="treatc_nsp2",]$outcome_creditrf)/nrow(vignetteff[vignetteff$treat_c=="treatc_nsp2",])
var_c <- var(vignetteff[vignetteff$treat_c=="treatc_pref2",]$outcome_creditrf)/nrow(vignetteff[vignetteff$treat_c=="treatc_pref2",])

se_diff <- sqrt(var_t + var_c)

table[3, 2] <- c(paste(round(mean_ate, 2), round(se_diff, 2), sep=","))

#Right-side cells (differences across class groups)

#first cell 
mean_ate <- mean_ate1 - mean_ate2
  
#Standard error difference of means
var_t <- var(vignetteff[vignetteff$treat_c=="treatc_nsp",]$outcome_creditrf)/nrow(vignetteff[vignetteff$treat_c=="treatc_nsp",])
var_c <- var(vignetteff[vignetteff$treat_c=="treatc_nsp2",]$outcome_creditrf)/nrow(vignetteff[vignetteff$treat_c=="treatc_nsp2",])

se_diff <- sqrt(var_t + var_c)

table[1, 3] <- c(paste(round(mean_ate, 2), round(se_diff, 2), sep=","))

#second cell
mean_ate <- mean_ate3 - mean_ate4
  
#Standard-error of difference of means
var_t <- var(vignetteff[vignetteff$treat_c=="treatc_pref",]$outcome_creditrf)/nrow(vignetteff[vignetteff$treat_c=="treatc_pref",])
var_c <- var(vignetteff[vignetteff$treat_c=="treatc_pref2",]$outcome_creditrf)/nrow(vignetteff[vignetteff$treat_c=="treatc_pref2",])

se_diff <- sqrt(var_t + var_c)
 
table[2, 3] <- c(paste(round(mean_ate, 2), round(se_diff, 2), sep=","))

xtable(table)

#Blame Means
table <- matrix(NA, 3, 3)

#First cell
mean_ate1 <- round(mean(vignetteff[vignetteff$treat_b=="treatb_nsp",]$outcome_blamerf),2)
se_ate1 <- round(std.error(vignetteff[vignetteff$treat_b=="treatb_nsp",]$outcome_blamerf),2)
table[1, 1] <- c(paste(mean_ate1, se_ate1, sep=","))

#Second cell
mean_ate2 <- round(mean(vignetteff[vignetteff$treat_b=="treatb_nsp2",]$outcome_blamerf),2)
se_ate2 <- round(std.error(vignetteff[vignetteff$treat_b=="treatb_nsp2",]$outcome_blamerf),2)
table[1, 2] <- c(paste(mean_ate2, se_ate2, sep=","))

#Third cell
mean_ate3 <- round(mean(vignetteff[vignetteff$treat_b=="treatb_pref",]$outcome_blamerf),2)
se_ate3 <- round(std.error(vignetteff[vignetteff$treat_b=="treatb_pref",]$outcome_blamerf),2)
table[2, 1] <- c(paste(mean_ate3, se_ate3, sep=","))

#Fourth cell
mean_ate4 <- round(mean(vignetteff[vignetteff$treat_b=="treatb_pref2",]$outcome_blamerf),2)
se_ate4 <- round(std.error(vignetteff[vignetteff$treat_b=="treatb_pref2",]$outcome_blamerf),2)
table[2, 2] <- c(paste(mean_ate4, se_ate4, sep=","))

#Bottom cells (differences across type of provision)

#first cell 
mean_ate <- mean_ate1 - mean_ate3

#Standard error of difference of means
var_t <- var(vignetteff[vignetteff$treat_b=="treatb_nsp",]$outcome_blamerf)/nrow(vignetteff[vignetteff$treat_b=="treatb_nsp",])
var_c <- var(vignetteff[vignetteff$treat_b=="treatb_pref",]$outcome_blamerf)/nrow(vignetteff[vignetteff$treat_b=="treatb_pref",])

se_diff <- sqrt(var_t + var_c)

table[3, 1] <- c(paste(round(mean_ate, 2), round(se_diff, 2), sep=","))

#second cell
mean_ate <- mean_ate2 - mean_ate4

#Standard error of difference of means
var_t <- var(vignetteff[vignetteff$treat_b=="treatb_nsp2",]$outcome_blamerf)/nrow(vignetteff[vignetteff$treat_b=="treatb_nsp2",])
var_c <- var(vignetteff[vignetteff$treat_b=="treatb_pref2",]$outcome_blamerf)/nrow(vignetteff[vignetteff$treat_b=="treatb_pref2",])

se_diff <- sqrt(var_t + var_c)

table[3, 2] <- c(paste(round(mean_ate, 2), round(se_diff, 2), sep=","))

#Right-side cells (differences across class groups)

#first cell 
mean_ate <- mean_ate1 - mean_ate2

#Standard error difference of means
var_t <- var(vignetteff[vignetteff$treat_b=="treatb_nsp",]$outcome_blamerf)/nrow(vignetteff[vignetteff$treat_b=="treatb_nsp",])
var_c <- var(vignetteff[vignetteff$treat_b=="treatb_nsp2",]$outcome_blamerf)/nrow(vignetteff[vignetteff$treat_b=="treatb_nsp2",])

se_diff <- sqrt(var_t + var_c)

table[1, 3] <- c(paste(round(mean_ate, 2), round(se_diff, 2), sep=","))

#second cell
mean_ate <- mean_ate3 - mean_ate4

#Standard-error of difference of means
var_t <- var(vignetteff[vignetteff$treat_b=="treatb_pref",]$outcome_blamerf)/nrow(vignetteff[vignetteff$treat_b=="treatb_pref",])
var_c <- var(vignetteff[vignetteff$treat_b=="treatb_pref2",]$outcome_blamerf)/nrow(vignetteff[vignetteff$treat_b=="treatb_pref2",])

se_diff <- sqrt(var_t + var_c)

table[2, 3] <- c(paste(round(mean_ate, 2), round(se_diff, 2), sep=","))

xtable(table)



######################Observational analysis

table(obs$rocha)
table(obs$santa)
table(obs$check_pref)
table(obs$check_ngos)
table(obs$check_sc)
table(obs$check_APAE)
table(obs$fund_NGO)
table(obs$Fund_hosp)
table(obs$Fund_local)
table(obs$Fund_APAE)




