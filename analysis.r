library("ipw")
library("magrittr")
library("rlist")
library("tidyverse")
library("survey")
library("WaveletComp")

########################################################################################################################
####################################################               #####################################################
#################################################### DATA CLEANING #####################################################
####################################################               #####################################################
########################################################################################################################

stat_names <- c('Crossing', 'Finishing', 'HeadingAccuracy', 'ShortPassing', 'Volleys', 'Dribbling',
     'Curve', 'FKAccuracy', 'LongPassing', 'BallControl', 'Acceleration',
     'SprintSpeed', 'Agility', 'Reactions', 'Balance', 'ShotPower',
     'Jumping', 'Stamina', 'Strength', 'LongShots', 'Aggression',
     'Interceptions', 'Positioning', 'Vision', 'Penalties', 'Composure',
     'Marking', 'StandingTackle', 'SlidingTackle', 'GKDiving', 'GKHandling',
     'GKKicking', 'GKPositioning', 'GKReflexes')

data.raw <- read.csv('./aggregated_data.csv')

#adds in binary column of win rate, 0: <50%, 1: >=50%
data.raw %<>% mutate(win_num = 0)
data.raw$win_num[data.raw$win_rate %>% as.character() == ">=50%"] <- 1

#removes non numerical columns (just in case)
data.work <- data.raw[, !names(data.raw) %in% c("name", "win_rate")]

#splits into work rates of 0 and 1
#unique_work_rate <- data.raw$work_rate %>% unique()
unique_leagues <- data.raw$league %>% unique()

#create the league table
df.league <- data.frame(X = data.raw$X) #adds in the X as placeholder for number of desired columns
for(i in 1:length(unique_leagues)){
  df.league %<>% mutate(as.numeric(data.raw$league == unique_leagues[i])) #adds the new data column
  names(df.league)[i] <- i #gives numerical names for placeholder
}
#puts in the actual league names
names(df.league) <- c( "X",
  'EnglishLeagueOne','MajorLeagueSoccer','BarclaysPremierLeague','UEFAEuropaLeague','SwissRaiffeisenSuperLeague',
  'SpanishPrimeraDivision','ItalySerieA','EnglishLeagueTwo','ItalySerieB','DutchEredivisie',
  'German2.Bundesliga','TurkishTurkcellSuperLig','AustrianTMobileBundesliga','ArgentinaPrimeraDivision',
  'FrenchLigue1','SpanishSegundaDivision','GermanBundesliga','AustralianALeague','EnglishLeagueChampionship',
  'PortugueseLiga','SouthAfricanABSAPremierLeague','BrasileiroSerieA','UEFAChampionsLeague','SwedishAllsvenskan',
  'JapaneseJLeague','MexicanPrimeraDivisionTorneoClausura','BelgianJupilerLeague','NorwegianTippeligaen',
  'ChineseSuperLeague','ScottishPremiership','RussianPremierLiga','DanishSASLigaen','FrenchLigue2'
)

#names(df.league) <- c("X", unique_leagues %>% as.character()) #applies new names
df.combined <- left_join(data.work, df.league, by = "X")

#testing glm
#print(unique_leagues)

#gathers the names of the skills
skillNames <- colnames(df.combined)[2:35]

#sum up skills and adds the aggregated column
df.combined %<>% mutate(AggregateSkill = df.combined[,2:35] %>% rowSums())

#further reduces the existing dataframe to remove the "league" non-numeric column
df.reduced <- df.combined[,!names(df.combined) %in% c("league")] 

df.gauss <- df.reduced

#cleaning skills 
for(i in 2:35){
  column <- df.reduced[,i]
  change <- df.reduced[,i]
  avg <- df.reduced[,i] %>% mean()
  
  # greater than mean gets 1
  change[change > avg] <- 101
  
  # less mean gets 0
  change[change <= avg] <- 0
  
  change[change == 101] <- 1
  
  df.reduced[,i] <- change
}



########################################################################################################################
#################################################                       ################################################
################################################# IP WEIGHTING FUNCTION ################################################
#################################################                       ################################################
########################################################################################################################


msm_gen <- function(df, skillCol, modelName = "binomial"){
  #ipweight
  df %<>% mutate(skillCol)
  if(modelName == "binomial"){
    ipw.model <- ipwpoint(exposure = skillCol, family = "binomial",
                        link = "logit",
                        numerator = ~1, 
                        denominator = ~ EnglishLeagueOne+ MajorLeagueSoccer+ UEFAEuropaLeague+ SpanishPrimeraDivision+
                        ItalySerieA+ DutchEredivisie+ TurkishTurkcellSuperLig+ ArgentinaPrimeraDivision+
                        FrenchLigue1+ SpanishSegundaDivision+ GermanBundesliga+ PortugueseLiga+
                        SouthAfricanABSAPremierLeague+ BrasileiroSerieA+ UEFAChampionsLeague+ work_rate,
                        data = df
    )
  } else if(modelName == "gaussian"){
    ipw.model <- ipwpoint(exposure = skillCol, family = "gaussian",
                          numerator = ~1, 
                          denominator = ~ EnglishLeagueOne+ MajorLeagueSoccer+ UEFAEuropaLeague+ SpanishPrimeraDivision+
                            ItalySerieA+ DutchEredivisie+ TurkishTurkcellSuperLig+ ArgentinaPrimeraDivision+
                            FrenchLigue1+ SpanishSegundaDivision+ GermanBundesliga+ PortugueseLiga+
                            SouthAfricanABSAPremierLeague+ BrasileiroSerieA+ UEFAChampionsLeague+ work_rate,
                          data = df
    )
  }
  
  #ipwsummary
  ipw.summary <- summary(ipw.model$ipw.weights)
  
  #dsgn
  dsgn <- svydesign(id = ~1, weights = ~ipw.model$ipw.weights, data = df)
  
  #msm
  msm <- svyglm(win_num ~ skillCol, design = dsgn)
  
  #coefficients
  coef <- coef(msm)
  
  #confidence interval
  confint <- confint(msm)
  
  return (list(ipw.model, msm, coef, confint))
}

########################################################################################################################
#####################################                                  #################################################
#####################################  ANALYSIS OF IP WEIGHTED MODELS  #################################################
#####################################                                  #################################################
########################################################################################################################

#for loop over different skills
models.gauss <- list()
models.binom <- list()


for(i in 2:35){
  skillCol <- df.reduced[,i]
  models.gauss %<>%list.append(msm_gen(df.gauss, skillCol = skillCol, "gaussian"))
  models.binom %<>%list.append(msm_gen(df.reduced, skillCol = skillCol, "binomial"))
}

########################################################################################################################
#####################################                                  #################################################
#####################################          CAUSAL ANALYSIS         #################################################
#####################################                                  #################################################
########################################################################################################################
causality <- function(myModel) {
  exList <- list()
  for (i in 1:length(myModel))
  {
    #print(models.gauss[[i]][[2]][1])
    # get our values of interest
    untreated <- as.double(myModel[[i]][[2]]$coefficients[1])
    treated <- untreated + as.double(myModel[[i]][[2]]$coefficients[2])
    expected <- treated - untreated
    
    # get the expected difference
    exList <- list.append(exList, expected)
  }
  
  exList <- unlist(exList)
  mInd <- which.max(exList)
  mVal <- exList[mInd]
  return (list(exList, mInd))
}

gCaus <- causality(models.gauss)
bCaus <- causality(models.binom)

# gauss <- models.gauss[[2]]
# binom <- models.binom[[1]]

# g <- gauss[[2]]
# 
# v <- predict(g, newdata = as.data.frame(1))
# v2 <- as.data.frame(v)
# 
# plot(df.reduced$win_num, v2$link)
# 
