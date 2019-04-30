library("ipw")
library("magrittr")
library("tidyverse")
library("survey")
library("WaveletComp")

########################################################################################################################
#################################################### DATA CLEANING #####################################################
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

#separate tables by work rate of 0 and 1
#data.work_zero <- data.work[data.work$work_rate == 0,]
#data.work_one <- data.work[data.work$work_rate == 1,]

#Unique leagues for work rates of 0 and 1
#unique_league_zero <- data.work_zero$league %>% unique()
#unique_league_one  <- data.work_one$league %>% unique()

#ipw for work rate of one
#ipw.one <- ipwpoint(exposure = win_num, family = "binomial", link = "logit", denominator = ~1, data = data.work_one)
#weights.one <- ipw.one$ipw.weights

#create the league table
df.league <- data.frame(X = data.raw$X) #adds in the X as placeholder for number of desired columns
for(i in 1:length(unique_leagues)){
  df.league %<>% mutate(as.numeric(data.raw$league == unique_leagues[i])) #adds the new data column
  names(df.league)[i+1] <- i #gives numerical names for placeholder
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
df.combined <- left_join(data.work, df.league)

#testing glm
#print(unique_leagues)

#gathers the names of the skills
skillNames <- colnames(df.combined)[2:35]

#sum up skills and adds the aggregated column
df.combined %<>% mutate(AggregateSkill = df.combined[,2:35] %>% rowSums())

########################################################################################################################
################################################# BEGINS IP WEIGHTING ##################################################
########################################################################################################################

#creates the ip weighted model based on aggregate skill and stratified by leagues
weightedModel <- ipwpoint(exposure = AggregateSkill,
    family = "gaussian",
    numerator = ~1,
    denominator = ~ 
      EnglishLeagueOne+
      MajorLeagueSoccer+
      UEFAEuropaLeague+
      SpanishPrimeraDivision+
      ItalySerieA+
      DutchEredivisie+
      TurkishTurkcellSuperLig+
      ArgentinaPrimeraDivision+
      FrenchLigue1+
      SpanishSegundaDivision+
      GermanBundesliga+
      PortugueseLiga+
      SouthAfricanABSAPremierLeague+
      BrasileiroSerieA+
      UEFAChampionsLeague,
    data = df.combined
)

#summary statistics for our weighted model
weightedModel.summary <- summary(weightedModel$ipw.weights)
#weightedModel.summary

#creates a density plot of the ip weights
weightedModel.plot <- ipwplot(weights = weightedModel$ipw.weights, logscale = FALSE, main = "weights")
weightedModel.plot

#further reduces the existing dataframe to remove the "league" non-numeric column
df.reduced <- df.combined[,!names(df.combined) %in% c("league")] #%>% as.matrix()

########################################################################################################################
######################################## CREATES THE MARGINAL STRUCTURAL MODEL #########################################
########################################################################################################################

#msm <- svyglm(WinRate ~ win_num, design = svydesign(id = ~1, weights = weightedModel$ipw.weights, data = df.reduced))
dsgn <- svydesign(id = ~1, weights = ~weightedModel$ipw.weights, data = df.combined)
msm <- svyglm(win_num ~ 
                AggregateSkill,#+
                # EnglishLeagueOne+
                # MajorLeagueSoccer+
                # BarclaysPremierLeague+
                # UEFAEuropaLeague+
                # SwissRaiffeisenSuperLeague+
                # SpanishPrimeraDivision+
                # ItalySerieA+
                # DutchEredivisie+
                # TurkishTurkcellSuperLig+
                # AustrianTMobileBundesliga+
                # ArgentinaPrimeraDivision+
                # FrenchLigue1+
                # SpanishSegundaDivision+
                # GermanBundesliga+
                # AustralianALeague+
                # EnglishLeagueChampionship+
                # PortugueseLiga+
                # SouthAfricanABSAPremierLeague+
                # BrasileiroSerieA+
                # UEFAChampionsLeague+
                # ChineseSuperLeague+
                # RussianPremierLiga,
                design = dsgn)


coef(msm)
confint(msm)


########################################################################################################################
####################################################### OLD CODE #######################################################
########################################################################################################################


#length(data.raw$win_num[data.raw$win_num == 1])
#length(data.raw$win_num[data.raw$win_num == 0])










#for(i in unique_league_zero){

#data.work.i <- data.work_zero[data.work_zero$league == unique_league_one[2],]
#data.work.i <- data.work.i[, !names(data.work.i) %in% c("league", "name", "win_rate")]
#one <- data.work.i[1,] %>% unlist() %>% unname()
#two <- data.work.i[2,] %>% unlist() %>% unname()
#bruh <- glm(one ~ two)
#anova(bruh)

#summary(bruh)

#ipwpoint(bruh, family = "ordinal")


#}






#for (wr in csv$work_rate)
##{
#  wr_data <- csv$work_rate == wr
#  for (league in csv[wr_data]$league)
##  {
#    for (stat in csv[league]$)
#    linear_model <- glm(win_rate ~ )
#    logistic_model <- glm(win_rate ~)
#  }
#}
