library("ipw")
library("ggrepel")
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
#####################################         (Survey Style)           #################################################
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
#####################################  ANALYSIS OF IP WEIGHTED MODELS  #################################################
#####################################       (The Right Style)          #################################################
########################################################################################################################

rs_msm_gen <- function(df, skillCol, modelName = "binomial"){
  #ipweight
  df %<>% mutate(skillCol)
  if(modelName == "binomial")
    {
    ipw.model <- ipwpoint(exposure = skillCol, family = "binomial",
                          link = "logit",
                          numerator = ~1, 
                          denominator = ~ EnglishLeagueOne+ MajorLeagueSoccer+ UEFAEuropaLeague+ SpanishPrimeraDivision+
                            ItalySerieA+ DutchEredivisie+ TurkishTurkcellSuperLig+ ArgentinaPrimeraDivision+
                            FrenchLigue1+ SpanishSegundaDivision+ GermanBundesliga+ PortugueseLiga+
                            SouthAfricanABSAPremierLeague+ BrasileiroSerieA+ UEFAChampionsLeague+ work_rate,
                          data = df
    )
    
    rs_msm <- glm(win_num ~ skillCol, family = binomial(link="logit"), df, weights = ipw.model$ipw.weights)
  } 
  else if(modelName == "gaussian")
    {
    ipw.model <- ipwpoint(exposure = skillCol, family = "gaussian",
                          numerator = ~1, 
                          denominator = ~ EnglishLeagueOne+ MajorLeagueSoccer+ UEFAEuropaLeague+ SpanishPrimeraDivision+
                            ItalySerieA+ DutchEredivisie+ TurkishTurkcellSuperLig+ ArgentinaPrimeraDivision+
                            FrenchLigue1+ SpanishSegundaDivision+ GermanBundesliga+ PortugueseLiga+
                            SouthAfricanABSAPremierLeague+ BrasileiroSerieA+ UEFAChampionsLeague+ work_rate,
                          data = df
    )
    
    rs_msm <- glm(win_num ~ skillCol, family = gaussian, df, weights = ipw.model$ipw.weights)
  }
  
  #ipwsummary
  ipw.summary <- summary(ipw.model$ipw.weights)
  
  #coefficients
  coef <- coef(rs_msm)
  
  #confidence interval
  confint <- confint(rs_msm)
  
  return (list(ipw.model, rs_msm, coef, confint))
}

#for loop over different skills
rs_gauss <- list()
rs_binom <- list()

#rs_msm_gen(df.gauss, skillCol = df.reduced[,2], "gaussian")

for(i in 2:35){
   skillCol <- df.reduced[,i]
   rs_gauss %<>%list.append(rs_msm_gen(df.gauss, skillCol = skillCol, "gaussian"))
   rs_binom %<>%list.append(rs_msm_gen(df.reduced, skillCol = skillCol, "binomial"))
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
    #untreated <- as.double(myModel[[i]][[2]]$coefficients[1])
    #treated <- untreated + as.double(myModel[[i]][[2]]$coefficients[2])
    #expected <- treated - untreated
    
    # In our case, expected value is just the slope of the line
    expected <- as.double(myModel[[i]][[2]]$coefficients[2])
    # get the expected difference
    exList <- list.append(exList, expected)
  }
  
  exList <- unlist(exList)
  mInd <- which.max(exList)
  mVal <- exList[mInd]
  return (list(exList, mInd))
}

gCausSvy <- causality(models.gauss)
bCausSvy <- causality(models.binom)

gCaus <- causality(rs_gauss)
bCaus <- causality(rs_binom)

##################################################
#################### PLOTTING ####################
##################################################

# GLM: Gaussian vs Binomial Models
gData <- data.frame(type=matrix("GGLM", ncol=1, nrow=34), skill=skillNames, xPoint=1:34, yPoint=gCaus[[1]])
bData <- data.frame(type=matrix("LGLM", ncol=1, nrow=34), skill=skillNames, xPoint=1:34, yPoint=bCaus[[1]])
gbData <- rbind(gData, bData)
gbData <- gbData[order(gbData$yPoint),]
p <- ggplot(data = gbData,
            mapping = aes(x = xPoint, y = yPoint, color=type))

glmDataInd <- gbData$type == 'GGLM' 
glm_mets <- p + geom_point() + 
  labs(x = "x", y = "GLM: Expected Difference", title = "GLM Model") +
  geom_text_repel(data = subset(gbData, yPoint %in% gbData$yPoint[glmDataInd][32:34] | yPoint %in% gbData$yPoint[!glmDataInd][32:34]),
                  mapping = aes(label = skill))
  #geom_text_repel(data = subset(gbData, yPoint == max(gbData$yPoint[glmDataInd]) | yPoint == max(gbData$yPoint[!glmDataInd])),
                  #mapping = aes(label = skill))
glm_mets

# Survey: Gaussian vs Bionmial Models
gSvyData <- data.frame(type=matrix("GSVY", ncol=1, nrow=34), skill= skillNames, xPoint=1:34, yPoint=gCausSvy[[1]])
bSvyData <- data.frame(type=matrix("LSVY", ncol=1, nrow=34), skill= skillNames, xPoint=1:34, yPoint=bCausSvy[[1]])
gbSvyData <- rbind(gSvyData, bSvyData)
gbSvyData <- gbSvyData[order(gbSvyData$yPoint),]

gbSvyInd <- gbSvyData$type == 'GSVY'
s <- ggplot(data = gbSvyData,
            mapping = aes(x = xPoint, y = yPoint, color=type))

svy_mets <- s + geom_point() + 
  labs(x = "x", y = "Svy: Expected Difference", title = "Survey Model") +
  geom_text_repel(data = subset(gbSvyData, yPoint %in% gbSvyData$yPoint[gbSvyInd][32:34] | yPoint %in% gbSvyData$yPoint[!gbSvyInd][32:34]),
                  mapping = aes(label = skill))
  #geom_text_repel(data = subset(gbSvyData, yPoint == max(gbSvyData$yPoint[gbSvyInd]) | yPoint == max(gbSvyData$yPoint[!gbSvyInd])),
  #                mapping = aes(label = skill))
svy_mets

# GLM vs Survey for Gaussian Distro
gsData <- rbind(gData, gSvyData)
gsData <- gsData[order(gsData$yPoint),]
gsInd <- gsData$type == 'GGLM'
gs <- ggplot(data = gsData,
             mapping = aes(x = xPoint, y = yPoint, color = type))

gs_graph <- gs + geom_point() + 
  labs(x = "x", y = "Expected Difference", title = "Gaussian: GLM vs Survey") +
  geom_text_repel(data = subset(gsData, yPoint %in% gsData$yPoint[gsInd][32:34] | yPoint %in% gsData$yPoint[!gsInd][32:34]),
                  mapping = aes(label = skill))
  #geom_text_repel(data = subset(gsData, yPoint == max(yPoint)),
  #                mapping = aes(label = skill))
gs_graph

# BLM vs Survey for Binomial Distro
bsData <- rbind(bSvyData, bData)
bsData <- bsData[order(bsData$yPoint),]
bsInd <- bsData$type == 'LSVY'

bs <- ggplot(data = bsData,
             mapping = aes(x = xPoint, y = yPoint, color = type))

bs_graph <- bs + geom_point() + 
  labs(x = "x", y = "Expected Difference", title = "Binomial: GLM vs Survey") +
  geom_text_repel(data = subset(bsData, yPoint %in% bsData$yPoint[bsInd][32:34] | yPoint %in% bsData$yPoint[!bsInd][32:34]),
                  mapping = aes(label = skill))
  #geom_text_repel(data = subset(bsData, yPoint == max(yPoint)),
  #               mapping = aes(label = skill))
bs_graph

# gauss <- models.gauss[[2]]
# binom <- models.binom[[1]]
#
# g <- gauss[[2]]
# v <- predict(g, newdata = as.data.frame(1))
# v2 <- as.data.frame(v)
#
# plot(df.reduced$win_num, v2$link)