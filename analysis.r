library("ipw")

stat_names <- c('Crossing', 'Finishing', 'HeadingAccuracy', 'ShortPassing', 'Volleys', 'Dribbling',
     'Curve', 'FKAccuracy', 'LongPassing', 'BallControl', 'Acceleration',
     'SprintSpeed', 'Agility', 'Reactions', 'Balance', 'ShotPower',
     'Jumping', 'Stamina', 'Strength', 'LongShots', 'Aggression',
     'Interceptions', 'Positioning', 'Vision', 'Penalties', 'Composure',
     'Marking', 'StandingTackle', 'SlidingTackle', 'GKDiving', 'GKHandling',
     'GKKicking', 'GKPositioning', 'GKReflexes')

csv <- read.csv('aggregated_data.csv')




for (wr in csv$work_rate)
{
  wr_data <- csv$work_rate == wr
  for (league in csv[wr_data]$league)
  {
    for (stat in csv[league]$)
    linear_model <- glm(win_rate ~ )
    logistic_model <- glm(win_rate ~)
  }
}
