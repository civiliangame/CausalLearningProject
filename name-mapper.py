import pandas as pd
import numpy as np
import math
#it's in pip
import unidecode as uni

teams = pd.read_csv('fifa_team.csv', header=None)[2].drop_duplicates().dropna().to_numpy()
players = pd.read_csv('fifa_player.csv')['Club'].drop_duplicates().dropna().to_numpy()

# team team : player team
mapping = {}
for player_team in players:
    #add any exact matches
    if player_team in teams:
        mapping[player_team] = player_team

    #add any accents
    for team_team in teams:
        if uni.unidecode(team_team) == uni.unidecode(player_team):
            if team_team not in mapping:
                mapping[team_team] = player_team



print(mapping, len(mapping))
