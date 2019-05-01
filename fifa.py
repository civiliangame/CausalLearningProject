# Import libraries
import pandas as pd
import numpy as np
import team_stats
import unidecode as uni
from team_stats import Player
from sklearn.linear_model import LogisticRegression
from sklearn.model_selection import GridSearchCV
from sklearn.metrics import mean_squared_error
from sklearn.model_selection import cross_val_score


"""
Read in FIFA data

Returns:
    fifa_player_df      FIFA player data (Pandas dataframe)
    fifa_team           FIFA team data
"""
def read_data():
    # Player data
    fifa_player_df = pd.read_csv("fifa_player.csv")
    fifa_player_df = fifa_player_df.drop(['ID', 'Photo', 'Flag', 'Club Logo', 'Special', 'Real Face', 'Jersey Number', 'Joined', 'Contract Valid Until', 'Release Clause'], axis=1)

    # Team data
    team_stats.create_team_from_data()
    fifa_team = team_stats.get_teams()

    return fifa_player_df, fifa_team


"""
Creates a mapping of the club names in the player dataset to club names in the teams
dataset. The mapping is incomplete, and may be expanded later.
"""
def name_mapping(player_data, team_data):
    players = player_data['Club'].drop_duplicates().dropna().to_numpy()
    teams = np.array([x for x in team_data.keys()])

    mapping = {}
    for player_team in players:
        # Add any exact matches
        if player_team in teams:
            mapping[player_team] = player_team

        for team_team in teams:
            # Add any matches after eliminating accents
            if uni.unidecode(team_team) == uni.unidecode(player_team):
                if team_team not in mapping:
                    mapping[player_team] = team_team

            # Add any matches after adding FC or variants
            if team_team + " FC" == player_team or "FC " + team_team == player_team \
            or team_team + " CF" == player_team or "CF " + team_team == player_team \
            or player_team + " CF" == team_team or "CF " + player_team == team_team \
            or player_team + " FC" == team_team or "FC " + player_team == team_team \
            or team_team + " AC" == player_team or "AC " + team_team == player_team \
            or team_team + " CA" == player_team or "CA " + team_team == player_team \
            or player_team + " CA" == team_team or "CA " + player_team == team_team \
            or player_team + " AC" == team_team or "AC " + player_team == team_team:
                mapping[player_team] = team_team

    return mapping


"""
Join player and team FIFA data based on team names

Arg    return fifa_player_df, fifa_team
uments:
    team_data       FIFA team data (Pandas dataframe)
    player_data     FIFA player data (Pandas dataframe)
"""

def match_teams(team_data, player_data):
    mapper = name_mapping(player_data, team_data)

    for index, row in player_data.iterrows():
        # Create Player object
        player = team_stats.Player(row)

        if player.team in mapper:
            # Add each player to the correct team
            team_data[mapper[player.team]].add_player(player)

    team_df = pd.DataFrame()
    for team in team_data:
        if team_data[team].players != {}:
            new_row = pd.Series(team_data[team].aggregate_stats())
            team_df = team_df.append(new_row, ignore_index=True)

    return team_df

def main():
    fifa_player_df, fifa_team = read_data()
    aggregated_data = match_teams(fifa_team, fifa_player_df)
    mean_work_rate = np.mean(aggregated_data['work_rate'])
    aggregated_data.loc[aggregated_data['work_rate'] < mean_work_rate, 'work_rate'] = 0
    aggregated_data.loc[aggregated_data['work_rate'] >= mean_work_rate, 'work_rate'] = 1

    aggregated_data.to_csv('aggregated_data.csv')


if __name__ == "__main__":
    main()
