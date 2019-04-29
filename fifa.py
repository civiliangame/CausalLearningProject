# Import libraries
import pandas as pd
import numpy as np
import team_stats
import unidecode as uni
from sklearn.linear_model import LogisticRegression
from sklearn.model_selection import GridSearchCV
from sklearn.metrics import mean_squared_error


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
        #add any exact matches
        if player_team in teams:
            mapping[player_team] = player_team

        #add any accents
        for team_team in teams:
            if uni.unidecode(team_team) == uni.unidecode(player_team):
                if team_team not in mapping:
                    mapping[team_team] = player_team

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


def bin_work_rate(num_bins, work_rate):
    pass


"""
Tunes and fits a logistic regression model using sklearn. Returns a fit model that can give probabilities
"""
def logreg(data):
    param_grid = {'C':[.5, 1., 5., 10.], 'tol':[1e-6, 1e-4, 1e-2]}
    #maximum power
    tuning_results = GridSearchCV(LogisticRegression(), param_grid, n_jobs=-1).fit(data.drop('win_rate'), data['win_rate'])
    print('Best logreg parameters: %s' % (tuning_results.best_params_))
    return tuning_results.best_estimator_


"""
Least Mean Square Error
"""
def mmse(data):
    x = mean_squared_error(data, data_pred)

    return x


"""
"""
def ip_weighting(population, used_stat):

    # Calculate weight
    '''
    Pseudocode:
        1. Create a new array to hold errything
        2. Condtion on League
        3. Condition on work Rate
        4. Condition on statistic
        5. for each statistic

    '''
    return


"""
Call either LMSE or Logistic Regression models

Arguments:
    choice      model to use
    data        FIFA full dataset (Pandas dataframe)
"""
def model_data(choice, data):
    if choice is 'logreg':
        logreg(data)
    elif choice is 'lmse':
        lmse(data)

    return


def main():
    fifa_player_df, fifa_team = read_data()
    aggregated_data = match_teams(fifa_team, fifa_player_df)
    print(aggregated_data)


if __name__ == "__main__":
    main()
