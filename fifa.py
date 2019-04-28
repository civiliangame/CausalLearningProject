# Import libraries
import pandas as pd
import numpy as np
import team_stats
import unidecode as uni
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
    teams = team_data[2].drop_duplicates().dropna().to_numpy()

    mapping = {}
    for player_team in players:
        # Add any exact matches
        if player_team in teams:
            mapping[player_team] = player_team

        # Add any matches after eliminating accents
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
        if team.players != {}:
            new_row = pd.Series(team.aggregate_stats())
            team_df.append(new_row)

    return team_df


"""
Determines the values associated with each bin (in the histogram) of the work rate

Arguments:
    num_bins        number of bins for the histogram
    data            player data (continuous) in the team (Python list or NumPy array)

Return:
    bin_val         values (average of the edges of the bin) corresponding to each bin (NumPy array)
    binned_data     binned player data (continuous --> nominal) in the team (NumPy array)
"""
def bin_data(num_bins, data):
    np_data = data

    # If data is a Python list, convert it to NumPy array
    if isinstance(data, list)
        np_data = np.array(data)

    # Make histogram from data to get histogram bins
    hist, bin_edges = np.histogram(np_data, bins=num_bins)
    bin_val = np.zeros(bin_edges.size - 1)
    for i in range(bin_edges.size - 1):
        bin_val[i] = (bin_edges[i] +  bin_edges[i + 1]) / 2

    # Convert data (continuous) to their corresponding bin (nominal) values
    binned_data = np.zeros(np_data.size)
    for i in range(np_data.size):
        for j in range(bin_edges.size - 1):
            if bin_edges[j] < np_data[i] and np_data[i] <= bin_edges[j + 1]:
                binned_data[i] = bin_val[j]

            # Edge case for left-most bin
            if j is 0 and bin_edges[j] == np_data[i]:
                binned_data[i] = bin_val[j]

    return bin_val, binned_data


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
def lmse(data_true, data_pred):
    x = mean_squared_error(data_true, data_pred)
    return x



"""
IP weighting
"""
def ip_weighting(pop_data, used_stat, stat_bins, numerator_prob):

    # Calculate weight
    '''
    Pseudocode:
        1. Create a new array to hold errything
        2. Condtion on League
            -> get p(league)
        3. Condition on work rate
            -> p(Work Rate)
            -> p(Work Rate | League)
        4. Condition on statistic bins
            -> p(stat) = 1
        5. for each statisticample, the statement data[‘first_name’] == ‘Antonio’] produces a Pandas Series with a T
            -> mult. total by p(outcome)
            -> weight = p_we_want / p(a|covariates)
    '''
    # IP will have enough rows to hold the entire population * num unique work rates * num unique leagues
    leagues = pop_data.league.unique()
    rates = pop_data.work_rate.unique()
    ip = np.empty(pop.shape[0] * leagues.shape[0] * rates.shape[0], 3)

    index_count = 0
    # Condition on leagues
    for lg in leagues:
        lg_data = pop_data.loc[pop_data['league'] == lg]

        # Condition on work rate
        for rt in rates:
            rt_data = lg_data.loc[lg_data['work_rate'] == rt]
            ip[index_count] = j;alksdjfkjdsf
            index_count += 1


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
    else:
        print("There's a problem with your model, sir/ma'am")

    return


def main():
    fifa_player_df, fifa_team = read_data()
    aggregated_data = match_teams(fifa_team, fifa_player_df)


if __name__ == "__main__":
    main()
