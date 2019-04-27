# Import libraries
import pandas as pd
import numpy as np
import team_stats


"""
Read in FIFA data

Returns:
    fifa_player_df      FIFA player data (Pandas dataframe)
    fifa_team_df        FIFA team data (Pandas dataframe)
"""
def read_data():
    # Player data
    fifa_player_df = pd.read_csv("fifa_player.csv")
    fifa_player_df = fifa_player_df.drop(['ID', 'Photo', 'Flag', 'Club Logo', 'Special', 'Real Face', 'Jersey Number', 'Joined', 'Contract Valid Until', 'Release Clause'], axis=1)

    # Team data
    fifa_team_df = pd.read_csv("fifa_team.csv")

    return fifa_player_df, fifa_team_df


"""
Join player and team FIFA data based on team names

Arguments:
    team_data       FIFA team data (Pandas dataframe)
    player_data     FIFA player data (Pandas dataframe)
"""

def match_teams(team_data, player_data):
    team_data = team_data.drop_duplicates('Subteam')
    print(team_data)


def main():
    fifa_player_df, fifa_team_df = read_data()
    match_teams(fifa_team_df, fifa_player_df)

if __name__ == "__main__":
    main()
