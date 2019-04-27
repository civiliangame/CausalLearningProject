# Import libraries
import pandas as pd
import numpy as np
import team_stats


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
Join player and team FIFA data based on team names

Arguments:
    team_data       FIFA team data (Pandas dataframe)
    player_data     FIFA player data (Pandas dataframe)
"""

def match_teams(team_data, player_data):
    for index, row in player_data.iterrows():
        # Create Player object
        player = team_stats.Player(row)

        # Add each player to the correct team
        # NOTE: we may need to handle for varying team names
        team_data[player.team].add_player(player)

    team_df = pd.DataFrame()
    for team in team_data:
        new_row = pd.Series(team.aggregate_stats())
        team_df.append(new_row)

    print(team_df)


def model_data():
    pass


def main():
    fifa_player_df, fifa_team = read_data()
    match_teams(fifa_team, fifa_player_df)


if __name__ == "__main__":
    main()
