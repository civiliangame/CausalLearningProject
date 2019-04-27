import pandas as pd
import numpy as np

def read_data():
    # Player data
    fifa_player_df = pd.read_csv("fifa_player.csv")

    # Team data - Need to drop every other row
    fifa_team_df = pd.read_csv("fifa_team.csv")
    fifa_team_df = fifa_team_df.drop("", axis=0)

def match_teams(team_data, player_data):
    pass

def main():
    pass

if __name__ == "__main__":
    main()
