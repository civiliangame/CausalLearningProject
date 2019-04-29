# Import libraries
import numpy as np


teams = {}

"""
FIFA Team
"""
class Team:
	class Year:
		def __init__(self, y):
			self.year = y
			self.wins = 0
			self.loss = 0
			self.draws = 0
			return

	def __init__(self, name, league):
		self.name = name
		self.league = league
		self.years = {}
		self.players = {}
		return

	def win(self, year):
		self.years[year].wins += 1
		return

	def loss(self, year):
		self.years[year].loss += 1
		return

	def draw(self, year):
		self.years[year].draws += 1
		return

	def add_player(self, player):
		self.players[player.name] = player

	def aggregate_stats(self):
		if '2018' not in self.years:
			return None

		results = {'win_rate': '>=50%' if self.years['2018'].wins >= (self.years['2018'].loss + .5 * self.years['2018'].draws) else '<50%'}
		for stat in Player.stat_names:
			best_player_name, best_player = max(self.players.items(), key=lambda name, player: player.abilities[stat])
			results[stat] = best_player.abilities[stat]

		results['work_rate'] = np.mean(np.mean(player.defensive_work_rate, player.offensive_work_rate) for _, player in self.players.items())
		return results

	def w2file(self, wfile):
		wfile.write("Year,League,Team,Wins,Losses,Draws\n")
		for key, y in self.years.items():
			twrite = key + ","
			twrite += self.league + ","
			twrite += self.name + ","
			twrite += str(y.wins) + ","
			twrite += str(y.loss) + ","
			twrite += str(y.draws) + "\n"
			wfile.write(twrite)
		return


"""
FIFA Player
"""
class Player:
	stat_names = ['Crossing', 'Finishing', 'HeadingAccuracy', 'ShortPassing', 'Volleys', 'Dribbling',
       'Curve', 'FKAccuracy', 'LongPassing', 'BallControl', 'Acceleration',
       'SprintSpeed', 'Agility', 'Reactions', 'Balance', 'ShotPower',
       'Jumping', 'Stamina', 'Strength', 'LongShots', 'Aggression',
       'Interceptions', 'Positioning', 'Vision', 'Penalties', 'Composure',
       'Marking', 'StandingTackle', 'SlidingTackle', 'GKDiving', 'GKHandling',
       'GKKicking', 'GKPositioning', 'GKReflexes']

	def __init__(self, stats):
		# Pass stats as a dataframe row
		self.name = stats['Name']
		self.team = stats['Club']

		# Get player's stats
		self.abilities = {}
		for stat in Player.stat_names:
			self.abilities[stat] = stats[stat]

		# Get whether player is goalkeeper or not
		if stats['Position'] == 'GK':
			self.goalkeeper = True
		else:
			self.goalkeeper = False

		# Get player's preferred position
		self.position = stats['Position']

		# Get player's work rate
		work_rate = str(stats['Work Rate'])
		work_rate = work_rate.split("/")

		if len(work_rate) > 1:
			work_rate[0] = work_rate[0].strip()
			if work_rate[0] == 'High':
				self.attacking_work_rate = 3
			elif work_rate[0] == 'Medium':
				self.attacking_work_rate = 2
			elif work_rate[0] == 'Low':
				self.attacking_work_rate = 1

			work_rate[1] = work_rate[1].strip()
			if work_rate[1] == 'High':
				self.defensive_work_rate = 3
			elif work_rate[1] == 'Medium':
				self.defensive_work_rate = 2
			elif work_rate[1] == 'Low':
				self.defensive_work_rate = 1
		else:
			self.offensive_work_rate = 0
			self.defensive_work_rate = 0


def add_team(teams, name, league):
	teams[name] = Team(name, league)
	return

def add_year(teams, name, year):
	teams[name].years[year] = Team.Year(year)
	return

def get_teams():
	return teams

"""
Create FIFA Team from FiveThirtyEight data
"""
def create_team_from_data():
	stats = open("spi_matches.csv", "r")

	for line in stats:
		cv = line.split(",")

		# 14 and 15 for score
		score1 = cv[14]
		score2 = cv[15]

		if score1 == '' or score2 == '':
			continue

		team1 = cv[3]
		team2 = cv[4]
		league = cv[2]
		if team1 not in teams:
			add_team(teams, team1, league)

		if team2 not in teams:
			add_team(teams, team2, league)

		date = cv[0].split("-")
		year = date[0]

		if year not in teams[team1].years:
			add_year(teams, team1, year)

		if year not in teams[team2].years:
			add_year(teams, team2, year)

		if score1 > score2:
			teams[team1].win(year)	#.wins += 1
			teams[team2].loss(year) #loss += 1
		elif score1 < score2:
			teams[team1].loss(year) # += 1
			teams[team2].win(year) # += 1
		elif score1 == score2:
			teams[team1].draw(year) #s += 1
			teams[team2].draw(year) #s += 1

	wfile = open("fifa_team.csv", "w")

	for key, team in teams.items():
		team.w2file(wfile)

	stats.close()
	wfile.close()
