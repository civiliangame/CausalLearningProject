import csv
import requests
import codecs
from bs4 import BeautifulSoup

# teams = set([])
# types_of_encoding = ["utf8", "cp1252"]
# for encoding_type in types_of_encoding:
#     with codecs.open("C:/Users/nowib/PycharmProjects/CausalLearningProject/fifa-data.csv", encoding=encoding_type, errors="replace") as csvfile:
#         try:
#             readcsv = csv.reader(csvfile)
#             for row in readcsv:
#                 #print(row[9])
#                 teams.add(row[9])
#         except Exception:
#             pass
counter = 0
allteamurls = set([])

while counter <=600:
    url = "https://sofifa.com/teams/club?offset=" + str(counter)
    r = requests.get(url)
    print(url)
    soup = BeautifulSoup(r.content, "lxml")
    #print(soup.prettify())
    for table in soup.find_all("table", {"class":"table table-hover persist-area"}):
        for tr in table.find("tbody").find_all("tr"):
            for a in tr.find_all("a"):
                teamurl = ("https://sofifa.com" + a.get("href"))
                if teamurl[len(teamurl) - 1] == "/":
                    allteamurls.add(teamurl + "live")
    counter +=60




with open("scrapingresults.csv", mode="w", encoding="utf-8") as file:
    output = csv.writer(file, delimiter=",")

    output.writerow(["Parent Team", "Subteam", "Matches Played", "Wins", "Draws", "Losses", "Goals For", "Goals Against", "Goal Difference", "Points"])
    for team in allteamurls:
        r = requests.get(team)
        print(team)
        soup = BeautifulSoup(r.content, "lxml")

        teamName = (soup.find("div", {"class": "info"}).get_text())
        for tr in soup.find("tbody").find_all("tr"):
            td = tr.find_all("td")
            #print(td)
            output.writerow([teamName, td[1].get_text(),td[3].get_text(),td[4].get_text(),td[5].get_text(),td[6].get_text(),td[7].get_text(),td[8].get_text(),td[9].get_text(),td[10].get_text()])
            # tempcounter =
            # for i in range(13):
            #     if i == 2 or i == 11 or i == 12:
            #         continue




