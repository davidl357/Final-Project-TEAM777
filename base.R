library("httr")
library("jsonlite")
library("knitr")
library("dplyr")
source("steam api key.R")
base.url<-"http://api.steampowered.com/"
key<-paste0("?key=",api.key)

#Get List of Games and arrange in order
get.owned.games<-"IPlayerService/GetOwnedGames/v0001/"
steam.id<-"76561198043898894"
paste0

results<-GET(paste0(base.url,get.owned.games,key,"&steamid=",steam.id,"&format=json&include_appinfo=1&include_played_free_games=1"))
results<-content(results,"text")

results<-fromJSON(results)
my.data <- data.frame(results, stringsAsFactors = FALSE)
my.data <- arrange(my.data, 4)
View(my.data)
# gets total count of games
total.games<-results$response$game_count
results<-results$response$games
names(results)

# gets games with no time played
results<-arrange(results,-playtime_forever)
zero.minutes<-filter(results,playtime_forever==0) %>% nrow()
zero.minutes/nrow(results)


