library("httr")
library("jsonlite")
library("knitr")
library("dplyr")
source("steam api key.R")
base.url<-"http://api.steampowered.com/"
key<-paste0("?key=",api.key)



#Get List of Games and arrange in order
get.owned.games<-"IPlayerService/GetOwnedGames/v0001/"
steam.id<-"76561198042574722"
paste0

results<-GET(paste0(base.url,get.owned.games,key,"&steamid=",steam.id,"&format=json&include_appinfo=1&include_played_free_games=1"))
results<-content(results,"text")

results<-fromJSON(results)
total.games<-results$response$game_count
results<-results$response$games
names(results)

results<-arrange(results,-playtime_forever)
zero.minutes<-filter(results,playtime_forever==0) %>% nrow()
zero.minutes/nrow(results)
#Name of Game


