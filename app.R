library("rsconnect")
library("shiny")
library("dplyr")
library("ggplot2")
library("httr")
library("jsonlite")
library("knitr")
library("tidyr")
library("maps")
library("countrycode")
library("RColorBrewer")

source("steam api key.R")

base.url <- "http://api.steampowered.com/"
key <- paste0("?key=", api.key)

# Get List of Games and arrange in order
get.owned.games <- "IPlayerService/GetOwnedGames/v0001/"
# steam.id <- "76561198064703938" # James
# steam.id <- "76561198043898894" # David
# steam.id <- "76561198042574722" # Jesse

my.ui <- fluidPage(
  
  tags$style("body {background: -webkit-linear-gradient(white, gray);}"),
  
  titlePanel("Statistics About Your Steam Profile"),

  tags$div(

    tags$br(),

    tags$img(src = "http://mediaserver.pulse2.com/uploads/2010/10/440px-Steam_logo.png", width = "440px", height = "126px"),

    tags$br()

  ),
  
  tags$br(), 
  
  textOutput("games.summary"),
  
  tags$body(
    a("Link to lookup Steam ID", href="https://steamid.io/lookup")
  ),
  
  tags$br(),
  
  plotOutput("map"),
  
  sidebarLayout(
    
    # data
    sidebarPanel(
      
      textInput("id", "Input 64 Digit Steam ID", placeholder = "User ID here...", value = "76561197986603983"),
      textOutput("steamid"),
      
      sliderInput("num.games", "Number of Games to include: (for top games)",
                  min = 1, max = 20, value = 20)
  
    ),
  
  # navigation
    mainPanel(
      
      tabsetPanel(type = "tabs",
        tabPanel("Graph", plotOutput("games.graph")),     
        tabPanel("Table (Top games played)", tableOutput("top.table")),
        tabPanel("Table (Games with no playtime)", tableOutput("no.table"))
        
      )
      
    )
  
  )
  
)

my.server <- function(input, output) {
  
  output$top.table <- renderTable({
    
    results <- GET(paste0(base.url,get.owned.games,key,"&steamid=",input$id,"&format=json&include_appinfo=1&include_played_free_games=1"))
    results <- content(results,"text")
    
    results <- fromJSON(results)
    # Converts the JSON into a data frame
    my.data <- data.frame(results, stringsAsFactors = FALSE)
    
    # arranges the data frame by forever playtime
    my.data <- arrange(my.data, -response.games.playtime_forever)
    
    # Converts the total play time from minutes to hours
    my.data <- mutate(my.data, "Total Time Played(in hours)" = round(response.games.playtime_forever / 60, digits = 1))
    my.data <- mutate(my.data, "Total Time Played Last 2 Weeks (in hours)" = round(response.games.playtime_2weeks / 60, digits = 1))
    
    # Games with no playtime
    no.time <- filter(my.data, response.games.playtime_forever == 0)
    no.time <- select(no.time, 3)
    colnames(no.time) <- c("Name of Game")
    no.time.list <- as.list(no.time)
    
    # Only gets the relevant data
    table.data <- select(my.data, 3, 9, 10)
    colnames(table.data) <- c("Name of Game", "Total Game Time (in hours)", "Game Time in the Last 2 Week")
    
    top.table.games <- head(table.data, input$num.games)
    
  })
  
  output$games.graph <- renderPlot({
    
    results <- GET(paste0(base.url,get.owned.games,key,"&steamid=",input$id,"&format=json&include_appinfo=1&include_played_free_games=1"))
    results <- content(results,"text")
    
    results <- fromJSON(results)
    # Converts the JSON into a data frame
    my.data <- data.frame(results, stringsAsFactors = FALSE)
    
    # arranges the data frame by forever playtime
    my.data <- arrange(my.data, -response.games.playtime_forever)
    
    # Converts the total play time from minutes to hours
    my.data <- mutate(my.data, "Total Time Played(in hours)" = round(response.games.playtime_forever / 60, digits = 1))
    my.data <- mutate(my.data, "Total Time Played Last 2 Weeks (in hours)" = round(response.games.playtime_2weeks / 60, digits = 1))
    
    # Only gets the relevant data
    table.data <- select(my.data, 3, 9, 10)
    colnames(table.data) <- c("Name of Game", "Total Game Time (in hours)", "Game Time in the Last 2 Week")
    
    top.games <- head(table.data, input$num.games)
    
    ggplot(data = top.games) +
      geom_bar(mapping = aes(x = `Name of Game`, y = `Total Game Time (in hours)`, 
          fill = `Name of Game`), stat = "identity") +
      ylab("Total Game Time (in hours)") +
      xlab("Game") +
      ggtitle("Top Games Played") +
      theme(axis.title.x=element_text(size=17), # Makes text on graph bigger
            axis.text.x=element_text(size=17),
            axis.title.y=element_text(size=17),
            axis.text.y=element_text(size=17),
            plot.title = element_text(size=20)) +
      guides(fill=FALSE) +
      coord_flip()
    
  })
  
  output$games.summary <- renderText({
    
    # Sets input from slider into a var
    num <- input$num.games
    
    message <- paste0("(THIS APPLICATION WILL NOT WORK WITHOUT A USER ID) \n This table shows the top ", num, " played. The graph shows 
                      the top ", num, " played for your library in lifetime hours. The table, shows the top ", num, " games played 
                      with the name of the game, lifetime hours played, and amount of hours played in the last 2 weeks. (SAMPLE ID: 76561197986603983)")
    
    return(message)
    
  })
  
  output$no.table <- renderTable({
    
    results <- GET(paste0(base.url,get.owned.games,key,"&steamid=",input$id,"&format=json&include_appinfo=1&include_played_free_games=1"))
    results <- content(results,"text")
    
    results <- fromJSON(results)
    # Converts the JSON into a data frame
    my.data <- data.frame(results, stringsAsFactors = FALSE)
    
    # arranges the data frame by forever playtime
    my.data <- arrange(my.data, -response.games.playtime_forever)
    
    # Games with no playtime
    no.time <- filter(my.data, response.games.playtime_forever == 0)
    no.time <- select(no.time, 3)
    colnames(no.time) <- c("Name of Game with Zero Playtime")
    
    no.time
    
  })

  # stores user input steam id 
  output$map <- renderPlot({
    
    get.friend.list<-"ISteamUser/GetFriendList/v0001/"
    results<-GET(paste0(base.url,get.friend.list,key,"&steamid=",input$id))
    results <- content(results, "text")
    results <- fromJSON(results)
    results<-data.frame(results, stringsAsFactors = FALSE)
    results<-flatten(results)
    is.data.frame(results)
    
    test<-c(TRUE, FALSE, FALSE)
    steam.id.from.friends <- results[test]
    steam.id.from.friends<-gather(steam.id.from.friends)
    collected <- steam.id.from.friends$value[1]
    data.frame(steam.id.from.friends, stringsAsFactors = FALSE)
    
    all.except.one<-steam.id.from.friends$value
    nrow(steam.id.from.friends)
    all.except.one<- all.except.one[c(2:100)]
    all.except.one<-all.except.one[complete.cases(all.except.one)]
    for(id in all.except.one){
      collected<-paste0(collected,",",id)
    }
    print(collected)
    
    response.steam <- GET(paste0("http://api.steampowered.com/ISteamUser/GetPlayerSummaries/v0002/?key=3651612D23372B3B62F2F3F6DF2B6C76&steamids=",collected))
    body.steam <- fromJSON(content(response.steam, "text"))
    steam <- data.frame(body.steam)

    # ISO3
    steam.friends.iso3 <- data.frame(countrycode(steam$response.players.loccountrycode, "iso2c", "iso3c"))
    colnames(steam.friends.iso3) <- "FriendISO3"
    steam.friends.iso3 <- na.omit(steam.friends.iso3)
    world.data<-map_data("world")
    world.data$ISO3<-iso.alpha(world.data$region,n=3)
    unique.iso3 <- unique(select(world.data, ISO3))

    temp <- unique.iso3
    
    country.count <- c()
    
    for(x in unique.iso3$ISO3)
    {
      steam.friends.filtered.iso3 <- filter(steam.friends.iso3, FriendISO3 == x)
      iso3.count <- nrow(steam.friends.filtered.iso3)
      country.count[x] <- iso3.count
    }
    
    temp <- mutate(temp, count = country.count)
    
    # Map
    world.data <- left_join(world.data, temp, by = "ISO3")
    
    ggplot(data = world.data) +
      geom_polygon(aes(x = long, y = lat, group = group, fill = cut(count, breaks=c(0:100))))+
      scale_color_brewer(palette = "Set2") + labs(title = "Number of Friends from Around the World", fill = "Number of Friends")
      
  })
  
}

shinyApp(my.ui, my.server)

