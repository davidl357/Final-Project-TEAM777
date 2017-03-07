library("rsconnect")
library("shiny")
library("dplyr")
library("ggplot2")
library("httr")
library("jsonlite")
library("knitr")

source("steam api key.R")
base.url <- "http://api.steampowered.com/"
key <- paste0("?key=", api.key)

# Get List of Games and arrange in order
get.owned.games <- "IPlayerService/GetOwnedGames/v0001/"
# steam.id <- "76561198064703938" # James
steam.id <- "76561198043898894" # David
# steam.id <- "76561198042574722" # Jesse
paste0

results <- GET(paste0(base.url,get.owned.games,key,"&steamid=",steam.id,"&format=json&include_appinfo=1&include_played_free_games=1"))
results <- content(results,"text")

results <- fromJSON(results)
# Converts the JSON into a data frame
my.data <- data.frame(results, stringsAsFactors = FALSE)

# arranges the data frame by forever playtime
my.data <- arrange(my.data, -response.games.playtime_forever)

# Converts the total play time from minutes to hours
my.data <- mutate(my.data, "Total Time Played(in hours)" = round(response.games.playtime_forever / 60, digits = 1))

# Only gets the relevant data
table.data <- select(my.data, 3, 8, 9)
colnames(table.data) <- c("Name of Games", "Game Time in the Last 2 Week", "Total Game Time (in hours)")

my.ui <- fluidPage(
  
  titlePanel("Steam api"),
  
  # data
  sidebarLayout(
    
    sidebarPanel(
      
      sliderInput("num.games", "Number of Games to include: ",
                  min = 2, max = 20, value = 20)
  
    ),
  
  # navigation
    mainPanel(
      
      tabsetPanel(type = "tabs",
        tabPanel("Graph", plotOutput("games.graph")),     
        tabPanel("Table", tableOutput("top.table"))
        
      )
      
    )
  
  )
  
)

my.server <- function(input, output) {
  
  output$top.table <- renderTable({
    
    # Top 5 games played
    top.table.games <- head(table.data, input$num.games)
    
  })
  
  output$games.graph <- renderPlot({
    top.games <- head(table.data, input$num.games)
    ggplot(data = top.games) +
      geom_bar(mapping = aes(x = top.games$`Name of Games`, y = top.games$`Total Game Time (in hours)`, 
          fill = top.games$`Name of Games`), stat = "identity") +
      ylab("Total Game Time (in hours)") +
      xlab("Game") +
      ggtitle("Top Games Played") +
      theme(axis.title.x=element_text(size=17),
            axis.text.x=element_text(size=17),
            axis.title.y=element_text(size=17),
            axis.text.y=element_text(size=17),
            plot.title = element_text(size=20)) +
      guides(fill=FALSE) +
      coord_flip()
  })
  
}

shinyApp(my.ui, my.server)

