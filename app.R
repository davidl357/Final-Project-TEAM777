library("rsconnect")
library("shiny")
library("dplyr")
library("ggplot2")
library("httr")
library("jsonlite")
library("knitr")
library(RColorBrewer)

base.url <- "http://api.steampowered.com/"
key <- paste0("?key=", api.key)

# Get List of Games and arrange in order
get.owned.games <- "IPlayerService/GetOwnedGames/v0001/"
steam.id <- "76561198064703938"
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
                  min = 0, max = 20, value = 5)
  
    ),
  
  # navigation
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  
        tabPanel("Table", tableOutput("top.table")),
        tabPanel("Graph", plotOutput("games.graph"))
        
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
          fill = top.games$`Total Game Time (in hours)`), stat = "identity") +
      ylab("Total Game Time (in hours)") +
      xlab("Game") +
      ggtitle("Top Games Played") +
      theme(axis.text.x = element_text(angle = 70, vjust = .5)) +
      scale_fill_continuous(name="Total Hours\n Played")
  })
  
}

shinyApp(my.ui, my.server)

