#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(gridExtra)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Olympic Medal Data for G7 Countries"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput("country",
                     "Select a Country:",
                     choices = c("Canada" = "CAN",
                                 "France" = "FRA",
                                 "Germany" = "GER",
                                 "Italy" = "ITA",
                                 "Japan" = "JPN",
                                 "United Kingdom" = "GBR",
                                 "United States" = "USA")
                     )
         ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("linePlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$linePlot <- renderPlot({
      
     
      summer <- read_rds("summer_shiny.rds") %>% 
       filter(Country == input$country) %>% 
       ggplot(aes(x = Year, y = Gold)) + 
       geom_line() +
       labs(title = "Number of Gold Medals per Country (Summer Olympics)",
            subtitle = "From 1896 Athens Games to 2012 London Games",
            x = "Year",
            y = "Number of Gold Medals")
     
     winter <- read_rds("winter_shiny.rds") %>% 
       filter(Country == input$country) %>% 
       ggplot(aes(x = Year, y = Gold)) + 
       geom_line() +
       labs(title = "Number of Gold Medals per Country (Winter Olympics)",
            subtitle = "From 1924 Chamonix Games to 2014 Sochi Games",
            x = "Year",
            y = "Number of Gold Medals")
     
     grid.arrange(summer,winter, ncol=1)
      
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

