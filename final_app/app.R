library(shiny)
library(haven)
library(sjlabelled)
library(rsconnect)
library(shinythemes)
library(plotly)
library(knitr)
library(scales)
library(stargazer)
library(tidyverse)

data <- read_rds("final_data.rds")

# Define UI for random distribution app ----
ui <- fluidPage(theme = shinytheme("flatly"),
  
  # App title ----
  navbarPage("U.S. Olympic Performance and Voter Turnout Rates for Presidential/Midterm Elections",
  
    tabPanel("Graph",
             # Sidebar layout with input and output definitions ----
             sidebarLayout(
               # Sidebar panel for inputs ----
               sidebarPanel(
                 selectInput("x_axis",
                             "Independent Variable:",
                             choices = c("Total Medal Rank" = "rank",
                                         "Gold Medal Percentage" = "gold_percent",
                                         "Gold Medal Change in Percentage" = "gold_change",
                                         "Total Medal Percentage" = "medal_percent",
                                         "Total Medal Change in Percentage" = "total_change")),
                 
                 tags$h6(helpText("\"Total Medal Rank\" refers to the U.S.'s standing in the country rankings by total medal count. \"Gold Medal Percentage\" refers to the absolute percentage of gold medals awarded in a given Olympic Games the U.S. was awarded while \" Gold Medal Change in Percentage\" refers to the difference between this years gold medal percentage and that of the Olympics four years prior. Total Medal variables follow the same pattern as Gold Medal variables.")),
                 # br() element to introduce extra vertical spacing ----
                 br(),
                 
                 # Input: menu for the number of observations to generate ----
                 radioButtons("olympics",
                              "Olympic Season:",
                              choices = c("Summer" = "summer",
                                          "Winter" = "winter")),
                 
                 tags$h6(helpText("\"Summer\" refers to the Summer Olympic Games contested every 4 years while \"Winter\" refers to the Winter Olympic Games contested every 4 years.")),
                 # br() element to introduce extra vertical spacing ----
                 br(),
                 
                 # Input: Select the random distribution type ----
                 radioButtons("model_type", 
                              "Dependent Variable:",
                              c("Percentage" = "percent",
                                "Change in Percentage" = "percent_change")),
                 
                 tags$h6(helpText("\"Percent\" refers to the absolute percentage of the U.S. voting age population who voted in a given year's election. \"Change in Percentage\" refers to the difference between this years election and the election four years prior.")),
                 # br() element to introduce extra vertical spacing ----
                 br(),
                 
                 # create checkbox for linear model
                 checkboxInput("line", 
                               label = "Add linear model",
                               value = TRUE)
               ),
             
               # Main panel for displaying outputs ----
               mainPanel(
                 plotOutput("plot")
               )
             )
    ),
    
    tabPanel("Models",
             # Sidebar layout with input and output definitions ----
             sidebarLayout(
               # Sidebar panel for inputs ----
               sidebarPanel(
                 # Input: menu for the number of observations to generate ----
                 radioButtons("season",
                              "Olympic Season:",
                              choices = c("Summer" = "summer",
                                          "Winter" = "winter")),
                 
                 tags$h6(helpText("\"Summer\" refers to the Summer Olympic Games contested every 4 years while \"Winter\" refers to the Winter Olympic Games contested every 4 years.")),
                 # br() element to introduce extra vertical spacing ----
                 br(),
                 
                 # Input: Select the random distribution type ----
                 radioButtons("response", 
                              "Dependent Variable:",
                              c("Percentage" = "percent",
                                "Change in Percentage" = "percent_change")),
                 
                 tags$h6(helpText("\"Percent\" refers to the absolute percentage of the U.S. voting age population who voted in a given year's election. \"Change in Percentage\" refers to the difference between this years election and the election four years prior.")),
                 # br() element to introduce extra vertical spacing ----
                 br()
               ),
               
               # Main panel for displaying outputs ----
               mainPanel(
                 htmlOutput("model")
               )
             )
    ),
    
    tabPanel("Insights",
             # Sidebar layout with input and output definitions ----
             sidebarLayout(
               # Sidebar panel for inputs ----
               sidebarPanel(
                 selectInput("year",
                             "Years of Note:",
                             choices = c("1936" = 1936,
                                         "1960" = 1960,
                                         "1968" = 1968,
                                         "1972" = 1972,
                                         "1980" = 1980,
                                         "1984" = 1984,
                                         "1988" = 1988,
                                         "1994" = 1994,
                                         "1996" = 1996,
                                         "2000" = 2000,
                                         "2002" = 2002,
                                         "2008" = 2008)),
                 tags$h6(helpText("* Years listed above are years for which the United States had one or more instances of surprising results/upsets in their favor. For each year there is a brief description of the surprising result, a link to find more information on that instance of the Olympic Games, and voter turnout information for that year."))
                 ),
               
               # Main panel for displaying outputs ----
               mainPanel(
                 htmlOutput("insights")
               )
             )
    )
    )
)

# Define server logic for random distribution app ----
server <- function(input, output) {
  
  x_label <- reactive({
    req(input$x_axis)
    if(input$x_axis == "rank"){
      x_label <- "Total Medal Rank"
    } else if(input$x_axis == "gold_percent"){
      x_label <- "Gold Medal Percentage"
    } else if(input$x_axis == "gold_change"){
      x_label <- "Gold Medal Change in Percentage"
    } else if(input$x_axis == "medal_percent"){
      x_label <- "Total Medal Percentage"
    } else if(input$x_axis == "total_change"){
      x_label <- "Total Medal Change in Percentage"
    }})
  
  y_label <- reactive({
    req(input$model_type)
    if(input$model_type == "percent"){
      y_label <- "Percentage"
    } else if(input$model_type == "percent_change"){
      y_label <- "Change in Percentage"
    }})
  
  # Generate a plot of the data ----
  # Also uses the inputs to build the plot label. Note that the
  # dependencies on the inputs and the data reactive expression are
  # both tracked, and all expressions are called in the sequence
  # implied by the dependency graph.
  output$plot <- renderPlot({
    if (input$line == TRUE) {
      data %>%
        filter(olympics == input$olympics) %>% 
        ggplot(aes_string(x = input$x_axis, y = input$model_type)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE) + 
        labs(x = x_label(),
             y = y_label(),
             title = "Voter Turnout in Relation to Various Measures of Olympic Success",
             subtitle = " X-Variables are each percentage point numbers (other than rank) so that conclusions can be drawn and related across variables.")
    }
    else {
      data %>% 
        filter(olympics == input$olympics) %>%
        ggplot(aes_string(x = input$x_axis, y = input$model_type)) +
        geom_point() +
        labs(x = x_label(),
             y = y_label(),
             title = "Voter Turnout in Relation to Various Measures of Olympic Success",
             subtitle = " X-Variables are each percentage point numbers (other than rank) so that conclusions can be drawn and related across variables.")
    }
  })
  
  # Generate a regression table of the data ----
  output$model <- renderUI({
    
    lm_data <- data %>% 
      filter(olympics == input$season)
    
    if (input$response == "percent") {
      HTML(stargazer(lm(data = lm_data, 
                        percent ~ gold_percent + gold_change + medal_percent + total_change), 
                    type = "html",
                    covariate.labels = c("Total Medal Rank", "Percent of Total Gold Medals", "Percent Change in Gold Medals", "Percent of Total Medals", "Percent Change in Total Medals")
                    )
         )
    }
    else {
      HTML(stargazer(lm(data = lm_data, 
                        percent_change ~ gold_percent + gold_change + medal_percent + total_change), 
                     type = "html",
                     covariate.labels = c("Total Medal Rank", "Percent of Total Gold Medals", "Percent Change in Gold Medals", "Percent of Total Medals", "Percent Change in Total Medals")
      )
      )
    }
    
  })
  
  # Generate an HTML table view of the data ----
  output$insights <- renderUI({
    
    if (input$year == 1936) {
      str1 = "<b>1936</b>"
      str2 = "In 1936, American Jesse Owens won four gold medals after taking the 100 meters, 200 meters, long jump, and 4×100 meter relay events. Hosted in Berlin, the 1936 Summer Games were to be a showcase by Adolf Hitler of the superiority of the Aryan race, a showcase smashed by Owens' prolific success. Owens went on to go down both in Olympic and sporting history."
      str3 = a("Read more here.", href = "https://www.olympic.org/berlin-1936")
      str4 = "The Voter Turnout Rate was 60.9% and the Change from Previous Voter Turnout Rate was an increase of 4.1 percentage points."
      
      HTML(paste(str1, str2, str3, str4, sep = '<br/><br/>'))
    }
    
    else if (input$year == 1960) {
      str1 = "<b>1960</b>"
      str2 = "In 1960, American Cassius Cay won gold in the light heavyweight boxing division after winning all four of his matches, three of them on unanimous decisions. Clay then turned pro a month later taking the name we know him to have today: Muhammad Ali. In the final, Clay fought and defeated three-time European champion Zbigniew Pietrzykowski of Poland."
      str3 = a("Read more here.", href = "https://www.olympic.org/rome-1960")
      str4 = "The Voter Turnout Rate was 64.8% and the Change from Previous Voter Turnout Rate was an increase of 3.6 percentage points."
      
      HTML(paste(str1, str2, str3, str4, sep = '<br/><br/>'))
    }
    
    else if (input$year == 1968) {
      str1 = "<b>1968 Summer</b>"
      str2 = "In the Summer of 1968, American Dick Fosbury introduced a revolutionary technique to the high jump called the \"Fosbury Flop\" and won gold in his event, changing it forever. Also, American Bob Beamon won gold in the long jump setting a world record of 8.90m that would last for the next 22 years."
      str3 = a("Read more here.", href = "https://www.olympic.org/mexico-1968")
      str4 = "<b>1968 Winter</b>"
      str5 = "In the Winter of 1968, American Peggy Fleming won America's only gold medal in figure skating. While seemingly unimportant, Fleming was part of a new wave of AMerican figure skaters building the team after all its members, including her coach, died in a plane crash heading to the 1961 World Championships."
      str6 = a("Read more here.", href = "https://www.olympic.org/grenoble-1968")
      str7 = "The Voter Turnout Rate was 60.5% and the Change from Previous Voter Turnout Rate was a decrease of 1.5 percentage points."
      
      HTML(paste(str1, str2, str3, str4, str5, str6, str7, sep = '<br/><br/>'))
    }
    
    else if (input$year == 1972) {
      str1 = "<b>1972</b>"
      str2 = "In 1972, American Mark Spitz won gold in seven swimming events while simultaneously breaking seven world records. Until Michael Phelps dominated the 2008 Olympics in Beijing, Spitz had won the most golds in a single Olympics and was the most decorated athlete in the history of the Summer Games."
      str3 = a("Read more here.", href = "https://www.olympic.org/munich-1972")
      str4 = "The Voter Turnout Rate was 56.4% and the Change from Previous Voter Turnout Rate was a decrease of 4.1 percentage points."
      
      HTML(paste(str1, str2, str3, str4, sep = '<br/><br/>'))
    }
    
    else if (input$year == 1980) {
      str1 = "<b>1980</b>"
      str2 = "In 1980, The American Men's Hockey Team defied unbelievable odds to defeat the USSR in a semifinal game now known as \"The Miracle\" before defeating Finland in the finals to win gold. This game came during the heat of the Cold War with the U.S. as heavy underdogs and has been called both the greatest moment in U.S. Olympic history and U.S. sports history. Also, not to be forgotten, Eric Heiden won and set Olympic record in all five speed skating events becoming the first person in Olympic history to win five gold medals in individual events at the same Olympics."
      str3 = a("Read more here.", href = "https://www.olympic.org/lake-placid-1980")
      str4 = "The Voter Turnout Rate was 54.7% and the Change from Previous Voter Turnout Rate was a decrease of 0.3 percentage points."
      
      HTML(paste(str1, str2, str3, str4, sep = '<br/><br/>'))
    }
    
    else if (input$year == 1984) {
      str1 = "<b>1984</b>"
      str2 = "In 1984, American Mary Lou Retton became the first American woman to win a gold medal in all-around gymnastics, winning silver medals in the team event and vault and bronze medals in the floor exercise and uneven bars competitions. She also was Sports Illustrated's \"Sportswoman of the Year\". Also, American Carl Lewis matched Jesse Owens' 1936 performance winning gold medals in the same four events: 100m, 200m, 4x100m relay and long jump."
      str3 = a("Read more here.", href = "https://www.olympic.org/los-angeles-1984")
      str4 = "The Voter Turnout Rate was 55.9% and the Change from Previous Voter Turnout Rate was an increase of 1.2 percentage points."
      
      HTML(paste(str1, str2, str3, str4, sep = '<br/><br/>'))
    }
    
    else if (input$year == 1988) {
      str1 = "<b>1988 Summer</b>"
      str2 = "In the Summer of 1988, American Greg Louganis became the first male diver to sweep the diving events two Olympic Games in a row. Fellow American Florence Griffith Joyner earned herself the nickname the \"fastest woman in the world\" winning three gold medals setting records in the 100 meter and 200 meter events."
      str3 = a("Read more here.", href = "https://www.olympic.org/seoul-1988")
      str4 = "<b>1988 Winter</b>"
      str5 = "In the Winter of 1988, American Brian Boitano won a gold medal in figure skating. Competing in his rival's home country, Boitano was able to defeat Canadian Brian Orser because of his technical scores even though four judges voted for Orser while three voted for Boitano."
      str6 = a("Read more here.", href = "https://www.olympic.org/calgary-1988")
      str7 = "The Voter Turnout Rate was 53% and the Change from Previous Voter Turnout Rate was a decrease of 2.9 percentage points."
      
      HTML(paste(str1, str2, str3, str4, str5, str6, str7, sep = '<br/><br/>'))
    }
    
    else if (input$year == 1994) {
      str1 = "<b>1994</b>"
      str2 = "In 1994, American speed skater Bonnie Blair won her third gold in the 500m speed skating race and set a workd record in the 1000m. 1994 was also the year of the infamous Nancy/Tonya controversy."
      str3 = a("Read more here.", href = "https://www.olympic.org/lillehammer-1994")
      str4 = "The Voter Turnout Rate (midterm election) was 40.9% and the Change from Previous Voter Turnout Rate was an increase of 2.4 percentage points."
      
      HTML(paste(str1, str2, str3, str4, sep = '<br/><br/>'))
    }
    
    else if (input$year == 1996) {
      str1 = "<b>1996</b>"
      str2 = "In 1996, American gymnast Kerri Strug earned America it's first-ever gold in the gymnastics team event, an achievment she acomplished after injuring her ankle and tearing multiple ligaments after her first vault attempt. American runner Michael Johnson also made Olympic history becoming the first athlete to ever win the 200m and 400m events in the same year, setting records in both events. Muhammad Ali's lighting of the Olympic torch this year also went down as one of the most emotional moments in sports history."
      str3 = a("Read more here.", href = "https://www.olympic.org/atlanta-1996")
      str4 = "The Voter Turnout Rate was 51.4% and the Change from Previous Voter Turnout Rate was a decrease of 6.6 percentage points."
      
      HTML(paste(str1, str2, str3, str4, sep = '<br/><br/>'))
    }
    
    else if (input$year == 2000) {
      str1 = "<b>2000</b>"
      str2 = "In 2000, American wrestler Rulon Gardner wrestled Russian Aleksandr Karelin, a man who had not lost a competition in 15 years and had won three straight gold medals. He pulled off one of the biggest upsets in Olympics history winning gold in the event."
      str3 = a("Read more here.", href = "https://www.olympic.org/sydney-2000")
      str4 = "The Voter Turnout Rate was 54.2% and the Change from Previous Voter Turnout Rate was an increase of 2.8 percentage points."
      
      HTML(paste(str1, str2, str3, str4, sep = '<br/><br/>'))
    }
    
    else if (input$year == 2002) {
      str1 = "<b>2002</b>"
      str2 = "In 2002, American speed skater Apolo Ohno came around the final turn of the short track competition in first place but fell, sliding across the finish in second place with a sliced leg. Later that week, the South Korean skater initally awarded gold was disqualified for an illegal pass leaving Ohno with the gold medal."
      str3 = a("Read more here.", href = "https://www.olympic.org/salt-lake-city-2002")
      str4 = "The Voter Turnout Rate (midterm election) was 39.7% and the Change from Previous Voter Turnout Rate was an increase of 1.8 percentage points."
      
      HTML(paste(str1, str2, str3, str4, sep = '<br/><br/>'))
    }
    
    else {
      str1 = "<b>2008</b>"
      str2 = "In 2008, American swimmer Michael Phelps passed Mark Spitz's 1972 record winning eight gold medals. Phelps and the American Team's performance in the 4x100m relay, which won him his eight and final medal, resulted in the setting of a new world record."
      str3 = a("Read more here.", href = "https://www.olympic.org/beijing-2008")
      str4 = "The Voter Turnout Rate was 62.7% and the Change from Previous Voter Turnout Rate was an increase of 2.1 percentage points."
      
      HTML(paste(str1, str2, str3, str4, sep = '<br/><br/>'))
    }
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)