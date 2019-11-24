#load in all packages that I will need throughout the app

library(shiny)
library(ggplot2)
library(shinythemes)
library(nflscrapR)
library(readxl)
library(tidyverse)


player_data <- read.csv("Basic_Stats copy.csv")

active_player_data <- player_data %>% 
  filter(Current.Status == "Active")

pbp_2018 <- scrape_season_play_by_play(2018, "reg", team = "CIN")

# Set Interface 

ui <- 
    fluidPage(theme = shinytheme("united"),
                navbarPage("Cincinnati Bengals Football Analysis",
                 tabPanel("About", includeMarkdown("about.md")),
                 tabPanel("Play Call Summaries",
                          tabsetPanel(
                            
                            tabPanel("Run Play Analysis",
                                     
                                     h3("What is the Distribution of Yards Gained across Run Plays"),
                                     
                                     br(),
                                     
                                     sidebarPanel(h4("Run Plays")),
                                     
                                     mainPanel(plotOutput("runPlot"))),
                            
                            tabPanel("Pass Play Analysis",
                                     
                                     h3("What is the Distribution of Yards Gained across Pass Plays"),
                                     
                                     br(),
                                     
                                     sidebarPanel(h4("Pass")),
                                     
                                     mainPanel(plotOutput("passPlot")))
                            ),
                 tabPanel("NFL Player Analysis",
                          tabsetPanel(
                              
                              tabPanel("Height",
                                       
                                       h3("What Are Height Trends Like For NFL Athletes?"),
                                       
                                       br(),
                                       
                                       sidebarPanel(h4("Height")),
                                       
                                       mainPanel(plotOutput("heightPlot"))),
                              
                              tabPanel("Weight",
                                       
                                       h3("What Are Weight Trends Like For NFL Athletes?"),
                                       
                                       br(),
                                       
                                       sidebarPanel(h4("Weight")),
                                       
                                       mainPanel(plotOutput("weightPlot")))
                                    )
                                  )
                                )
                              )
                            )

# Define server logic

server <- function(input, output) {
                  
                          output$runPlot <- renderPlot({
                            pbp_2018 %>%
                              filter(play_type == "run") %>% 
                              ggplot(aes(x = yards_gained)) +
                                  geom_density(alpha = 0.7) +
                                  labs(title = "Bengals 2018 Yards Gained Distribution", x = "Yards Gained", y = "Density")
                          }
                          )    
   
                          output$passPlot <- renderPlot({
                            pbp_2018 %>%
                              filter(play_type == "pass") %>% 
                              ggplot(aes(x = yards_gained)) +
                              geom_density(alpha = 0.7) +
                              labs(title = "Bengals 2018 Yards Gained Distribution", x = "Yards Gained", y = "Density")
                          }
                          )    
    
                          output$weightPlot <- renderPlot({
                            active_player_data %>% 
                              ggplot(aes(x = Weight..lbs.)) +
                                  geom_density() +
                                  labs(title = "Player Weight Density", xlab = "Weight", ylab = "Density")
                          })
                          
                          output$heightPlot <- renderPlot({
                            active_player_data %>% 
                              ggplot(aes(x = Height..inches.)) +
                                  geom_density() +
                                  labs(title = "Player Height Density", xlab = "Height", ylab = "Density")
                          })
}   

    
# Run the application 
shinyApp(ui = ui, server = server)