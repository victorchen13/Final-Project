#load in all packages that I will need throughout the app

library(shiny)
library(ggplot2)
library(shinythemes)
library(nflscrapR)
library(readxl)
library(plotly)
library(tidyverse)
library(gt)
library(DT)


active_player_data <- readRDS("active_player_data")

pbp_2018 <- readRDS("play_by_play_2018")

bengals_games <- readRDS("bengals_games")

# nfl_teamcolors <- teamcolors %>% filter(league == "nfl")
# cin_color <- nfl_teamcolors %>%
#   filter(name == "Cincinnati Bengals") %>%
#   pull(secondary)
# ind_color <- nfl_teamcolors %>%
#   filter(name == "Indianapolis Colts") %>%
#   pull(primary)
# bal_color <- nfl_teamcolors %>%
#   filter(name == "Baltimore Ravens") %>%
#   pull(primary)
# car_color <- nfl_teamcolors %>%
#   filter(name == "Carolina Panthers") %>%
#   pull(primary)
# atl_color <- nfl_teamcolors %>%
#   filter(name == "Atlanta Falcons") %>%
#   pull(primary)
# mia_color <- nfl_teamcolors %>%
#   filter(name == "Indianapolis Colts") %>%
#   pull(primary)

# Set Interface 

ui <- 
    fluidPage(theme = shinytheme("united"),
                navbarPage("Cincinnati Bengals Football Analysis",
                 tabPanel("About", includeMarkdown("about.md")),
                 tabPanel("Bengals Win Probability",
                          selectInput("week", h5("Week to Analyze"), selected = 1, choices = 1:16),
                          tabPanel("Win Probability",

                                   h6("Due to an error in the NFL's API, the win probability is missing for Week 9, CIN vs NO"),

                                   br(),
                                   
                                   dataTableOutput("score_table"),

                                   fluidRow(width = 10, height = "80%", plotlyOutput("win_prob")))),
                          
                 tabPanel("Bengals Trend Explorer",
                               tabsetPanel(type = "tabs",
                                              tabPanel("Expected Points",
                                                fluidRow(width = 10, height = "80%", plotlyOutput("explore_ep"))),
                                                    tabPanel("Win Probability",
                                                fluidRow(width = 10, height = "80%", plotlyOutput("explore_wp"))),
                                                    tabPanel("Passes vs Rushes - Density",
                                                fluidRow(width = 10, height = "80%", plotlyOutput("explore_pass_and_rush"))),
                                                     tabPanel("Passes vs Rushes - Scatter",
                                                fluidRow(width = 10, height = "80%", plotlyOutput("explore_pass_rush_scatter")))
                                                             )),
                 tabPanel("Play Call Summaries",
                          tabsetPanel(
                            
                            tabPanel("Run Play Analysis",
                                     
                                     h3("What is the Distribution of Yards Gained across Run Plays"),
                                     
                                     br(),
                                     
                                     sidebarPanel(h4("Run Plays")),
                                     
                                     mainPanel(plotlyOutput("runPlot"))),
                            
                            tabPanel("Pass Play Analysis",
                                     
                                     h3("What is the Distribution of Yards Gained across Pass Plays"),
                                     
                                     br(),
                                     
                                     sidebarPanel(h4("Pass")),
                                     
                                     mainPanel(plotlyOutput("passPlot")))
                            ),
                 tabPanel("NFL Player Analysis",
                          tabsetPanel(
                              
                              tabPanel("Height",
                                       
                                       h3("What Are Height Trends Like For NFL Athletes?"),
                                       
                                       br(),
                                       
                                       sidebarPanel(h4("Height")),
                                       
                                       mainPanel(plotlyOutput("heightPlot"))),
                              
                              tabPanel("Weight",
                                       
                                       h3("What Are Weight Trends Like For NFL Athletes?"),
                                       
                                       br(),
                                       
                                       sidebarPanel(h4("Weight")),
                                       
                                       mainPanel(plotlyOutput("weightPlot")))
                                    )
                                  )
                                )
                              )
                            )

# Define server logic

server <- function(input, output) {
                  
                          output$runPlot <- renderPlotly({
                            p =pbp_2018 %>%
                              filter(play_type == "run") %>% 
                              ggplot(aes(x = yards_gained)) +
                                  geom_density(alpha = 0.7) +
                                  labs(title = "Bengals 2018 Yards Gained Distribution", x = "Yards Gained", y = "Density")
                          }
                          )    
   
                          output$passPlot <- renderPlotly({
                           p = pbp_2018 %>%
                              filter(play_type == "pass") %>% 
                              ggplot(aes(x = yards_gained)) +
                              geom_density(alpha = 0.7) +
                              labs(title = "Bengals 2018 Yards Gained Distribution", x = "Yards Gained", y = "Density")
                          }
                          )    
    
                          output$weightPlot <- renderPlotly({
                           p = active_player_data %>% 
                              ggplot(aes(x = Weight..lbs.)) +
                                  geom_density() +
                                  labs(title = "Player Weight Density", xlab = "Weight", ylab = "Density")
                          })
                          
                          output$heightPlot <- renderPlotly({
                           p =  active_player_data %>% 
                              ggplot(aes(x = Height..inches.)) +
                                  geom_density() +
                                  labs(title = "Player Height Density", xlab = "Height", ylab = "Density")
                          })
                         
                          output$explore_ep <- renderPlotly({
                            p = pbp_2018 %>%
                              group_by(play_type) %>%
                              filter(play_type != "null") %>% 
                              summarize_if(is.numeric, mean, na.rm = T) %>%
                              mutate(play_type = reorder(play_type, ep, mean)) %>%
                              ggplot() +
                              aes(y = ep, x = play_type, group = play_type, fill = play_type)+
                              geom_col(position = 'dodge')+
                              coord_flip()+
                              labs(x = "", y = "", title = "Average Expected Points by Play Type")
                            hide_legend(p)
                          })
                          
                          output$explore_wp <- renderPlotly({
                            p = pbp_2018 %>%
                              group_by(play_type) %>%
                              filter(play_type != "null") %>% 
                              summarize_if(is.numeric, mean, na.rm = T) %>%
                              mutate(play_type = reorder(play_type, wp, mean)) %>%
                              ggplot()+
                              aes(y = wp, x = play_type, group = play_type, fill = play_type)+
                              geom_col(position = 'dodge')+
                              coord_flip()+
                              labs(x = "", y = "", title = "Average Win Probability by Play Type")
                            hide_legend(p)
                          })
                          
                          output$explore_pass_and_rush <- renderPlotly({
                            p = pbp_2018 %>%
                              filter(play_type != "null") %>% 
                              filter(play_type == "run" | play_type == "pass") %>%
                              ggplot()+
                              aes(x = epa, fill = play_type, group = play_type)+
                              geom_density(alpha = .6)+
                              scale_x_continuous(breaks = seq(-10,10,2.5))+
                              labs(x = "Expected Points Added", y = "Percent of Total Plays",
                                   title = "Distribution  of Expected Points Added, Run Plays vs Pass Plays")
                          })
                          
                          output$explore_pass_rush_scatter <- renderPlotly({
                            p = pbp_2018 %>%
                              filter(play_type != "null") %>% 
                              filter(play_type == "run" | play_type == "pass") %>%
                              ggplot()+
                              aes(x = wpa, y = epa, color = play_type)+
                              geom_smooth()+
                              geom_point(alpha = .3)+
                              scale_x_continuous(limits = c(-0.5,0.5))+
                              labs(x = "Win Probability Added",
                                   y = "Expected Points Added",
                                   title = ("Win Probability Added by Expected Points Added"))
                          })
                          
                          winprobtable_reactive = reactive({
                            bengals_games %>%
                              filter(week == input$week) %>%
                              select(home_team, away_team, home_score, away_score)
                          })
                          
                          output$score_table = DT::renderDataTable({
                           datatable(winprobtable_reactive(), colnames=c("Home Team", "Away Team", "Home Score", "Away Score"))
                          })
                          
                          winprob_reactive = reactive({
                           bengals_games %>%
                              filter(week == input$week) %>%
                              pull(game_id) %>%
                              game_play_by_play()
                          })
                          
                          away_team_reactive = reactive({
                            bengals_games %>%
                              filter(week == input$week) %>%
                              pull(away_team)
                          })
                          
                          home_team_reactive = reactive({
                            bengals_games %>%
                              filter(week == input$week) %>%
                              pull(home_team)
                          })
                          
                          away_color_reactive = reactive({
                            bengals_games %>%
                              filter(week == input$week) %>%
                              pull(away_team_color)
                          })
                          
                         home_color_reactive = reactive({
                            bengals_games %>%
                              filter(week == input$week) %>%
                              pull(home_team_color)
                          })
                          
                          output$win_prob <- renderPlotly({
                          p = winprob_reactive() %>%
                            filter(!is.na(Home_WP_pre),
                                   !is.na(Away_WP_pre)) %>%
                                select(TimeSecs,
                                          Home_WP_pre,
                                          Away_WP_pre) %>%
                            gather(team, WPA, -TimeSecs) %>%
                            ggplot(aes(x = TimeSecs, y = WPA, color = team)) +
                            geom_line(size = 2) +
                            geom_hline(yintercept = 0.5, color = "gray", linetype = "dashed") +
                            scale_color_manual(labels = c("Away", "Home"),
                                               values = c(away_color_reactive(),home_color_reactive()),
                                               guide = FALSE) +
                            scale_x_reverse(breaks = seq(0, 3600, 300)) +
                            annotate("text", x = 3000, y = .75, label = away_team_reactive(), color = away_color_reactive(), size = 8) +
                            annotate("text", x = 3000, y = .25, label = home_team_reactive(), color = home_color_reactive(), size = 8) +
                            geom_vline(xintercept = 900, linetype = "dashed", black) + 
                            geom_vline(xintercept = 1800, linetype = "dashed", black) + 
                            geom_vline(xintercept = 2700, linetype = "dashed", black) + 
                            geom_vline(xintercept = 0, linetype = "dashed", black) + 
                            labs(
                              x = "Time Remaining (seconds)",
                              y = "Win Probability",
                              title = paste0("Week ", input$week, " Win Probability"),
                              subtitle = "Indianapolis Colts vs. Cincinnati Bengals, 2018",
                              caption = "Data from nflscrapR"
                            ) + theme_bw()
                          })
                          
}   

    
# Run the application 
shinyApp(ui = ui, server = server)