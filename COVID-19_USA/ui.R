#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# The objective of the application is to provide information on the 
# USA COVID-19  
# 
# Author: Shahadat Hossain   
# Date: April 20, 2020

library(shiny)
library(leaflet)
library(leaflet.extras)
library(tidyverse)
library(readr)
library(lubridate)
library(plotly)

# Define UI for application that draws a histogram
navbarPage("COVID 19", id="nav",
           tabPanel("USA Cum.",
                    div(class="outer",
                        
                        tags$head(
                            # Include our custom CSS
                            includeCSS("styles.css"),
                            includeScript("gomap.js")
                        ),
                        
                        # If not using custom CSS, set height of leafletOutput to a number instead of percent
                        leafletOutput("mapcum.us", width="100%", height="100%"),
                        
                        # Shiny versions prior to 0.11 should use class = "modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = FALSE, top = "6%", right = "auto", left = "2.5%",
                                      bottom = "1.5%", width = "16%", height = "auto",
                                      
                                      h3("Input panel"),
                                      
                                      uiOutput("s.name.us"),
                                      
                                      uiOutput("t.ype.us"),
                                      
                                      uiOutput("stat.us"),
                                      
                                      h3(textOutput("text.date.us")),
                                      h3(textOutput("text.ustot")),
                                      h3(""),
                                      h3(textOutput("text.statespectot")),
                                      h3(""),
                                      h4("Top 10 states:" ),
                                      plotlyOutput('plot10.us', width = "auto", height = "auto"),
                                      h5("By Shahadat Hossain"),
                                      h5("Email: shahadat.h9n@gmail.com"),
                                      h5("Version: 1.0")
                        ),
                        
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = FALSE, top = "6%", right = "1%", left = "auto",
                                      bottom = "1.5%", width = "22%", height = "90%",

                                      h3("Summary"),
                                      h4(textOutput("text.statespeccum")),
                                      plotlyOutput('plotstatespec.cumdaily.us',
                                                   width = "97%", height = "25%"),
                                      
                                      h4(textOutput("text.statespeccumlog")),
                                      plotlyOutput('plotstatespec.cumdailylog.us',
                                                   width = "97%", height = "25%"),

                                      h4(textOutput("text.statespec.cumdays")),
                                      plotlyOutput('plotstatespec.cumdailyday.us',
                                                   width = "97%", height = "25%")
                        ),

                        absolutePanel(id = "controls", class = "panel panel-scroll", fixed = FALSE,
                                      draggable = FALSE, top = "auto", right = "23.5%", left = "auto",
                                      bottom = "1.5%", width = "25%", height = "30%",

                                      h4(textOutput("text.statespec.cumpermipopu")),

                                      plotlyOutput('plotstatespec.cumpermipopu',
                                                   width = "97%", height = "90%")
                        ),

                        absolutePanel(id = "controls", class = "panel panel-scroll", fixed = TRUE,
                                      draggable = FALSE, top = "auto", right = "auto", left = "19%",
                                      bottom = "1.5%", width = "25%", height = "30%",

                                      h4(textOutput("text.totcumdaily.us")),

                                      plotlyOutput('plot.totcumdaily.us',
                                                   width = "97%", height = "90%")
                        ),

                        tags$div(id="cite",
                                 'Source: ', tags$em('https://github.com/CSSEGISandData/COVID-19')
                        )
                    )
                    
                    
           )
)