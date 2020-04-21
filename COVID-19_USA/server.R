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

# Download data
path = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/'

us.covid.cases <- read_csv(url(paste0(path, "time_series_covid19_confirmed_US.csv")))
us.covid.deaths <- read_csv(url(paste0(path, "time_series_covid19_deaths_US.csv")))

USA_Capitals <- read_rds(path = "USA_Capitals.rds")

us.covid.df <- us.covid.cases %>%
    gather(date, cases, -c(UID:Combined_Key)) %>%
    select(Admin2:cases) %>%
    left_join(., us.covid.deaths %>%
                  gather(date, deaths, -c(UID:Population)) %>%
                  select(Combined_Key:deaths),
              by = c("Combined_Key", "date")) %>%
    gather(type, cum, c(cases, deaths)) %>%
    rename(Long = Long_) %>%
    group_by(Province_State, Country_Region, date, type) %>%
    summarise(Population = sum(Population, na.rm = TRUE),
              cum = sum(cum, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(color = type,
           type = recode_factor(type,
                                `cases` = "Confirmed cases",
                                `deaths` = "Deaths"),
           date = as.Date(mdy(date)),
           color = as.character(recode_factor(color,
                                              `cases` = "#a6611a",
                                              `deaths` = '#ca0020'))) %>%
    left_join(., USA_Capitals %>% 
                  mutate(Province_State = as.character(Province_State)), 
              by = "Province_State") %>%
    mutate(Lat = ifelse(Province_State == "Tennessee", 36.17446, Lat),
           Long = ifelse(Province_State == "Tennessee", -86.76796, Long),
           
           Lat = ifelse(Province_State == "American Samoa", -14.271, Lat),
           Long = ifelse(Province_State == "American Samoa", -170.132, Long),
           
           Lat = ifelse(Province_State == "Diamond Princess", 0, Lat),
           Long = ifelse(Province_State == "Diamond Princess", 0, Long),
           
           Lat = ifelse(Province_State == "Grand Princess", 0, Lat),
           Long = ifelse(Province_State == "Grand Princess", 0, Long),
           
           Lat = ifelse(Province_State == "Guam", 13.4443, Lat),
           Long = ifelse(Province_State == "Guam", 144.7937, Long),
           
           Lat = ifelse(Province_State == "Northern Mariana Islands", 15.0979, Lat),
           Long = ifelse(Province_State == "Northern Mariana Islands", 145.6739, Long),
           
           Lat = ifelse(Province_State == "Puerto Rico", 18.2208, Lat),
           Long = ifelse(Province_State == "Puerto Rico", -66.5901, Long),
           
           Lat = ifelse(Province_State == "Virgin Islands", 18.3358, Lat),
           Long = ifelse(Province_State == "Virgin Islands", -64.8963, Long),
           
           
           City_Name = ifelse(Province_State == "Tennessee",
                              "Nashville", City_Name),
           cum_perMiPop = round((cum/Population) * 10^6,0)) %>%
    arrange(Province_State, type, date) %>%
    group_by(Province_State, type) %>%
    filter(cum > 0) %>%
    mutate(daily = cum-lag(cum),
           day = row_number()) %>%
    ungroup() %>%
    mutate(daily = ifelse(is.na(daily), cum, daily))




# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    output$s.name.us <- renderUI({
        selectInput("sname", "Choose state", choices = unique(pull(us.covid.df, Province_State)),
                    multiple = TRUE, selected = "New York")
    })
    
    output$t.ype.us <- renderUI({
        selectInput("type.us", "Type of statistics", choices = unique(pull(us.covid.df, type)),
                    selected = "Confirmed cases")
    })
    

    output$text.date.us <- renderText({
        paste0("Last update: ", format(max(unique(us.covid.df$date)), "%d %b %Y"))
    })
    
    output$text.ustot <- renderText({
        paste0("Total ", input$type.us, " (US): ",  sum(data.mapcum.us()$cum))
    })
    
    output$text.statespectot <- renderText({
        paste0("Total ", input$type.us, " (",
               paste(unique(data.cspecmapcum.us()$Province_State), collapse = ", "),
               "): ",  sum(data.cspecmapcum.us()$cum))
        
    })
    
    output$text.statespeccum <- renderText({
        paste0("Actual number of ", input$type.us)
        
    })
    
    output$text.statespeccumlog <- renderText({
        paste0("Log of actual number of ", input$type.us)
        
    })
    
    output$text.statespec.cumdays <- renderText({
        paste0("Days and cases since 1st ", input$type.us, " recorded")
        
    })
    
    output$text.statespec.cumpermipopu <- renderText({
        paste0(input$type.us, " in per million of population")
        
    })
    
    output$text.totcumdaily.us <- renderText({
        paste0("Actual number of ", input$type.us, " in US")
        
    })
    

    
    output$plot10.us <- renderPlotly(
        
        data.mapcum.us() %>%
            arrange(desc(cum)) %>%
            head(10) %>%
            plot_ly(x = ~cum, y = ~reorder(Province_State, cum),
                    name = "",
                    hovertemplate = "",
                    type = 'bar', orientation = 'h',
                    text = ~cum, textposition = 'auto',
                    opacity=0.6,
                    marker = list(color = ~color)) %>%
            layout(legend = list(orientation = 'h'),
                   xaxis =  list(title = "",
                                 zeroline = FALSE,
                                 showline = FALSE,
                                 showticklabels = FALSE,
                                 showgrid = FALSE),
                   yaxis = list(title = ""))
        
        
    )
    
    
    data.cumdaily.us <- reactive({
        
        us.covid.df %>%
            filter(type %in% input$type.us) %>% 
            filter(Province_State %in% input$sname)
        
    })
    
    
    output$plotstatespec.cumdaily.us <- renderPlotly(
        # Cumulative
        data.cumdaily.us() %>%  
            plot_ly(x = ~date, y=~cum, name = ~Province_State,
                    type = 'scatter', mode = 'lines', 
                    opacity=0.7, 
                    line = list(color = ~color,
                                width = 2),
                    linetype = ~Province_State
            ) %>% 
            layout(xaxis = list(title = ""),
                   yaxis = list(title = ""),
                   legend = list(orientation = 'h'))
        
        
    )
    
    output$plotstatespec.cumdailylog.us <- renderPlotly(
        # Cumulative
        data.cumdaily.us() %>%  
            plot_ly(x = ~date, y=~log(cum), name = ~Province_State,
                    type = 'scatter', mode = 'lines', 
                    opacity=0.7, 
                    line = list(color = ~color,
                                width = 2),
                    linetype = ~Province_State
            ) %>% 
            layout(xaxis = list(title = ""),
                   yaxis = list(title = ""),
                   legend = list(orientation = 'h'))
        
        
    )
    
    output$plotstatespec.cumdailyday.us <- renderPlotly(
        # Cumulative
        data.cumdaily.us() %>%  
            plot_ly(x = ~day, y=~cum, name = ~Province_State,
                    type = 'scatter', mode = 'lines', 
                    opacity=0.7, 
                    line = list(color = ~color,
                                width = 2),
                    linetype = ~Province_State
            ) %>% 
            layout(xaxis = list(title = "Days"),
                   yaxis = list(title = ""),
                   legend = list(orientation = 'h'))
        
        
    )
    
    output$plotstatespec.cumpermipopu <- renderPlotly(
        # Cumulative
        data.cumdaily.us() %>%  
            plot_ly(x = ~date, y=~cum_perMiPop, name = ~Province_State,
                    type = 'scatter', mode = 'lines', 
                    opacity=0.7, 
                    line = list(color = ~color,
                                width = 2),
                    linetype = ~Province_State
            ) %>% 
            layout(xaxis = list(title = "Days"),
                   yaxis = list(title = ""),
                   legend = list(orientation = 'h'))
        
        
    )
    
    
    data.totcumdaily.us <- reactive({
        
        us.covid.df %>% 
            group_by(type, date, color) %>% 
            summarise(cum = sum(cum, na.rm = TRUE)) %>% 
            ungroup %>%
            filter(type %in% input$type.us)
        
    })
    
    output$plot.totcumdaily.us <- renderPlotly(
        # Cumulative
        data.totcumdaily.us() %>%
            plot_ly(x = ~date, y=~cum, name = "",
                    type = 'scatter', mode = 'lines', 
                    opacity=0.7, 
                    line = list(color = ~color,
                                width = 2)
            ) %>% 
            layout(xaxis = list(title = ""),
                   yaxis = list(title = ""))
        
        
    )
    
    
    data.mapcum.us <- reactive({
        
        us.covid.df %>%
            filter(date == max(date)) %>%
            mutate(label = paste0(type, ": ", as.character(cum)),
                   Combined_Key = paste(Province_State, "US", sep = ", ")) %>%
            group_by(Combined_Key) %>%
            mutate(label = paste(label, collapse = "<br>")) %>%
            ungroup() %>%
            filter(type %in% input$type.us)
        
    })
    
    data.cspecmapcum.us <- reactive({
        
        data.mapcum.us() %>%
            filter(Province_State %in% input$sname)
        
    })
    
    # Create the map
    output$mapcum.us <- renderLeaflet({
        
        leaflet(data.mapcum.us()) %>%
            addTiles() %>%
            setView(lng = -98, 
                    lat = 30, 
                    zoom = 4) %>%
            addPulseMarkers(lng = data.cspecmapcum.us()$Long,
                            lat = data.cspecmapcum.us()$Lat,
                            icon = makePulseIcon(heartbeat = .5,
                                                 color = ~data.cspecmapcum.us()$color),
                            label = ~data.cspecmapcum.us()$Combined_Key,
                            popup = ~paste("State: <b>", data.cspecmapcum.us()$Combined_Key, "</b> <br>",
                                           "Date: ", format(data.cspecmapcum.us()$date, "%d %b %Y"), "<br>",
                                           data.cspecmapcum.us()$label)
            ) %>%
            addCircleMarkers(~Long, ~Lat,
                             popup = ~paste("State: <b>", Combined_Key, "</b> <br>",
                                            "Date: ", format(date, "%d %b %Y"), "<br>",
                                            label),
                             label = ~Combined_Key,
                             color = ~color,
                             radius = ~log(cum)*1.3,
                             stroke = F,
                             fillOpacity = 0.6, weight = 15)  %>%
            addEasyButton(easyButton(
                icon    = "glyphicon glyphicon-globe", title = "Reset zoom",
                onClick = JS("function(btn, map){ map.setView([30, -98], 4); }")))
        
        
    })
    
    
    
})
