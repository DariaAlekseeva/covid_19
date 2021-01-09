## app.R ##
library(shiny)
library(shinydashboard)
library(dashboardthemes)


library(tidyverse)
library(dplyr)
library(lubridate)
library(plotly)
library(shinydashboard)
library(flexdashboard)
library(zoo)
library(shiny)

library(maps)
library(mapdata)
library(maptools)
library(rgdal)
library(ggmap)
library(ggplot2)
library(rgeos)
library(broom)
library(plyr)
library(DT)
library(feather)

library(heatmaply)
library(tidyverse)
library(memoise)
library(httr)






#' Extracts paginated data by requesting all of the pages
#' and combining the results.
#'
#' @param filters    API filters. See the API documentations for 
#'                   additional information.
#'                   
#' @param structure  Structure parameter. See the API documentations 
#'                   for additional information.
#'                   
#' @return list      Comprehensive list of dictionaries containing all 
#'                   the data for the given ``filter`` and ``structure`.`
get_paginated_data <- function (filters, structure) {
  
  endpoint     <- "https://api.coronavirus.data.gov.uk/v1/data"
  results      <- list()
  current_page <- 1
  
  repeat {
    
    httr::GET(
      url   = endpoint,
      query = list(
        filters   = paste(filters, collapse = ";"),
        structure = jsonlite::toJSON(structure, auto_unbox = TRUE),
        page      = current_page
      ),
      timeout(10)
    ) -> response
    
    # Handle errors:
    if ( response$status_code >= 400 ) {
      err_msg = httr::http_status(response)
      stop(err_msg)
    } else if ( response$status_code == 204 ) {
      break
    }
    
    # Convert response from binary to JSON:
    json_text <- content(response, "text")
    dt        <- jsonlite::fromJSON(json_text)
    results   <- rbind(results, dt$data)
    
    if ( is.null( dt$pagination$`next` ) ){
      break
    }
    
    current_page <- current_page + 1;
    
  }
  
  return(results)
  
}

### NATIONS

# Create filters:
query_filters <- c(
  "areaType=nation"
)

# Create the structure as a list or a list of lists:
query_structure <- list(
  date       = "date", 
  name       = "areaName", 
  code       = "areaCode", 
  cases      = "newCasesByPublishDate",
  deaths = "newDeaths28DaysByPublishDate"
)

result <- get_paginated_data(query_filters, query_structure)


deaths <- as.data.frame(result)




### DATA PROCESSING
#---------------------------------------------------------------------------------------------------------------------------------------------------

  # cases data
  cases <- read_csv('https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv')

  # deaths data
  #deaths <- read_csv('https://coronavirus.data.gov.uk/downloads/csv/coronavirus-deaths_latest.csv') -- old source

  # England population data

  # add population data
  path <- "population"
  population <- read_feather(path)



  # rename columns

  cases <- cases %>%
    dplyr::rename(cases=`Daily lab-confirmed cases`, day =`Specimen date`, area_name='Area name', area_type = `Area type`, id = `Area code`)

  deaths <- deaths %>%
    dplyr::rename(day =`date`, area_name='name') %>%
    mutate(day = as.Date(day))



  # define 1 and 2 weeks ago
  
  one_weeks_ago_end = Sys.Date() - 7
  one_weeks_ago_start = Sys.Date() - 7*2
  two_weeks_ago_end = Sys.Date() - 7*2+1
  two_weeks_ago_start = Sys.Date() - 7*3
  
  
  # identify days corresponding to 1 and 2 weeks ago
  cases <- cases %>%
    mutate(date_range = case_when(day >= one_weeks_ago_start & day < one_weeks_ago_end ~ "last_week",
                                  day >= two_weeks_ago_start & day < two_weeks_ago_end ~ "two_weeks_ago"))


  deaths <- deaths %>%
    mutate(date_range = case_when(day >= one_weeks_ago_start & day < one_weeks_ago_end ~ "last_week",
                                  day >= two_weeks_ago_start & day < two_weeks_ago_end ~ "two_weeks_ago"))
  deaths[is.na(deaths)] <- 0
  


  # create comparison table between two past weeks

  compare_weeks = cases %>%
    dplyr::filter(area_type == "ltla") %>%
    dplyr::group_by(area_name, id, date_range) %>%
    dplyr::summarise(total = sum(cases)) %>%
    spread(date_range, total)

  compare_weeks[is.na(compare_weeks)] <- 0

  compare_weeks =
    compare_weeks %>%
    mutate(delta = last_week - two_weeks_ago,
           delta_pct =  round(((last_week - two_weeks_ago)/two_weeks_ago) * 100,0)) %>%
    arrange(-delta) %>%
    dplyr::select(-"<NA>")

  compare_weeks[is.na(compare_weeks)] <- 0


  compare_weeks = join(population, compare_weeks, by="id")

  compare_weeks[is.na(compare_weeks)] <- 0
  
  compare_weeks =
  compare_weeks %>% mutate(
    
    cases_per_100k_lastweek = round(last_week/compare_weeks$X2018 * 100000,0),
    delta_100k = round((last_week - two_weeks_ago) / X2018 * 100000,1)
  )

  if (nrow(compare_weeks[compare_weeks$delta_pct == Inf,])>0) {
  compare_weeks[compare_weeks$delta_pct == Inf,]$delta_pct <- 100
  }
  
  
  # get all local authorities which had Covid cases

  local = cases %>%
    filter(area_type == "ltla")
  
  
  # this is table data to output as table
  tb = 
    compare_weeks %>% 
    ungroup() %>% 
    select(area_name, cases_per_100k_lastweek, delta_100k,last_week, two_weeks_ago, delta) %>% 
    dplyr::rename(area = "area_name", 
                  `last week` = "last_week", 
                  `2 weeks ago` = "two_weeks_ago", 
                  `delta, n of cases` = "delta",
                  `cases per 100k population last week` = "cases_per_100k_lastweek", 
                  `2 weeks' delta per 100k population` = "delta_100k") %>%  
    arrange(-`cases per 100k population last week`)
  
  
  # this data is for regions plot
  
  regions = cases %>% dplyr::filter(area_type=='region')
  
  cases_per_regions = cases %>% 
    dplyr::filter(area_type == "region") %>% 
    dplyr::group_by(date_range, area_name) %>% 
    dplyr::summarise(total = sum(cases)) %>%
    spread(date_range, total) %>%
    dplyr::select(-"<NA>")%>%
    mutate(pct = (last_week - two_weeks_ago)/two_weeks_ago*100)
  
  
  
  # England cases daily
  
  england = cases %>% dplyr::filter(area_type=='nation') %>%
    dplyr::group_by(day) %>% 
    dplyr::summarise(total = sum(cases))
  
  # deaths by nation
  
  deaths_by_nation = deaths %>% select(day, area_name, deaths) 
  
  
  
  
  
  
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------  



ui <- dashboardPage(
  dashboardHeader(title = "Covid-19 in England"),
  dashboardSidebar(  
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Heatmap by authority", tabName = "map", icon = icon("map"))
    )
  ),
  dashboardBody(
    shinyDashboardThemes(
      theme = "grey_dark"
    ),
    
    tags$head(tags$style(HTML('
    /* toggle button when hovered  */                    
         .skin-blue .main-header .navbar .sidebar-toggle
         {color: #FFFFFF;}
         .selectize-dropdown, .selectize-input, .selectize-input input  
         {color: #FFFFFF;}'
                              )
                         )
              ),
    
    tabItems(

      ### page 1
      
      tabItem(tabName = "dashboard",

      fluidRow(
        box(width = 3, title = "total cases", valueBoxOutput("box_total_eng"), background = "red"),
        box(width = 3, title = "cases last week", valueBoxOutput("box_lw_eng"), background = "red"),
        box(width = 3, title = "total deaths", valueBoxOutput("box_total_d_eng"), background = "red"),
        box(width = 3, title = "deaths last week", valueBoxOutput("box_lw_d_eng"), background = "red")
        
      ),
      
      # Boxes need to be put in a row (or column)
      fluidRow(
        
        box(width = 6,
            valueBoxOutput("box"),
            title = "Choose your borough:",
            selectInput(inputId = "area_name", label = (""), choices = unique(local$area_name), selected = 'Reading', width = "200px"),
            
            plotlyOutput(outputId = "plot", height = "400px")),
        
        box(width = 6,
            title = "Which areas have the highest risk of lockdown?", status = "primary",
            div(style = 'overflow-x: scroll', 
                DT::dataTableOutput("table", height = "500px")))
        
        
      ),
      
      fluidRow(      
        box(width = 12,
            title = "Timeline of daily cases per region",
            dateRangeInput(inputId = "day", label = ("Filter by date to explore how regions affect one another:"), start  = min(regions$day), weekstart = 1),
            plotlyOutput(outputId = "plot_map", height = "500px")
        )
        
      ),
      
      fluidRow(
        
        box(width = 6, 
            title = "Change in cases from previous week, %",
            plotlyOutput("plot_rg_change", height = '500px')
        ),
        
        box(width = 6,
            valueBoxOutput("box_rg"),
            title = "Choose your region:",
            selectInput(inputId = "area_name_rg", label = (""), choices = unique(regions$area_name), selected = 'London', width = "200px"),
            plotlyOutput(outputId = "plot_rg", height = '400px'))
      ),
      
      fluidRow(
        box(title = "Daily cases in England, total",
            plotlyOutput(outputId = "plot_eng_daily", height = '400px')),
        box(title = "UK deaths by nation",
            plotlyOutput(
              outputId = "plot_deaths", 
              height = '400px'))
      )),
    
      
      ### page 2
      
      
      tabItem(tabName = "map",
      h2("Timeline of daily cases per authority"),
      
      
      fluidRow(      
        box(width = 12,
            dateRangeInput(inputId = "day_l", label = ("Filter by date to explore how areas affect one another:"), start  = min(local$day), weekstart = 1),
            plotlyOutput(outputId = "plot_map2", height = "5000px")
        )
      )
    )
  )
)
)  


server <- function(input, output) { 

  
################### Total England cases
  
  output$box_total_eng<- renderValueBox({
    
  england <- cases %>% filter(area_type=='nation')
  total_cases = england[which.max(england$day),]
  
  valueBox(as.character(total_cases$`Cumulative lab-confirmed cases`))
  })
  
################### Last week data England cases
  
  output$box_lw_eng<- renderValueBox({
    
    england <- cases %>% dplyr::filter(area_type=='nation') %>%
      dplyr::group_by(date_range) %>% 
      dplyr::summarise(total = sum(cases)) %>%
      spread(date_range, total) 
    
    pct = round((england$last_week - england$two_weeks_ago) / england$two_weeks_ago * 100,0)
    
    if (pct > 0) {pct = paste("+", pct, sep = "")}
    
    valueBox( paste("+ ",as.character(england$last_week)," (", pct,"%)"))
    
  })

  ################### Total England deaths
  
  output$box_total_d_eng<- renderValueBox({
    
    england <- deaths %>% filter(area_name == 'England')

    total_deaths = sum(england$deaths,na.rm = TRUE)
    
    valueBox(as.character(total_deaths))
  })
  

  ################### Last week England deaths
  
  output$box_lw_d_eng<- renderValueBox({
    england <- deaths %>% dplyr::filter(area_name == 'England') %>%
      dplyr::group_by(date_range) %>% 
      dplyr::summarise(total = sum(deaths)) %>%
      spread(date_range, total) 
    
    pct = round((england$last_week - england$two_weeks_ago) / england$two_weeks_ago * 100,0)
    
    if (pct > 0) {pct = paste("+", pct, sep = "")}
    
    valueBox( paste("+", as.character(england$last_week),"(", pct,"%)"))
  })
  
  
      
###################### cases by authority plot 
  
  filtered_data<- reactive({
    dplyr::filter(local, local$area_name==input$area_name)
  })
  
  filtered_data_lw<- reactive({
    dplyr::ungroup(compare_weeks) %>% dplyr::filter(compare_weeks$area_name==input$area_name) 
  })
  
  filtered_data_2wago<- reactive({
    dplyr::ungroup(compare_weeks) %>% dplyr::filter(compare_weeks$area_name==input$area_name) 
  })

  output$plot <- renderPlotly({
    
    g = local %>% 
      filter(area_name %in% input$area_name) %>% 
      ggplot() +
      geom_bar(stat = "identity", fill = '#e76f51', aes(x= filtered_data()$day, y= filtered_data()$cases, 
                                                        text = paste(" date:",filtered_data()$day, "\n", "cases:", filtered_data()$cases ))) + 
      geom_line(color = "white", aes(x= filtered_data()$day, y=rollmean(filtered_data()$cases, 7, na.pad=TRUE))) +
      xlab("date") + ylab("cases") + 
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA),
            axis.title = element_text(colour = "white"),
            axis.text = element_text(colour = "white"))
    
    ggplotly(g, tooltip = "text")
    
  })

  
###################### authorities stats
  
  output$box <- renderValueBox({
    
    pct = round((filtered_data_lw()$last_week - filtered_data_2wago()$two_weeks_ago) / filtered_data_2wago()$two_weeks_ago * 100,0)
    
    if (is.nan(pct)) {pct = paste("+", 0, sep = "")
    }
    if (pct == Inf) {pct = paste("+", 100, sep = "")
    }
    if (pct >= 0) {pct = paste("+", pct, sep = "")
    } 
    else {pct}
  
    valueBox(paste(filtered_data_lw()$last_week," cases", " (", pct,"%) ",  " were recorded last week in"))
  })
  
  
  
  
##################### map regions
  
  
  regions_filtered<- reactive({
    regions %>% dplyr::filter(regions$day >= input$day[1] & regions$day <= input$day[2]) 
  })
  

  
  output$plot_map <- renderPlotly({    
  
    # heat map data
    
    hm_r = regions_filtered() %>% select(area_name, day, cases) %>%
      spread(day, cases) %>% ungroup()
    
    hm_r = hm_r %>% remove_rownames %>%
      column_to_rownames(var="area_name")
    
    
    hm_r[is.na(hm_r)] = 0
    
  
  heatmaply(hm_r, Colv = NULL,
            xlab = "date", ylab = "",
            scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(
              mid = "#086375",
              high = "#fe5d26"), 
              margins = c(0,50,30,5),
              showticklabels = c(FALSE, TRUE),
              show_dendrogram = c(FALSE,FALSE),
              heatmap_layers = theme(
                panel.background = element_rect(fill = "transparent",colour = NA),
                plot.background = element_rect(fill = "transparent",colour = NA),
                legend.background = element_rect(fill = "transparent",colour = NA),
                axis.text.y =  element_text(colour = "white", size = 8),
                legend.position = "none"),
            k_row = 2
            
  )
            
            
    

  })  
  

##################### map local
  
  local_filtered<- reactive({
    local %>% dplyr::filter(local$day >= input$day_l[1] & local$day <= input$day_l[2]) 
  })
  
  
  
  output$plot_map2 <- renderPlotly({    
    

    # heat map per authority
    
    hm_l = local_filtered() %>% select(area_name, day, cases) %>%
      spread(day, cases) %>% ungroup()
    
    
    hm_l = hm_l %>% remove_rownames %>%
      column_to_rownames(var="area_name")
    
    
    hm_l[is.na(hm_l)] = 0
    
    
    
    heatmaply(hm_l,
              Colv = NULL,
              xlab = "date", ylab = "",
              scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(
                mid = "#086375",
                high = "#fe5d26"), 
              margins = c(t = 0,130,30,0),
              showticklabels = c(FALSE, TRUE),
              show_dendrogram = c(FALSE,FALSE),
              heatmap_layers = theme(
                panel.background = element_rect(fill = "transparent",colour = NA),
                plot.background = element_rect(fill = "transparent",colour = NA),
                legend.background = element_rect(fill = "transparent",colour = NA),
                axis.text.y =  element_text(colour = "white", size = 8),
                legend.position = "none")
              )
              
    
    
  })
  
  

  
  
###################### authorities table
  
  output$table <- DT::renderDataTable({
    

      datatable(tb,
                options = list(initComplete = JS(
                    "function(settings, json) {",
                    "$(this.api().table().header()).css({'background-color': '#40434F', 'color': '#fff'});",
                    "}")
                )
                ) %>%
      DT::formatStyle(columns = colnames(tb)[c(1,3,4,5,6)], color="white") %>%
      DT::formatStyle(columns = colnames(tb)[c(2)], color="#fe5d26")
    
    })

  

  
##################### changes per region  
  
  
  output$plot_rg_change <- renderPlotly({
    
    g <- 
      cases_per_regions %>% 
      ggplot() +
      geom_bar(stat = "identity", fill = '#e76f51', aes(x= pct, y= area_name, 
                                                        text = paste(" region:", area_name, "\n", "change in %:", round(pct))))+
      xlab("%") + ylab("") +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA),
            axis.title = element_text(colour = "white"),
            axis.text = element_text(colour = "white"),
            axis.text.y =  element_text(size = 8),
      )
    
    ggplotly(g, tooltip = "text")
  })
    

  
  
###################### cases by region plot
  
  
  
  filtered_data_rg<- reactive({
    dplyr::filter(regions, regions$area_name==input$area_name_rg)
  })
  
  filtered_data_weeks_rg<- reactive({
    dplyr::ungroup(cases_per_regions) %>% dplyr::filter(cases_per_regions$area_name==input$area_name_rg)
  })
  
  
  output$plot_rg <- renderPlotly({
    
    g <- regions %>% 
      filter(area_name %in% input$area_name_rg) %>%
      ggplot() +
      geom_bar(stat = "identity", fill = '#e76f51', aes(x= filtered_data_rg()$day, y= filtered_data_rg()$cases), 
                                                        text = paste(" date:",filtered_data_rg()$day, "\n", "cases:", filtered_data_rg()$cases)) +
      geom_line(color = "white", aes(x = filtered_data_rg()$day, y=rollmean(filtered_data_rg()$cases, 7, na.pad=TRUE))) +
      xlab("date") + ylab("daily cases") +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA),
            axis.title = element_text(colour = "white"),
            axis.text = element_text(colour = "white"))
    
    ggplotly(g, tooltip = "text")
    
  })
  
##################### region stats
  
  output$box_rg <- renderValueBox({

    pct = round((filtered_data_weeks_rg()$last_week - filtered_data_weeks_rg()$two_weeks_ago) / filtered_data_weeks_rg()$two_weeks_ago * 100,0)

    if (pct > 0) {pct = paste("+", pct, sep = "")}

    valueBox(paste(filtered_data_weeks_rg()$last_week, " cases"," (", pct,"%) ",  " were recorded last week in"))
  })
  

  output$plot_eng_daily <- renderPlotly({
    
  g <- england %>%  ggplot()+
    geom_col(fill = '#e76f51', aes(x=day, y=total, text = paste(" date:", day, "\n", "cases:", total))) +
    xlab("date") + ylab("daily cases") +
    geom_line(color = "white", aes(x=day, y=rollmean(total, 7, na.pad=TRUE))) +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA),
          axis.title = element_text(colour = "white"),
          axis.text = element_text(colour = "white"))
  
  
  
  ggplotly(g, tooltip = "text")
  
  })
  

  
########### deaths stats in UK
  
  output$plot_deaths <- renderPlotly({
    
    g <- deaths_by_nation %>%  
      ggplot(aes(x=day, y=deaths, color = area_name, group = area_name)) +
      geom_line() + 
      xlab("date") + ylab("daily deaths") + 
      scale_color_manual(name = "",values = c("#E76D4B", "#9f98c3", "#086375",  "#E9C46A" )) +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA),
            plot.background = element_rect(fill = "transparent",colour = NA),
            legend.background = element_blank(),
            legend.text = element_text(colour = "white"),
            axis.title = element_text(colour = "white"),
            axis.text = element_text(colour = "white"))
    
    
    
    ggplotly(g)   %>%
      layout(legend = list(orientation = "h", x = 0, y = 1)
      )
    
    
  })
  
   
  
}
  
shinyApp(ui, server)


