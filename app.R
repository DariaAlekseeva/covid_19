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



### DATA PROCESSING
#---------------------------------------------------------------------------------------------------------------------------------------------------

  # cases data
  cases <- read_csv('https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv')

  # deaths data
  deaths <- read_csv('https://coronavirus.data.gov.uk/downloads/csv/coronavirus-deaths_latest.csv')

  # England population data

  # add population data
  path <- "population"
  population <- read_feather(path)



  # rename columns

  cases <- cases %>%
    dplyr::rename(cases=`Daily lab-confirmed cases`, day =`Specimen date`, area_name='Area name', area_type = `Area type`, id = `Area code`)

  deaths <- deaths %>%
    dplyr::rename(deaths=`Daily change in deaths`, day =`Reporting date`, area_name='Area name', area_type = `Area type`)



  # define 1 and 2 weeks ago
  
  if (weekdays(Sys.Date()) == "Monday") {
    one_week_ago = isoweek(Sys.Date()) - 2
    } else {
      one_week_ago = isoweek(Sys.Date()) - 1
    }
  
  two_weeks_ago = one_week_ago - 1


  # identify days corresponding to 1 and 2 weeks ago
  cases <- cases %>%
    mutate(date_range = case_when(isoweek(day) == one_week_ago ~ "last_week",
                                  isoweek(day) == two_weeks_ago ~ "two_weeks_ago"))


  deaths <- deaths %>%
    mutate(date_range = case_when(isoweek(day) == one_week_ago ~ "last_week",
                                  isoweek(day) == two_weeks_ago ~ "two_weeks_ago"))


  # create comparison table between two past weeks

  compare_weeks = cases %>%
    dplyr::filter(area_type == "Lower tier local authority") %>%
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
  
  compare_weeks$cases_per_100k_past2weeks = round((compare_weeks$last_week+compare_weeks$two_weeks_ago)/compare_weeks$X2018 * 100000,0)

  compare_weeks[compare_weeks$delta_pct == Inf,]$delta_pct <- 100
  
  

  # get all local authorities which had Covid cases

  local = cases %>%
    filter(area_type == "Lower tier local authority")
  
  
  # this is table data to output as table
  tb = 
    compare_weeks %>% 
    ungroup() %>% 
    select(area_name, last_week, two_weeks_ago, delta, delta_pct, cases_per_100k_past2weeks) %>% 
    dplyr::rename(area = "area_name", `last week` = "last_week", `2 weeks ago` = "two_weeks_ago", `delta, n of cases` = "delta",
                  `delta, %` = "delta_pct", `cases per 100k in last 2 weeks` = "cases_per_100k_past2weeks") %>%  arrange(-`delta, n of cases`)
  
  
  # this data is for regions plot
  
  regions = cases %>% dplyr::filter(area_type=='Region')
  
  cases_per_regions = cases %>% 
    dplyr::filter(area_type == "Region") %>% 
    dplyr::group_by(date_range, area_name) %>% 
    dplyr::summarise(total = sum(cases)) %>%
    spread(date_range, total) %>%
    dplyr::select(-"<NA>") %>%
    mutate(pct = (last_week - two_weeks_ago)/two_weeks_ago*100)
  
  
  
  # England cases daily
  
  england = cases %>% dplyr::filter(area_type=='Nation') %>%
    dplyr::group_by(day) %>% 
    dplyr::summarise(total = sum(cases))
  
  # deaths by nation
  
  deaths_by_nation = deaths %>% dplyr::filter(area_type=='Nation') %>%
    dplyr::group_by(day, area_name) %>% 
    dplyr::summarise(total = sum(deaths))
  
  
  
  # heat map data
  
  hm_r = regions %>% select(area_name, day, cases) %>%
    spread(day, cases) %>% ungroup()
  
  hm_r = hm_r %>% remove_rownames %>%
    column_to_rownames(var="area_name")
  
  
  hm_r[is.na(hm_r)] = 0
  
  # heat map per authority
  
  hm_l = local %>% select(area_name, day, cases) %>%
    spread(day, cases) %>% ungroup()
  
  
  hm_l = hm_l %>% remove_rownames %>%
    column_to_rownames(var="area_name")
  
  
  hm_l[is.na(hm_l)] = 0
  
  
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
            title = "Which area is at the highest risk of lockdown?", status = "primary",
            div(style = 'overflow-x: scroll', 
                DT::dataTableOutput("table", height = "500px")))
        
        
      ),
      
      fluidRow(      
        box(width = 12,
            title = "Timeline of daily cases per region",
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
            plotlyOutput(outputId = "plot_deaths", height = '400px'))
      )),
    
      
      ### page 2
      
      
      tabItem(tabName = "map",
      h2("Timeline of daily cases per authority"),
      
      
      fluidRow(      
        box(width = 12,
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
    
  england <- cases %>% filter(area_type=='Nation')
  total_cases = england[which.max(england$day),]
  
  valueBox(as.character(total_cases$`Cumulative lab-confirmed cases`))
  })
  
################### Last week data England cases
  
  output$box_lw_eng<- renderValueBox({
    
    england <- cases %>% dplyr::filter(area_type=='Nation') %>%
      dplyr::group_by(date_range) %>% 
      dplyr::summarise(total = sum(cases)) %>%
      spread(date_range, total) 
    
    pct = round((england$last_week - england$two_weeks_ago) / england$two_weeks_ago * 100,0)
    
    if (pct > 0) {pct = paste("+", pct, sep = "")}
    
    valueBox( paste("+ ",as.character(england$last_week)," (", pct,"%)"))
    
  })

  ################### Total England deaths
  
  output$box_total_d_eng<- renderValueBox({
    
    england <- deaths %>% filter(area_type=='Nation', area_name == 'England')
    total_deaths = england[which.max(england$day),]
    
    valueBox(as.character(total_deaths$`Cumulative deaths`))
  })
  

  ################### Last week England deaths
  
  output$box_lw_d_eng<- renderValueBox({
    england <- deaths %>% dplyr::filter(area_type=='Nation', area_name == 'England') %>%
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
  
  
  
  output$plot_map <- renderPlotly({    
  
    
  
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
  
  
  
  output$plot_map2 <- renderPlotly({    
    

    
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
      DT::formatStyle(columns = colnames(tb)[c(1,2,3,4,5,6)], color="white")
    
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
      ggplot(aes(x=day, y=total, color = area_name)) +
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


