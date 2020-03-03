##############################
## SHINY RUNNING DASHBOARD
##############################


pacman::p_load(tidyverse, shiny, janitor, lubridate, scales, viridis)


load_and_clean <- function(filepath) {
  df <- read_csv(filepath) %>% 
    clean_names()
  # removing runs without heart rate measurements
  df <- df[df$maks_puls != "--",]
  # Adding aerobic/anaerobic column
  df <- df %>% 
    mutate(aerob = ifelse(gennemsnitlig_puls < 155, 'Aerobic', 'Anaerobic'))
  }

#####################################
### UTILITY FOR PLOTTING PACE CORRECTLY
#####################################

# To reverse pace axis
c_trans <- function(a, b, breaks = b$breaks, format = b$format) {
  a <- as.trans(a)
  b <- as.trans(b)
  
  name <- paste(a$name, b$name, sep = "-")
  
  trans <- function(x) a$trans(b$trans(x))
  inv <- function(x) b$inverse(a$inverse(x))
  
  trans_new(name, trans, inv, breaks, format = format)
}

rev_date <- c_trans("reverse", "time")

# To apply multiple transformations (eg both hms and reverse)
`%::%`  <- function(atrans,btrans) {
  mytran <- scales::trans_new(name      = paste(btrans$name,'then',atrans$name),
                              transform = function(x) { atrans$transform(btrans$transform(x)) },
                              inverse   = function(y) { btrans$inverse(atrans$inverse(y)) },
                              domain    = btrans$domain,  # this could use improvement...
                              breaks    = btrans$breaks,  # not clear how this should work, tbh
                              format    = btrans$format)
  
}

###################################################
## PLOTS
###################################################


# Distance over time
dist_by_time <- function(df, xmin = NULL, xmax = NULL){
  p <- df %>% 
  mutate(dato = as.Date(dato)) %>% 
  ggplot(aes(dato, distance, color = aerob)) + 
  theme_bw() +
  geom_point() +
  stat_smooth(geom = 'line', method = 'lm', se = FALSE, alpha = 0.2) +
  scale_color_brewer(palette = 'Dark2') +
  theme(legend.title = element_blank()) +
  labs(x = 'Date', y = 'Distance (km)', title = 'Distance run')
  if (!is.null(xmin)){
    p <- p +
      scale_x_date(limits = as.Date(c(xmin, xmax), format="%d/%m/%Y"), date_labels = "%b-%Y") 
  }  
  return(p)
}



# Distance pr month/week
dist_by_month <- function(df, grouping, xmin = NULL, xmax = NULL){ 
  p <- df %>%
    group_by(dato=floor_date(dato, grouping)) %>%
    summarize(distance=sum(distance)) %>%
    mutate(dato = as.Date(dato)) %>%
    ggplot(aes(dato, distance)) +
    theme_bw() +
    geom_point(color = 'steelblue2') +
    geom_line(color = 'steelblue') +
    stat_smooth(geom = 'line', method = 'lm', se = FALSE, alpha = 0.4, col = 'lightsteelblue') +
    theme(legend.title = element_blank()) +
    labs(x = 'Date', y = 'Distance (km)', title = paste('Km run per', grouping))
  if (!is.null(xmin)){
    p <- p +
      scale_x_date(limits = as.Date(c(xmin, xmax), format="%d/%m/%Y"), date_labels = "%b-%Y")
  }
  else{
    p <- p +
      scale_x_date(date_labels = "%b-%Y")
  }
  return(p)
}
# # time pr run
# time_per_run <- ggplot(df, aes(dato, tid, color = aerob)) +
#   theme_bw() +
#   geom_point() +
#   stat_smooth(geom = 'line', method = 'lm', se = FALSE, alpha = 0.2) +
#   scale_color_brewer(palette = 'Dark2') +
#   theme(legend.title = element_blank()) +
#   scale_y_time(labels = time_format("%H:%M")) +
#   labs(x = 'Date', y = 'Time', title = 'Time pr. run')
# 
# # time spent running pr month
# time_per_month <- df %>% group_by(month = floor_date(dato, "month")) %>% 
#   summarize(tid = sum(minute(tid))) %>% 
#   ggplot(aes(month, tid)) + 
#   theme_bw() +
#   geom_point(color = 'steelblue2') +
#   geom_line(color = 'steelblue') +
#   stat_smooth(geom = 'line', method = 'lm', se = FALSE, alpha = 0.4, col = 'lightsteelblue') +
#   theme(legend.title = element_blank()) +
#   labs(x = 'Date', y = 'Time (min)', title = 'Minutes run per month')
# 
# # SPEED
# pace_per_run <- df %>% 
#   mutate(gennemsnitstempo = as.POSIXct(gennemsnitstempo, format = '%H:%M:%S')) %>% 
#   ggplot(aes(dato, gennemsnitstempo, color = aerob)) +
#   theme_bw() +
#   geom_point() +
#   scale_color_brewer(palette = 'Dark2') +
#   theme(legend.title = element_blank()) +
#   scale_y_continuous(trans = rev_date) +
#   labs(x = 'Date', y = 'Avg. pace (min/km)', title = 'Average pace')
# 
# pace_per_month <- df %>% 
#   group_by(month = floor_date(dato, 'month')) %>% 
#   summarize(pace = mean(period_to_seconds(hms(gennemsnitstempo)))) %>%
#   mutate(pace = seconds_to_period(pace)) %>% 
#   ggplot(aes(month, pace)) + 
#   theme_bw() +
#   geom_point(color = 'steelblue2') +
#   geom_line(color = 'steelblue') +
#   stat_smooth(geom = 'line', method = 'lm', se = FALSE, alpha = 0.6, col = 'lightsteelblue') +
#   theme(legend.title = element_blank()) +
#   scale_y_continuous(trans=scales::reverse_trans() %::% scales::hms_trans(), labels = time_format("%H:%M")) +
#   labs(x = 'Date', y = 'Time (min)', title = 'Average pace pr. month')
# 
# 
# 
# # pace, distance
# pace_distance <-df %>% 
#   mutate(gennemsnitstempo = as.POSIXct(gennemsnitstempo, format = '%H:%M:%S')) %>% 
#   ggplot(aes(dato, gennemsnitstempo, color = distance)) +
#   theme_bw() +
#   geom_point(size = 2) +
#   scale_color_viridis(option = 'A', direction = -1) +
#   scale_y_continuous(trans = rev_date) +
#   labs(x = 'Date', y = 'Pace (min/km)', title = 'Average pace by distance run', color = 'Distance')
# 
# #####
# ## Ved mere data - evt. plot tendenslinjer for specifikke distance, fx 5, 10k 
# #####
# 
# # pace, heart rate
# pace_heartrate <- df %>% 
#   mutate(gennemsnitstempo = as.POSIXct(gennemsnitstempo, format = '%H:%M:%S')) %>% 
#   ggplot(aes(gennemsnitstempo, gennemsnitlig_puls)) +
#   theme_bw() +
#   geom_point(color = "steelblue") +
#   geom_smooth(method = "lm", se = F, col = 'lightsteelblue', alpha = 0.6) +
#   scale_color_viridis(option = 'A', direction = -1) +
#   scale_x_continuous(trans = rev_date) +
#   labs(x = 'Avg. Pace', y = 'Avg. Heart Rate', title = 'Average heart rate by pace')

  
##### Add some tables
###
# Choose time interval for scales 
# Choose grouping (month, year)
# Tables: how many runs pr. month, year, week
###




#############################################################
###
###
##  SHINY
##
###
###
############################################################



library(shiny)
library(shinydashboard)
library(ggplot2)

ui <- dashboardPage(
  
  dashboardHeader(title = "Run Tracker"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName ="Dashboard_tab", icon = icon("tachometer-alt"))
    )
  ), #sidebar
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "Dashboard_tab",
              box(title = "Upload data", status = "info", solidHeader = TRUE, width = 12,
                  fluidRow(
                    column(6,
                      fileInput("file", h4("Upload a .csv file containing your activity data"),
                                accept = c(
                                  "text/csv",
                                  "text/comma-separated-values,text/plain",
                                  ".csv")
                      )
                    )
                  )
                ),
              box(title = "Set Variables", status = "info", solidHeader = TRUE, width = 12,
                  fluidRow(
                    column(6,
                           dateRangeInput('date_range',
                                          label = h4('Select date range:'),
                                          start = Sys.Date() - 550, end = Sys.Date()
                           )
                    ),
                    column(6,
                           selectInput("grouping", 
                                       label = h4("Select grouping"), 
                                       choices = list("1 Week" = "week", 
                                                      "2 Weeks" = "14 days",
                                                      "Month" = "month",
                                                      "Year" = "year"), 
                                       selected = 3)
                    )
                  )
              ),
              box(width=12, status = "success", title = "Distance",
                  fluidRow(
                    column(6,
                           plotOutput("dist_by_time")
                    ),
                    column(6,
                           plotOutput("dist_by_month")
                    )
                  )
              ),
              fluidRow(
                column(4, offset = 1,
                       valueBox("22", "valueBox", color = "aqua", width = 12)
                ),
                column(4, offset = 2,
                       valueBox("22", "valueBox", color = "red", width = 12)
                )
              )
              
      )#tabItem ends
    )#tabItems
  )#body
)#ui end


server <- function(input, output) {
  ### Reading data from input file
  data <- reactive({
    req(input$file)
    df <- load_and_clean(input$file$datapath)
    return(df)
  })
  
  values <- reactiveValues()
  values$xmin <- NULL
  values$xmax <- NULL
  values$grouping <- NULL
  
  # Update date values when set
  observeEvent(input$date_range, {
    values$xmin <- input$date_range[1]
    values$xmax <- input$date_range[2]
  })
  
  # Update date values when set
  observeEvent(input$grouping, {
    values$grouping <- input$grouping
  })
  
  ####################################### Distance plots
  output$dist_by_time <- renderPlot({
    # load your data
    df <- data()
    # only plot if data has been input
    if(is.null(data)){
      return(NULL)
    }else{
      plot <- dist_by_time(df, values$xmin, values$xmax)
      print(plot)
    }})
  
  output$dist_by_month <- renderPlot({
    # load your data
    df <- data()
    # only plot if data has been input
    if(is.null(data)){
      return(NULL)
    }else{
      plot <- dist_by_month(df, values$grouping, values$xmin, values$xmax)
      print(plot)
    }})
    
  
  output$testDataPlot2 <- renderPlot({
    ggplot(appData) + geom_point(aes_string(input$selectIrisColumnRight, "Sepal.Length"))
  })
  
#  }
}#server end


shinyApp(ui, server) #Run app
