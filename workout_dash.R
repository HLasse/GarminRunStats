##############################
## SHINY RUNNING DASHBOARD
##############################


#devtools::install_github("Roche/ggtips")
pacman::p_load(tidyverse, shiny, janitor, lubridate, scales, viridis, plotly, ggtips)


# https://stackoverflow.com/questions/49404394/format-hover-data-labels-plotly-r
# https://support.garmin.com/en-IE/?faq=FMKY5NYJJ71DbuPmFP4O7A
# https://www.garmin.com/en-US/blog/general/get-zone-train-using-heart-rate/
# https://stackoverflow.com/questions/38917101/how-do-i-show-the-y-value-on-tooltip-while-hover-in-ggplot2
# https://github.com/Roche/ggtips

#### VED PLOTLY
#  Se på dato akser (til det samme format)
#  Flyt legend
###

column_names <- c('activity_type', 'date', 'favorite', 'title', 'distance', 'calories', 'time', 'avg_hr', 'max_hr', 'avg_run_cadence',
                  'max_run_cadence', 'avg_pace', 'best_pace', 'elev_gain', 'elev_loss', 'avg_stride_length', 'avg_vertical_ratio')

load_and_clean <- function(filepath) {
  df <- read_csv(filepath) %>% 
    select(c(1:17)) # Only keeping the columns which are not empty and actually used
  
  colnames(df) <- column_names
  df <- df[df$max_hr != "--",]
  df <- df %>% 
    filter(activity_type %in% c('Løb', 'Running')) %>% 
    mutate(avg_hr = as.numeric(avg_hr),
           # converting time to minutes
           time = as.numeric(hms(time)) / 60)

  # if decimals are seperated by , not .
  if (isTRUE(all.equal(df$distance, as.integer(df$distance)))){
    df$distance <- df$distance / 100
    df$avg_stride_length <- df$avg_stride_length / 100
  }
  
  
  # removing runs without heart rate measurements
  # Adding aerobic/anaerobic column
  df <- df %>% 
    mutate(aerob = ifelse(avg_hr < 155, 'Aerobic', 'Anaerobic'))
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
    mutate(date = as.Date(date)) %>% 
    ggplot(aes(date, distance, color = aerob)) + 
      theme_bw() +
      geom_point() +
      stat_smooth(geom = 'line', method = 'lm', se = FALSE, alpha = 0.5) +
      scale_color_brewer(palette = 'Dark2') +
      theme(legend.title = element_blank()) +
      labs(x = 'Date', y = 'Distance (km)', title = 'Distance run') +
      theme(axis.text.x = element_text(size = 13),
            axis.text.y = element_text(size = 13),
            axis.title.x = element_text(size = 13),
            axis.title.y = element_text(size = 13),
            legend.position = c(0.92, 0.1),
            legend.text = element_text(size = 13),
            legend.key = element_blank(),
            legend.background = element_blank())
  if (!is.null(xmin)){
    p <- p +
      scale_x_date(limits = as.Date(c(xmin, xmax), format="%d/%m/%Y"), date_labels = "%b-%Y") 
  }  
  return(p)
}



# Distance pr month/week
dist_by_month <- function(df, grouping, xmin = NULL, xmax = NULL){ 
  p <- df %>%
    group_by(date=floor_date(date, grouping)) %>%
    summarize(distance=sum(distance)) %>%
    mutate(date = as.Date(date)) %>%
    ggplot(aes(date, distance)) +
      theme_bw() +
      geom_point(color = 'steelblue2') +
      geom_line(color = 'steelblue') +
      stat_smooth(geom = 'line', method = 'lm', se = FALSE, alpha = 0.4, col = 'lightsteelblue') +
      theme(legend.title = element_blank()) +
      labs(x = 'Date', y = 'Distance (km)', title = paste('Km run per', grouping)) +
      theme(axis.text.x = element_text(size = 13),
            axis.text.y = element_text(size = 13),
            axis.title.x = element_text(size = 13),
            axis.title.y = element_text(size = 13),
            legend.position = c(0.92, 0.1),
            legend.text = element_text(size = 13),
            legend.key = element_blank(),
            legend.background = element_blank())
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

# time pr run
time_per_run <- function(df, xmin = NULL, xmax = NULL) {
  p <- df %>% 
    mutate(date = as.Date(date)) %>% 
    ggplot(aes(date, time, color = aerob)) +
    theme_bw() +
    geom_point() +
    stat_smooth(geom = 'line', method = 'lm', se = FALSE, alpha = 0.5) +
    scale_color_brewer(palette = 'Dark2') +
    theme(legend.title = element_blank()) +
   # scale_y_time(labels = time_format("%H:%M")) +
    labs(x = 'Date', y = 'Minutes', title = 'Time pr. run') +
    scale_x_date(limits = as.Date(c(xmin, xmax), format="%d/%m/%Y"), date_labels = "%b-%Y") +
    theme(axis.text.x = element_text(size = 13),
          axis.text.y = element_text(size = 13),
          axis.title.x = element_text(size = 13),
          axis.title.y = element_text(size = 13),
          legend.position = c(0.92, 0.1),
          legend.text = element_text(size = 13),
          legend.key = element_blank(),
          legend.background = element_blank())
  
  return(p)
}

# # time spent running pr grouping
time_per_month <- function(df, grouping, xmin, xmax){
  p <- df %>% 
    group_by(date = floor_date(date, grouping)) %>%
    summarize(time = sum(time)) %>%
    mutate(date = as.Date(date)) %>% 
    ggplot(aes(date, time)) +
      theme_bw() +
      geom_point(color = 'steelblue2') +
      geom_line(color = 'steelblue') +
      stat_smooth(geom = 'line', method = 'lm', se = FALSE, alpha = 0.4, col = 'lightsteelblue') +
      theme(legend.title = element_blank()) +
      scale_x_date(limits = as.Date(c(xmin, xmax), format="%d/%m/%Y"), date_labels = "%b-%Y") +
      labs(x = 'Date', y = 'Minutes', title = paste('Minutes run per', grouping)) +
      theme(axis.text.x = element_text(size = 13),
            axis.text.y = element_text(size = 13),
            axis.title.x = element_text(size = 13),
            axis.title.y = element_text(size = 13),
            legend.position = c(0.92, 0.1),
            legend.text = element_text(size = 13),
            legend.key = element_blank(),
            legend.background = element_blank())
  return(p)
}

# SPEED
pace_per_run <- function(df, xmin, xmax){
  p <- df %>%
    mutate(avg_pace = as.POSIXct(avg_pace, format = '%H:%M:%S'),
           date = as.Date(date)) %>%
    ggplot(aes(date, avg_pace, color = aerob)) +
      theme_bw() +
      geom_point() +
      stat_smooth(geom = 'line', method = 'lm', se = FALSE, alpha = 0.5) +
      scale_color_brewer(palette = 'Dark2') +
      theme(legend.title = element_blank()) +
      scale_y_continuous(trans = rev_date) +
      scale_x_date(limits = as.Date(c(xmin, xmax), format="%d/%m/%Y"), date_labels = "%b-%Y") +
      labs(x = 'Date', y = 'Avg. pace (min/km)', title = 'Average pace') +
      theme(axis.text.x = element_text(size = 13),
            axis.text.y = element_text(size = 13),
            axis.title.x = element_text(size = 13),
            axis.title.y = element_text(size = 13),
            legend.position = c(0.92, 0.1),
            legend.text = element_text(size = 13),
            legend.key = element_blank(),
            legend.background = element_blank())
}

pace_per_month <-function(df, grouping, xmin, xmax){
  p <- df %>%
    group_by(date = floor_date(date, grouping)) %>%
    summarize(pace = mean(period_to_seconds(hms(avg_pace)))) %>%
    mutate(pace = seconds_to_period(pace),
           date = as.Date(date)) %>%
    ggplot(aes(date, pace)) +
    theme_bw() +
    geom_point(color = 'steelblue2') +
    geom_line(color = 'steelblue') +
    stat_smooth(geom = 'line', method = 'lm', se = FALSE, alpha = 0.6, col = 'lightsteelblue') +
    theme(legend.title = element_blank()) +
    scale_y_continuous(trans=scales::reverse_trans() %::% scales::hms_trans(), labels = time_format("%H:%M")) +
    scale_x_date(limits = as.Date(c(xmin, xmax), format="%d/%m/%Y"), date_labels = "%b-%Y") +
    labs(x = 'Date', y = 'Time (min)', title = paste('Average pace per', grouping)) +
    theme(axis.text.x = element_text(size = 13),
          axis.text.y = element_text(size = 13),
          axis.title.x = element_text(size = 13),
          axis.title.y = element_text(size = 13),
          legend.position = c(0.92, 0.1),
          legend.text = element_text(size = 13),
          legend.key = element_blank(),
          legend.background = element_blank())
}


# pace, distance
pace_distance <- function(df, xmin, xmax){
  p <- df %>%
    mutate(avg_pace = as.POSIXct(avg_pace, format = '%H:%M:%S'),
           date = as.Date(date)) %>%
    ggplot(aes(date, avg_pace, color = distance)) +
    theme_bw() +
    geom_point(size = 2) +
    scale_color_viridis(option = 'A', direction = -1) +
    scale_y_continuous(trans = rev_date) +
    scale_x_date(limits = as.Date(c(xmin, xmax), format="%d/%m/%Y"), date_labels = "%b-%Y") +
    labs(x = 'Date', y = 'Pace (min/km)', title = 'Average pace by distance run', color = 'Distance') +
    theme(axis.text.x = element_text(size = 13),
          axis.text.y = element_text(size = 13),
          axis.title.x = element_text(size = 13),
          axis.title.y = element_text(size = 13),
          legend.position = c(0.92, 0.17),
          legend.text = element_text(size = 13),
          legend.key = element_blank(),
          legend.background = element_blank())
}


pace_date_heartrate <- function(df, xmin, xmax){
  p <- df %>%
    mutate(avg_pace = as.POSIXct(avg_pace, format = '%H:%M:%S'),
           date = as.Date(date)) %>%
    ggplot(aes(date, avg_pace, color = avg_hr)) +
    theme_bw() +
    geom_point(size = 2) +
    scale_color_viridis(option = 'A', direction = -1) +
    scale_y_continuous(trans = rev_date) +
    scale_x_date(limits = as.Date(c(xmin, xmax), format="%d/%m/%Y"), date_labels = "%b-%Y") +
    labs(x = 'Date', y = 'Pace (min/km)', title = 'Average pace by heartrate', color = 'Avg. Heartrate') +
    theme(axis.text.x = element_text(size = 13),
          axis.text.y = element_text(size = 13),
          axis.title.x = element_text(size = 13),
          axis.title.y = element_text(size = 13),
          legend.position = c(0.92, 0.17),
          legend.text = element_text(size = 13),
          legend.key = element_blank(),
          legend.background = element_blank())
}


# #####
# ## Ved mere data - evt. plot tendenslinjer for specifikke distance, fx 5, 10k
# #####

# pace, heart rate
pace_heartrate <- function(df, xmin, xmax)
  p <- df %>%
  filter(as.Date(date) > xmin & as.Date(date) < xmax) %>% 
  mutate(avg_pace = as.POSIXct(avg_pace, format = '%H:%M:%S')) %>%
  ggplot(aes(avg_pace, avg_hr)) +
  theme_bw() +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", se = F, col = 'lightsteelblue', alpha = 0.6) +
  scale_color_viridis(option = 'A', direction = -1) +
  scale_x_continuous(trans = rev_date) +
  labs(x = 'Avg. Pace', y = 'Avg. Heart Rate', title = 'Average heart rate by pace') +
  theme(axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        legend.position = c(0.92, 0.1),
        legend.text = element_text(size = 13),
        legend.key = element_blank(),
        legend.background = element_blank())



##### Add some tables
###
# Tables: how many runs pr. month, year, week (janitor tabyl)
# See app for ideer 
# Interactive plots: hover to show value
# plots med elevation gain
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
              fluidPage(
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
                      column(4,
                             dateRangeInput('date_range',
                                            label = h4('Select date range:'),
                                            start = Sys.Date() - 550, end = Sys.Date()
                             )
                      ),
                      column(4, offset = 2,
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
                box(width = 12, status = "success", title = "Time",
                    fluidRow(
                      column(6,
                             plotOutput("time_per_run")
                      ),
                      column(6,
                             plotOutput("time_per_month")
                      )
                    )
                ),
                box(width=12, status = "success", title = "Pace",
                    fluidRow(
                      column(6,
                             plotOutput("pace_per_run")
                      ),
                      column(6,
                             plotOutput("pace_per_month")
                      )
                    ),
                    fluidRow(
                      column(6,
                             plotOutput("pace_distance")
                      ),
                      column(6,
                             plotOutput('pace_date_heartrate')
                      )
                    ),
                    fluidRow(
                      column(6,
                             plotOutput("pace_heartrate"))
                    )
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
  
  ##### TIME PLOTS
  output$time_per_run <- renderPlot({
    # load your data
    df <- data()
    # only plot if data has been input
    if(is.null(data)){
      return(NULL)
    }else{
      plot <- time_per_run(df, values$xmin, values$xmax)
      print(plot)
    }})  
  
  output$time_per_month <- renderPlot({
    # load your data
    df <- data()
    # only plot if data has been input
    if(is.null(data)){
      return(NULL)
    }else{
      plot <- time_per_month(df, values$grouping, values$xmin, values$xmax)
      print(plot)
    }})
  ###### PACE
  output$pace_per_run <- renderPlot({
    # load your data
    df <- data()
    # only plot if data has been input
    if(is.null(data)){
      return(NULL)
    }else{
      plot <- pace_per_run(df, values$xmin, values$xmax)
      print(plot)
    }})  
  
  output$pace_per_month <- renderPlot({
    # load your data
    df <- data()
    # only plot if data has been input
    if(is.null(data)){
      return(NULL)
    }else{
      plot <- pace_per_month(df, values$grouping, values$xmin, values$xmax)
      print(plot)
    }})
  
  output$pace_distance <- renderPlot({
    # load your data
    df <- data()
    # only plot if data has been input
    if(is.null(data)){
      return(NULL)
    }else{
      plot <- pace_distance(df, values$xmin, values$xmax)
      print(plot)
    }})
  
  output$pace_date_heartrate <- renderPlot({
    # load your data
    df <- data()
    # only plot if data has been input
    if(is.null(data)){
      return(NULL)
    }else{
      plot <- pace_date_heartrate(df, values$xmin, values$xmax)
      print(plot)
    }})
  
  output$pace_heartrate <- renderPlot({
    # load your data
    df <- data()
    # only plot if data has been input
    if(is.null(data)){
      return(NULL)
    }else{
      plot <- pace_heartrate(df, values$xmin, values$xmax)
      print(plot)
    }})
  #  }
}#server end


shinyApp(ui, server) #Run app
