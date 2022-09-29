##############################
## SHINY RUNNING DASHBOARD
##############################


#devtools::install_github("Roche/ggtips")
library(tidyverse)
library(shiny)
library(janitor)
library(lubridate)
library(scales)
library(viridis)
library(DT)
library(formattable)
library(RColorBrewer)
library(shinyjs)
library(shinycssloaders)
#githubinstall::githubinstall("rasmusab/fullcalendar")
library(fullcalendar)
# https://stackoverflow.com/questions/49404394/format-hover-data-labels-plotly-r
# https://support.garmin.com/en-IE/?faq=FMKY5NYJJ71DbuPmFP4O7A
# https://www.garmin.com/en-US/blog/general/get-zone-train-using-heart-rate/
# https://stackoverflow.com/questions/38917101/how-do-i-show-the-y-value-on-tooltip-while-hover-in-ggplot2
# https://github.com/Roche/ggtips

#### VED PLOTLY
#  Se på dato akser (til det samme format)
#  Flyt legend
###

######
#
# Flyt data choosers til sidebar og arranger det hele i tabs?
#
##########

column_names <- c('activity_type', 'date', 'favorite', 'title', 'distance', 'calories', 'time', 'avg_hr', 'max_hr', 'avg_run_cadence',
                  'max_run_cadence', 'avg_pace', 'best_pace', 'elev_gain', 'elev_loss', 'avg_stride_length', 'avg_vertical_ratio')



calc_hr_zone <- function(max_hr, hr){
  if (hr < max_hr * 0.6){
    return("HR Zone 1")
  } else if (hr >= max_hr * 0.6 & hr < max_hr * 0.7){
    return("HR Zone 2")
  } else if (hr >= max_hr * 0.7 & hr < max_hr * 0.8){
    return("HR Zone 3")
  } else if (hr >= max_hr * 0.8 & hr < max_hr * 0.9){
    return("HR Zone 4")
  } else if (hr >= max_hr * 0.9){
    return("HR Zone 5")
  }
}


load_and_clean <- function(filepath, gender = 'male', age = 25) {
  df <- read_csv(filepath) %>% 
    select(c(1:17)) # Only keeping the columns which are not empty and actually used
  
  colnames(df) <- column_names
  df <- df[df$max_hr != "--",]
  df <- df %>% 
    filter(activity_type %in% c('Løb', 'Running')) %>% 
    mutate(avg_hr = as.numeric(avg_hr),
           # converting time to minutes
           time = as.numeric(hms(time)) / 60)
  
  # If running was not filtered when loading the data
  if (class(df$avg_pace) == "character"){
    df <- df %>% 
      mutate(avg_pace = parse_time(avg_pace))
  }
  
  # if decimals are seperated by , not .
  if (isTRUE(all.equal(df$distance, as.integer(df$distance)))){
    df$distance <- df$distance / 100
    df$avg_stride_length <- df$avg_stride_length / 100
  }
  
  
  # removing runs without heart rate measurements
  # Adding heart rate zone 
  df$max_hr <- ifelse(gender == 'male', 220-age, 226-age)
  
  df <- df %>% 
    rowwise() %>% 
    mutate(hr_zone = calc_hr_zone(max_hr, avg_hr)) %>% 
    select(-max_hr)
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
    ggplot(aes(date, distance, color = hr_zone)) + 
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
    ggplot(aes(date, time, color = hr_zone)) +
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
    ggplot(aes(date, avg_pace, color = hr_zone)) +
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
# Tables: how many runs pr. month, year, week (janitor tabyl), change from previous week/month
# See app for ideer 
# Interactive plots: hover to show value
# plots med elevation gain
###

grouping = "6 months"
xmin = "2018-10-10"
xmax = "2020-03-03"

summary_tab <-function(df, grouping){
  p <- df %>%
    group_by(date = floor_date(date, grouping)) %>%
    summarize(n_runs = n(),
              avg_distance = mean(distance),
              total_distance = sum(distance),
              min_distance = min(distance),
              max_distance = max(distance),
              avg_minutes_pr_run = mean(time),
              total_minutes_spent_running = sum(time),
              min_minutes = min(time),
              max_minutes = max(time),
              min_pace = seconds_to_period(min(period_to_seconds(hms(avg_pace)))),
              max_pace = seconds_to_period(max(period_to_seconds(hms(avg_pace)))),
              avg_pace = seconds_to_period(mean(period_to_seconds(hms(avg_pace))))
    ) 
}


distance_tab <- function(df, grouping){
  p <- df %>%
    group_by(date = floor_date(date, grouping)) %>%
    summarize(n_runs = n(),
              avg_distance = mean(distance),
              total_distance = sum(distance),
              max_distance = max(distance),
              total_time = sum(time)) %>% 
    mutate(diff_distance = total_distance - lag(total_distance),
              percent_greater_distance = (total_distance - lag(total_distance)) / lag(total_distance) * 100,
              diff_time = total_time - lag(total_time)
    ) %>% 
    mutate_if(is.numeric, round, 2) %>% 
    mutate(percent_greater_distance = paste0(percent_greater_distance, "%")) %>% 
    arrange(desc(date)) %>% 
    formattable(align = c("l", rep("r", NCOL(.) - 1)),
                list(date = formatter("span", style = ~ style(color = "grey", font.weight = "bold")),
                     n_runs = color_tile("transparent", "#74C476"),
                     max_distance = color_tile("transparent", "#74C476"),
                     avg_distance = color_tile("transparent", "#74C476"),
                     total_distance = color_tile("transparent", "#74C476"),
                     diff_distance = color_tile("transparent", "#74C476"),
                     percent_greater_distance = color_tile("transparent", "#74C476"),
                     total_time = color_tile("transparent", "#74C476"),
                     diff_time = color_tile("transparent", "#74C476")),
                col.names = c("Date", "N Runs", "Avg. Distance", "Total Distance", "Max Distance", "Total time",  "Diff distance", "% greater distance",  "Diff time")
    )
}

distance_tab <- function(df, grouping){
  df %>%
  group_by(date = floor_date(date, grouping)) %>%
  summarize(n_runs = n(),
            avg_distance = mean(distance),
            total_distance = sum(distance),
            max_distance = max(distance),
            total_time = sum(time)) %>% 
  mutate(diff_distance = total_distance - lag(total_distance),
         percent_greater_distance = (total_distance - lag(total_distance)) / lag(total_distance) * 100,
         diff_time = total_time - lag(total_time)
  ) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  mutate(
    percent_greater_distance= case_when(
      percent_greater_distance > 10 ~ cell_spec(percent_greater_distance, background = "red"),
      percent_greater_distance > 0 & percent_greater_distance < 10  ~ 
        cell_spec(percent_greater_distance, background = "lightgreen"),
      TRUE ~ cell_spec(percent_greater_distance)
    )
  )%>% 
  mutate(percent_greater_distance = paste0(percent_greater_distance, "%")) %>% 
  arrange(desc(date)) %>% 
  kbl(digits=2, escape=F, col.names = c("Date", "N Runs", "Avg. Distance", "Total Distance", "Max Distance", "Total time",  "Diff distance", "% greater distance",  "Diff time")) %>% 
  kable_styling()
}
#############################################################
###
###
##  SHINY
##
###
###
############################################################

## Shiny functions
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  
  dashboardHeader(title = "Garmin Run Tracker"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName ="Dashboard_tab", icon = icon("tachometer-alt")),
      numericInput("age",
                   label = h5("Set your age"),
                   value = 28)
    ),
    selectInput("gender",
                label = h5("Select your gender"),
                choices = list("Male" = "male",
                               "Female" = "female")
    ),
    fileInput("file", h5("Upload a .csv file containing your activity data"),
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv")
    )
  ), #sidebar
  
  dashboardBody(
    useShinyjs(),
    tabItems(
      tabItem(tabName = "Dashboard_tab",
              fluidPage(
                div(id = "welcome_box_outer",
                    box(id = "welcome_box", width = 12, div(id='welcome',
                                                            h2("Welcome to Garmin Run Tracker!"),
                                                            h5(htmlOutput("text")),
                                                            actionButton("show_intro", "Got it!", icon = NULL, width = "200px")
                    )
                    )
                ),
                box(title = "Dynamic Variables", status = "primary", solidHeader = TRUE, width = 12,
                    fluidRow(
                      column(5,
                             dateRangeInput('date_range',
                                            label = h4('Date Range')#,
                                           # start = Sys.Date() - 550, end = Sys.Date()
                             )
                      ),
                      column(5, offset = 1,
                             selectInput("grouping", 
                                         label = h4("Grouping"), 
                                         choices = list("1 Week" = "week", 
                                                        "2 Weeks" = "14 days",
                                                        "Month" = "month",
                                                        "3 Months" = "3 months",
                                                        "6 Months" = "6 months",
                                                        "Year" = "year"), 
                                         selected = 3)
                      )
                    )
                ),
                tabsetPanel(type='tabs',
                            tabPanel(title = strong("Plots"),
                                     
                                     fluidRow(        
                                       box(width=12, status = "success", title = "Distance",
                                           fluidRow(
                                             column(6,
                                                    plotOutput("dist_by_time") %>% 
                                                      withSpinner(6)
                                             ),
                                             column(6,
                                                    plotOutput("dist_by_month") %>% 
                                                      withSpinner(6)
                                             )
                                           )
                                       ),
                                       box(width = 12, status = "success", title = "Time",
                                           fluidRow(
                                             column(6,
                                                    plotOutput("time_per_run") %>% 
                                                      withSpinner(6)
                                             ),
                                             column(6,
                                                    plotOutput("time_per_month") %>% 
                                                      withSpinner(6)
                                             )
                                           )
                                       ),
                                       box(width=12, status = "success", title = "Pace",
                                           fluidRow(
                                             column(6,
                                                    plotOutput("pace_per_run") %>% 
                                                      withSpinner(6)
                                             ),
                                             column(6,
                                                    plotOutput("pace_per_month") %>% 
                                                      withSpinner(6)
                                             )
                                           ),
                                           fluidRow(
                                             column(6,
                                                    plotOutput("pace_distance") %>% 
                                                      withSpinner(6)
                                             ),
                                             column(6,
                                                    plotOutput('pace_date_heartrate') %>% 
                                                      withSpinner(6)
                                             )
                                           ),
                                           fluidRow(
                                             column(6,
                                                    plotOutput("pace_heartrate")) %>% 
                                               withSpinner(6)
                                           )
                                       )
                                     )
                            ),
                            tabPanel(title = strong("Tables"),
                                     fluidRow(
                                       htmlOutput("dist_table")
                                     )),
                            tabPanel(title = strong("Calendar"),
                                     fullcalendarOutput("calendar", 
                                                        height = 'auto')
                                     ),
                            
                            tabPanel(title = strong("Raw Data"),
                                     dataTableOutput("raw_data"))
                )
              )
      )#tabItem ends
    )#tabItems
  )#body
)#ui end


server <- function(input, output, session) {
  ### Reading data from input file
  data <- reactive({
    req(input$file)
    df <- load_and_clean(input$file$datapath, input$gender, input$age)
    return(df)
  })
  
  values <- reactiveValues()
  values$xmin <- NULL
  values$xmax <- NULL
  values$grouping <- NULL
  
  values$date_start <- NULL
  
  # Update date values when set
  observeEvent(input$date_range, {
    values$xmin <- input$date_range[1]
    values$xmax <- input$date_range[2]
  })
  
  # Update date values when set
  observeEvent(input$grouping, {
    values$grouping <- input$grouping
  })
  
  # Remove welcome box
  observeEvent(input$show_intro, {
    hide(id = "welcome_box_outer", anim = TRUE)
  })
  
  # Automatically scale date range
  observe({
    req(data())
    updateDateRangeInput(session, "date_range",
                         start = min(as.Date(data()$date)))
  })
  
  # Collapse sidebar after uploading file
  observeEvent(input$file, {
    shinyjs::addClass(selector = "body", class = "sidebar-collapse")
  })
  
  
  ####### DOWNLOAD CSV GUIDE
  output$text <- renderUI({
    str0 <- "To get started, input your age and gender in the sidebar to the left to calibrate heart rate zones.<br/>"
    str1 <- "Then, log in to "
    url <- a("Garmin Connect", href="https://connect.garmin.com/")
    str2 <- " and click 'Activities' --> 'All activities'.<br/>"
    str3 <- "<br>Click on the running symbol to only display runs, scroll to the bottom of the page and keep scrolling until it no longer refreshes.<br/>"
    str4 <- "<br>Click 'Export CSV' and upload the file in the box in the sidebar.<br/>"
    str5 <- "<br>The date range and grouping of graphs and tables can dynamically be set using the buttons in the box below.<br/>"
    str6 <- "<br>Navigate the app by clicking on the tabs.<br/>"
    str7 <- "<br>Have fun!"
    HTML(paste0(str0, str1, url, str2, str3, str4, str5, str6, str7))
  })
  
  
  
  ####### RAW DATA
  output$raw_data <- renderDT(
    {data() %>% mutate_if(is.numeric, round, 2)},
    class = "display nowrap compact", # style
    filter = "top", # location of column filters
    
    options = list( 
      scrollX = TRUE) # allow user to scroll wide tables horizontally
  )
  
  ###### TABLES
  output$dist_table <- renderText({
    req(data())
    distance_tab(data(), values$grouping)
  })
  
  #### CALENDAR WIDGET
  output$calendar <- renderFullcalendar({
    req(data())
    df <- data()
    data <- data.frame(title = paste(df$distance, "km in", round(df$time, 0), "min"), 
                       start = as.Date(df$date),
                       end = as.Date(df$date),
                       color = '#74C476')
    
    fullcalendar(data)
  })
  
  
  ####################################### PLOTS
  ### distance
  output$dist_by_time <- renderPlot({
    req(data())
    dist_by_time(data(), values$xmin, values$xmax)
  })
  
  output$dist_by_month <- renderPlot({
    req(data())
    dist_by_month(data(), values$grouping, values$xmin, values$xmax)
    
  })
  
  ##### TIME PLOTS
  output$time_per_run <- renderPlot({
    req(data())
    time_per_run(data(), values$xmin, values$xmax)
  })  
  
  output$time_per_month <- renderPlot({
    req(data())
    time_per_month(data(), values$grouping, values$xmin, values$xmax)
  })
  
  # pace
  output$pace_per_run <- renderPlot({
    if(is.null(data)){
      return(NULL)
    }else{
      plot <- pace_per_run(data(), values$xmin, values$xmax)
      plot
    }})
  
  output$pace_per_month <- renderPlot({
    if(is.null(data)){
      return(NULL)
    }else{
      plot <- pace_per_month(data(), values$grouping, values$xmin, values$xmax)
      print(plot)
    }})
  
  output$pace_distance <- renderPlot({
    if(is.null(data)){
      return(NULL)
    }else{
      plot <- pace_distance(data(), values$xmin, values$xmax)
      print(plot)
    }})
    
  output$pace_date_heartrate <- renderPlot({
    if(is.null(data)){
      return(NULL)
    }else{
      plot <- pace_date_heartrate(data(), values$xmin, values$xmax)
      print(plot)
    }})
  
  output$pace_heartrate <- renderPlot({
    if(is.null(data)){
      return(NULL)
    }else{
      plot <- pace_heartrate(data(), values$xmin, values$xmax)
      print(plot)
    }})
  #  }
}#server end


shinyApp(ui, server) #Run app
