##############################
## SHINY RUNNING DASHBOARD
##############################


pacman::p_load(tidyverse, shiny, janitor, lubridate, scales)

df <- read_csv('Activities.csv') %>% 
  clean_names()

# removing runs without heart rate measurements
df <- df[df$maks_puls != "--",]

# Adding aerobic/anaerobic column
df <- df %>% 
  mutate(aerob = ifelse(gennemsnitlig_puls < 155, 'Aerobic', 'Anaerobic'))



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
dist_by_time <- ggplot(df, aes(dato, distance, color = aerob)) + 
  theme_bw() +
  geom_point() +
  stat_smooth(geom = 'line', method = 'lm', se = FALSE, alpha = 0.2) +
  scale_color_brewer(palette = 'Dark2') +
  theme(legend.title = element_blank()) +
  labs(x = 'Date', y = 'Distance (km)', title = 'Distance run')


# Distance pr month
dist_by_month <- df %>% group_by(month=floor_date(dato, "month")) %>%
  summarize(distance=sum(distance)) %>% 
  ggplot(aes(month, distance)) + 
  theme_bw() +
  geom_point(color = 'steelblue2') +
  geom_line(color = 'steelblue') +
  stat_smooth(geom = 'line', method = 'lm', se = FALSE, alpha = 0.4, col = 'lightsteelblue') +
  theme(legend.title = element_blank()) +
  labs(x = 'Date', y = 'Distance (km)', title = 'Km run per month')

# time pr run
time_per_run <- ggplot(df, aes(dato, tid, color = aerob)) +
  theme_bw() +
  geom_point() +
  stat_smooth(geom = 'line', method = 'lm', se = FALSE, alpha = 0.2) +
  scale_color_brewer(palette = 'Dark2') +
  theme(legend.title = element_blank()) +
  scale_y_time(labels = time_format("%H:%M")) +
  labs(x = 'Date', y = 'Time', title = 'Time pr. run')

# time spent running pr month
time_per_month <- df %>% group_by(month = floor_date(dato, "month")) %>% 
  summarize(tid = sum(minute(tid))) %>% 
  ggplot(aes(month, tid)) + 
  theme_bw() +
  geom_point(color = 'steelblue2') +
  geom_line(color = 'steelblue') +
  stat_smooth(geom = 'line', method = 'lm', se = FALSE, alpha = 0.4, col = 'lightsteelblue') +
  theme(legend.title = element_blank()) +
  labs(x = 'Date', y = 'Time (min)', title = 'Minutes run per month')

# SPEED
pace_per_run <- df %>% 
  mutate(gennemsnitstempo = as.POSIXct(gennemsnitstempo, format = '%H:%M:%S')) %>% 
  ggplot(aes(dato, gennemsnitstempo, color = aerob)) +
  theme_bw() +
  geom_point() +
  scale_color_brewer(palette = 'Dark2') +
  theme(legend.title = element_blank()) +
  scale_y_continuous(trans = rev_date) +
  labs(x = 'Date', y = 'Avg. pace (min/km)', title = 'Average pace')

pace_per_month <- df %>% 
  group_by(month = floor_date(dato, 'month')) %>% 
  summarize(pace = mean(period_to_seconds(hms(gennemsnitstempo)))) %>%
  mutate(pace = seconds_to_period(pace)) %>% 
  ggplot(aes(month, pace)) + 
  theme_bw() +
  geom_point(color = 'steelblue2') +
  geom_line(color = 'steelblue') +
  stat_smooth(geom = 'line', method = 'lm', se = FALSE, alpha = 0.6, col = 'lightsteelblue') +
  theme(legend.title = element_blank()) +
  scale_y_continuous(trans=scales::reverse_trans() %::% scales::hms_trans(), labels = time_format("%H:%M")) +
  labs(x = 'Date', y = 'Time (min)', title = 'Average pace pr. month')

# pace, distance
pace_distance <-df %>% 
  mutate(gennemsnitstempo = as.POSIXct(gennemsnitstempo, format = '%H:%M:%S')) %>% 
  ggplot(aes(dato, gennemsnitstempo, color = distance)) +
  theme_bw() +
  geom_point(size = 2) +
  scale_color_viridis(option = 'A', direction = -1) +
  scale_y_continuous(trans = rev_date) +
  labs(x = 'Date', y = 'Pace (min/km)', title = 'Average pace by distance run', color = 'Distance')

#####
## Ved mere data - evt. plot tendenslinjer for specifikke distance, fx 5, 10k 
#####

# pace, heart rate
pace_heartrate <- df %>% 
  mutate(gennemsnitstempo = as.POSIXct(gennemsnitstempo, format = '%H:%M:%S')) %>% 
  ggplot(aes(gennemsnitstempo, gennemsnitlig_puls)) +
  theme_bw() +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", se = F, col = 'lightsteelblue', alpha = 0.6) +
  scale_color_viridis(option = 'A', direction = -1) +
  scale_x_continuous(trans = rev_date) +
  labs(x = 'Avg. Pace', y = 'Avg. Heart Rate', title = 'Average heart rate by pace')

  
##### Add some tables
###
# Choose time interval for scales 
# Choose grouping (month, year)
###





