# Importing libraries
library(tidyverse)
library(lubridate)

# Function to download data from HealthData.gov
get_data <- function(domain, identifier, limit = 50000, offset = 0, date_query) {
  require(jsonlite)
  
  URL <- sprintf('https://%s/resource/%s.json', domain, identifier)
  parameters <- sprintf("?$order=:id&$limit=%s&$offset=%s&$where=%s",
                        limit, offset, date_query)
  data <- read_json(paste(URL, parameters, sep = ''), simplifyVector = TRUE)
  
  return(data)
}

region_viz <- function(y, linesize = 1, pointsize = 2, 
                       colors = c('darkgoldenrod3', 'steelblue', 'darkred', 'aquamarine3')) {
  ggplot(regional) + 
    geom_rect(aes(xmin = as.POSIXct("2021-12-01"), xmax = as.POSIXct("2022-02-01"), 
                  ymin = 0, ymax = Inf), fill = "grey90", 
              alpha = 0.2, col = "grey90", inherit.aes = FALSE) +
    geom_line(aes(x = week, y = !!ensym(y), color = region), size = linesize) +
    geom_point(aes(x = week, y = pct_disrupted, color = region, shape = region), size = pointsize) +
    scale_color_manual(values = colors) +
    labs(x = 'Date', y = 'Probability of Disruption (%)', 
         title = sprintf('%s Estimated Probability of School Disruption by Region', 
                         str_to_title(strsplit(y, '_')[[1]][1]))) +
    theme_light()
  
}

model_viz <- function(region, size = 1.1) {
  ggplot(model_comparisons[regional$region == region,]) +
    geom_rect(aes(xmin = as.POSIXct("2021-12-01"), 
                  xmax = as.POSIXct("2022-02-01"), 
                  ymin = 0, ymax = Inf), 
              fill = "grey90", 
              alpha = 0.2, col = "grey90", inherit.aes = FALSE) +
    geom_line(aes(x = date, y = model1_probs, 
                  linetype = 'Model 1', color = 'Model 1'), 
              size = size) +
    geom_line(aes(x = date, y = model2_probs, 
                  linetype = 'Model 2', color = 'Model 2'), 
              size = size) +
    geom_point(aes(x = date, y = pct_disrupted)) +
    labs(x = 'Date', y = 'Probability of Disruption', 
         title = sprintf('%s: Observed and Estimated Probability of School Disruption', 
                         str_to_sentence(region)),
         linetype = 'Legend', color = 'Legend') +
    scale_color_manual(values = c('#a6611a', '#7b3294')) +
    theme_light()
}

# State crosswalk
crosswalk <- data.frame(state_full = state.name, state = state.abb, 
                        region = str_replace(state.region, 'North Central', 'Midwest'))
crosswalk <- rbind(crosswalk, c('District of Columbia', 'DC', 'South'))

# Grab Census population estimates
pop_est <- read.csv('https://www2.census.gov/programs-surveys/popest/datasets/2020-2021/state/totals/NST-EST2021-alldata.csv')
pop_est <- pop_est %>%
  left_join(crosswalk, by = c('NAME' = 'state_full')) %>%
  select(state, region, POPESTIMATE2021)

# Grab vaccination data
vaccines_df <- get_data('data.cdc.gov', identifier = 'unsk-b7fc', 
                        date_query = "date between '2021-07-26' and '2022-05-29'")

# Grab epidemiologic data
epi_df <- get_data('data.cdc.gov', identifier = '9mfq-cb36', 
                   date_query = "submission_date between '2021-07-26' and '2022-05-29'")

# Grab the first 90000 rows and dataset schema for the learning modality data
lm_df <- get_data('HealthData.gov', 'aitj-yx37', limit = 90000, offset = 0, 
                  date_query = "week between '2021-08-01' and '2022-06-01'")

# Load remaining data, 90000 rows at a time
for(i in 1:6) {
  batch <- get_data('HealthData.gov', 'aitj-yx37', limit = 90000, offset = 90000*i, 
                    date_query = "week between '2021-08-01' and '2022-06-01'")
  lm_df <- bind_rows(lm_df, batch)
  rm(batch)
  Sys.sleep(10)
}

# Transforming the vaccine data
vaccines_df <- vaccines_df %>%
  mutate(date = as.Date(date),
         administered = as.numeric(administered),
         administered_18plus = as.numeric(administered_18plus),
         ped_vaccinations = administered - administered_18plus) %>%
  filter(date %in% unique(as.Date(lm_df$week))) %>%
  select(date, location, ped_vaccinations) 

# Transforming the epidemiologic data and aggregating by week
epi_df <- epi_df %>%
  filter(state != 'PR' & state != 'FSM' & state != 'GU' & state != 'MP' & state != 'AS'
         & state != 'NYC' & state != 'PW' & state != 'RMI' & state != 'VI') %>%
  select(submission_date, state, new_case, new_death) %>%
  mutate(submission_date = as.Date(submission_date),
         new_case = as.numeric(new_case),
         new_death = as.numeric(new_death)) %>%
  group_by(state, year = isoyear(submission_date), week = isoweek(submission_date)) %>% 
  summarise_if(is.numeric, sum) %>%
  mutate(date = parse_date_time(sprintf('%s-%s-0', year, week), '%Y-%U-%w') + days(7)) %>%
  left_join(crosswalk, by = c('state' = 'state'))

# Transform the learning modality data and merging all the datasets
df <- lm_df %>%
  filter(state != 'BI' & state != 'PR') %>%
  mutate(learning_modality = if_else(learning_modality == 'In Person', 0, 1),
         week = as.Date(week),
         time = as.numeric(week - min(week)),
         operational_schools = as.integer(operational_schools),
         student_count = as.integer(student_count)) %>%
  left_join(epi_df, by = c('state' = 'state', 'week' = 'date')) %>%
  left_join(vaccines_df, by = c('state' = 'location', 'week' = 'date')) %>%
  left_join(crosswalk, by = c('state' = 'state')) %>%
  left_join(pop_est, by = c('state' = 'state')) %>%
  mutate(region = as.factor(region),
         total_vaccines_per_100k = 100000 * ped_vaccinations / POPESTIMATE2021,
         cases_per_100k = 100000 * new_case / POPESTIMATE2021,
         deaths_per_100k = 100000 * new_death / POPESTIMATE2021,
         students_per_school = student_count / operational_schools) %>%
  select(district_nces_id, district_name, week, learning_modality,
         operational_schools, student_count, city, state, zip_code, total_vaccines_per_100k,
         time, cases_per_100k, deaths_per_100k, students_per_school, region) %>%
  drop_na(operational_schools, student_count)
