# Import libraries
library(tidyverse)
library(lubridate)
library(splines)
library(gridExtra)

# Function to download data from HealthData.gov
get_data <- function(domain, identifier, limit = 50000, offset = 0, date_query) {
  require(jsonlite)
  
  URL <- sprintf('https://%s/resource/%s.json', domain, identifier)
  parameters <- sprintf("?$order=:id&$limit=%s&$offset=%s&$where=%s",
                        limit, offset, date_query)
  data <- read_json(paste(URL, parameters, sep = ''), simplifyVector = TRUE)
  
  return(data)
}

# Inverse logit function
inv.logit <- function(x) {
  return(exp(x)/(1 + exp(x)))
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
pop_estimate_regional <- pop_est %>%
  group_by(region) %>%
  summarise_if(is.numeric, sum)

# Grab vaccination data
vaccines_df <- get_data('data.cdc.gov', identifier = 'unsk-b7fc', 
                     date_query = "date between '2021-07-26' and '2022-05-29'")

# Grab epidemiologic data
epi_df <- get_data('data.cdc.gov', identifier = '9mfq-cb36', 
                   date_query = "submission_date between '2021-07-26' and '2022-05-29'")

# Transforming the vaccine data
vaccines_df <- vaccines %>% 
  select(date, location, administered, administered_18plus) 

  mutate(date = as.Date(date),
         new_case = as.numeric(new_case),
         new_death = as.numeric(new_death)) %>%
  group_by(state, year = isoyear(submission_date), week = isoweek(submission_date)) %>% 
  summarise_if(is.numeric, sum) %>%
  mutate(date = parse_date_time(sprintf('%s-%s-0', year, week), '%Y-%U-%w') + days(7)) %>%
  left_join(crosswalk, by = c('state' = 'state'))

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

# Transform the learning modality data
df <- lm_df %>%
  filter(state != 'BI' & state != 'PR') %>%
  mutate(learning_modality = if_else(learning_modality == 'In Person', 0, 1),
         week = as.Date(week),
         time = as.numeric(week - min(week)),
         operational_schools = as.integer(operational_schools),
         student_count = as.integer(student_count)) %>%
  left_join(epi_df, by = c('state' = 'state', 'week' = 'date')) %>%
  left_join(crosswalk, by = c('state' = 'state')) %>%
  left_join(pop_est, by = c('state' = 'state')) %>%
  mutate(region = as.factor(region),
         cases_per_100k = 100000 * new_case / POPESTIMATE2021,
         deaths_per_100k = 100000 * new_death / POPESTIMATE2021,
         students_per_school = student_count / operational_schools) %>%
  select(district_nces_id, district_name, week, learning_modality,
         operational_schools, student_count, city, state, zip_code, 
         time, cases_per_100k, deaths_per_100k, students_per_school, region) %>%
  drop_na(operational_schools, student_count)

# Missingness analysis
pivot <- df %>%
  select(district_nces_id, week, learning_modality) %>%
  pivot_wider(id_cols = district_nces_id, 
              names_from = week, values_from = learning_modality) %>%
  pivot_longer(!district_nces_id, names_to = 'week', values_to = 'is_missing') %>%
  mutate(is_missing = if_else(is.na(is_missing), 1, 0))

ggplot(pivot) + 
  geom_tile(aes(x = week, y = district_nces_id, fill = as.factor(is_missing))) +
  scale_fill_manual(values = c('beige','darkred')) +
  theme(
    axis.text.x = element_blank(), # Remove x axis labels
    axis.ticks.y = element_blank(), # Remove y axis ticks
    axis.text.y = element_blank()  # Remove y axis labels
  ) +
  labs(x = 'Week', y = 'District', title = 'Visualization of Missingness by District and Week')

missingness <- pivot %>%
  group_by(district_nces_id) %>%
  summarise(pct_missing = 100 * mean(is_missing))

# Distribution of data missingness
ggplot(missingness) +
  geom_histogram(aes(x = pct_missing),
                 color = 'black',
                 fill = 'darkred',
                 alpha = 0.7,
                 bins = 10) +
  labs(x = '% of Data Missing', y = 'Number of Districts', 
       title = 'Distribution of Data Missingness') +
  theme_light()

# Districts with no missing data
complete_districts <- missingness[missingness$pct_missing == 0,]$district_nces_id

# Only keep districts with no missing data
df_original <- df
df <- df_original %>%
  filter(district_nces_id %in% complete_districts)

# Aggregating/averaging the data by region
regional <- df %>%
  group_by(region, week) %>%
  summarise(pct_disrupted = 100 * mean(learning_modality),
            students_per_school = mean(students_per_school),
            cases_per_100k = mean(cases_per_100k),
            deaths_per_100k = mean(deaths_per_100k)) %>% 
  mutate(time = as.numeric(as.Date(week) - min(as.Date(week))))

# Graphing the regional learning modality data
ggplot(regional) + 
  geom_rect(aes(xmin = as.POSIXct("2021-12-01"), xmax = as.POSIXct("2022-02-01"), 
                ymin = 0, ymax = Inf), fill = "grey90", 
            alpha = 0.2, col = "grey90", inherit.aes = FALSE) +
  geom_line(aes(x = week, y = pct_disrupted, color = region), size = 1) + 
  geom_point(aes(x = week, y = pct_disrupted, color = region, shape = region), size = 1.5) +
  scale_color_manual(values = c('darkgoldenrod3', 'steelblue', 'darkred', 'aquamarine3')) +
  labs(x = 'Date', y = '% of Districts in Hybrid or Remote', 
       title = '% of Districts in Hybrid or Remote over Time') +
  theme_light()

# Epi curves
ggplot(regional) + 
  geom_rect(aes(xmin = as.POSIXct("2021-12-01"), xmax = as.POSIXct("2022-02-01"), 
                ymin = 0, ymax = Inf), fill = "grey90", 
            alpha = 0.2, col = "grey90", inherit.aes = FALSE) +
  geom_line(aes(x = week, y = cases_per_100k, color = region), size = 1) + 
  geom_point(aes(x = week, y = cases_per_100k, color = region, shape = region), size = 1.5) +
  scale_color_manual(values = c('darkgoldenrod3', 'steelblue', 'darkred', 'aquamarine3')) +
  labs(x = 'Date', y = 'Cases per 100K', 
       title = 'Average Cases per 100K by Region') +
  theme_light()

# Model 1
model1 <- glm(learning_modality ~ region + time + I(time^2) + region:time, data = df, family = 'binomial')
summary(model1)

# Model 2
model2 <- glm(learning_modality ~ region + cases_per_100k + region:cases_per_100k + 
                deaths_per_100k + region:deaths_per_100k  + students_per_school + 
                region:students_per_school + time + I(time^2), 
              data = df, family = 'binomial')
summary(model2)

# Model 3
model3 <- glm(learning_modality ~ region + ns(time, df = 12) + region:ns(time, df = 12), 
              data = df, family = binomial())
summary(model3)

regional$model1_probs <- 100 * predict.glm(model1, regional[,c('region', 'time')], type = 'response')
regional$model2_probs <- 100 * predict.glm(model2, regional[,c('region', 'students_per_school', 'cases_per_100k', 'deaths_per_100k', 'time')], type = 'response')
regional$model3_probs <- 100 * predict.glm(model3, regional[,c('region', 'time')], type = 'response')

region_viz <- function(y, linesize = 1, pointsize = 2, 
                       colors = c('darkgoldenrod3', 'steelblue', 'darkred', 'aquamarine3')) {
  ggplot(model_comparisons) + 
    geom_rect(aes(xmin = as.POSIXct("2021-12-01"), xmax = as.POSIXct("2022-02-01"), 
                  ymin = 0, ymax = Inf), fill = "grey90", 
              alpha = 0.2, col = "grey90", inherit.aes = FALSE) +
    geom_line(aes(x = date, y = !!ensym(y), color = region), size = linesize) +
    geom_point(aes(x = date, y = pct_disrupted, color = region, shape = region), size = pointsize) +
    scale_color_manual(values = colors) +
    labs(x = 'Date', y = 'Probability of Disruption (%)', 
         title = sprintf('%s Estimated Probability of School Disruption by Region', 
                         str_to_title(strsplit(y, '_')[[1]][1]))) +
    theme_light()
  
}

chart_mod1 <- region_viz('model1_probs')
chart_mod2 <- region_viz('model2_probs')
chart_mod3 <- region_viz('model3_probs')

grid.arrange(chart_mod1, chart_mod2, chart_mod3, nrow = 3)

model_viz <- function(region, size = 1.1) {
  ggplot(model_comparisons[model_comparisons$region == region,]) +
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
    geom_line(aes(x = date, y = model3_probs, 
                  linetype = 'Model 3', color = 'Model 3'), 
              size = size) +
    geom_point(aes(x = date, y = pct_disrupted)) +
    labs(x = 'Date', y = 'Probability of Disruption', 
         title = sprintf('%s: Observed and Estimated Probability of School Disruption', 
                         str_to_sentence(region)),
         linetype = 'Legend', color = 'Legend') +
    scale_color_manual(values = c('#a6611a', '#7b3294', '#018571')) +
    theme_light()
}

chart_west <- model_viz('west')
chart_midwest <- model_viz('midwest')
chart_northeast <- model_viz('northeast')
chart_south <- model_viz('south')

grid.arrange(chart_west, chart_midwest, chart_northeast, chart_south, nrow = 2)
