# Import libraries
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

# Inverse logit function
inv.logit <- function(x) {
  return(exp(x)/(1 + exp(x)))
}

# Grab epidemiologic data
epi_df <- get_data('data.cdc.gov', identifier = '9mfq-cb36', date_query = "submission_date between '2021-07-26' and '2022-05-29'")

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
  mutate(date = parse_date_time(sprintf('%s-%s-0', year, week), '%Y-%U-%w') + days(7))

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

# Crosswalk for US states and Census regions
northeast <- c('CT', 'ME', 'MA', 'NH', 'RI', 'VT', 'NJ', 'NY', 'PA')
midwest <- c('IN', 'IL', 'MI', 'OH', 'WI', 'IA', 'NE', 'KS', 'ND', 'MN', 'SD', 'MO')
south <- c('DE', 'DC', 'FL', 'GA', 'MD', 'NC', 'SC', 'VA', 'WV', 'AL', 'KY', 'MS',
           'TN', 'AR', 'LA', 'OK', 'TX')
west <- c('AZ', 'CO', 'ID', 'NM', 'MT', 'UT', 'NV', 'WY', 'AK', 'CA', 'HI', 'OR', 'WA')

# Transform the learning modality data
df <- lm_df %>%
  filter(state != 'BI' & state != 'PR') %>%
  mutate(learning_modality = ifelse(learning_modality == 'In Person', 0, 1),
         week = as.Date(week),
         time = as.numeric(week - min(week)),
         operational_schools = as.integer(operational_schools),
         student_count = as.integer(student_count),
         region = as.factor(case_when(state %in% northeast ~ 'northeast',
                            state %in% midwest ~ 'midwest',
                            state %in% south ~ 'south',
                            state %in% west ~ 'west'))) %>%
  left_join(epi_df, by = c('state' = 'state', 'week' = 'date')) %>%
  select(district_nces_id, district_name, week, learning_modality,
         operational_schools, student_count, city, state, zip_code, time, 
         region, new_case, new_death)

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
  summarise(pct_missing = 100 *mean(is_missing))

# Distribution of data missingness
ggplot(missingness) +
  geom_histogram(aes(x = pct_missing),
                 color = 'black',
                 fill = 'darkred',
                 alpha = 0.7,
                 bins = 10) +
  labs(x = '% of Data Missing', y = 'Number of Districts', 
       title = 'Distribution of Data Missingness')

# Aggregating by school data
national <- df %>%
  group_by(week) %>%
  summarise(pct_disrupted = 100 * mean(learning_modality))

regional <- df %>%
  group_by(region, week) %>%
  summarise(pct_disrupted = 100 * mean(learning_modality))

#
ggplot(national) + 
  geom_line(aes(x = week, y = pct_disrupted)) + 
  geom_point(aes(x = week, y = pct_disrupted)) +
  labs(x = 'Date', y = '% of Districts in Hybrid or Remote', 
       title = '% of Districts in Hybrid or Remote over Time')

ggplot(regional) + 
  geom_line(aes(x = week, y = pct_disrupted, color = region)) + 
  geom_point(aes(x = week, y = pct_disrupted, color = region, shape = region)) +
  labs(x = 'Date', y = '% of Districts in Hybrid or Remote', 
       title = '% of Districts in Hybrid or Remote over Time')

# Aggregating the epidemiologic data
epi_regional <- epi_df %>%
  mutate(region = as.factor(case_when(state %in% northeast ~ 'northeast',
                                      state %in% midwest ~ 'midwest',
                                      state %in% south ~ 'south',
                                      state %in% west ~ 'west'))) %>%
  group_by(region, date) %>%
  summarise(cases = sum(new_case),
            deaths = sum(new_death),
            avg_cases = mean(new_case),
            avg_deaths = mean(new_death)) %>%
  mutate(date = as.Date(date),
         time = as.numeric(date - min(date)))

# Epi curves
ggplot(epi_regional) + geom_line(aes(x = date, y = cases, color = region))
ggplot(epi_regional) + geom_line(aes(x = date, y = deaths, color = region))

# Districts with no missing data
complete_districts <- missingness[missingness$pct_missing == 0,]$district_nces_id

# Model 1
model1 <- glm(learning_modality ~ region + region*new_case, data = df, family = 'binomial')
summary(model1)
betas <- model1$coefficients

# Model 2
model2 <- glm(learning_modality ~ region + time + I(time^2) + region * time, data = df, family = 'binomial')
summary(model2)
gammas <- model2$coefficients

# Model 3
model3 <- glm(learning_modality ~ region + ns(time, df = 10) + region*ns(time, df = 10), 
              data = df, family = binomial())
summary(model3)


week_grid <- sort(unique(df$week))
time_grid <- sort(unique(df$time))

model2_probs <- data.frame(week_grid,
                  midwest = 100 * inv.logit(gammas[1:4] %*% c(1, 0, 0, 0) + gammas[5]*time_grid + gammas[6]*time_grid^2),
                  northeast = 100 * inv.logit(sum(gammas[c(1, 2)]) + sum(gammas[c(5, 7)])*time_grid + gammas[6]*time_grid^2),
                  south = 100 * inv.logit(sum(gammas[c(1, 3)]) + sum(gammas[c(5, 8)])*time_grid + gammas[6]*time_grid^2),
                  west = 100 * inv.logit(sum(gammas[c(1, 4)]) + sum(gammas[c(5, 9)])*time_grid + gammas[6]*time_grid^2))

model2_probs <- model2_probs %>%
  pivot_longer(!week_grid, names_to = 'region', values_to = 'model2_probs')
  

model_comparisons <- epi_regional %>%
  mutate(model1_probs = case_when(region == 'midwest' ~ 100 * inv.logit(betas[1:4] %*% c(1, 0, 0, 0) +
                                                                          as.numeric(betas[5])*avg_cases),
                                  region == 'northeast' ~ 100 * inv.logit(sum(betas[c(1, 2)]) + 
                                                                            sum(betas[c(5, 6)])*avg_cases),
                                  region == 'south' ~ 100 * inv.logit(sum(betas[c(1, 3)]) + 
                                                                        sum(betas[c(5, 7)])*avg_cases),
                                  region == 'west' ~ 100 * inv.logit(sum(betas[c(1, 4)]) + 
                                                                       sum(betas[c(5, 8)])*avg_cases)
                                  
  )) %>%
  left_join(model2_probs, by = c('region' = 'region', 'date' = 'week_grid')) %>%
  left_join(regional, by = c('date' = 'week', 'region' = 'region'))

model_comparisons$model3_probs <- 100 * predict.glm(model3, model_comparisons[,c('region', 'time')], type = 'response')

region_viz <- function(y, linesize = 1, pointsize = 2, 
                       colors = c('darkgoldenrod3', 'steelblue', 'darkred', 'aquamarine3')) {
  ggplot(model_comparisons) + 
    geom_line(aes(x = date, y = !!ensym(y), color = region), size = linesize) +
    geom_point(aes(x = date, y = pct_disrupted, color = region, shape = region), size = pointsize) +
    scale_color_manual(values = colors) +
    labs(x = 'Date', y = 'Probability of Disruption (%)', 
         title = sprintf('%s Estimated Probability of School Disruption by Region', 
                         str_to_title(strsplit(y, '_')[[1]][1])))
  
}

region_viz('model1_probs')
region_viz('model2_probs')
region_viz('model3_probs')

model_viz <- function(region, size = 1.1) {
  ggplot(model_comparisons[model_comparisons$region == region,]) +
    geom_line(aes(x = date, y = model1_probs, linetype = 'Model 1', color = 'Model 1'), size = size) +
    geom_line(aes(x = date, y = model2_probs, linetype = 'Model 2', color = 'Model 2'), size = size) +
    geom_line(aes(x = date, y = model3_probs, linetype = 'Model 3', color = 'Model 3'), size = size) +
    geom_point(aes(x = date, y = pct_disrupted)) +
    labs(x = 'Date', y = 'Probability of Disruption', 
         title = sprintf('%s: Observed and Estimated Probability of School Disruption', str_to_sentence(region)),
         linetype = 'Legend', color = 'Legend')
}

model_viz('west')
model_viz('midwest')
model_viz('northeast')
model_viz('south')
