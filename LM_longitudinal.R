# Import libraries
library(tidyverse)
library(lubridate)
library(splines)
library(gridExtra)

# Loading the data
source('data_integration.R')

# Missingness analysis
pivot <- df %>%
  select(district_nces_id, week, learning_modality) %>%
  pivot_wider(id_cols = district_nces_id, 
              names_from = week, values_from = learning_modality) %>%
  pivot_longer(!district_nces_id, names_to = 'week', values_to = 'is_missing') %>%
  mutate(is_missing = if_else(is.na(is_missing), 1, 0),
         week = as.Date(week))

# Plot of missing data for each district by week
ggplot(pivot) + 
  geom_tile(aes(x = week, y = district_nces_id, fill = as.factor(is_missing))) +
  scale_fill_manual(values = c('beige','darkred'), labels = c('No', 'Yes')) +
  theme(
    axis.ticks.y = element_blank(), # Remove y axis ticks
    axis.text.y = element_blank(),  # Remove y axis labels
    legend.background = element_rect(color = 'black',
                                     size = 1.1),
    legend.position = c(0.85, 0.85)
  ) +
  labs(x = 'Week', y = 'District', 
       title = 'Visualization of Missingness by District and Week',
       fill = 'Data Missing?') +
  scale_x_date(date_labels = "%b", date_breaks = "month", name = "Month")

# Distribution of data missingness
missingness <- pivot %>%
  group_by(district_nces_id) %>%
  summarise(pct_missing = 100 * mean(is_missing))

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
            total_vaccines_per_100k = mean(total_vaccines_per_100k),
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
model1_full <- glm(learning_modality ~ region + cases_per_100k + deaths_per_100k + 
                     students_per_school + total_vaccines_per_100k + 
                     region:cases_per_100k + region:deaths_per_100k  + 
                     region:students_per_school + region:total_vaccines_per_100k, 
                   data = df, family = binomial())
model1_reduced <- glm(learning_modality ~ cases_per_100k + deaths_per_100k + 
                        students_per_school + total_vaccines_per_100k, 
                      data = df, family = binomial())
anova(model1_reduced, model1_full, test = 'LRT')
model1 <- model1_full
summary(model1)

# Model 2
model2_full <- glm(learning_modality ~ region + bs(time, df = 6) + region:bs(time, df = 6), 
              data = df, family = binomial())
model2_reduced <- glm(learning_modality ~ bs(time, df = 6), 
                   data = df, family = binomial())
anova(model2_reduced, model2_full, test = 'LRT')
model2 <- model2_full
summary(model2)

# Model predictions
regional$model1_probs <- 100*predict.glm(model1, 
                                         regional[,c('region','students_per_school', 'total_vaccines_per_100k', 'cases_per_100k', 'deaths_per_100k')], 
                                         type = 'response')
regional$model2_probs <- 100*predict.glm(model2, 
                                         regional[,c('region', 'time')], 
                                         type = 'response')

chart_mod1 <- region_viz('model1_probs')
chart_mod2 <- region_viz('model2_probs')

grid.arrange(chart_mod1, chart_mod2, nrow = 2)

chart_west <- model_viz('West')
chart_midwest <- model_viz('Midwest')
chart_northeast <- model_viz('Northeast')
chart_south <- model_viz('South')

grid.arrange(chart_west, chart_midwest, chart_northeast, chart_south, nrow = 2)

temp <- df %>%
  group_by(district_nces_id) %>%
  mutate(lag_total_vaccines_per_100k = lag(total_vaccines_per_100k, order_by = week))
