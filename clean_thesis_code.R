## -----------------------------------------------------------------------------
---
title: "Thesis"
output: html_notebook
---


## -----------------------------------------------------------------------------
library(tidyverse)
library(dplyr)
library(jtools)
library(ggplot2)
library(lubridate)
library(psych)
library(corrplot)
library(naniar)
library(GGally)
library(janitor)
library(ResourceSelection)
library(car)
library(lmtest)
library(sandwich)
library(Hmisc)
library(visreg)
library(purrr)
library(psych)
library(broom)
library(openxlsx)


## -----------------------------------------------------------------------------
#read the file and save it as a data frame
movie_data <- as.data.frame(read_csv("thesis_data.csv"))

#remove rows where all values are NAs (we leave a gap after the coma since we don't need to specify columns)
movie_data <- movie_data[-(133:138), ]

#check types of variables
str(movie_data)



## -----------------------------------------------------------------------------
#remove dollar sign from the front of the numbers
movie_data_clean <- movie_data %>%
  mutate(gross_total_in_thousand_dollars = gsub("\\$", "", gross_total_in_thousand_dollars),
         gross_opening_wknd_in_thousand_dollars = gsub("\\$", "", gross_opening_wknd_in_thousand_dollars),
         production_budget_in_thousand_dollars = gsub("\\$", "", production_budget_in_thousand_dollars)
  )

#remove the commas
movie_data_clean <- movie_data_clean %>%
  mutate(gross_total_in_thousand_dollars = gsub(",", "", gross_total_in_thousand_dollars),
         gross_opening_wknd_in_thousand_dollars = gsub(",", "", gross_opening_wknd_in_thousand_dollars),
         production_budget_in_thousand_dollars = gsub(",", "", production_budget_in_thousand_dollars)
  )

#turn the columns into numeric and deivide my 1,000,000 (not sure about this)
movie_data_clean <- movie_data_clean %>%
  mutate(gross_total_in_thousand_dollars = round(as.numeric(gross_total_in_thousand_dollars)/1000000, 2),
         gross_opening_wknd_in_thousand_dollars = round(as.numeric(gross_opening_wknd_in_thousand_dollars)/1000000, 2),
         production_budget_in_thousand_dollars = round(as.numeric(production_budget_in_thousand_dollars)/1000000, 2)
  )

#check if it is numeric
str(movie_data_clean)

#rename the columns
movie_data_clean <- movie_data_clean %>%
  rename(
    gross_total_millions = gross_total_in_thousand_dollars,
    gross_opening_millions = gross_opening_wknd_in_thousand_dollars,
    production_budget_millions = production_budget_in_thousand_dollars,
    R_rated = MPAA_rating
  )



## -----------------------------------------------------------------------------
#shift the scale from -1 to 1 to 0 to 2 and normalize it to a o to 1 scale (0=-1, 0.5=0, 1=1)
movie_data_clean <- movie_data_clean %>%
  mutate(news_valence = (news_valence + 1)/2)



## -----------------------------------------------------------------------------
movie_data_clean %>%
   summary() 

#histograms to observe distribution and skewness
ggplot(movie_data_clean, aes(x = gross_opening_millions)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Gross Opening Weekend", x = "Million $", y = "Frequency")

ggplot(movie_data_clean, aes(x = gross_opening_millions)) +
  geom_density(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Gross Opening Weekend", x = "Million $", y = "Frequency")

ggplot(movie_data_clean, aes(x = news_volume)) +
  geom_histogram(binwidth = 50, fill = "yellow3", color = "black") +
  labs(title = "News Volume", x = "Number of News", y = "Frequency")

ggplot(movie_data_clean, aes(x = news_valence)) +
  geom_histogram(binwidth = 0.1, fill = "pink", color = "black") +
  labs(title = "News Valence", x = "Sentiment", y = "Frequency")

ggplot(movie_data_clean, aes(x = search_volume)) +
  geom_histogram(binwidth = 5, fill = "green2", color = "black") +
  labs(title = "Search Volume", x = "Google Searches", y = "Frequency")

ggplot(movie_data_clean, aes(x = reddit_comments)) +
  geom_histogram(binwidth = 100, fill = "purple3", color = "black") +
  labs(title = "Engagement", x = "Reddit Comments", y = "Frequency")

ggplot(movie_data_clean, aes(x = production_budget_millions)) +
  geom_histogram(binwidth = 10, fill = "orange", color = "black") +
  labs(title = "Production Budget", x = "Million $", y = "Frequency")



## -----------------------------------------------------------------------------
#select only numeric columns and calculate correlations
correlation_matrix <- movie_data_clean %>%
  select_if(is.numeric) %>%
  cor(use = "complete.obs")

#visualize the correlation matrix
corrplot(correlation_matrix, method = "color", addCoef.col = "black", number.cex = 0.6, tl.cex = 0.6)

result <- rcorr(as.matrix(movie_data_clean %>% 
            select_if(is.numeric))
            )
p_values <- result$P
#get rid of the e in numbers
formatted_p_values <- format(round(p_values, 3), scientific = FALSE)

print(formatted_p_values)




## -----------------------------------------------------------------------------
#boxplots

ggplot(movie_data_clean, aes(y = gross_opening_millions)) +
  geom_boxplot(fill = "blue", color = "black") +
  labs(title = "Boxplot of Gross Opening Weekend", x = "", y = "Million $")

ggplot(movie_data_clean, aes(y = news_volume)) +
  geom_boxplot(fill = "yellow3", color = "black") +
  labs(title = "Boxplot of News Volume", x = "", y = "Number of News")

ggplot(movie_data_clean, aes(y = news_valence)) +
  geom_boxplot(fill = "purple1", color = "black") +
  labs(title = "Boxplot of News Valence", x = "", y = "Sentiment")

ggplot(movie_data_clean, aes(y = search_volume)) +
  geom_boxplot(fill = "green2", color = "black") +
  labs(title = "Boxplot of Search Volume", x = "", y = "Google Searches")

ggplot(movie_data_clean, aes(y = reddit_comments)) +
  geom_boxplot(fill = "pink", color = "black") +
  labs(title = "Boxplot of Engagement", x = "", y = "Reddit Comments")

ggplot(movie_data_clean, aes(y = production_budget_millions)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Boxplot of Production Budget", x = "", y = "Million $")

summary(movie_data_clean)


## -----------------------------------------------------------------------------
#log transformation of skewed variables
#use log +1 to avoid (Inf) when the variables contain zeros or negative values

movie_data_clean_log <- movie_data_clean %>%
 mutate(
  log_gross_opening = log(gross_opening_millions +1),
  log_news_volume = log(news_volume +1),
  log_search_volume = log(search_volume +1),
  log_reddit_comments = log(reddit_comments +1),
  log_production_budget = log(production_budget_millions +1)
       )

#observe new distributions
ggplot(movie_data_clean_log, aes(x = log_gross_opening)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black") +
  labs(title = "Gross Opening Weekend Logged", x = "Million $", y = "Frequency")

ggplot(movie_data_clean_log, aes(x = log_news_volume)) +
  geom_histogram(binwidth = 0.5, fill = "yellow3", color = "black") +
  labs(title = "News Volume Logged", x = "Number of News", y = "Frequency")

ggplot(movie_data_clean_log, aes(x = log_search_volume)) +
  geom_histogram(binwidth = 0.5, fill = "green2", color = "black") +
  labs(title = "Search Volume Logged", x = "Google Searches", y = "Frequency")

ggplot(movie_data_clean_log, aes(x = log_reddit_comments)) +
  geom_histogram(binwidth = 0.5, fill = "purple3", color = "black") +
  labs(title = "Engagement Logged", x = "Reddit Comments", y = "Frequency")

ggplot(movie_data_clean_log, aes(x = log_production_budget)) +
  geom_histogram(binwidth = 0.5, fill = "orange", color = "black") +
  labs(title = "Production Budget", x = "Million $", y = "Frequency")


## -----------------------------------------------------------------------------
# Using the pipe operator to transform the data, fit the model, and summarize it
model_1e <- movie_data_clean_log %>%
  mutate(
    genre_cat = as.factor(genre_cat),
    release_period = as.factor(release_period),
    centered_log_news_volume = log_news_volume - mean(log_news_volume, na.rm = TRUE),
    centered_news_valence = news_valence - mean(news_valence, na.rm = TRUE)
    ) %>%

 lm(log_reddit_comments ~ centered_log_news_volume * trailer_uncertainty + 
    centered_news_valence * trailer_uncertainty +
    genre_cat + star_power + director_power + sequel_remake +
    adaptation + log_production_budget + R_rated + release_period, data = ., na.action = na.exclude)

#model summary
#summary(model)
summ(model_1e, digits = 3, vifs = T)

#assumption testing
#expected value of error term is 0
residuals_1e <- resid(model_1e)
summary(residuals_1e)

#error term in normally distributed
qqnorm(resid(model_1e), main = "Normal Q-Q Plot Model 1e")
qqline(resid(model_1e), col = "red", lwd = 2)

#heteroskedasticity
plot(residuals_1e)

# Tidy the model output
tidy_model_1e <- tidy(model_1e)

tidy_model_1e <- tidy_model_1e %>%
  mutate_if(is.numeric, ~ round(., 3))

# View the tidy model
print(tidy_model_1e)

# Write the tidy model to an Excel file
write.xlsx(tidy_model_1e, file = "model_1e_output.xlsx", sheetName = "Model 1e Results", rowNames = FALSE)

# Confirm the file was created
file.exists("model_1e_output.xlsx")


## -----------------------------------------------------------------------------
#scatter plot 
ggplot(movie_data_clean_log, aes(x = news_volume, y = reddit_comments)) +
  geom_point() +  
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Scatter Plot",
       x = "News Volume",
       y = "Reddit Comments")


## -----------------------------------------------------------------------------
#transform data and fit model
model_1a <- movie_data_clean_log %>%
  mutate(
    genre_cat = as.factor(genre_cat),
    release_period = as.factor(release_period),
    centered_log_news_volume = log_news_volume - mean(log_news_volume, na.rm = TRUE),
    centered_news_valence = news_valence - mean(news_valence, na.rm = TRUE)
    ) %>%

 lm(log_reddit_comments ~ 
    genre_cat + star_power + director_power + sequel_remake +
    adaptation + log_production_budget + R_rated + release_period, data = ., na.action = na.exclude)

#model summary
#summary(model)
summ(model_1a, digits = 3, vifs = T)


## -----------------------------------------------------------------------------
#assumption testing

#expected value of error term is 0
residuals_1a <- resid(model_1a)
summary(residuals_1a)

#error term in normally distributed
qqnorm(resid(model_1a), main = "Normal Q-Q Plot Model 1a")
qqline(resid(model_1a), col = "red", lwd = 2)

#heteroskedasticity
plot(residuals_1a)

#tidy model
tidy_model_1a <- tidy(model_1a)

tidy_model_1a <- tidy_model_1a %>%
  mutate_if(is.numeric, ~ round(., 3))

print(tidy_model_1a)

#write model to excel file
write.xlsx(tidy_model_1a, file = "model_1a_output.xlsx", sheetName = "Model 1a Results", rowNames = FALSE)



## -----------------------------------------------------------------------------
#transform data and fit model
model_1b <- movie_data_clean_log %>%
  mutate(
    genre_cat = as.factor(genre_cat),
    release_period = as.factor(release_period),
    centered_log_news_volume = log_news_volume - mean(log_news_volume, na.rm = TRUE),
    centered_news_valence = news_valence - mean(news_valence, na.rm = TRUE)
    ) %>%

 lm(log_reddit_comments ~ centered_log_news_volume + centered_news_valence + trailer_uncertainty +
    genre_cat + star_power + director_power + sequel_remake +
    adaptation + log_production_budget + R_rated + release_period, data = ., na.action = na.exclude)

#model summary
#summary(model)
summ(model_1b, digits = 3, vifs = T)


## -----------------------------------------------------------------------------

#expected value of error term is 0
residuals_1b <- resid(model_1b)
summary(residuals_1b)

#error term in normally distributed
qqnorm(resid(model_1b), main = "Normal Q-Q Plot Model 1b")
qqline(resid(model_1b), col = "red", lwd = 2)

#heteroskedasticity
plot(residuals_1b)

#tidy model
tidy_model_1b <- tidy(model_1b)

tidy_model_1b <- tidy_model_1b %>%
  mutate_if(is.numeric, ~ round(., 3))

print(tidy_model_1b)

#write model to excel file
write.xlsx(tidy_model_1b, file = "model_1b_output.xlsx", sheetName = "Model 1b Results", rowNames = FALSE)



## -----------------------------------------------------------------------------
#transform data and fit model
model_1c <- movie_data_clean_log %>%
  mutate(
    genre_cat = as.factor(genre_cat),
    release_period = as.factor(release_period),
    centered_log_news_volume = log_news_volume - mean(log_news_volume, na.rm = TRUE),
    centered_news_valence = news_valence - mean(news_valence, na.rm = TRUE)
    ) %>%

 lm(log_reddit_comments ~ centered_log_news_volume * trailer_uncertainty + centered_news_valence +
    genre_cat + star_power + director_power + sequel_remake +
    adaptation + log_production_budget + R_rated + release_period, data = ., na.action = na.exclude)

#model summary
#summary(model)
summ(model_1c, digits = 3, vifs = T)


## -----------------------------------------------------------------------------

#expected value of error term is 0
residuals_1c <- resid(model_1c)
summary(residuals_1c)

#error term in normally distributed
qqnorm(resid(model_1c), main = "Normal Q-Q Plot Model 1c")
qqline(resid(model_1c), col = "red", lwd = 2)

#heteroskedasticity
plot(residuals_1c)

#tidy model
tidy_model_1c <- tidy(model_1c)

tidy_model_1c <- tidy_model_1c %>%
  mutate_if(is.numeric, ~ round(., 3))

print(tidy_model_1c)

#write model to excel file
write.xlsx(tidy_model_1c, file = "model_1c_output.xlsx", sheetName = "Model 1c Results", rowNames = FALSE)



## -----------------------------------------------------------------------------

# Create a sequence of values for centered_log_news_volume

# Create a new dataset with necessary transformations
movie_data_transformed <- movie_data_clean_log %>%
  mutate(
    genre_cat = as.factor(genre_cat),
    release_period = as.factor(release_period),
    centered_log_news_volume = log_news_volume - mean(log_news_volume, na.rm = TRUE),
    centered_news_valence = news_valence - mean(news_valence, na.rm = TRUE)
  )

# Create a sequence of values for centered_log_news_volume
centered_log_news_volume_seq <- seq(min(movie_data_transformed$centered_log_news_volume, na.rm = TRUE), 
                                    max(movie_data_transformed$centered_log_news_volume, na.rm = TRUE), 
                                    length.out = 100)

# Define levels of the dummy moderator (0 and 1)
moderator_levels <- c(0, 1)

# Create a new data frame for prediction
prediction_data <- expand.grid(centered_log_news_volume = centered_log_news_volume_seq, 
                               trailer_uncertainty = moderator_levels)

# Ensure centered_news_valence is included in the new data for prediction
prediction_data$centered_news_valence <- mean(movie_data_transformed$centered_news_valence, na.rm = TRUE)
prediction_data$genre_cat <- factor(levels(movie_data_transformed$genre_cat)[1],
                                    levels =levels(movie_data_transformed$genre_cat))
prediction_data$star_power <- mean(movie_data_transformed$star_power, na.rm = TRUE)
prediction_data$director_power <- mean(movie_data_transformed$director_power, na.rm = TRUE)
prediction_data$sequel_remake <- mean(movie_data_transformed$sequel_remake, na.rm = TRUE)
prediction_data$adaptation <- mean(movie_data_transformed$adaptation, na.rm = TRUE)
prediction_data$log_production_budget <- mean(movie_data_transformed$log_production_budget, na.rm = TRUE)
prediction_data$R_rated <- mean(movie_data_transformed$R_rated, na.rm = TRUE)
prediction_data$release_period <- factor(levels(movie_data_transformed$release_period)[1],
                                         levels =levels(movie_data_transformed$release_period))

# Predict values
prediction_data$log_reddit_comments <- predict(model_1c, newdata = prediction_data)

# Convert moderator to factor for plotting
prediction_data$trailer_uncertainty <- factor(prediction_data$trailer_uncertainty,
                                              labels = c("Certain", "Uncertain"))



# Create the interaction plot
ggplot(prediction_data, aes(x = centered_log_news_volume, y = log_reddit_comments, color = trailer_uncertainty)) +
  geom_line(size = 1) +  # Line plot for predicted values
  labs(title = "Interaction Plot: Log News Volume and Trailer Uncertainty on Log Reddit Comments",
       x = "Log News Volume",
       y = "Log Reddit Comments",
       color = "Trailer Uncertainty") +
  theme_minimal()  # Minimal theme for cleaner look




## -----------------------------------------------------------------------------
#transform data and fit model
model_1d <- movie_data_clean_log %>%
  mutate(
    genre_cat = as.factor(genre_cat),
    release_period = as.factor(release_period),
    centered_log_news_volume = log_news_volume - mean(log_news_volume, na.rm = TRUE),
    centered_news_valence = news_valence - mean(news_valence, na.rm = TRUE)
    ) %>%

 lm(log_reddit_comments ~ centered_log_news_volume + centered_news_valence * trailer_uncertainty +
    genre_cat + star_power + director_power + sequel_remake +
    adaptation + log_production_budget + R_rated + release_period, data = ., na.action = na.exclude)

#model summary
#summary(model)
summ(model_1d, digits = 3, vifs = T)


## -----------------------------------------------------------------------------

#expected value of error term is 0
residuals_1d <- resid(model_1d)
summary(residuals_1d)

#error term in normally distributed
qqnorm(resid(model_1d), main = "Normal Q-Q Plot Model 1d")
qqline(resid(model_1d), col = "red", lwd = 2)

#heteroskedasticity
plot(residuals_1d)

#tidy model
tidy_model_1d <- tidy(model_1d)

tidy_model_1d <- tidy_model_1d %>%
  mutate_if(is.numeric, ~ round(., 3))

print(tidy_model_1d)

#write model to excel file
write.xlsx(tidy_model_1d, file = "model_1d_output.xlsx", sheetName = "Model 1d Results", rowNames = FALSE)



## -----------------------------------------------------------------------------
#transform data and fit model
model_2e <- movie_data_clean_log %>%
  mutate(
    genre_cat = as.factor(genre_cat),
    release_period = as.factor(release_period),
    centered_log_news_volume = log_news_volume - mean(log_news_volume, na.rm = TRUE),
    centered_news_valence = news_valence - mean(news_valence, na.rm = TRUE)
    #centered_log_search_volume = log_search_volume - mean(log_search_volume, na.rm = TRUE)
  ) %>%
#regression
 lm(log_search_volume ~ centered_log_news_volume * trailer_uncertainty + centered_news_valence * trailer_uncertainty +
    genre_cat + star_power + director_power + sequel_remake + adaptation + log_production_budget + R_rated +
      release_period,
    data = ., na.action = na.exclude)

#model summary
#summary(model)
summ(model_2e, digits = 3, vifs = T)


## -----------------------------------------------------------------------------

#expected value of error term is 0
residuals_2e <- resid(model_2e)
summary(residuals_2e)

#heteroskedasticity
plot(residuals_2e)

#error term in normally distributed
qqnorm(resid(model_2e), main = "Normal Q-Q Plot Model 2e")
qqline(resid(model_2e), col = "red", lwd = 2)

#tidy model
tidy_model_2e <- tidy(model_2e)

tidy_model_2e <- tidy_model_2e %>%
  mutate_if(is.numeric, ~ round(., 3))

print(tidy_model_2e)

#write model in excel file
write.xlsx(tidy_model_2e, file = "model_2e_output.xlsx", sheetName = "Model 2e Results", rowNames = FALSE)



## -----------------------------------------------------------------------------
#tranform the data and fit the model
model_2a <- movie_data_clean_log %>%
  mutate(
    genre_cat = as.factor(genre_cat),
    release_period = as.factor(release_period),
    centered_log_news_volume = log_news_volume - mean(log_news_volume, na.rm = TRUE),
    centered_news_valence = news_valence - mean(news_valence, na.rm = TRUE)
    #centered_log_search_volume = log_search_volume - mean(log_search_volume, na.rm = TRUE)
  ) %>%
#regression
 lm(log_search_volume ~
    genre_cat + star_power + director_power + sequel_remake + adaptation + log_production_budget + R_rated +
      release_period,
    data = ., na.action = na.exclude)

#model summary
#summary(model)
summ(model_2a, digits = 3, vifs = T)


## -----------------------------------------------------------------------------

#expected value of error term is 0
residuals_2a <- resid(model_2a)
summary(residuals_2a)

#heteroskedasticity
plot(residuals_2a)

#error term in normally distributed
qqnorm(resid(model_2a), main = "Normal Q-Q Plot Model 2a")
qqline(resid(model_2a), col = "red", lwd = 2)

#tidy model
tidy_model_2a <- tidy(model_2a)

tidy_model_2a <- tidy_model_2a %>%
  mutate_if(is.numeric, ~ round(., 3))

print(tidy_model_2a)

#write model to excel file
write.xlsx(tidy_model_2a, file = "model_2a_output.xlsx", sheetName = "Model 2a Results", rowNames = FALSE)




## -----------------------------------------------------------------------------
#tranform the data and fit the model
model_2b <- movie_data_clean_log %>%
  mutate(
    genre_cat = as.factor(genre_cat),
    release_period = as.factor(release_period),
    centered_log_news_volume = log_news_volume - mean(log_news_volume, na.rm = TRUE),
    centered_news_valence = news_valence - mean(news_valence, na.rm = TRUE)
    #centered_log_search_volume = log_search_volume - mean(log_search_volume, na.rm = TRUE)
  ) %>%
#regression
 lm(log_search_volume ~ centered_log_news_volume + centered_news_valence + trailer_uncertainty +
    genre_cat + star_power + director_power + sequel_remake + adaptation + log_production_budget + 
      R_rated + release_period,
    data = ., na.action = na.exclude)

#model summary
#summary(model)
summ(model_2b, digits = 3, vifs = T)


## -----------------------------------------------------------------------------

#expected value of error term is 0
residuals_2b <- resid(model_2b)
summary(residuals_2b)

#heteroskedasticity
plot(residuals_2b)

#error term in normally distributed
qqnorm(resid(model_2b), main = "Normal Q-Q Plot Model 2b")
qqline(resid(model_2b), col = "red", lwd = 2)

#tidy model
tidy_model_2b <- tidy(model_2b)

tidy_model_2b <- tidy_model_2b %>%
  mutate_if(is.numeric, ~ round(., 3))

print(tidy_model_2b)

#write model in excel file
write.xlsx(tidy_model_2b, file = "model_2b_output.xlsx", sheetName = "Model 2b Results", rowNames = FALSE)



## -----------------------------------------------------------------------------
#transform data and fit the model
model_2c <- movie_data_clean_log %>%
  mutate(
    genre_cat = as.factor(genre_cat),
    release_period = as.factor(release_period),
    centered_log_news_volume = log_news_volume - mean(log_news_volume, na.rm = TRUE),
    centered_news_valence = news_valence - mean(news_valence, na.rm = TRUE)
    #centered_log_search_volume = log_search_volume - mean(log_search_volume, na.rm = TRUE)
  ) %>%
#regression
 lm(log_search_volume ~ centered_log_news_volume * trailer_uncertainty + centered_news_valence +
    genre_cat + star_power + director_power + sequel_remake + adaptation + log_production_budget + R_rated +
      release_period,
    data = ., na.action = na.exclude)

#model summary
#summary(model)
summ(model_2c, digits = 3, vifs = T)


## -----------------------------------------------------------------------------

#expected value of error term is 0
residuals_2c <- resid(model_2c)
summary(residuals_2c)

#heteroskedasticity
plot(residuals_2c)

#error term in normally distributed
qqnorm(resid(model_2c), main = "Normal Q-Q Plot Model 2c")
qqline(resid(model_2c), col = "red", lwd = 2)

#tidy model
tidy_model_2c <- tidy(model_2c)

tidy_model_2c <- tidy_model_2c %>%
  mutate_if(is.numeric, ~ round(., 3))

print(tidy_model_2c)

#write model to excel file
write.xlsx(tidy_model_2c, file = "model_2c_output.xlsx", sheetName = "Model 2c Results", rowNames = FALSE)



## -----------------------------------------------------------------------------
#transform data and fit model
model_2d <- movie_data_clean_log %>%
  mutate(
    genre_cat = as.factor(genre_cat),
    release_period = as.factor(release_period),
    centered_log_news_volume = log_news_volume - mean(log_news_volume, na.rm = TRUE),
    centered_news_valence = news_valence - mean(news_valence, na.rm = TRUE)
    #centered_log_search_volume = log_search_volume - mean(log_search_volume, na.rm = TRUE)
  ) %>%
#regression
 lm(log_search_volume ~ centered_log_news_volume + centered_news_valence * trailer_uncertainty +
    genre_cat + star_power + director_power + sequel_remake + adaptation + log_production_budget + R_rated +
    release_period, data = ., na.action = na.exclude)

#model summary
#summary(model)
summ(model_2d, digits = 3, vifs = T)


## -----------------------------------------------------------------------------

#expected value of error term is 0
residuals_2d <- resid(model_2d)
summary(residuals_2d)

#heteroskedasticity
plot(residuals_2d)

#error term in normally distributed
qqnorm(resid(model_2d), main = "Normal Q-Q Plot Model 2d")
qqline(resid(model_2d), col = "red", lwd = 2)

#tidy model
tidy_model_2d <- tidy(model_2d)

tidy_model_2d <- tidy_model_2d %>%
  mutate_if(is.numeric, ~ round(., 3))

print(tidy_model_2d)

# write model to excel file
write.xlsx(tidy_model_2d, file = "model_2d_output.xlsx", sheetName = "Model 2d Results", rowNames = FALSE)



## -----------------------------------------------------------------------------
#tranform data and fit the model
model_3 <- movie_data_clean_log %>%
  #filter(log_reddit_comments > 0) %>%
  mutate(
    genre_cat = as.factor(genre_cat),
    release_period = as.factor(release_period)
  ) %>%
 lm(log_gross_opening ~ log_reddit_comments + genre_cat + star_power + director_power + sequel_remake +
    adaptation + log_production_budget + R_rated + release_period, data = ., na.action = na.exclude)

#model summary
#summary(model)
summ(model_3, digits = 3, vifs = T)


## -----------------------------------------------------------------------------

#multicolinearity
round(vif(model_3),3)

plot(model_3, which = 1)

#expected value of error term is 0
residuals_3 <- resid(model_3)
summary(residuals_3)

#heteroskedasticity
plot(residuals_3)
ncvTest(model_3)

#error term in normally distributed
qqnorm(resid(model_3), main = "Normal Q-Q Plot Model 3")
qqline(resid(model_3), col = "red", lwd = 2)

#tidy model
tidy_model_3 <- tidy(model_3)

tidy_model_3 <- tidy_model_3 %>%
  mutate_if(is.numeric, ~ round(., 3))

print(tidy_model_3)

#write model to excel file
write.xlsx(tidy_model_3, file = "model_3_output.xlsx", sheetName = "Model 3 Results", rowNames = FALSE)



## -----------------------------------------------------------------------------
#transform data and fit the model
model_4 <- movie_data_clean_log %>%
  #filter(log_reddit_comments > 0) %>%
  mutate(
    genre_cat = as.factor(genre_cat),
    release_period = as.factor(release_period)
  ) %>%
 lm(log_gross_opening ~ log_search_volume + genre_cat + star_power + director_power + sequel_remake +
    adaptation + log_production_budget + R_rated + release_period, data = ., na.action = na.exclude)

#model summary
#summary(model)
summ(model_4, digits = 3, vifs = T)

plot(model_4, which = 1)


## -----------------------------------------------------------------------------

#expected value of error term is 0
residuals_4 <- resid(model_4)
summary(residuals_4)

#heteroskedasticity
plot(residuals_4)
ncvTest(model_4)

#error term in normally distributed
qqnorm(resid(model_4), main = "Normal Q-Q Plot Model 4")
qqline(resid(model_4), col = "red", lwd = 2)

#tidy model
tidy_model_4 <- tidy(model_4)

tidy_model_4 <- tidy_model_4 %>%
  mutate_if(is.numeric, ~ round(., 3))

print(tidy_model_4)

#write model to excel file
write.xlsx(tidy_model_4, file = "model_4_output.xlsx", sheetName = "Model 4 Results", rowNames = FALSE)



## -----------------------------------------------------------------------------
#calculate correlations
correlation_matrix_log <- movie_data_clean_log %>%
  select_if(is.numeric) %>%
  cor(use = "complete.obs")

#visualize the correlation matrix
corrplot(correlation_matrix_log, method = "color", addCoef.col = "black", number.cex = 0.6, tl.cex = 0.6)



## -----------------------------------------------------------------------------
#removing zero values from reddit comments
movie_data_no_zeros <- movie_data_clean %>%
  filter(reddit_comments != 0)

#inspecting distribution
ggplot(movie_data_no_zeros, aes(x = reddit_comments)) +
  geom_histogram(binwidth = 100, fill = "purple3", color = "black") +
  labs(title = "Engagement", x = "Reddit Comments", y = "Frequency")

movie_data_no_zeros_log <- movie_data_no_zeros %>%
 mutate(
  log_gross_opening = log(gross_opening_millions +1),
  log_news_volume = log(news_volume +1),
  log_search_volume = log(search_volume +1),
  log_reddit_comments = log(reddit_comments +1),
  log_production_budget = log(production_budget_millions +1)
       )

#check logged distribution
ggplot(movie_data_no_zeros_log, aes(x = log_reddit_comments)) +
  geom_histogram(binwidth = 0.5, fill = "purple3", color = "black") +
  labs(title = "Engagement Logged", x = "Reddit Comments", y = "Frequency")

#regression model with no zeros
model_1e_no_zeros <- movie_data_no_zeros_log %>%
  mutate(
    genre_cat = as.factor(genre_cat),
    release_period = as.factor(release_period),
    centered_log_news_volume = log_news_volume - mean(log_news_volume, na.rm = TRUE),
    centered_news_valence = news_valence - mean(news_valence, na.rm = TRUE)
    ) %>%

 lm(log_reddit_comments ~ centered_log_news_volume + trailer_uncertainty + 
    centered_news_valence +
    genre_cat + star_power + director_power + sequel_remake +
    adaptation + log_production_budget + R_rated + release_period, data = ., na.action = na.exclude)

#model summary
#summary(model)
summ(model_1e_no_zeros, digits = 3, vifs = T)


## -----------------------------------------------------------------------------
#expected value of error term is 0
residuals_1e_no_zeros <- resid(model_1e_no_zeros)
summary(residuals_1e_no_zeros)

#error term in normally distributed
qqnorm(resid(model_1e_no_zeros), main = "Normal Q-Q Plot Model 1e_no_zeros")
qqline(resid(model_1e_no_zeros), col = "red", lwd = 2)

#heteroskedasticity
plot(residuals_1e_no_zeros)


