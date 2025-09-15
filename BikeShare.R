## load libraries
library(tidyverse)
library(tidymodels)
library(vroom)
library(patchwork)
library(dplyr)
library(ggplot2)
library(car)
library(corrplot)
library(ggcorrplot)

## read in the data
bikeshare <- vroom("GItHub/KaggleBikeShare/train.csv")
test <- vroom("GItHub/KaggleBikeShare/test.csv")
head(bikeshare)

## data wrangling
bikeshare <- bikeshare |>
  select(-c(casual,registered))|>
  mutate(season = as.factor(season), holiday = as.factor(holiday), workingday = as.factor(workingday), count = log(count))
test <- test|>
  mutate(season = as.factor(season), holiday = as.factor(holiday), workingday = as.factor(workingday))
head(bikeshare)

## feature engineering
bikeshare <- bikeshare |>
  mutate(weather = ifelse(weather == 4, 3, weather), weather = as.factor(weather), hour = hour(datetime), sunny = ifelse(weather == 1 & 5 <= hour & hour <= 20, 1, 0), sunny = as.factor(sunny))
test <- test|>
  mutate(weather = ifelse(weather == 4, 3, weather), weather = as.factor(weather), hour = hour(datetime), sunny = ifelse(weather == 1 & 5 <= hour & hour <= 20, 1, 0), sunny = as.factor(sunny))
head(bikeshare)

bikeshare <- bikeshare %>%
  mutate(hour_sin = sin(2 * pi * hour / 24),
         hour_cos = cos(2 * pi * hour / 24))|>
  select(-hour)
test <- test %>%
  mutate(hour_sin = sin(2 * pi * hour / 24),
         hour_cos = cos(2 * pi * hour / 24))|>
  select(-hour)


bikeshare_lm <- lm(count ~ temp + I(temp^2) + humidity + windspeed + season + weather + hour_sin + hour_cos, bikeshare)
vif(bikeshare_lm)
plot(bikeshare_lm)

ggplot(bikeshare, aes(x = temp, y = count, color = cut(humidity, breaks = c(-100, 30, 60, 1000), labels = c("low", "med", "high"))))+
  geom_smooth(method = "lm", se = FALSE)
ggplot(bikeshare, aes(y = count, color = workingday))+
    geom_boxplot()


## Generate predictions on original scale
bike_predictions <- predict(bikeshare_lm, newdata = test)
bike_predictions <- exp(bike_predictions)  # back-transform
bike_predictions <- pmax(0, bike_predictions)  # ensure non-negative

## Create submission
kaggle_submission <- bind_cols(
  test %>% select(datetime),
  tibble(count = bike_predictions)  # bind vector as column
) %>%
  mutate(datetime = as.character(format(datetime)))

## Write CSV
vroom_write(kaggle_submission, file = "./LinearPreds.csv", delim = ",")

head(bikeshare)

