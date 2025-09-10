library(tidyverse)
library(tidymodels)
library(vroom)
library(patchwork)
library(dplyr)
library(ggplot2)
library(car)
library(corrplot)
library(ggcorrplot)
bikeshare <- vroom("GItHub/KaggleBikeShare/train.csv")
test <- vroom("GItHub/KaggleBikeShare/test.csv")
bikeshare <- bikeshare |>
  mutate(season = as.factor(season), holiday = as.factor(holiday), workingday = as.factor(workingday), weather = as.factor(weather))
glimpse(bikeshare)
test <- test|>
  mutate(season = as.factor(season), holiday = as.factor(holiday), workingday = as.factor(workingday), weather = as.factor(weather))
glimpse(bikeshare)

plot1 <- ggplot(bikeshare, aes(x = windspeed, y = count, color = weather)) +
  geom_smooth()+
  geom_point()

plot2 <- ggplot(bikeshare, aes(x = count, color = weather)) +
  geom_boxplot()

plot3 <- ggplot(bikeshare, aes(y = count, color = workingday)) +
  geom_bar()

plot4 <- ggplot(bikeshare, aes(y = count, x = temp))+
  geom_point()+
  geom_smooth()

(plot1 + plot2) / (plot3 + plot4)
VIFS(bikeshare)
lm <- lm(count~., bikeshare)
vif(lm)
plot(lm)
ggcorrplot(bikeshare)
interaction.plot(bikeshare$humidity, bikeshare$weather, bikeshare$count)
ggplot(bikeshare, aes(x = windspeed, y = count, color = workingday))+
  geom_smooth(method = "lm", se = FALSE)
ggplot(bikeshare, aes(y = count, color = workingday))+
    geom_boxplot()


library(tidymodels)
## Setup and Fit the Linear Regression Model
my_linear_model <- linear_reg() %>% #Type of model
  set_engine("lm") %>% # Engine = What R function to use
  set_mode("regression") %>% # Regression just means quantitative response
  fit(formula=log(count)~datetime+humidity*season+windspeed*holiday+weather+temp, data=bikeshare)

## Generate Predictions Using Linear Model
bike_predictions <- predict(my_linear_model,
new_data=test) # Use fit to predict11
bike_predictions ## Look at the output


kaggle_submission <- bike_predictions %>%
bind_cols(., test) %>% #Bind predictions with test data3
  select(datetime, .pred) %>% #Just keep datetime and prediction variables4
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)5
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)6
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle7

## Write out the file
vroom_write(x=kaggle_submission, file="./LinearPreds.csv", delim=",")
