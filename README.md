Kaggle Bike Sharing Demand Prediction

Overview This repository contains my solution for the Bike Sharing Demand competition hosted on Kaggle. The challenge is to predict the total number of bikes rented in each hour, based on historical usage patterns and weather information. Accurate demand forecasting helps bike sharing systems optimize availability and improve customer satisfaction. This repo showcases my workflow in R, including data preprocessing, model training, and submission file generation.

Dataset

Source: https://www.kaggle.com/c/bike-sharing-demand

Description:

train.csv – Training dataset with hourly bike rental counts.

test.csv – Test dataset for predictions.

sampleSubmission.csv – Example submission format.

Features:

Date/time stamps.

Weather conditions (temperature, humidity, wind speed).

Season and holiday indicators.

Hourly rental counts (target variable in training set).

Methodology Implemented entirely in R:

Data preprocessing: parsing datetime features (hour, day, month, season), handling categorical variables, normalizing continuous features.

Modeling approaches: regression models for continuous demand prediction, random forests and other ensemble methods for improved accuracy.

Evaluation: Kaggle leaderboard metric is Root Mean Squared Logarithmic Error (RMSLE). Predictions saved in CSV format for submission.

Reproducibility: scripts (BikeShare.R) included, workflow organized for easy reruns and extensions.

Results

Predictions generated and saved in submission files.

Models achieved competitive scores on Kaggle’s leaderboard.

Feature engineering on datetime variables significantly improved performance.

Repository structure ├── BikeShare.R # Main R script with analysis and modeling ├── train.csv # Training dataset ├── test.csv # Test dataset ├── sampleSubmission.csv # Kaggle sample submission format └── README.md # Project documentation

How to run

Clone the repository: git clone https://github.com/supermaxaroni/KaggleBikeShare.git

Open BikeShare.R in RStudio or run via R console.

Install required packages: install.packages(c("randomForest", "caret", "data.table"))

Execute the script to generate predictions.

Submission files (*.csv) can be uploaded directly to Kaggle.

Future work

Explore gradient boosting methods (XGBoost, LightGBM).

Implement stacking/ensembles for improved accuracy.

Perform deeper feature engineering on weather and holiday variables.

Automate hyperparameter tuning for Random Forests.

Acknowledgments

Kaggle for hosting the competition.

R community packages (randomForest, caret, data.table) that made modeling efficient.

Inspiration from Kaggle kernels and discussions.
