nfl_modeling2

Forecasting NFL outcomes against the spread ATS, using machine learning. Rebuilt arout caret package. 

Files Descriptions:

-NFL Data Set Up: Loads in NFL data using nflfastR. Aggregates play-by-play data to game and team-game grain. 

-NFL Functions: Creates functions used in all other scripts. Includes functions for data wrangling and creating dataframes for modeling, fuctions that generate predictions using trained models, functions to evaluate these predictions and their performance ATS, and functions to store model performance statistics and model predictions in SQL databases. 

-run_NFL_model(): Wrapper function built around caret's train(), preProcess(), and rfe() functions. Trains a model to predict a user's desired outcome. Model is trained on specified seasons or a user-supplied training dataset, and evaluated on a specific test season or user-provided test dataset. Returns a list containing caret model objects, compiled model evaluation statistics, training and test dfs, and training and test dfs with predictions and individual game ATS results. 
