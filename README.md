# Text analysis of r/wallstreetbets

Text analysis of the internet forum [r/wallstreetbets subreddit](https://www.reddit.com/r/wallstreetbets/) to predict the popularity of posts from the post's text.

Final project for NYU Messy Data and Machine Learning class. Contains explicit language.

<br>
<p align="center">
<img src="analyses/plots/precision.png" width=80%>
</p>

<br>
<p align="center">
<img src="analyses/plots/importance_lasso_content.png" width=80%>
</p>

<br>
<p align="center">
<img src="analyses/plots/raw_daily_counts.png" width=80%>
</p>


## Folder structure

    .
    ├── analyses          # Feature engineering, model fitting, and performance estimates
    │   └── plots         # Plots
    ├── data              # Cleaned data and cleaning scripts
    ├── inputs            # Raw input data and scraping scripts
    ├── material          # Class material (proposal and paper)
    └── README.md


## Reproducibility
To reproduce, run the scripts in the following order:  
1. `inputs/scrape_WBS.R`  
2. `data/cleaning.R`  
3.  Features:
    1. `analyses/topic_modeling.R`  
    2. `analyses/sentiment_scoring.py`  
    3. `analyses/GME_price.R`  
    4. `analyses/comment_hierarchy.R`  
4. `analyses/feature_engineering.R`  
5. `analyses/feature_selection.R`  
6. `analyses/create_train_test_split.R`
7. `analyses/model_upvotes.R`  
