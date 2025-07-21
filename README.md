# Machine Learning-based Anomaly Detection in Computer Networks

This repository contains R scripts I've used in bachelor's thesis for exploratory data analysis (EDA), binary classification, and multiclass classification on the CICIDS2017 dataset.

Raw data is not included here — please download it from the official source (see below).

## 🛠 Prerequisites

- **R** ≥ 4.0  
- Install the following R packages (you can copy‑paste this into your R console):
  ```r
  install.packages(c(
    "tidyverse",
    "data.table",
    "glmnet",
    "plotmo",
    "rsample",
    "ggplot2",
    "yardstick",
    "rpart",
    "rpart.plot",
    "tidymodels",
    "ranger"
  ))

## 🚀 Data

1. **Download the CICIDS2017 dataset**  
   Visit the official site:  
   [CICIDS2017 Dataset – UNB](https://www.unb.ca/cic/datasets)

## ✅ Quick Pipeline Steps

1. **src/eda.R**  
   Exploratory Data Analysis (data loading, cleaning checks, summary visuals)

2. **src/binary_classification.R**  
   Binary classification workflow (LASSO, CART, Random Forest, evaluation)

3. **src/multiclass_classification.R**  
   Multiclass classification workflow (LASSO, CART, Random Forest, evaluation)
