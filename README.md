# Sleep Health and Lifestyle Dashboard

## Purpose / Objectives

This dashboard allows interactive exploration of sleep health in relation to lifestyle and demographic factors.  

Key objectives:
- Perform exploratory data analysis of sleep duration, quality, and disorders.
- Allow filtering by **gender** and **occupation**.
- Visualize relationships between sleep and physical/physiological measures (activity, steps, heart rate, blood pressure, BMI, stress).

Features:
- **Interactive Filters:** Sex, Occupation, and Sleep Measures. 
- **Dynamic Plots:** Scatterplots (with optional regression), violin/box plots, and bar charts.  
- **Interactive Elements:** Hover, zoom, and select points using Plotly.  


## Data
- Source: [Kaggle Sleep Health and Lifestyle Dataset](https://www.kaggle.com/datasets/uom190346a/sleep-health-and-lifestyle-dataset/data). Visit the link for more information on variable measurements.
- CSV file: `data/Sleep_health_and_lifestyle_dataset.csv` 


## Libraries / Tools
- **Shiny:** Builds the dashboard with reactive inputs and server-side rendering.  
- **Plotly:** Adds interactivity to plots.  
- **dplyr:** Filters, summarizes, and transforms data.  
- **readr:** Loads CSV files.  
- **tidyr:** Reshapes and tidies data.

## Installation & Running the App

1. Clone the repo:
```bash
git clone https://github.com/anna-crifasi/sleep_dashboard.git

