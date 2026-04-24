# Exploring Daily Weather Patterns in State College, PA

This project explores daily weather patterns in State College, Pennsylvania using NOAA daily weather data. Our team is interested in understanding how local weather changes across time, seasons, and years by focusing on variables such as maximum temperature, minimum temperature, precipitation, snowfall, and snow depth. We are also especially interested in looking at rainfall patterns in State College as part of the broader weather analysis.

## Overview

The purpose of this project is to conduct an exploratory data analysis of daily weather in State College, PA. We want to identify patterns in weather over time, compare different months and seasons, and better understand how local weather conditions vary from year to year. Through this project, we also want to build our skills in data cleaning, data visualization, teamwork, and reproducible reporting in R.

Our project uses real daily weather data from NOAA for the State College weather station. This dataset gives us enough observations and variables to explore trends in temperature, precipitation, snowfall, and snow depth. Since the project is centered on exploratory data analysis, our main goal is to examine the data carefully, create clear visualizations and summary tables, and explain what the data show in a way that is easy to follow.

## Project Focus

Our main topic is daily weather patterns in State College, Pennsylvania.

Main variables of interest:

- TMAX: (maximum temperature)
- TMIN: (minimum temperature)
- PRCP: (precipitation)
- SNOW: (snowfall)
- SNWD: (snow depth)

Within this broader project, we are especially interested in exploring rainfall and precipitation patterns in State College over time.

## Main Research Question

What patterns can we observe in daily weather in State College, Pennsylvania over the last ten years?

## Possible Sub Questions

- How do maximum and minimum temperatures change across months and seasons?
- Which months or seasons tend to have the most precipitation?
- How does rainfall vary from year to year?
- How does snowfall differ across winters?
- Are there noticeable differences in weather conditions across years?

## Data Source

Our main data source is NOAA Climate Data Online.

Dataset information:

- Source: NOAA Climate Data Online
- Station: STATE COLLEGE, PA US
- Station ID: GHCND:USC00368449
- File used for this project: NOAA daily weather data downloaded as a CSV file

URLs:

- https://www.ncdc.noaa.gov/cdo-web/
- https://www.ncdc.noaa.gov/cdo-web/datasets/GHCND/stations/GHCND:USC00368449/detail

We acknowledge NOAA as the original source of the weather data used in this project.

## Current Plan

Our team plans to import the NOAA weather data into R, clean and organize it, and then carry out exploratory data analysis focused on daily weather patterns in State College. We plan to create helpful variables such as month, year, and season so that we can compare weather conditions across time more clearly.

We will create multiple graphs and tables to examine temperature patterns, precipitation patterns, rainfall trends, snowfall differences, and seasonal weather variation. We also plan to describe the data provenance, discuss FAIR and CARE principles, and organize our final work into a reproducible QMD report that renders to PDF. These are part of the course project requirements.  [oai_citation:0‡Spring2026_Project_Guidelines.docx](sediment://file_0000000072e071fd99d3dd4be5fc1070)

## Repo Structure

`README.md`  
General overview of the project, topic, goals, and data source.

`SC_weather_10.csv`  
Main NOAA weather State College, PA 10 year dataset used for the project.

`Project_Guidelines.md`  
Course project guidelines and requirements.

`.gitignore`  
Git ignore file from the course template.

`.lintr`  
Linting configuration file from the course template.

`linting_script.R`  
Linting script included with the course template.

`apa7.csl` and `MLA9.csl`  
Citation style files included with the course template.

Additional files will be added as the project develops, such as the QMD report, cleaned data files, figures, tables, and planning documents.

## Team Members

- Ata
- Jincheng
- Qihaohan

This is a team project for STAT 184 Section 4.

## Workflow

Our team plans to use GitHub to organize and manage the project. We will keep a main branch and create one development branch for each team member. We will use issues to track work, pull requests to merge work into the main branch, and commit messages that clearly describe what was changed. 

## Project Goals

- Explore daily weather patterns in State College, PA
- Study how temperature, precipitation, snowfall, and snow depth change over time
- Pay special attention to rainfall and precipitation patterns
- Create clear and professional data visualizations and summary tables
- Write a reproducible and organized report in R and Quarto
- Work collaboratively as a team using GitHub

## Notes

This project is focused on exploratory data analysis. Our goal is to understand the data, identify meaningful patterns, and communicate those findings clearly rather than move into advanced modeling methods.
