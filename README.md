# DietDiveR
# Welcome to DietDiveR, a toolkit for ASA24 and NHANES data analysis with R.

## Project website has tutorials for analyzing ASA24 and NHANES data with DietDiveR:
https://computational-nutrition-lab.github.io/DietDiveR/

## This repo contains:
1. **"eg_data"** folder with example datasets that can be used to follow the tutorials on the website.
2. **"lib"** folder with source functions that are to be called within the R scripts. DietDiveR users do not normally need to directly edit the functions stored in "lib". 
3. **"users"** folder with R scripts that contain annotated R code to analyze ASA24 or NHANES data.
4. **"README.md"** file, which is a document used to construct this explanation on the GitHub page that you are reading now; not needed for dietary data analysis.

## Customize working directory in your R environment
- Working directory is a specific location on your computer where files are pulled from and saved in when using R.
- Each script starts with a line of code to set your working directory, and it assumes that you have a directory called "DietDiveR" in a folder called "GitHub", which is in your home directory.
- You can customize it to suit your own folder structures, but the most straightforward way to use the R scripts in "users" is to create a "GitHub" folder in your home directory, and inside it, save the downloaded repo as "DietDiveR".
- That way, the code to set your working directory, `setwd("~/GitHub/DietDiveR")`, can be run as is. 

## Find out more about DietR in our publication and cite DietDiveR
DietDiveR has been initially published as "DietR" on MedRxiv, but
has been renamed as "DietDiveR" and will be published under the name of 
"DietDiveR" in a peer-reviewed journal. Stay tuned!

Dietary pattern and diversity analysis using 'DietR' package in R
Rie Sadohara, David Jacobs, Mark A Pereira, Abigail J Johnson
medRxiv 2023.07.07.23292390; doi: https://doi.org/10.1101/2023.07.07.23292390

The tutorials assume that you are using [R Studio](https://posit.co/downloads/). However, [basic R](https://www.r-project.org/) alone can also work. Please note that you need R in order to run R Studio.

<img src='lib/DietR Logo.png' alt="DietR logo" width=60% height=auto/>
