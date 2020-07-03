# Supplementary material for Kerswell et al. (2020, G3)

This repository stores supplementary material for the manuscript *Backarc lithospheric thickness and serpentine stability control slab-mantle coupling depths in subduction zones* and includes:
- A partial dataset (the data to reproduce Figures 1, 3, 9, 10, A3, A4, A5 can be downloaded [here](https://osf.io/zjac3/))
- Scripts to reproduce all results (reduce and visualize the complete dataset)
- A web-based application for calculating lithospheric thickness and predicting coupling depth

I recommend cloning this repository. This will ensure that the scripts run without issue.

## Reproducing results
The dataset directory contains raw output from the thermomechanical code I2VIS (dataset > numerical_models; [Gerya and Yuen, 2003](https://www.sciencedirect.com/science/article/pii/S0031920103001900?casa_token=5ZIFpFKE41IAAAAA:HMw6o_z2VJymwtw600c74_9MvJCM5yl7ejm2aimfrDUsBE6wZdbN9_N7qaMGRFftlTUqKp5Oczw)) and measurements (dataset > heatflow). Data from [Wada & Wang (2009)](https://agupubs.onlinelibrary.wiley.com/doi/full/10.1029/2009GC002570) can be found in the regressions directory (regressions > segment_z1100.txt). These directories and files are all that is needed to reproduce all of the results, tables, and figures in the paper.

### Reading and reducing the raw data
Simply run the R sript 'read_process_data.R' with the following files and and directories in the same directory as 'read_process_data.R':
- antstability.csv
- PD15.csv
- model_geotherms.csv
- './heatflow/' (heatflow directory)

An R data file called 'reduced_coupling_data.RData' will be saved.

### Reproducing regressions
Simply run the R script 'regressions.R' making sure that the previously generated R data file 'reduced_coupling_data.RData' and 'segment_z1100.txt' are in the same directory as 'regressions.R'. The script will print a pdf of plots used to visualize and evaluate the reliability of the regression models. Another R data file 'regressions_data.RData' will be saved.

Within R, it is simple to view summary statistics of the various models by using `summary()`. For example `summary(cubicMultivariateReg)` will show the model's:
- Formula
- The distribution of residuals (residuals can be plotted using `hist(cubicMultivariateReg$residuals)` or `plot(cubicMultivariateReg$residuals)`.
- The coefficients with their estimates, standard errors, t value and p value ('Pr(>|t|)')
- Multiple and adjusted R-squared
- F-statistic
- A p value for the overall model fit
- One may also simply call a regression model, e.g. `cubicMultivariateReg` to see its formula and coefficients.

### Reproducing tables
Each individual table can be reproduced by running the scripts in the 'table_scripts' directory. Make sure that the R data files 'reduced_coupling_data.RData' and 'regressions_data.RData' are located in the same directory as the table scripts. The tables can be generated in PDF, docx, xlsx, tex, or html (although PDF seems to have formatting issues) by (un)commenting the lines that say `quick_html(file = 'table2.html')`, `quick_xlsx(file = 'table2.xlsx')`, etc. Only one format can be printed at a time.

The directory 'table_scripts' contains directories with all tables, in all formats, already correctly formatted and generated since the table formatting in the scripts themselves is not so straightforward.

### Reproducing figures
The directory 'figure_scripts' contains a single script to reproduce each figure. The scripts are written in two languages: R and MATLAB.

To run the R scripts, just make sure that the two R data files 'reduced_coupling_data.RData' and 'regressions_data.RData' are present in the directory.

The MATLAB scripts will ask for the parent directory to the raw numerical dataset (i.e. `./numerical_models/`), so it will be necessary to put in the correct path to the directory at the top of each script. For example, `directory = ['/path/to/your/directory/' nname];` and again `cd(['/path/to/your/directory/' nname]);` making sure that `nname='cdf78';` is the model you want to visualize. The 'numerical_models' dataset can be downloaded [here](https://osf.io/zjac3/).

## Web App
There are a few options for running the web application located in the 'coupling_app' directory:
1. Run straight from RStudio by opening and running the 'app.R' script
2. [Run](https://kerswell.shinyapps.io/coupling_app/) the app from a web browser

There is a cap of 25hr of app runtime per month. Feel free to explore, but please be mindful of closing the app when you're finished.

The application plots the backarc geotherm and predicted coupling depth for modern subduction zone segments (data from [Wada & Wang (2009)](https://agupubs.onlinelibrary.wiley.com/doi/full/10.1029/2009GC002570)), or you can 'customize' your own backarc by selecting NA for the segment and using the 'Custom' tab to enter your own parameters. *Note:* the thermal parameter used in this app, as presented in the paper, is the product of slab age and convergence rate, divided by 100. It *does not* include the sine of the dip angle.