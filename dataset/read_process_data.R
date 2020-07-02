# Read heatflow data 
# Load Libraries
library(tidyverse)
# Set working directory to this scripts' location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Read data for antigorite stability thru time ----
# Set working directory to this scripts' location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Read data for antigorite stability depth thru time for cdf models
antstab <- read.csv('antstability.csv')

# Read Penniston-Dorland et al. (2015) dataset ----
PD15 <- read.csv('PD15.csv')
# Read model geotherms ----
geotherms <- read.csv('model_geotherms.csv')

# Reading data for 46km lithosphere models ----

# Go into folder containing data for 46km thick lithosphere experiments
setwd('./heatflow/46km')
# List files
filelist <- list.files(pattern="*.txt")
# Extract model names from filenames (e.g. "cda46" from "cda46_hf10ma.txt")
file_names46 <- substr(filelist,1,5)
# Read data and assign the names of each model to the dataframe objects
lith46models <- sapply(file_names46, FUN = function(file_names46){
  filepath <- file.path('.', paste(file_names46, '_hf10ma.txt', sep = ''))
  assign(file_names46, read.table(filepath, header=TRUE))
}, simplify = FALSE)

# Reading data for 62km lithosphere models ----
# Set working directory to this scripts' location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Go into folder containing data for 62km thick lithosphere experiments
setwd('./heatflow/62km')
# List files
filelist <- list.files(pattern="*.txt")
# Extract model names from filenames (e.g. "cda62" from "cda62_hf10ma.txt")
file_names62 <- substr(filelist,1,5)
# Read data and assign the names of each model to the dataframe objects
lith62models <- sapply(file_names62, FUN = function(file_names62){
  filepath <- file.path('.', paste(file_names62, '_hf10ma.txt', sep = ''))
  assign(file_names62, read.table(filepath, header=TRUE))
}, simplify = FALSE)

# Reading data for 78km lithosphere models ----
# Set working directory to this scripts' location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Go into folder containing data for 62km thick lithosphere experiments
setwd('./heatflow/78km')
# List files
filelist <- list.files(pattern="*.txt")
# Extract model names from filenames (e.g. "cda78" from "cda78_hf10ma.txt")
file_names78 <- substr(filelist,1,5)
# Read data and assign the names of each model to the dataframe objects
lith78models <- sapply(file_names78, FUN = function(file_names78){
  filepath <- file.path('.', paste(file_names78, '_hf10ma.txt', sep = ''))
  assign(file_names78, read.table(filepath, header=TRUE))
}, simplify = FALSE)

# Reading data for 94km lithosphere models ----
# Set working directory to this scripts' location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Go into folder containing data for 94km thick lithosphere experiments
setwd('./heatflow/94km')
# List files
filelist <- list.files(pattern="*.txt")
# Extract model names from filenames (e.g. "cda94" from "cda94_hf10ma.txt")
file_names94 <- substr(filelist,1,5)
# Read data and assign the names of each model to the dataframe objects
lith94models <- sapply(file_names94, FUN = function(file_names94){
  filepath <- file.path('.', paste(file_names94, '_hf10ma.txt', sep = ''))
  assign(file_names94, read.table(filepath, header=TRUE))
}, simplify = FALSE)
rm(filelist)

# Create new columns to add to dataframes ----
# including columns for: model ID, plate age, and plate velocity
# Create vector for plate ages
ages<-c(rep(32.6,4), rep(55,4), rep(85,4), rep(110,4))
# Create vector for velocities
velocities <- rep(c(40,66,80,100),4)

# Manipulating and reducing dataframes ----
# Add new columns to existing dataframes, rename column headers, and reduce into
# smaller dataframes for easier regression

# 46km thick lithosphere models ---- 
# Adding model, age, and convergence velocity columns
lith46models <- mapply(cbind, lith46models, 'model' = as.factor(file_names46),
                                   'age' = ages,
                                   'cv' = velocities,
            SIMPLIFY = F)
# Transforming list of dataframes into a single datafrmae
lith46models <- bind_rows(lith46models)
# Renaming column headers
colnames(lith46models) <- c('norm.dist', 'surf.hf', 'smoothed.hf', 'tparam', 
                            'z.coupling', 'z1100', 'trench.loc', 'coupling.loc', 
                            'timestep', 'time', 'model', 'plate.age', 'conv.velocity')
# Extracting unique values of coupling depth, thermal parameter, and lithospheric
# thickness from large dataframe
coupling.46 <- lith46models$z.coupling[seq(1, length(lith46models$z.coupling), 921)]
tparam.46<-lith46models$tparam[seq(1,length(lith46models$tparam),921)]
z1100.46<-lith46models$z1100[seq(1,length(lith46models$z1100),921)]
model.46<-lith46models$model[seq(1,length(lith46models$model),921)]
# Combining vectors to form small (3 x 16) dataframe
reducedDataFrame46<-data.frame('tparam'=tparam.46,'z.coupling'=coupling.46, 'z1100'=z1100.46, 'model'=model.46)
# Remove unwanted variables to clean up environment
rm(coupling.46, tparam.46, z1100.46, model.46, file_names46)

# 62km thick lithosphere models ----
lith62models <- mapply(cbind, lith62models, 'model' = as.factor(file_names62),
                       'age' = ages,
                       'cv' = velocities,
                       SIMPLIFY = F)
lith62models <- bind_rows(lith62models)
colnames(lith62models) <- c('norm.dist', 'surf.hf', 'smoothed.hf', 'tparam', 
                            'z.coupling', 'z1100', 'trench.loc', 'coupling.loc', 
                            'timestep', 'time', 'model', 'plate.age', 'conv.velocity')
coupling.62<-lith62models$z.coupling[seq(1,length(lith62models$z.coupling),921)]
tparam.62<-lith62models$tparam[seq(1,length(lith62models$tparam),921)]
z1100.62<-lith62models$z1100[seq(1,length(lith62models$z1100),921)]
model.62<-lith62models$model[seq(1,length(lith62models$model),921)]
reducedDataFrame62<-data.frame('tparam'=tparam.62,'z.coupling'=coupling.62, 'z1100'=z1100.62, 'model'=model.62)

rm(coupling.62,tparam.62,z1100.62, model.62, file_names62)

# 78km thick lithosphere models ----
lith78models <- mapply(cbind, lith78models, 'model' = as.factor(file_names78),
                       'age' = ages,
                       'cv' = velocities,
                       SIMPLIFY = F)
lith78models <- bind_rows(lith78models)
colnames(lith78models) <- c('norm.dist', 'surf.hf', 'smoothed.hf', 'tparam', 
                            'z.coupling', 'z1100', 'trench.loc', 'coupling.loc', 
                            'timestep', 'time', 'model', 'plate.age', 'conv.velocity')
coupling.78<-lith78models$z.coupling[seq(1,length(lith78models$z.coupling),921)]
tparam.78<-lith78models$tparam[seq(1,length(lith78models$tparam),921)]
z1100.78<-lith78models$z1100[seq(1,length(lith78models$z1100),921)]
model.78<-lith78models$model[seq(1,length(lith78models$model),921)]
reducedDataFrame78<-data.frame('tparam'=tparam.78,'z.coupling'=coupling.78, 'z1100'=z1100.78, 'model'=model.78)

rm(coupling.78, tparam.78, z1100.78, file_names78, model.78)

# 94km thick lithosphere models ----
lith94models <- mapply(cbind, lith94models, 'model' = as.factor(file_names94),
                       'age' = ages,
                       'cv' = velocities,
                       SIMPLIFY = F)
lith94models <- bind_rows(lith94models)
colnames(lith94models) <- c('norm.dist', 'surf.hf', 'smoothed.hf', 'tparam', 
                            'z.coupling', 'z1100', 'trench.loc', 'coupling.loc', 
                            'timestep', 'time', 'model', 'plate.age', 'conv.velocity')
coupling.94<-lith94models$z.coupling[seq(1,length(lith94models$z.coupling),921)]
tparam.94<-lith94models$tparam[seq(1,length(lith94models$tparam),921)]
z1100.94<-lith94models$z1100[seq(1,length(lith94models$z1100),921)]
model.94<-lith94models$model[seq(1,length(lith94models$model),921)]
reducedDataFrame94<-data.frame('tparam'=tparam.94,'z.coupling'=coupling.94, 'z1100'=z1100.94, 'model'=model.94)

rm(coupling.94, tparam.94, z1100.94, file_names94, model.94)
rm(ages, velocities)

# Combining seperate categorical datasets into one massive dataset ----
allModelsDataframe <- bind_rows(lith46models,lith62models,lith78models,lith94models)
allModelsDataframe$model <- as.factor(allModelsDataframe$model)
reducedAllModelsDataframe <- bind_rows(reducedDataFrame46,reducedDataFrame62,reducedDataFrame78,reducedDataFrame94)

# Save dataset ----
# Set working directory to this scripts' location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
save.image(file = 'reduced_coupling_data.RData')