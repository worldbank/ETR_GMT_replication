# 0. MASTER 

# Title : Effective Tax Rate and Firm Size
# Authors: Pierre Bachas, Anne Brockmeyer, Roel Dom and Camille Semelet
# Data Sources: CIT admin data, WDI

rm(list=ls(all=TRUE))

# IMPORTANT: 
#   step 1: get the USERNAME, type "Sys.getenv()" within the R Console 
#   step 2: replace username in line 119
#   step 3: adapt the path. Need Github access
#   

#########################################################################.
# 0. PACKAGES

# define a list of required packages

packages <- c("tidyverse", "data.table", "countrycode", "wbstats", "collapse" ,
              "stargazer", "lazyeval",
              "fixest", "broom", "jtools", "ggformula", "huxtable", "ggrepel",
              "readxl", "DescTools", "RStata",
              "xtable", "ggbrace")
# loop through the list of packages and check if they are installed and loaded
for (p in packages) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p)
    library(p, character.only = TRUE)
  }
}


#########################################################################.
# 1. USERS

    ## Paths & objects 
    directory <- "XXX"  
    gitHub <- paste0("XXX")  
    country_name <- c("Country Name") 
    country_code <- c("ISO3")
    pre <- "" ## Only needed if several users
    # Select countries where we run the complete GMT analysis
    country_GMT <- c("ISO3")


#########################################################################.
# 3. PATHS

project <- "ETR/Replication Package"

## On gitHub
code <- paste0(gitHub, project, "/codes/")    ## dofiles
aux_source <- paste0(gitHub, project, "/codes/Auxillary codes/")
regressions <-  paste0(gitHub, project, "/output/regressions/")
metadata <-  paste0(gitHub, project, "/output/metadata/")
output <-  paste0(gitHub, project, "/output/")


#########################################################################.
# 4. PREPARE RAW CIT DATA

  options("RStata.StataPath" = "\"C:\\Program Files\\Stata16\\StataMP-64\"")
  options("RStata.StataVersion" = 16)
  setwd(code)
  stata("cleaning_template.do")
  

#########################################################################

# 5. LOAD CIT DATA

data_raw <- list() # Creates an empty list. This is where we store the .dta files
files <- paste0(country_code, "_withvars.csv", sep = "") # Names of the Files to be read. 

for (i in 1: length(files)){
  setwd(directory)
  df  <- fread(files[i], integer64 = "numeric") 
  
  ## fread read some columns as integer64 instead of numeric. 
  ## The conversion above does not work when column is being bumped to integer64
  ## (i.e. out of sample type bump occuring) 
  ## a workaround is as follows:
  
  coln.int64 <- names(which(sapply(df, bit64::is.integer64)))
  if (length(coln.int64) > 0L)
    df[ , c(coln.int64) := lapply(.SD, as.numeric), .SDcols = coln.int64]
  data_raw[[i]] <- df %>% as.data.frame()
  print(country_name[i])
} #end loop

print("Data loaded")


#########################################################################.
# 6. READ & PREPARE DATA

# Take raw data add WDI variables create the main dataset:
#   cross-sectional (last year) (list df.sample)
#   panel data (list data)
#   STR is 
#   Restrictions are as follows: 
#       - turnover > 1
#       - df.sample is the latest year & != 2020
# 
#########################################################################.



#########################################################################.
# 7. GMT ANALYSIS 
#
prepare_data <- "GMT_cleaning"
#
setwd(code)
source("1_prepare_data.R")
#
setwd(code)
source("2.1_GMT_simulation.R")

# Line 80 you need to specify years and CPI index:
#   
#   if (country_GMT[[c]]=="ISO3"){
#     index <- 0.8580305 ## REPLACE HERE
#     year_t <- 2018 ## REPLACE HERE
#     year_1_t <- 2017 ## REPLACE HERE
#   }

#
setwd(code)
source("2.2_Minimum_Tax_Top1.R")
#
setwd(code)
source("2.3_Minimum_Tax_Top1_output.R")


options("RStata.StataPath" = "\"C:\\Program Files\\Stata16\\StataMP-64\"")
options("RStata.StataVersion" = 16)
setwd(code)
stata("2_4_GMT_output.do")

### Clean environment
rm(list = ls(all = TRUE)[sapply(mget(ls(all = TRUE)), class) != "character" &
                           !ls(all = TRUE) %in% 
                           c("data_raw", "data.cross", "data.panel")])

#########################################################################.
# 8. ETR ANALYSIS 

prepare_data <- "ETR_cleaning"
#
setwd(code)
source("1_prepare_data.R")
#
setwd(code)
source("3.1_descriptive_ETR.R")
#
setwd(code)
source("3.2_regression_ETR.R")
#
setwd(code)
source("3.3_output_ETR.R")


