
#### Mise en place ####

rm(list=ls())
setwd(getwd())

# essential libraries
library(httr) # GET()
library(jsonlite) # converting to JSON
library(dplyr) # tibbles
library(zoo) # month-year objects
library(forecast) # AR-stuff
library(tidyr)

####
# ----
#### get transformed data ####

source('palveluiden_ulkomaankaupan_ennustaminen_A.R')
source('palveluiden_ulkomaankaupan_ennustaminen_B.R')

####

# ----

# forecast finnish service exports ###

# finnish service exports_t = B0 +
# B_1 * transport_t-1 + (other lags eg. lag = 4 might be more suitable; this has to be checked item by item) 
# B_2 * travel_t-1 +
# B_3 * construction_t-1
# B_4 * insurance_t-1
# ...
# B_n * global service demand (lag = 4)
# B_n * price of copper, oil, raw materials (Finnish exports are mostly done by large industrial corporations)
# B_n * exchange rate eg. EUR/USD
# B_n * unit labour cost
# B_n * imports (eg. imports of other business services?)
# B_n * ostopäällikköindeksit palveluille ja teollisuudelle
# ...
# seasonal dummies: 
# error term
# AR-term

####

# neural network model for benchmark ####


####

# ----