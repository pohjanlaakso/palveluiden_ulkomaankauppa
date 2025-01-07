
#### Comments ####
# palveluiden ulkomaankaupan ennustaminen World Bank datalla
# https://ualibweb.github.io/UALIB_ScholarlyAPI_Cookbook/src/r/world-bank.html
# vaihtoehtoinen datalähde: https://comtradeplus.un.org/

####

# ----
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
### WORLD BANK ####

wb_url <- 'https://api.worldbank.org/v2/'
wb_country_url <- paste0(wb_url, 'country?format=json&per_page=500')

wb_raw_country_data <- GET(wb_country_url)

wb_prelim_country_data <- fromJSON( # reads raw 8 bit data to characters
  rawToChar(wb_raw_country_data$content), 
  flatten = T
  )

wb_country_data <- wb_prelim_country_data[[2]]

# generate web api urls
wb_indicators <- list('TM.VAL.SERV.CD.WT', 'TX.VAL.SERV.CD.WT') #WB indicators: https://data.worldbank.org/indicator?tab=all

wb_finland_api_url <- c()

for(indicator in wb_indicators) {
  wb_finland_api_url<- append(x = wb_finland_api_url, 
                           values = paste(wb_url, 'country/FI/indicator/', indicator, '?format=json&per_page=500', sep =''))
}

wb_finland_api_url

####




 # ---- # 

# -----
### WTO environment variables ####

# user guide: https://apiportal.wto.org/api-details#api=version1&operation=get-data-i-i
wto_url <- 'https://api.wto.org/timeseries/v1/data?i='
wto_key <- 'bf9819df987c463fb9c73b7b55ff7281' # subscription key
wto_indicators <- list('ITS_CS_MX', 'ITS_CS_MM') #WTO indicators & query builder: https://stats.wto.org/

####

# ----
#### WTO monthly master data frame #### 

wto_monthly_url <- c()
wto_monthly_indicators <- list('ITS_CS_MX', 'ITS_CS_MM') # exports and imports

for(indicator in wto_monthly_indicators) {
  wto_monthly_url <- append(x = wto_monthly_url,
                                values = paste(wto_url, 
                                               indicator, 
                                               '&r=156,246,276,528,752,840', # Germany, USA, Finland, Sweden, and China
                                               '&p=000',
                                               '&pc=SOX,S,SPX1', # commercial services (SOX) v. total services (S)
                                               '&fmt=json', 
                                               '&ps=all', 
                                               '&max=1000000',
                                               '&subscription-key=', 
                                               wto_key, 
                                               sep = ''))
}; wto_monthly_url

wto_monthly_indicator_data <- list()

for(url in wto_monthly_url) {
  temp_data <- rawToChar(GET(url)$content)
  Encoding(temp_data) <- 'latin1' # bug fix: # https://stackoverflow.com/questions/54627177/lexical-error-invalid-bytes-in-utf8-string
  temp_data <- tibble(fromJSON(temp_data, flatten = T)[[1]])
  wto_monthly_indicator_data <- append(wto_monthly_indicator_data, list(temp_data))
}

####

# ----
### WTO quarterly master data frame ####

wto_quarter_url <- c()
wto_quarter_indicators <- list('ITS_CS_QX', 'ITS_CS_QM') # exports and imports

for(indicator in wto_quarter_indicators) {
  wto_quarter_url <- append(wto_quarter_url,
                           paste(wto_url,
                                 indicator,
                                 '&r=156,246,276,528,752,840', # the API works different than it is supposed to!!!
                                 '&p=000',
                                 '&pc=SOX,S,SPX1', # SOX data only for US, GER, and CHN!
                                 '&fmt=json', 
                                 '&ps=all',
                                 '&max=1000000',
                                 '&subscription-key=',
                                 wto_key,
                                 sep = ''))
}; wto_quarter_url

wto_quarterly_indicator_data <- list()

for(url in wto_quarter_url) {
  temp_data <- rawToChar(GET(url)$content)
  Encoding(temp_data) <- 'latin1' # bug fix (see stack overflow link above)
  temp_data <- tibble(fromJSON(temp_data, flatten = T)[[1]])
  wto_quarterly_indicator_data <- append(wto_quarterly_indicator_data, list(temp_data))
}

####

# ----
#### WTO monthly % change master data frame ####

wto_percentage_url <- c()
wto_percentage_indicators <- list('ITS_CS_MX_GR', 'ITS_CS_MM_GR')

for(indicator in wto_percentage_indicators) {
  wto_percentage_url <- append(wto_percentage_url,
                            paste(wto_url,
                                  indicator,
                                  '&r=156,246,276,528,752,840', # the API works different than it is supposed to!!!
                                  '&p=000',
                                  '&pc=SOX,S,SPX1', # SOX data only for US, GER, and CHN!
                                  '&fmt=json', 
                                  '&ps=all',
                                  '&max=1000000',
                                  '&subscription-key=',
                                  wto_key,
                                  sep = ''))
}; wto_percentage_url

wto_percentage_indicator_data <- list()

for(url in wto_percentage_url) {
  temp_data <- rawToChar(GET(url)$content)
  Encoding(temp_data) <- 'latin1' # bug fix (see stack overflow link above)
  temp_data <- tibble(fromJSON(temp_data, flatten = T)[[1]])
  wto_percentage_indicator_data <- append(wto_percentage_indicator_data, list(temp_data))
}

####
#----
# WTO sectors with sub-items master data frame ####

wto_subsector_url <- c()
wto_subsector_indicators <- list('ITS_CS_QX', 'ITS_CS_QM') # quarterly exports and imports

for(indicator in wto_subsector_indicators) {
  wto_subsector_url <- append(
    wto_subsector_url,
    paste(
      wto_url,
      indicator,
      '&r=156,246,276,528,752,840',
      '&p=000',
      '&pc=SOX,SPX4,SA,SAY,SAZ,SB,SC,SC1,SC11,SC11Z,SC12,SC13,SC2,SC21,SC21Z,SC22,SC23,SC3,SC31,SC31Z,SC32,SC33,SC3A,SC3B,SC3B1,SC3B2,SC3B3,SC3C,SC3C1,SC3C2,SC3C3,SC3D,SC3D1,SC3D2,SC3D3,SC3E,SC3F,SC3G,SC4,SC41X,SC42X,SCA,SCAZ,SCB,SCC,SCC1,SD,SD1,SD2,SD3,SD4,SD5,SD5Y,SD5Z,SDA,SDA1,SDA2,SDB,SDB1,SDB2,SDB3,SDZ,SOX1,SE,SE1,SE2,SF,SF1,SF11,SF11Y,SF11Z,SF12,SF12Y,SF12Z,SF13,SF13Y,SF13Z,SF2,SF3,SF4,SF41,SF42,SG,SG1,SG11,SG12,SG13,SG14,SG15,SG16,SG2,SH,SH1,SH11,SH12,SH2,SH3,SH4,SH41,SH411,SH412,SH413,SH42,SI,SI1,SI2,SI21,SI21Z,SI22,SI221,SI222,SI3,SI31,SI32,SJ,SJ1,SJ11,SJ111,SJ112,SJ1121,SJ1122,SJ1123,SJ1124,SJ12,SJ2,SJ21,SJ211,SJ212,SJ213,SJ22,SJ22Z,SJ221,SJ222,SJ3,SJ31,SJ311,SJ312,SJ313,SJ32,SJ321,SJ322,SJ323,SJ33,SJ34,SJ35,SJ35Z,SK,SK1,SK11,SK11Z,SK11Z1,SK11Z2,SK111,SK112,SK1121,SK1122,SK12,SK2,SK21,SK22,SK23,SK24,SN',
      '&fmt=json',
      '&ps=all',
      '&max=1000000',
      '&subscription-key=',
      wto_key,
      sep = ''
    )
  )
}; wto_subsector_url

wto_subsector_indicator_data <- list()

for(url in wto_subsector_url) {
  temp_data <- rawToChar(GET(url)$content)
  Encoding(temp_data) <- 'latin1'
  temp_data <- tibble(fromJSON(temp_data, flatten = T)[[1]])
  wto_subsector_indicator_data <- append(wto_subsector_indicator_data, list(temp_data))
}


####

# ----
# monthly global service imports (proxy for service demand) ####

# the redundancy of writing so much boilerplate code could be reduced: 
# https://stackoverflow.com/questions/48694626/creating-a-function-in-r-with-variable-number-of-arguments

wto_world_demand_url <- c()
wto_world_demand_indicators <- list ('ITS_CS_QX', 'ITS_CS_QM', 'ITS_CS_MX', 'ITS_CS_MM', 'ITS_CS_MX_GR', 'ITS_CS_MM_GR')

for(indicator in wto_world_demand_indicators) {
  wto_world_demand_url <- append(
    wto_world_demand_url,
    paste(
      wto_url,
      indicator,
      '&p=000',
      '&pc=SOX,S',
      '&fmt=json',
      '&max=1000000',
      '&ps=all',
      '&subscription-key=',
      wto_key,
      sep = ''
    )
  )
}; wto_world_demand_url

wto_world_demand_indicator_data <- list()

for(url in wto_world_demand_url) {
  temp_data <- rawToChar(GET(url)$content)
  Encoding(temp_data) <- 'latin1'
  temp_data <- tibble(fromJSON(temp_data, flatten = T)[[1]])
  wto_world_demand_indicator_data <- append(wto_world_demand_indicator_data, list(temp_data))
}


####
# ----
# annual world service imports (proxy for service demand) ####

wto_annual_world_demand_url <- c()
wto_annual_world_demand_indicators <- list('ITS_CS_AX5', 'ITS_CS_AM5')

for(indicator in wto_annual_world_demand_indicators) {
  wto_annual_world_demand_url <- append(
    wto_annual_world_demand_url,
    paste(
      wto_url,
      indicator,
      '&r=000',
      '&pc=S200CS,S200',
      '&ps=all',
      '&fmt=json',
      '&max=1000000',
      '&subscription-key=',
      wto_key,
      sep = ''
    )
  )
}; wto_annual_world_demand_url

wto_annual_world_demand_indicator_data <- list()

for(url in wto_annual_world_demand_url) {
  temp_data <- rawToChar(GET(url)$content)
  Encoding(temp_data) <- 'latin1'
  temp_data <- tibble(fromJSON(temp_data, flatten = T)[[1]])
  wto_annual_world_demand_indicator_data <- append(wto_annual_world_demand_indicator_data, list(temp_data))
} # another idea would be to get data --> write to a csv + name: "_time stamp"; more error tolerant

####
# ----
#### Bug fix (don't delete) ####

#test_a <- GET(wto_finland_api_url[1])
#test_aa <- rawToChar(test_a$content)
#Encoding(test_aa) <- 'latin1' # https://stackoverflow.com/questions/54627177/lexical-error-invalid-bytes-in-utf8-string
#test_aaa <- fromJSON(test_aa, flatten = T)
#test_aaaa <- test_aaa[[1]]

# creating a date object and ordering data
#test_aaaa$year_month <- paste(test_aaaa$Period, test_aaaa$Year)
#test_aaaa$month_number <- substr(test_aaaa$PeriodCode, 2, 3)
#test_aaaa$date <- (paste('01', test_aaaa$month_number, test_aaaa$Year, sep ='-'))
#test_aaaa$date <- as.Date(test_aaaa$date, format = '%d-%m-%Y')
#test_aaaa <- test_aaaa[order(as.Date(test_aaaa$date, format = '%d-%m-%Y')),] # https://stackoverflow.com/questions/6246159/how-to-sort-a-data-frame-by-date
#plot.ts(test_aaaa$Value)
####

# ----
#### Transforming monthly WTO export data ####
wto_monthly_exports <- wto_monthly_indicator_data[[1]]; sapply(wto_monthly_exports, class)

# create a date attribute
wto_monthly_exports$month_number <- substr(wto_monthly_exports$PeriodCode, 2, 3)
wto_monthly_exports$Date <- paste('01', wto_monthly_exports$month_number, wto_monthly_exports$Year, sep = '-')
wto_monthly_exports$Date <- as.Date(wto_monthly_exports$Date, format = '%d-%m-%Y')

# order by date attribute
wto_monthly_exports <- wto_monthly_exports[order(as.Date(wto_monthly_exports$Date, format = '%d-%m-%Y')), ]

# visual check
plot.ts(subset(wto_monthly_exports, ReportingEconomy == 'Finland' & ProductOrSectorCode == 'S')$Value, ylim = c(1000, 10000))
lines(subset(wto_monthly_exports, ReportingEconomy == 'Sweden' & ProductOrSectorCode == 'S')$Value, col = 2)

####

# ----
#### Transforming monthly WTO import data ####
wto_monthly_imports <- wto_monthly_indicator_data[[2]]; sapply(wto_monthly_imports, class)

# create a date attribute
wto_monthly_imports$month_number <- substr(wto_monthly_imports$PeriodCode, 2, 3)
wto_monthly_imports$Date <- paste('01', wto_monthly_imports$month_number, wto_monthly_imports$Year, sep = '-')
wto_monthly_imports$Date <- as.Date(wto_monthly_imports$Date, format = '%d-%m-%Y')

# order by date attribute
wto_monthly_imports <- wto_monthly_imports[order(as.Date(wto_monthly_imports$Date, format = '%d-%m-%Y')), ]

# visual check
plot.ts(subset(wto_monthly_imports, ReportingEconomy == 'Finland' & ProductOrSectorCode == 'S')$Value, ylim = c(1000, 10000))
lines(subset(wto_monthly_imports, ReportingEconomy == 'Sweden' & ProductOrSectorCode == 'S')$Value, col = 2)

####



# ----
#### Transforming quarterly WTO export data ####

wto_quarterly_exports <- wto_quarterly_indicator_data[[1]]; sapply(wto_quarterly_exports, class); unique(wto_quarterly_exports$ReportingEconomy)

# create a date attribute
wto_quarterly_exports$month_number <- substr(wto_quarterly_exports$PeriodCode, 2, 3)
wto_quarterly_exports$Date <- as.Date(paste('01', wto_quarterly_exports$month_number, wto_quarterly_exports$Year, sep ='-'), format = '%d-%m-%Y')

# order by date
wto_quarterly_exports <- wto_quarterly_exports[order(wto_quarterly_exports$Date), ]

# visual check
plot.ts(subset(wto_quarterly_exports, ReportingEconomy == 'Finland' & ProductOrSectorCode == 'S')$Value, ylim = c(1000, 20000))
lines(subset(wto_quarterly_exports, ReportingEconomy == 'Sweden' & ProductOrSectorCode == 'S')$Value, col = 2)

####
# ----
#### Transforming quarterly WTO import data ####

wto_quarterly_imports <- wto_quarterly_indicator_data[[2]]; sapply(wto_quarterly_imports, class); unique(wto_quarterly_imports$ReportingEconomy)

# create a date attribute
wto_quarterly_imports$month_number <- substr(wto_quarterly_imports$PeriodCode, 2, 3)
wto_quarterly_imports$Date <- as.Date(paste('01', wto_quarterly_imports$month_number, wto_quarterly_imports$Year, sep = '-'), format = '%d-%m-%Y')

# order by date
wto_quarterly_imports <- wto_quarterly_imports[order(wto_quarterly_imports$Date), ]

# visual check
plot.ts(subset(wto_quarterly_imports, ReportingEconomy == 'Finland' & ProductOrSectorCode == 'S')$Value, ylim = c(1000, 20000))
lines(subset(wto_quarterly_imports, ReportingEconomy == 'Sweden' & ProductOrSectorCode == 'S')$Value, col = 2)

# ----
#### Transforming percentage WTO import data ####

wto_percentage_imports <- wto_percentage_indicator_data[[2]]

# creating a date attribute
wto_percentage_imports$month_number <- substr(wto_percentage_imports$PeriodCode, 2, 3)
wto_percentage_imports$Date <- as.Date(paste('01', wto_percentage_imports$month_number, wto_percentage_imports$Year, sep = '-'), format = '%m-%d-%Y')

# order by date
wto_percentage_imports <- wto_percentage_imports[order(wto_percentage_imports$Date), ]

# visual checks
plot.ts(subset(wto_percentage_imports, ReportingEconomy == 'Finland' & ProductOrSectorCode == 'S')$Value)
abline(h=0, col = 'blue', lty = 4)
lines(subset(wto_percentage_imports, ReportingEconomy == 'Sweden' & ProductOrSectorCode == 'S')$Value, col = 2)

####

# ----
#### Transforming percentage WTO export data ####

wto_percentage_exports <- wto_percentage_indicator_data[[1]]

# create a date attribute
wto_percentage_exports$month_number <- substr(wto_percentage_exports$PeriodCode, 2, 3)
wto_percentage_exports$Date <- as.Date(paste('01', wto_percentage_exports$month_number, wto_percentage_exports$Year, sep = '-'), format = '%m-%d-%Y')

# order by date
wto_percentage_exports <- wto_percentage_exports[order(wto_percentage_exports$Date), ]

# visual checks
plot.ts(subset(wto_percentage_exports, ReportingEconomy == 'Finland' & ProductOrSectorCode == 'S')$Value)
abline(h=0, col = 'blue', lty = 4)
lines(subset(wto_percentage_exports, ReportingEconomy == 'Sweden' & ProductOrSectorCode == 'S')$Value, col = 2)


####


# ----
# Transforming sub sector WTO export data ####

wto_subsector_exports <- wto_subsector_indicator_data[[1]]

# create a date attribute
wto_subsector_exports$month_number <- substr(wto_subsector_exports$PeriodCode, 2, 3)
wto_subsector_exports$Date <- as.Date(paste('01', wto_subsector_exports$month_number, wto_subsector_exports$Year, sep = '-'), format = '%d-%m-%Y')

# order by date
wto_subsector_exports <- wto_subsector_exports[order(wto_subsector_exports$Date), ]

# visual check
plot.ts(subset(wto_subsector_exports, ReportingEconomy == 'Finland' & ProductOrSectorCode == 'SI')$Value)

####
# ----
# Transforming sub sector WTO import data ####

wto_subsector_imports <- wto_subsector_indicator_data[[2]]

# create a date attribute
wto_subsector_imports$month_number <- substr(wto_subsector_imports$PeriodCode, 2, 3)
wto_subsector_imports$Date <- as.Date(paste('01', wto_subsector_imports$month_number, wto_subsector_imports$Year, sep = '-'), format = '%d-%m-%Y')

# order by date
wto_subsector_imports <- wto_subsector_imports[order(wto_subsector_imports$Date), ]

# visual check
plot.ts(subset(wto_subsector_imports, ReportingEconomy == 'Sweden' & ProductOrSectorCode == 'SE')$Value)

####

# ----
# Transforming global service imports ####

# quarterly imports and exports
wto_quarterly_global_exports <- wto_world_demand_indicator_data[[1]]
wto_quarterly_global_imports <- wto_world_demand_indicator_data[[2]]

# monthly imports and exports
wto_monthly_global_exports <- wto_world_demand_indicator_data[[3]]
wto_monthly_global_imports <- wto_world_demand_indicator_data[[4]]

# monthly import and exports growth rates
wto_monthly_global_exports_growth <- wto_world_demand_indicator_data[[5]]
wto_monthly_global_imports_growth <- wto_world_demand_indicator_data[[6]]

# add date attribute function
date_attribute <- function(wto_df) {
  wto_df$month_number <- substr(wto_df$PeriodCode, 2, 3)
  wto_df$Date <- as.Date(paste('01', wto_df$month_number, wto_df$Year, sep = '-'), format = '%d-%m-%Y')
  wto_df <- wto_df[order(wto_df$Date), ]
  return(wto_df)
}

# use function: quarterly data
wto_quarterly_global_exports <- date_attribute(wto_quarterly_global_exports)
wto_quarterly_global_imports <- date_attribute(wto_quarterly_global_imports)

# use function: monthly data
wto_monthly_global_exports <- date_attribute(wto_monthly_global_exports)
wto_monthly_global_imports <- date_attribute(wto_monthly_global_imports)

# use function: monthly growth data
wto_monthly_global_exports_growth <- date_attribute(wto_monthly_global_exports_growth)
wto_monthly_global_imports_growth <- date_attribute(wto_monthly_global_imports_growth)

####
# ----
# Transforming annual global service exports ####

wto_annual_global_exports <- wto_annual_world_demand_indicator_data[[1]]
wto_annual_global_imports <- wto_annual_world_demand_indicator_data[[2]]

####




# ----
#### subset of the quarterly master data frame ####

finland_exportsQ <- subset(wto_quarterly_exports, ReportingEconomy=='Finland' & ProductOrSectorCode == 'S')
finland_importsQ <- subset(wto_quarterly_imports, ReportingEconomy=='Finland' & ProductOrSectorCode == 'S')

sweden_exportQ <- subset(wto_quarterly_exports, ReportingEconomy=='Sweden' & ProductOrSectorCode == 'S')

plot(finland_exportsQ$Date, finland_exportsQ$Value, type ='l', ylim = c(2000, 12000))
lines(finland_importsQ$Date, finland_importsQ$Value, col = 2)

plot.ts(finland_exportsQ$Value, ylim = c(2000, 12000))
lines(finland_importsQ$Value, col = 2)


####

# ----
#### Next up ####

#https://happygitwithr.com/rstudio-git-github & https://www.geeksforgeeks.org/link-your-github-account-with-r-studio/
# at some point maybe: https://airbnb.io/javascript/react/ & https://github.com/airbnb/javascript

####
# ----
