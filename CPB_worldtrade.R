
# mise en place
rm(list =ls())
setwd(getwd())
library(zoo)

# CPB Netherlands: Development of world trade on behalf of the European Commission
url <- 'https://www.cpb.nl/sites/default/files/omnidownload/CPB-World-Trade-Monitor-July-2024.xlsx'

refresh_data <- function(url, month, year) {
  return(gsub('2024', year, gsub('July', month, url))) # nested gsub: https://stackoverflow.com/questions/33949945/replace-multiple-strings-in-one-gsub-or-chartr-statement-in-r
}

new_url <- refresh_data(url, 'August', '2024')

file_name <- 'cpb.xlsx'
file_name_timestamp <- paste(substr(Sys.time(), 0, 10), 'cpb.xlsx', sep='_')
file_path <- 'C:/Users/03149822/Desktop/palveluiden_ulkomaankauppa/tiedostohaut/'

# download.file(export_maittain, 

# paste('C:/Users/03149822/Desktop/palveluiden_ulkomaankauppa/tiedostohaut/', 'export_maittain.csv', sep=''), mode ='wb')


download.file(
  url,
  paste(file_path, file_name, sep=''), mode ='wb'
)

download.file(
  url,
  paste(file_path, file_name_timestamp, sep=''), mode ='wb'
)

# countryExport_df <- read.csv('tiedostohaut/export_maittain.csv', skip = 1, header = F, stringsAsFactors = F, fileEncoding = 'latin1')

cpb <- read.csv(
  'tiedostohaut/cpb.xlsx',
  skip = 1,
  header = F,
  stringsAsFactors = F,
  fileEncoding = 'latin1'
)

# https://www.r-bloggers.com/2021/06/reading-data-from-excel-files-xlsxlsxcsv-into-r-quick-guide/

