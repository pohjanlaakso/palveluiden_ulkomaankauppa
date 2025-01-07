
# https://pxdata.stat.fi/PxWeb/pxweb/fi/StatFin/StatFin__tpulk/statfin_tpulk_pxt_12gr.px/

# mise en place
rm(list =ls())
setwd(getwd())
library(zoo)

#### USA DATA ####

# linkit ja datalähteet
usa_vienti <- 'https://pxdata.stat.fi:443/PxWeb/sq/c9c92fd6-af04-40f5-9419-7ef62a242ea0'
usa_tuonti <- 'https://pxdata.stat.fi:443/PxWeb/sq/8ad65a76-2fc6-44aa-9e13-89f16941ec3b'

# lataa USA vientidata
download.file(usa_vienti, paste('C:/Users/03149822/Desktop/palveluiden_ulkomaankauppa/tiedostohaut/',
                                'usa_vienti.csv', sep = ''), mode = 'wb')
usa_vienti <- read.csv('tiedostohaut/usa_vienti.csv', skip = 1, header = F,
                       stringsAsFactors = F, fileEncoding = 'latin1')

# lataa USA tuontidata
download.file(usa_tuonti, paste('C:/Users/03149822/Desktop/palveluiden_ulkomaankauppa/tiedostohaut/',
                                'usa_tuonti.csv', sep = ''), mode = 'wb')
usa_tuonti <- read.csv('tiedostohaut/usa_tuonti.csv', skip = 1, header = F,
                       stringsAsFactors = F, fileEncoding = 'latin1')

# remove columns and rows
colnames(usa_vienti) <- usa_vienti[1,]
colnames(usa_tuonti) <- usa_tuonti[1,]

usa_vienti <- usa_vienti[-1,-2]
usa_tuonti <- usa_tuonti[-1,]

# change datatype
usa_vienti_numeric <- as.data.frame(lapply(usa_vienti[,2:63], as.numeric))
usa_tuonti_numeric <- as.data.frame(lapply(usa_tuonti[,2:63], as.numeric))

# calculate balance
usa_tase <- usa_vienti_numeric[,2:62] - usa_tuonti_numeric[,2:62]

# plot: https://www.tutorialspoint.com/how-to-create-barplot-from-data-frame-in-r-using-rows-as-categories
barplot((as.matrix(usa_tase)), beside=T)

barplot(usa_tase$SI.Televiestintä...tietotekniikka..ja.tietopalvelut, beside =T, 
        names.arg = usa_tuonti$Vuosi, ylim = c(0, 5000), xlab = 'Vuosi', ylab = 'MEUR',
        main = 'Televiestintä-, tietotekniikka- ja tietopalveluiden tase: Yhdysvallat')

####


#### Irlanti data ####

irlanti_vienti <- 'https://pxdata.stat.fi:443/PxWeb/sq/63bd3fe7-357a-4d6f-8c1e-9c321f73d54b'
irlanti_tuonti <- 'https://pxdata.stat.fi:443/PxWeb/sq/1bf24e08-cea8-4bac-9e9b-af72a963dac9'


####

#### Hollanti data ####

hollanti_vienti <- 'https://pxdata.stat.fi:443/PxWeb/sq/ece3f4d0-b752-47fc-8dbc-be7f2879d731'
hollanti_tuonti <- 'https://pxdata.stat.fi:443/PxWeb/sq/6be858c8-8df9-4f95-8a3d-b15b602d1b96'

####

#### TANSKA DATA ####

tanska_vienti <- 'https://pxdata.stat.fi:443/PxWeb/sq/0099674f-4681-4d7d-a238-73518a025fc4'
tanska_tuonti <- 'https://pxdata.stat.fi:443/PxWeb/sq/d06a65bd-9db2-4fca-bf13-fc24f106c227'

####


#### RUOTSI DATA ####

ruotsi_vienti <- 'https://pxdata.stat.fi:443/PxWeb/sq/25ad59c9-bd1b-44f2-9d3b-4f19b8d7cc1d'
ruotsi_tuonti <- 'https://pxdata.stat.fi:443/PxWeb/sq/c8891968-9646-45eb-b239-2336134a7dcc'

####





