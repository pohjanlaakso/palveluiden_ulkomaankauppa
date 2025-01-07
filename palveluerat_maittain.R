
# palvelut maittain ja palveluerittäin: https://pxdata.stat.fi/PxWeb/pxweb/fi/StatFin/StatFin__tpulk/statfin_tpulk_pxt_12gr.px/

# Mise en place
rm(list =ls())
setwd(getwd())
library(zoo)

# get data
web <- 'https://pxdata.stat.fi:443/PxWeb/sq/d3e0604c-c0df-4717-8891-ed5e8c570bdd'
download.file(web, paste('C:/Users/03149822/Desktop/palveluiden_ulkomaankauppa/tiedostohaut/',
                         'palveluerat_maittain.csv', sep = ''), mode = 'wb')

palveluerat_df <- read.csv('tiedostohaut/palveluerat_maittain.csv', skip = 1, header = F, 
                           stringsAsFactors = F, fileEncoding = 'latin1')

# hae tuontidata
tuonti_url <- 'https://pxdata.stat.fi:443/PxWeb/sq/73911018-8507-42ee-bde4-e0b128ef0e59'
download.file(tuonti_url, paste('C:/Users/03149822/Desktop/palveluiden_ulkomaankauppa/tiedostohaut/',
                                'palveluerat_tuontiMaittain.csv', sep = ''), mode = 'wb')

tuonti <- read.csv('tiedostohaut/palveluerat_tuontiMaittain.csv', skip = 1, header = F,
                   stringsAsFactors = F, fileEncoding = 'latin1')

# rename columns
colnames(palveluerat_df) <- palveluerat_df[1,]
palveluerat_df <- palveluerat_df[-1,]

rownames(palveluerat_df) <- palveluerat_df[, 1]
#palveluerat_df <- palveluerat_df[,-1]

# rename import column
colnames(tuonti) <- tuonti[1,]
tuonti <- tuonti[-1,]

rownames(tuonti) <- tuonti[, 1]
#tuonti <- tuonti[, -1]

# as numeric
vienti_numeric <- as.data.frame(lapply(palveluerat_df[,1:13], as.numeric))
sapply(vienti_numeric, class)

tuonti_numeric <- as.data.frame(lapply(tuonti[,1:13], as.numeric))
sapply(tuonti_numeric, class)

rownames(tuonti_numeric) <- tuonti[, 1]
tuonti_numeric <- tuonti_numeric[,-1]

rownames(vienti_numeric) <- palveluerat_df[,1]
vienti_numeric <- vienti_numeric[,-1]

# drop non-quintessentials
sub_vienti <- vienti_numeric[c(3-1, 16-1, 22-1, 51-1, 78-1, 81-1, 84-1, 86-1, 88-1, 
                        95-1, 100-1, 109-1, 123-1, 153-1, 166-1, 169-1, 174-1, 
                        179-1, 199-1, 204-1, 232-1, 233-1),]

sub_tuonti <- tuonti_numeric[c(3-1, 16-1, 22-1, 51-1, 78-1, 81-1, 84-1, 86-1, 88-1, 
                       95-1, 100-1, 109-1, 123-1, 153-1, 166-1, 169-1, 174-1, 
                       179-1, 199-1, 204-1, 232-1, 233-1),]

# tase
sub_tase <- sub_vienti - sub_tuonti

# Hollanti
barplot(t(sub_tase[1,2:12]), beside = T, names.arg=c('SA', 'SB', 'SC', 'SD', 'SE', 'SF', 'SG', 'SH', 'SI', 'SJ', 'SK'), 
        main = 'Alankomaat v. 2023', ylim = c(-1200, 200))
legend(1, -900, legend=c('SA: Tuotannolliset palvelut', 'SC: Kuljetus'))

# Irlanti
barplot(t(sub_tase[6, 2:12]), beside = T, names.arg=c('SA', 'SB', 'SC', 'SD', 'SE', 'SF', 'SG', 'SH', 'SI', 'SJ', 'SK'),
          main = 'Irlanti v. 2023', ylim = c(-1200, 200))
legend(1, -900, legend=c('SI: Televiestintä ym. palvelut ', 'SJ: Muut liike-elämän palvelut'))

# Ruotsi
barplot(t(sub_tase[17, 2:12]), beside = T, names.arg=c('SA', 'SB', 'SC', 'SD', 'SE', 'SF', 'SG', 'SH', 'SI', 'SJ', 'SK'),
        main = 'Ruotsi v. 2023', ylim = c(-800, 200))
legend(1, -400, legend=c('SA: Tuotannolliset palvelut',
                         'SC: Kuljetus',
                         'SG: Rahoituspalvelut',
                         'SI: Televiestintä ym. palvelut ', 
                         'SJ: Muut liike-elämän palvelut',
                         'SK: Henk.koht kulttuuri & virkistys p.'
                         ))

# Saksa
barplot(t(sub_tase[18, 2:12]), beside = T, names.arg=c('SA', 'SB', 'SC', 'SD', 'SE', 'SF', 'SG', 'SH', 'SI', 'SJ', 'SK'),
        main = 'Saksa v. 2023', ylim = c(-1000, 600))
legend(1, -600, legend=c('SC: Kuljetus', 'SI: Televiestintä ym. palvelut ', 'SJ: Muut liike-elämän palvelut'))

# Tanska
barplot(t(sub_tase[20, 2:12]), beside = T, names.arg=c('SA', 'SB', 'SC', 'SD', 'SE', 'SF', 'SG', 'SH', 'SI', 'SJ', 'SK'),
        main = 'Tanska v. 2023', ylim=c(-800, 200))
legend(1, -400, legend=c('SJ: Muut liike-elämän palvelut',
                         'SC: Kuljetus',
                         'SG: Rahoituspalvelut'))

# USA
barplot(t(sub_tase[22, 2:12]), beside = T, names.arg=c('SA', 'SB', 'SC', 'SD', 'SE', 'SF', 'SG', 'SH', 'SI', 'SJ', 'SK'),
        main = 'USA v. 2023', ylim = c(-2500, 3500))
legend(1, -1000, legend=c('SI: Televiestintä ym. palvelut ', 'SJ: Muut liike-elämän palvelut'))

# UK
barplot(t(sub_tase[21, 2:12]), beside = T, names.arg=c('SA', 'SB', 'SC', 'SD', 'SE', 'SF', 'SG', 'SH', 'SI', 'SJ', 'SK'),
        main = 'UK v. 2023', ylim = c(-600, 100))
legend(1, -500, legend=c('SJ: Muut liike-elämän palvelut'))

# Intia
barplot(t(sub_tase[5, 2:12]), beside = T, names.arg=c('SA', 'SB', 'SC', 'SD', 'SE', 'SF', 'SG', 'SH', 'SI', 'SJ', 'SK'),
        main = 'Intia v. 2023', ylim=c(-1000, 600))
legend(1, -700, legend=c('SH: Henkisen pääoman käytöstä perityt maksut & ym.', 'SJ: Muut liike-elämän palvelut'))


