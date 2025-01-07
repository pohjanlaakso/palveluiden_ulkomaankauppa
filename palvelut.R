# TODO: perehdy dokumentointiin mitä erät pitävät sisällään 

# Mise en place
rm(list =ls())
setwd(getwd())
#install.packages('zoo')
library(zoo)

# get import data: https://pxdata.stat.fi/PxWeb/pxweb/fi/StatFin/StatFin__tpulk/statfin_tpulk_pxt_12gq.px/
palvelut_vienti <- 'https://pxdata.stat.fi:443/PxWeb/sq/dea11a26-5c15-4e4f-9d02-5b8e46533f99'
download.file(palvelut_vienti, paste('C:/Users/03149822/Desktop/palveluiden_ulkomaankauppa/tiedostohaut/', 'palvelut_vienti.csv', sep=''), mode ='wb')

# get import data
palvelut_tuonti <- 'https://pxdata.stat.fi:443/PxWeb/sq/808bc4b1-9ed6-4fa3-99dd-d5195ff06d43'
download.file(palvelut_tuonti, paste('C:/Users/03149822/Desktop/palveluiden_ulkomaankauppa/tiedostohaut/', 'palvelut_tuonti.csv', sep=''), mode = 'wb')

# some problems with data extraction: https://stackoverflow.com/questions/14363085/invalid-multibyte-string-in-read-csv
palveluvienti_df <- read.csv('tiedostohaut/palvelut_vienti.csv', skip = 1, header=FALSE, stringsAsFactors=FALSE, fileEncoding='latin1')
colnames(palveluvienti_df) <- palveluvienti_df[1,]
palveluvienti_df <- palveluvienti_df[-1,]

# same for import data
palvelutuonti_df <- read.csv('tiedostohaut/palvelut_tuonti.csv', skip = 1, header = F, stringsAsFactors = F, fileEncoding = 'latin1')
colnames(palvelutuonti_df) <- palvelutuonti_df[1,]
palvelutuonti_df <- palvelutuonti_df[-1,]

# quick visual checks
plot.ts(palveluvienti_df$`Ulkomaat yhteensä`, col = 'blue', ylim=c(4000, 12000),
        xlab = 'Aika', ylab = 'MEUR', main = 'Palveluiden vienti ja tuonti vuosineljänneksittäin 2013Q1-2024Q2') # quick visual check
lines(palvelutuonti_df$`Ulkomaat yhteensä`, col = 'red')
legend(1, 12000, legend=c('Palvelutuonti', 'Palveluvienti'), col = c('red', 'blue'), lty = 1)


# get more granular export data
palveluvienti_eroteltu <- 'https://pxdata.stat.fi:443/PxWeb/sq/c8736713-110e-4097-973a-8b392fba0328'
download.file(palveluvienti_eroteltu, paste('C:/Users/03149822/Desktop/palveluiden_ulkomaankauppa/tiedostohaut/', 'palveluvienti_eroteltu.csv', sep = ''), mode = 'wb')

# get more granular import data
palvelutuonti_eroteltu <- 'https://pxdata.stat.fi:443/PxWeb/sq/1a7a8c7f-c5e6-4221-b3a6-18da23d79125'
download.file(palvelutuonti_eroteltu, paste('C:/Users/03149822/Desktop/palveluiden_ulkomaankauppa/tiedostohaut/', 'palvelutuonti_eroteltu.csv', sep = ''), mode = 'wb')

# skip rows and file encoding for import data ...
eroteltu_df <- read.csv('tiedostohaut/palveluvienti_eroteltu.csv', skip = 1, header = F, stringsAsFactors = F, fileEncoding = 'latin1')
colnames(eroteltu_df) <- eroteltu_df[1,] # first row as row names: https://stackoverflow.com/questions/32054368/use-first-row-data-as-column-names-in-r
eroteltu_df <- eroteltu_df[-1,]

# ... and same for export data
eroteltu_import_df <- read.csv('tiedostohaut/palvelutuonti_eroteltu.csv', skip = 1, header = F, stringsAsFactors = F, fileEncoding = 'latin1')
colnames(eroteltu_import_df) <- eroteltu_import_df[1,]
eroteltu_import_df <- eroteltu_import_df[-1,]

# visual checking exports
plot.ts(eroteltu_df$`SI Televiestintä-, tietotekniikka- ja tietopalvelut`, ylim=c(600,4500), col = 'red',
        xlab = 'Aika', ylab= 'MEUR', main = 'Palveluvienti 2013Q1-2024Q2')
lines(eroteltu_df$`SJ Muut liike-elämän palvelut`, col = 'blue')
lines(eroteltu_df$`SC Kuljetus`, col ='green')
lines(eroteltu_df$`SJ3 Tekniset, kaupankäyntiin liittyvät ja muut liike-elämän palvelut`, col='purple')
lines(eroteltu_df$`SH Henkisen omaisuuden käytöstä perityt maksut, muualle luokittelemattomat`, col ='cyan')
legend(1, 4500, legend = c('Televiestintä ym. palvelut', 
                           'Muut liike-elämän palvelut', 
                           'Kuljetus', 'Tekniset ym. liike-elämän palvelut', 
                           'Henkisen pääoman käyttö & luokittelemattomat'), 
       col = c('red', 'blue', 'green', 'purple', 'cyan'), lty = 1)

# checking imports
plot.ts(eroteltu_import_df$`SJ Muut liike-elämän palvelut`, ylim=c(200, 4900), col = 'red',
        xlab = 'Aika', ylab = 'MEUR', main = 'Palvelutuonti 2013Q1-2024Q2')
lines(eroteltu_import_df$`SJ3 Tekniset, kaupankäyntiin liittyvät ja muut liike-elämän palvelut`, col = 'blue')
lines(eroteltu_import_df$`SC Kuljetus`, col = 'green')
lines(eroteltu_import_df$`SD Matkailu`, col = 'purple')
lines(eroteltu_import_df$`SI Televiestintä-, tietotekniikka- ja tietopalvelut`, col = 'cyan')
legend(1, 4500, legend = c('Muut liike-elämän palvelut', 
                           'Teknikset ja muut liike-elämän palvelut',
                           'Kuljetus',
                           'Matkailu',
                           'Televiestintä ym. palvelut'), 
       col = c('red', 'blue', 'green', 'purple', 'cyan'), lty = 1)



# data comes from Tilastokeskus as 'characters'. Better to change these to numeric
# https://www.tutorialspoint.com/how-to-convert-more-than-one-column-in-r-data-frame-to-from-integer-to-numeric-in-a-single-line-code
eroteltu_df_test <- as.data.frame(lapply(eroteltu_df[, 4:19], as.numeric))
sapply(eroteltu_df_test, class)

# ... and same for import data
import_df_test <- as.data.frame(lapply(eroteltu_import_df[, 4:19], as.numeric))
sapply(eroteltu_df_test, class)

# more visual checking
plot.ts(diff(eroteltu_df_test$SI.Televiestintä...tietotekniikka..ja.tietopalvelut), col = 'red')
lines(diff(eroteltu_df_test$SJ.Muut.liike.elämän.palvelut), col = 'blue')
lines(diff(eroteltu_df_test$SC.Kuljetus), col = 'green')
lines(diff(eroteltu_df_test$SJ3.Tekniset..kaupankäyntiin.liittyvät.ja.muut.liike.elämän.palvelut), col = 'purple')
lines(diff(eroteltu_df_test$SH.Henkisen.omaisuuden.käytöstä.perityt.maksut..muualle.luokittelemattomat), col = 'cyan')

plot.ts(diff(import_df_test$SJ.Muut.liike.elämän.palvelut), col = 'red')
lines(diff(import_df_test$SJ3.Tekniset..kaupankäyntiin.liittyvät.ja.muut.liike.elämän.palvelut), col = 'blue')
lines(diff(import_df_test$SC.Kuljetus), col = 'green')
lines(diff(import_df_test$SD.Matkailu), col = 'purple')
lines(diff(import_df_test$SI.Televiestintä...tietotekniikka..ja.tietopalvelut), col = 'cyan')

# even more visual checks
plot.ts(eroteltu_df_test$S.Palvelut - import_df_test$S.Palvelut, xlab = 'Aika', ylab = 'MEUR', 
        main = 'Palvelutase 2013Q1-2024Q2')
abline(h=0, col = 'red', lty = 5)
abline(h=mean(eroteltu_df_test$S.Palvelut - import_df_test$S.Palvelut), col = 'blue', lty = 5)
abline(h=median(eroteltu_df_test$S.Palvelut - import_df_test$S.Palvelut), col = 'purple', lty = 5)
abline(h=1.96*median(eroteltu_df_test$S.Palvelut - import_df_test$S.Palvelut), col = 'purple', lty = 5)
legend(1, 500, legend = c('Mediaani', '95% luottamusväli'), col = c('purple', 'purple'), lty = 5)
text(37, -1173, '2021Q4')

# kokeile luoda diffi datataulu

testi <- eroteltu_df_test - import_df_test
plot.ts(testi$S.Palvelut)
sapply(testi, class)

testi_div <- testi[,2:16] / -testi[,1]

plot.ts(testi$SD.Matkailu, col = 2, ylim=c(-2700, 0), xlab = 'Aika', ylab = 'MEUR', main ='Palvelutase 2013Q1-2024Q2')
abline(h=0, col = 'blue', lty = 5)
lines(testi$SJ.Muut.liike.elämän.palvelut, col = 3)
lines(testi$SJ3.Tekniset..kaupankäyntiin.liittyvät.ja.muut.liike.elämän.palvelut, col = 4)
lines(testi$SJ1.Tutkimus..ja.kehityspalvelut, col = 5)
lines(testi$SC.Kuljetus, col = 6)
legend(1, -1800, legend = c('Matkailu', 
                            'Muut liike-elämän palvelut', 
                            'Tekniset ym. palvelut', 
                            'TK-palvelut',
                            'Kuljetus'), col = 2:6, lty = 1)
#
plot.ts(testi$S.Palvelut, ylim=c(-3000, 3000), xlab = 'Aika', ylab = 'MEUR',
        main = 'Televiestintäpalveluidentase 2013Q1-2024Q2')
lines(testi$SI.Televiestintä...tietotekniikka..ja.tietopalvelut, col = 2)
abline(h=0, col ='blue', lty = 5)
legend(1, -2000, legend=c('Palvelutase', 'Televiestinä ym. palvelut'), col =1:2, lty =1)

# use axis
labels <- as.yearqtr(eroteltu_df$Vuosineljännes)
labels

barplot(testi_div) # https://r-charts.com/part-whole/stacked-bar-graph/

# malli

summary(lm(diff(testi$S.Palvelut) 
           ~ diff(testi$SI.Televiestintä...tietotekniikka..ja.tietopalvelut)
           + diff(testi$SJ.Muut.liike.elämän.palvelut)
           + diff(testi$SD.Matkailu)
))

summary(lm(diff(testi$S.Palvelut) 
           ~ diff(testi$SI.Televiestintä...tietotekniikka..ja.tietopalvelut)
           + diff(testi$SJ.Muut.liike.elämän.palvelut)
           + diff(testi$SD.Matkailu)
           + diff(testi$SJ1.Tutkimus..ja.kehityspalvelut)
           + diff(testi$SJ3.Tekniset..kaupankäyntiin.liittyvät.ja.muut.liike.elämän.palvelut)
           + diff(testi$SJ2.Asiantuntijoiden.ja.liikehallinnon.konsulttipalvelut)
           ))
