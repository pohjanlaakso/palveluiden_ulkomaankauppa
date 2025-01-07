# Ulkomaankauppa Excelin tarkistus Tilastokeskuksen sivuilta

# Mise en place
rm(list =ls())
setwd(getwd())
library(zoo)

# DATA3_HINNAT VÄLILEHTI

# data tulee täältä: https://pxdata.stat.fi/PxWeb/pxweb/fi/StatFin/StatFin__ntp/statfin_ntp_pxt_132h.px/
data_url <- 'https://pxdata.stat.fi:443/PxWeb/sq/d8f8a397-bf24-405f-88dc-48c32306212a'
#download.file(data_url, paste('C:/Users/03149822/Desktop/palveluiden_ulkomaankauppa/tiedostohaut/', 'TK_sanity.csv', sep=''), mode = 'wb')

# TK:lta edellinen datan tarkistus Elokuussa, tässä 02.09.2024 haettu data
TK_data <- read.csv('tiedostohaut/TK_sanity.csv', skip = 1, header = F, stringsAsFactors = F, fileEncoding = 'latin1')

# Tilastokeskukselta 18.09.2024 massiivinen tilastopäivitys, jolla BKT:n alaerät päivittyneet vuosilta 2010-2024
download.file(data_url, paste('C:/Users/03149822/Desktop/palveluiden_ulkomaankauppa/tiedostohaut/',
                              'paivitetty_data.csv', sep = ''), mode = 'wb') 

TK_data_uusi <- read.csv('tiedostohaut/paivitetty_data.csv', skip = 1, header = F, stringsAsFactors = F, fileEncoding = 'latin1')

# change column names and order
colnames(TK_data) <- TK_data[1,]
TK_data <- TK_data[-1,]
TK_data <- TK_data[, c(1, 13, 14, 15, 10, 11, 12, 6, 7, 8, 3, 4, 5, 9, 2)] # use the same order as in Excel

# samat temput päivitetylle datalle
colnames(TK_data_uusi) <- TK_data_uusi[1,]
TK_data_uusi <- TK_data_uusi[-1,]
TK_data_uusi <- TK_data_uusi[, c(1, 13, 14, 15, 10, 11, 12, 6, 7, 8, 3, 4, 5, 9, 2)]

# sarjat erikseen
#kau_tk_kh <- 'https://pxdata.stat.fi:443/PxWeb/sq/6a138e56-d495-4255-9847-37efb095fa68' # (KAU)sitasoitettu (T)yöpäivä(K)orjattu sarja (K)äypiin (H)intoihin
#kau_tk_2015 <- 'https://pxdata.stat.fi:443/PxWeb/sq/cc2b1c46-8804-42f2-b8e7-6a40a9918fad' # (KAU)sitasoitettu (T)yöpäivä(K)orjattu sarja viitevuosi 2015


# DATA1_MÄÄRÄ VÄLILEHTI

# data tulee samasta lähteestä
data_url2 <- 'https://pxdata.stat.fi:443/PxWeb/sq/d91f9636-b31f-419a-a805-eb9c6d7d96d0'
#download.file(data_url2, paste('C:/Users/03149822/Desktop/palveluiden_ulkomaankauppa/tiedostohaut/', 'TK_sanity2.csv', sep=''), moder = 'wb')
TK_data2 <- read.csv('tiedostohaut/TK_sanity2.csv', skip = 1, header = F, stringsAsFactors = F, fileEncoding = 'latin1')

download.file(data_url2, paste('C:/Users/03149822/Desktop/palveluiden_ulkomaankauppa/tiedostohaut/', 
                               'paivitetty_data2.csv', sep = ''), mode = 'wb')

TK_data_uusi2 <- read.csv('tiedostohaut/paivitetty_data2.csv', skip = 1, header = F, 
                          stringsAsFactors = F, fileEncoding = 'latin1')

# change column names and order
colnames(TK_data2) <- TK_data2[1,]
TK_data2 <- TK_data2[-1,]
TK_data2 <- TK_data2[, c(1, 11, 10, 13, 12, 7, 6, 9, 8, 4, 3, 2, 5)] # same order as in Excel 

colnames(TK_data_uusi2) <- TK_data_uusi2[1,]
TK_data_uusi2 <- TK_data_uusi2[-1,]
TK_data_uusi2 <- TK_data_uusi2[, c(1, 11, 10, 13, 12, 7, 6, 9, 8, 4, 3, 2, 5)]

# DATA2_MÄÄRÄ VÄLILEHTI ... samat tiedot ja järjestys kuin TK_data


#### TK varmistukset ####

TK_data_numeric <- as.data.frame(lapply(TK_data, as.numeric))
TK_data2_numeric <- as.data.frame(lapply(TK_data2, as.numeric))

TK_data_uusi_numeric <- as.data.frame(lapply(TK_data_uusi, as.numeric))
TK_data_uusi2_numeric <- as.data.frame(lapply(TK_data_uusi2, as.numeric))

erotus <- (TK_data_uusi_numeric - TK_data_numeric)
erotus_pc <- erotus/TK_data_numeric

erotus2 <- (TK_data_uusi2_numeric - TK_data2_numeric)
erotus2_pc <- erotus2/TK_data2_numeric


####

erotus$Vuosineljännes <- TK_data$Vuosineljännes
erotus_pc$Vuosineljännes <- TK_data$Vuosineljännes

erotus2$Vuosineljännes <- TK_data$Vuosineljännes
erotus2_pc$Vuosineljännes <- TK_data$Vuosineljännes



plot.ts(erotus[,2], main = colnames(erotus)[2], cex.main = 0.7, xaxt='n')
erotus$Vuosineljännes <- as.yearqtr(erotus$Vuosineljännes)
axis(1, at= seq(round(min(erotus$Vuosineljännes)), round(max(erotus$Vuosineljännes)), by = 1),
     labels=seq(round(min(erotus$Vuosineljännes)), round(max(erotus$Vuosineljännes))))

colname(erotus[,2])
substr(colnames(erotus)[2], 81, 120)

main=paste(rep(colnames(erotus)[2], 2), sep='\n')

# Tilasto1 miljoonissa
for (i in 2:7) {
  plot.ts(erotus[81:138, i], main = colnames(erotus)[i], cex.main = 0.7, ylim = c(-650,650), 
          ylab = 'Kvartaalimuutos (MEUR)', xlab = 'Kvartaalidata 2010Q1-2024Q2')
  lines(erotus[81:138, i+6], col = 2)
  grid(nx=NA, ny=NULL, lty=1, col ='gray', lwd = 1)
  legend(1, -410, legend=c('V. 2015 hinnoin', 'Käypiin hintoihin'), col = 1:2, lty = 1)
}

# Tilasto 1 prosentteina
for (i in 2:7) {
  plot.ts(erotus_pc[81:138,i], main = colnames(erotus_pc)[i], cex.main = 0.7, ylim = c(-0.065, 0.065),
          ylab = 'Kvartaalimuutos (0.01 = 1%)', xlab = 'Kvartaalidata 2010Q1-2024Q2')
  lines(erotus_pc[81:138, i+6], col = 2)
  grid(nx=NA, ny=NULL, lty=1, col ='gray', lwd = 1)
  legend(1, -410, legend=c('V. 2015 hinnoin', 'Käypiin hintoihin'), col = 1:2, lty = 1)
}

for (i in 2:ncol(erotus2)) {
  plot.ts(erotus2[81:138, i], main = colnames(erotus2)[i], cex.main = 0.7, ylim = c(-650, 650),
          ylab = 'Kvartaalimuutos (MEUR)', xlab = 'Kvartaalidata 2010Q1-2024Q2')
  grid(nx=NA, ny=NULL, lty = 1, col = 'gray', lwd = 1)
}

#tarkista, että sama data
plot.ts(erotus$Kausitasoitettu.ja.työpäiväkorjattu.sarja..viitevuosi.2015..miljoonaa.euroa.P6K.Tavaroiden.ja.palvelujen.vienti..menona)
lines(erotus2$P6K.Tavaroiden.ja.palvelujen.vienti..menona.Kausitasoitettu.ja.työpäiväkorjattu.sarja..viitevuosi.2015..miljoonaa.euroa, col = 2)

plot.ts(erotus2$P7R.Tavaroiden.ja.palvelujen.tuonti..tulona.Kausitasoitettu.ja.työpäiväkorjattu.sarja..viitevuosi.2015..miljoonaa.euroa)
lines(erotus$Kausitasoitettu.ja.työpäiväkorjattu.sarja..viitevuosi.2015..miljoonaa.euroa.P7R.Tavaroiden.ja.palvelujen.tuonti..tulona, col = 2)

plot.ts(erotus$Kausitasoitettu.ja.työpäiväkorjattu.sarja.käypiin.hintoihin..miljoonaa.euroa.P6K.Tavaroiden.ja.palvelujen.vienti..menona)
lines(erotus2$P6K.Tavaroiden.ja.palvelujen.vienti..menona.Kausitasoitettu.ja.työpäiväkorjattu.sarja.käypiin.hintoihin..miljoonaa.euroa, col =2)

plot.ts(erotus$Kausitasoitettu.ja.työpäiväkorjattu.sarja.käypiin.hintoihin..miljoonaa.euroa.P7R.Tavaroiden.ja.palvelujen.tuonti..tulona)
lines(erotus2$P7R.Tavaroiden.ja.palvelujen.tuonti..tulona.Kausitasoitettu.ja.työpäiväkorjattu.sarja.käypiin.hintoihin..miljoonaa.euroa, col = 2)

# lisää tarkistuksia 1
plot.ts(erotus2$P6K.Tavaroiden.ja.palvelujen.vienti..menona.Alkuperäinen.sarja.käypiin.hintoihin..miljoonaa.euroa[81:138],
        ylim = c(-650, 650), ylab = 'Kvartaalimuutos (MEUR)', xlab = 'Kvartaalidata 2010Q1-2024Q2',
        main = 'Tavaroiden ja palvelujen vienti')
grid(nx=NA, ny=NULL, lty = 1, col = 'gray', lwd = 1)
lines(erotus2$P6K.Tavaroiden.ja.palvelujen.vienti..menona.Alkuperäinen.sarja..viitevuosi.2015..miljoonaa.euroa[81:138], col = 2)
legend(1, -410, legend=c('Alkuperäinen sarja käypiin hintoihin', 'Alkuperäinen sarja v. 2015 hinnoin'), col = 1:2, lty = 1)

# lisää tarkistuksia 2 
plot.ts(erotus2$P7R.Tavaroiden.ja.palvelujen.tuonti..tulona.Alkuperäinen.sarja.käypiin.hintoihin..miljoonaa.euroa[81:138],
        ylim = c(-650, 650), ylab = 'Kvartaalimuutos (MEUR)', xlab = 'Kvartaalidata 2010Q1-2024Q2',
        main = 'Tavaroiden ja palvelujen tuonti')
grid(nx=NA, ny=NULL, lty = 1, col = 'gray', lwd = 1)
lines(erotus2$P7R.Tavaroiden.ja.palvelujen.tuonti..tulona.Alkuperäinen.sarja..viitevuosi.2015..miljoonaa.euroa[81:138], col = 2)
legend(1, -410, legend=c('Alkuperäinen sarja käypiin hintoihin', 'Alkuperäinen sarja v. 2015 hinnoin'), col = 1:2, lty = 1)

# tärkein data 
plot.ts(erotus_pc$Kausitasoitettu.ja.työpäiväkorjattu.sarja..viitevuosi.2015..miljoonaa.euroa.P62K.Palvelujen.vienti..menona[81:138], 
        ylim = c(-0.065, 0.065), xlab = 'Kvartaalidata 2010Q1-2024Q2', ylab = 'Muutos prosentteina (0.01 = 1%)',
        main = 'Palveluiden vienti (Kausitasoitettu ja työpäiväkorjattu sarja)')
lines(erotus_pc$Kausitasoitettu.ja.työpäiväkorjattu.sarja.käypiin.hintoihin..miljoonaa.euroa.P62K.Palvelujen.vienti..menona[81:138], col = 2)
legend(1, -0.041, legend=c('Vuoden 2015 hinnoin', 'Käypiin hintoihin'), col = 1:2, lty = 1)
grid(nx=NA, ny=NULL, lty=1, col ='gray', lwd = 1)

# tasomuutos
plot.ts(erotus$Kausitasoitettu.ja.työpäiväkorjattu.sarja..viitevuosi.2015..miljoonaa.euroa.P62K.Palvelujen.vienti..menona[81:138], 
        ylim = c(-650, 650), xlab = 'Kvartaalidata 2010Q1-2024Q2', ylab = 'Muutos MEUR',
        main = 'Palveluiden vienti (Kausitasoitettu ja työpäiväkorjattu sarja)')
lines(erotus$Kausitasoitettu.ja.työpäiväkorjattu.sarja.käypiin.hintoihin..miljoonaa.euroa.P62K.Palvelujen.vienti..menona[81:138], col = 2)
legend(1, -410, legend=c('Vuoden 2015 hinnoin', 'Käypiin hintoihin'), col = 1:2, lty = 1)
grid(nx=NA, ny=NULL, lty=1, col ='gray', lwd = 1)

# vaihtotase
plot.ts(erotus$Kausitasoitettu.ja.työpäiväkorjattu.sarja..viitevuosi.2015..miljoonaa.euroa.P6K.Tavaroiden.ja.palvelujen.vienti..menona)
lines(erotus$Kausitasoitettu.ja.työpäiväkorjattu.sarja.käypiin.hintoihin..miljoonaa.euroa.P6K.Tavaroiden.ja.palvelujen.vienti..menona, col = 2)


tase_muutos = erotus$Kausitasoitettu.ja.työpäiväkorjattu.sarja..viitevuosi.2015..miljoonaa.euroa.P6K.Tavaroiden.ja.palvelujen.vienti..menona[81:138] - erotus$Kausitasoitettu.ja.työpäiväkorjattu.sarja..viitevuosi.2015..miljoonaa.euroa.P7R.Tavaroiden.ja.palvelujen.tuonti..tulona[81:138]
plot.ts(tase_muutos, main = 'Kauppataseen revisio', ylab = 'Muutos MEUR', xlab = 'Kvartaalidata 2010Q1-2024Q2')
grid(nx=NA, ny=NULL, lty=1, col ='gray', lwd = 1)

tase_muutos = (erotus$Kausitasoitettu.ja.työpäiväkorjattu.sarja..viitevuosi.2015..miljoonaa.euroa.P6K.Tavaroiden.ja.palvelujen.vienti..menona[81:138] - erotus$Kausitasoitettu.ja.työpäiväkorjattu.sarja..viitevuosi.2015..miljoonaa.euroa.P7R.Tavaroiden.ja.palvelujen.tuonti..tulona[81:138])


# Tilasto2 miljoonissa
for (i in 2:ncol(erotus2)) {
  plot.ts(erotus2[81:138,i], main = colnames(erotus2)[i], cex.main = 0.7, ylim = c(-650, 650),
          ylab ='MEUR', xlab = 'Kvartaalidata 2010Q1-2024Q2')
  grid(nx=NA, ny=NULL, lty=1, col ='gray', lwd = 1)
}

# Tilasto2 prosentteina
for (i in 2:ncol(erotus2_pc)) {
  plot.ts(erotus2_pc[81:138,i], main = colnames(erotus2_pc)[i], cex.main = 0.7, ylim =c(-0.06, 0.06))
  grid(nx=NA, ny=NULL, lty=1, col ='gray', lwd = 1)
}

plot.ts(erotus_pc$Kausitasoitettu.ja.työpäiväkorjattu.sarja..viitevuosi.2015..miljoonaa.euroa.P6K.Tavaroiden.ja.palvelujen.vienti..menona, ylim = c(-0.02, 0.02))
lines(erotus2_pc$P6K.Tavaroiden.ja.palvelujen.vienti..menona.Kausitasoitettu.ja.työpäiväkorjattu.sarja..viitevuosi.2015..miljoonaa.euroa, col = 2)

# oraakkeli check
vienti2023 <- sum(TK_data2_numeric$P6K.Tavaroiden.ja.palvelujen.vienti..menona.Kausitasoitettu.ja.työpäiväkorjattu.sarja..viitevuosi.2015..miljoonaa.euroa[133:136])
vienti2022 <- sum(TK_data2_numeric$P6K.Tavaroiden.ja.palvelujen.vienti..menona.Kausitasoitettu.ja.työpäiväkorjattu.sarja..viitevuosi.2015..miljoonaa.euroa[129:132])

TK_data2_numeric$P6K.Tavaroiden.ja.palvelujen.vienti..menona.Kausitasoitettu.ja.työpäiväkorjattu.sarja..viitevuosi.2015..miljoonaa.euroa

(vienti2023/vienti2022)*100-100

tuonti2023 <- sum(TK_data2_numeric$P7R.Tavaroiden.ja.palvelujen.tuonti..tulona.Alkuperäinen.sarja..viitevuosi.2015..miljoonaa.euroa[133:136])
tuonti2022 <- sum(TK_data2_numeric$P7R.Tavaroiden.ja.palvelujen.tuonti..tulona.Alkuperäinen.sarja..viitevuosi.2015..miljoonaa.euroa[129:132])

(tuonti2023/tuonti2022)*100-100

vienti2023_uusi <- sum(TK_data_uusi2_numeric$P6K.Tavaroiden.ja.palvelujen.vienti..menona.Alkuperäinen.sarja..viitevuosi.2015..miljoonaa.euroa[133:136])
vienti2022_uusi <- sum(TK_data_uusi2_numeric$P6K.Tavaroiden.ja.palvelujen.vienti..menona.Alkuperäinen.sarja..viitevuosi.2015..miljoonaa.euroa[129:132])

(vienti2023_uusi/vienti2022_uusi)*100-100

tuonti2023_uusi <- sum(TK_data_uusi2_numeric$P7R.Tavaroiden.ja.palvelujen.tuonti..tulona.Alkuperäinen.sarja..viitevuosi.2015..miljoonaa.euroa[133:136])
tuonti2022_uusi <- sum(TK_data_uusi2_numeric$P7R.Tavaroiden.ja.palvelujen.tuonti..tulona.Alkuperäinen.sarja..viitevuosi.2015..miljoonaa.euroa[129:132])

(tuonti2023_uusi/tuonti2022_uusi)*100-100

vienti2024Q1 <- TK_data2_numeric$P6K.Tavaroiden.ja.palvelujen.vienti..menona.Kausitasoitettu.ja.työpäiväkorjattu.sarja..viitevuosi.2015..miljoonaa.euroa[137]
vienti2023Q4 <- TK_data2_numeric$P6K.Tavaroiden.ja.palvelujen.vienti..menona.Kausitasoitettu.ja.työpäiväkorjattu.sarja..viitevuosi.2015..miljoonaa.euroa[136]

(vienti2024Q1/vienti2023Q4)*100-100

tuonti2024Q1 <- TK_data2_numeric$P7R.Tavaroiden.ja.palvelujen.tuonti..tulona.Alkuperäinen.sarja..viitevuosi.2015..miljoonaa.euroa[137]
tuonti2023Q4 <- TK_data2_numeric$P7R.Tavaroiden.ja.palvelujen.tuonti..tulona.Alkuperäinen.sarja..viitevuosi.2015..miljoonaa.euroa[136]

(tuonti2024Q1/tuonti2023Q4)*100-100

vienti2024Q1_uusi <- TK_data_uusi2_numeric$P6K.Tavaroiden.ja.palvelujen.vienti..menona.Alkuperäinen.sarja..viitevuosi.2015..miljoonaa.euroa[137]
vienti2023Q4_uusi <- TK_data_uusi2_numeric$P6K.Tavaroiden.ja.palvelujen.vienti..menona.Alkuperäinen.sarja..viitevuosi.2015..miljoonaa.euroa[136]

(vienti2024Q1_uusi/vienti2023Q4_uusi)*100-100

