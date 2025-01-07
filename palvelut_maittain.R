
# mise en place
rm(list =ls())
setwd(getwd())
#install.packages('zoo')
library(zoo)

# get export data: https://pxdata.stat.fi/PxWeb/pxweb/fi/StatFin/StatFin__tpulk/statfin_tpulk_pxt_12gq.px/
export_maittain <- 'https://pxdata.stat.fi:443/PxWeb/sq/29f6bc06-f4e4-429e-8d21-e785919b7412'
download.file(export_maittain, paste('C:/Users/03149822/Desktop/palveluiden_ulkomaankauppa/tiedostohaut/', 'export_maittain.csv', sep=''), mode ='wb')

# get import data:
import_maittain <- 'https://pxdata.stat.fi:443/PxWeb/sq/35067012-9a53-4380-a2ed-b8c7f092c58b'
download.file(import_maittain, paste('C:/Users/03149822/Desktop/palveluiden_ulkomaankauppa/tiedostohaut/', 'import_maittain.csv', sep=''), mode ='wb')

# skip rows and rename columns
countryExport_df <- read.csv('tiedostohaut/export_maittain.csv', skip = 1, header = F, stringsAsFactors = F, fileEncoding = 'latin1')
colnames(countryExport_df) <- countryExport_df[1,]
countryExport_df <- countryExport_df[-1,]

# skip rows and rename columns
countryImport_df <- read.csv('tiedostohaut/import_maittain.csv', skip = 1, header = F, stringsAsFactors = F, fileEncoding = 'latin1')
colnames(countryImport_df) <- countryImport_df[1,]
countryImport_df <- countryImport_df[-1,]

# change datatype to numeric
countryExport_numeric <- as.data.frame(lapply(countryExport_df[, 3:55], as.numeric))
countryImport_numeric <- as.data.frame(lapply(countryImport_df[, 3:55], as.numeric))

# calculate balance of trade in services
trade_balance <- countryExport_numeric - countryImport_numeric

# visual checks and eyeballing

# narratiivi: USA ja Aasia dippasivat 2023Q4
plot.ts(trade_balance$S.Palvelut.Amerikka, col = 2) #Yhdysvaltojen data volatiilia
lines(trade_balance$S.Palvelut.Kiina, col = 3)
abline(h=0, col = 'blue', lty = 5)

# kaikki
plot.ts(trade_balance$S.Palvelut.Alankomaat, ylim=c(-500, 100))
abline(h=0, col = 'blue', lty = 5)
lines(trade_balance$S.Palvelut.Belgia)
lines(trade_balance$S.Palvelut.Espanja) # tasaisen negatiivinen: turismi?
lines(trade_balance$S.Palvelut.Intia) # samansuuntainen trendi ja taso hollannin kanssa
lines(trade_balance$S.Palvelut.Irlanti, col = 2)
lines(trade_balance$S.Palvelut.Yhdistynyt.kuningaskunta, col = 3)
lines(trade_balance$S.Palvelut.Italia) # tasaisen negatiivinen: turismi?
lines(trade_balance$S.Palvelut.Kanada) # ei 
lines(trade_balance$S.Palvelut.Kreikka)
lines(trade_balance$S.Palvelut.Kroatia)
lines(trade_balance$S.Palvelut.Kypros)
lines(trade_balance$S.Palvelut.Portugali)
lines(trade_balance$S.Palvelut.Puola)
lines(trade_balance$S.Palvelut.Ranska)
lines(trade_balance$S.Palvelut.Ruotsi)
lines(trade_balance$S.Palvelut.Saksa)
lines(trade_balance$S.Palvelut.Tanska, col = 4)
lines(trade_balance$S.Palvelut.T.ekki) # ei
lines(trade_balance$S.Palvelut.Turkki) # ei
lines(trade_balance$S.Palvelut.Unkari) # ei
lines(trade_balance$S.Palvelut.Viro) # tasaisen negatiivinen: tuodaan työvoimaa

# 3-5 isointa
plot.ts(trade_balance$S.Palvelut.Irlanti, col = 2, xlab = 'Aika', ylab = 'MEUR',
        main = 'Palvelutase maittain 2013Q1-2024Q2')
lines(trade_balance$S.Palvelut.Yhdistynyt.kuningaskunta, col = 3)
lines(trade_balance$S.Palvelut.Tanska, col = 4)
lines(trade_balance$S.Palvelut.Puola, col = 5)
lines(trade_balance$S.Palvelut.Ruotsi, col = 6)
lines(trade_balance$S.Palvelut.Saksa, col = 7)
lines(trade_balance$S.Palvelut.Alankomaat, col = 8)
legend(1, -400, legend = c('Irlanti', 'Tanska', 'Alankomaat'), col = c(2, 4, 8), lty = 1)

# Irlanti v. Englanti & Alankomaat v. Belgia
par(mfrow = c(1,2))
plot.ts(trade_balance$S.Palvelut.Alankomaat, ylim=c(-500, 100), xlab = 'Aika', ylab = 'MEUR')
abline(h=0, col = 'blue', lty = 5)
lines(trade_balance$S.Palvelut.Belgia, col = 2)
lines(trade_balance$S.Palvelut.Belgia*2, col = 2, lty = 3)
legend(1, -350, legend=c('Belgia', 'Belgia X2', 'Hollanti'), col = c(2, 2, 1), lty = c (1, 3, 1))

plot.ts(trade_balance$S.Palvelut.Irlanti, ylim=c(-600, 100), xlab = 'Aika', ylab = 'MEUR')
abline(h=0, col = 'blue', lty = 5)
lines(trade_balance$S.Palvelut.Yhdistynyt.kuningaskunta, col = 2)
legend(1, -400, legend=c('Irlanti', 'UK'), col = 1:2, lty = 1)

# pohjoismaat
par(mfrow=c(1,1))
plot.ts(trade_balance$S.Palvelut.Tanska, ylim =c(-500, 100))
lines(trade_balance$S.Palvelut.Ruotsi, col = 2)
lines(trade_balance$S.Palvelut.Norja, col = 3)
lines(trade_balance$S.Palvelut.Saksa, col = 4)

# regressio
regression <- lm(trade_balance$S.Palvelut.Ulkomaat.yhteensä ~., data = trade_balance[, 15:50])
summary(regression)

plot.ts(trade_balance$S.Palvelut.Ulkomaat.yhteensä)

regression2 <- lm(trade_balance[10:46, 1] ~., data = trade_balance[10:46, 15:50])
summary(regression2)

# turismi 1
plot.ts(trade_balance$S.Palvelut.Ulkomaat.yhteensä)
abline(h=0, col='blue', lty=5)
text(36, trade_balance$S.Palvelut.Ulkomaat.yhteensä[36]+100, '2021Q3')
lines(trade_balance$S.Palvelut.Espanja, col = 3)
lines(trade_balance$S.Palvelut.Italia, col = 4)
lines(trade_balance$S.Palvelut.Kreikka, col = 5)
lines(trade_balance$S.Palvelut.Kroatia, col = 6)
lines(trade_balance$S.Palvelut.Kypros, col = 7)
lines(trade_balance$S.Palvelut.Portugali, col = 8)
lines(trade_balance$S.Palvelut.Ranska, col = 9)
lines(trade_balance$S.Palvelut.Turkki, col = 10)

# turismi 2
plot.ts(trade_balance$S.Palvelut.Espanja, col = 3, ylim=c(-300, 100))
abline(h=0, col='blue', lty=5)
text(36+1, -15, '2021Q3')
lines(trade_balance$S.Palvelut.Italia, col = 4)
lines(trade_balance$S.Palvelut.Kreikka, col = 5)
lines(trade_balance$S.Palvelut.Kroatia, col = 6)
lines(trade_balance$S.Palvelut.Kypros, col = 7)
lines(trade_balance$S.Palvelut.Portugali, col = 8)
lines(trade_balance$S.Palvelut.Ranska, col = 9)
lines(trade_balance$S.Palvelut.Turkki, col = 10)

# turismi 3
plot.ts(trade_balance$S.Palvelut.Ulkomaat.yhteensä, xlab = 'Aika', ylab = 'MEUR', main = 'Turismi-proxy 2013Q1-2024Q2')
#lines(rollmean(trade_balance$S.Palvelut.Ulkomaat.yhteensä, k = 4, fill = NA), col = 2)
abline(h=0, col='blue', lty=5)
lines(trade_balance$S.Palvelut.Espanja 
      + trade_balance$S.Palvelut.Italia 
      + trade_balance$S.Palvelut.Kreikka 
      + trade_balance$S.Palvelut.Kroatia 
      + trade_balance$S.Palvelut.Kypros 
      + trade_balance$S.Palvelut.Portugali
      + trade_balance$S.Palvelut.Ranska 
      + trade_balance$S.Palvelut.Turkki, col = 2)
legend(1, -2000, legend = c('Palvelutase', 'Turismi-proxy'), col=1:2, lty =1)

# puola
plot.ts(trade_balance$S.Palvelut.Ulkomaat.yhteensä)
abline(h=0, col='blue', lty=5)
lines(trade_balance$S.Palvelut.Puola + trade_balance$S.Palvelut.Luxemburg) # Nordea ja PwC

# pohjoismaat
plot.ts(trade_balance$S.Palvelut.Ulkomaat.yhteensä)
abline(h=0, col='blue', lty=5)
lines(trade_balance$S.Palvelut.Norja, col = 2)
lines(trade_balance$S.Palvelut.Tanska, col = 3)
lines(trade_balance$S.Palvelut.Ruotsi, col = 4)

# pohjoismaat 2
plot.ts(trade_balance$S.Palvelut.Ulkomaat.yhteensä, xlab = 'Aika', ylab ='MEUR',
        main = 'Palvelutase Pohjoismaat 2013Q1-2024Q2')
abline(h=0, col='blue', lty=5)
lines(trade_balance$S.Palvelut.Norja
      + trade_balance$S.Palvelut.Tanska
      + trade_balance$S.Palvelut.Ruotsi, col = 4)
legend(1, -2000, legend = c('Palvelutase', 'Norja, Tanska ja Ruotsi'), col=c(1,4), lty=1)

# isoimmat
plot.ts(trade_balance$S.Palvelut.Ulkomaat.yhteensä)
abline(h=0, col='blue', lty=5)
lines(trade_balance$S.Palvelut.Irlanti, col = 2) # omaa luokkaansa UK for comparison
lines(trade_balance$S.Palvelut.Alankomaat, col = 3)
lines(trade_balance$S.Palvelut.Intia, col = 4)
lines(trade_balance$S.Palvelut.Saksa, col = 5)
lines(trade_balance$S.Palvelut.Viro, col = 6)
lines(trade_balance$S.Palvelut.Yhdistynyt.kuningaskunta, col = 7)

# verojärjestelyt
plot.ts(trade_balance$S.Palvelut.Ulkomaat.yhteensä, xlab = 'Aika', ylab ='MEUR',
        main = 'Palvelutase Irlanti + Hollanti 2013Q1-2024Q2')
abline(h=0, col='blue', lty=5)
lines(trade_balance$S.Palvelut.Irlanti 
      + trade_balance$S.Palvelut.Alankomaat, col = 3)
legend(1, -2000, legend = c('Palvelutase', 'Irlanti ja Hollanti'), col=c(1,3), lty=1)

# kiina
plot.ts(trade_balance$S.Palvelut.Ulkomaat.yhteensä)
abline(h=0, col='blue', lty=5)
lines(trade_balance$S.Palvelut.Kiina, col = 2)

# kaikki muut
plot.ts(trade_balance$S.Palvelut.Ulkomaat.yhteensä)
abline(h=0, col='blue', lty=5)
lines(trade_balance$S.Palvelut.Belgia
      + trade_balance$S.Palvelut.Bulgaria
      + trade_balance$S.Palvelut.Latvia
      + trade_balance$S.Palvelut.Liettua
      + trade_balance$S.Palvelut.Romania
      + trade_balance$S.Palvelut.Slovakia
      + trade_balance$S.Palvelut.Slovenia
      + trade_balance$S.Palvelut.T.ekki
      + trade_balance$S.Palvelut.Unkari)

# kolikon kääntöpuoli (obverse) nettopositiiviset vientikohteet
plot.ts(trade_balance$S.Palvelut.Ulkomaat.yhteensä, ylim=c(-3000, 2000))
abline(h=0, col='blue', lty=5)
lines(trade_balance$S.Palvelut.Japani 
      + trade_balance$S.Palvelut.Korean.tasavalta..Etelä.Korea., col = 2)
lines(trade_balance$S.Palvelut.Sveitsi, col = 3)
lines(trade_balance$S.Palvelut.Yhdysvallat..USA., col = 4) # merkittävä!
cor(diff(trade_balance$S.Palvelut.Ulkomaat.yhteensä[34:46]), diff(trade_balance$S.Palvelut.Yhdysvallat..USA.[34:46]))
cor(diff(trade_balance$S.Palvelut.Ulkomaat.yhteensä), diff(trade_balance$S.Palvelut.Yhdysvallat..USA.))


# USA
plot.ts(trade_balance$S.Palvelut.Ulkomaat.yhteensä, xlab = 'Aika', ylab ='MEUR',
        main = 'Palvelutase USA 2013Q1-2024Q2', ylim=c(-3000, 2000))
abline(h=0, col='blue', lty=5)
lines(trade_balance$S.Palvelut.Yhdysvallat..USA., col = 2)
legend(1, -2000, legend = c('Palvelutase', 'USA'), col=1:2, lty=1)

# makro
plot.ts(trade_balance$S.Palvelut.Ulkomaat.yhteensä, ylim=c(-3000, 2000))
abline(h=0, col='blue', lty=5)
lines(trade_balance$S.Palvelut.Alankomaat + trade_balance$S.Palvelut.Irlanti)
lines(trade_balance$S.Palvelut.Intia)
lines(trade_balance$S.Palvelut.Puola + trade_balance$S.Palvelut.Luxemburg)
lines(trade_balance$S.Palvelut.Espanja + trade_balance$S.Palvelut.Kreikka + trade_balance$S.Palvelut.Italia)
lines(trade_balance$S.Palvelut.Ruotsi + trade_balance$S.Palvelut.Tanska)

# makro 2
plot.ts(trade_balance$S.Palvelut.Ulkomaat.yhteensä, ylim=c(-3000, 2000))
abline(h=0, col='blue', lty=5)
lines(trade_balance$S.Palvelut.Irlanti + trade_balance$S.Palvelut.Alankomaat
      + trade_balance$S.Palvelut.Yhdysvallat..USA.
      + trade_balance$S.Palvelut.Espanja)

# simple regressions
summary(lm(trade_balance$S.Palvelut.Ulkomaat.yhteensä ~ trade_balance$S.Palvelut.Yhdysvallat..USA.
           + trade_balance$S.Palvelut.Espanja
           + trade_balance$S.Palvelut.Irlanti))

plot.ts(trade_balance$S.Palvelut.Ulkomaat.yhteensä, ylim=c(-3000, 2000))
abline(h=0, col='blue', lty=5)
lines(trade_balance$S.Palvelut.Irlanti
      + trade_balance$S.Palvelut.Yhdysvallat..USA.
      + trade_balance$S.Palvelut.Espanja)

plot.ts(diff(trade_balance$S.Palvelut.Ulkomaat.yhteensä))

summary(lm(trade_balance$S.Palvelut.Ulkomaat.yhteensä ~ trade_balance$S.Palvelut.Irlanti 
           + trade_balance$S.Palvelut.Yhdysvallat..USA. 
           + trade_balance$S.Palvelut.Intia 
           + trade_balance$S.Palvelut.Tanska 
           + trade_balance$S.Palvelut.Espanja))

summary(lm(diff(trade_balance$S.Palvelut.Ulkomaat.yhteensä) 
           ~ diff(trade_balance$S.Palvelut.Yhdysvallat..USA.)
           + diff(trade_balance$S.Palvelut.Irlanti)))
D
# Irlanti: verovälttely
# Yhdysvallat: 
# Intia: Nokia (ym. isot yritykset ja asiakaspavelu) ja ulkoistus
# Tanska: Nordea
# Espanja: post-korona matkustelu


# differenced time-series 1
summary(lm(diff(trade_balance$S.Palvelut.Ulkomaat.yhteensä) 
           ~ diff(trade_balance$S.Palvelut.Yhdysvallat..USA.) 
           + diff(trade_balance$S.Palvelut.Espanja)
           + diff(trade_balance$S.Palvelut.Tanska)
           + diff(trade_balance$S.Palvelut.Alankomaat)
           + diff(trade_balance$S.Palvelut.Irlanti)))

# ... and 2
summary(lm(diff(trade_balance$S.Palvelut.Ulkomaat.yhteensä) 
           ~ diff(trade_balance$S.Palvelut.Yhdysvallat..USA.) 
           + diff(trade_balance$S.Palvelut.Espanja)
           + diff(trade_balance$S.Palvelut.Irlanti)
           + diff(trade_balance$S.Palvelut.Intia)
           + diff(trade_balance$S.Palvelut.Tanska)))
