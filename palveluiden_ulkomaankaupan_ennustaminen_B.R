
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

####

# ----
#### Finland: S and SOX Subsets ####

fin_importsM <- subset(wto_monthly_imports, ReportingEconomy=='Finland' & ProductOrSectorCode == 'S') # SOX and SPX1 doesn't exist
fin_exportsM <- subset(wto_monthly_exports, ReportingEconomy=='Finland' & ProductOrSectorCode == 'S') # SOX and SPX1 doesn't exist

fin_importsQ <- subset(wto_quarterly_imports, ReportingEconomy=='Finland' & ProductOrSectorCode == 'S')
fin_importsQ2 <- subset(wto_quarterly_imports, ReportingEconomy=='Finland' & ProductOrSectorCode == 'SOX')

fin_exportsQ <- subset(wto_quarterly_exports, ReportingEconomy=='Finland' & ProductOrSectorCode == 'S')
fin_exportsQ2 <- subset(wto_quarterly_exports, ReportingEconomy=='Finland' & ProductOrSectorCode == 'SOX')

####

#----
#### Finland's S v. SOX data: visual checks ####

# quarterly service imports Finland ...
plot.ts(fin_importsQ$Value, ylim = c(2000, 12000), type = 'l', xaxt = 'n', ylab ='Palvelukauppa (MEUR)', xlab='Aika',
        main = 'Suomen palvelutuonti vuosineljänneksittäin (2006-2024)')
lines(fin_importsQ2$Value, col = 2)
legend('topleft', legend=c('Total services', 'Commercial services'), col = 1:2, lty = 1)
axis(1, at = seq(1, 73, by = 4), labels = 2006:2024)

# ... and difference 
plot.ts( (fin_importsQ$Value - fin_importsQ2$Value)/fin_importsQ$Value * 100, ylab='%', ylim = c(0.0, 0.6),
         main = 'Total services (S) minus commercial services (SOX) ', xaxt = 'n')
axis(1, at = seq(1, 73, by = 4), labels = 2006:2024)

# quarterly service exports Finland
plot.ts(fin_exportsQ$Value, ylim = c(2000, 12000), type = 'l', xaxt = 'n', ylab ='Palvelukauppa (MEUR)', xlab='Aika',
        main = 'Suomen palveluvienti vuosineljänneksittäin (2006-2024)')
lines(fin_exportsQ2$Value, col = 2)
legend('topleft', legend=c('Total services', 'Commercial services'), col = 1:2, lty = 1)
axis(1, at = seq(1, 73, by = 4), labels = 2006:2024)

# ... and difference 
plot.ts( (fin_exportsQ$Value - fin_exportsQ2$Value)/fin_exportsQ$Value * 100, ylab='%', ylim = c(0.0, 1),
         main = 'Total services (S) minus commercial services (SOX) ', xaxt = 'n')
axis(1, at = seq(1, 73, by = 4), labels = 2006:2024)

####
# ----
#### SPX1 subsets ####

fin_importsQ_SPX1 <- subset(wto_quarterly_imports, ReportingEconomy=='Finland' & ProductOrSectorCode == 'SPX1')
fin_exportsQ_SPX1 <- subset(wto_quarterly_exports, ReportingEconomy=='Finland' & ProductOrSectorCode == 'SPX1')

####


# ----
#### SPX1 (memo item: other services) visual checks ####

plot.ts(fin_exportsQ$Value, ylim = c(1000, 12000), xaxt = 'n', ylab ='Palvelukauppa (MEUR)', xlab='Aika',
        main = 'Suomen palveluvienti vuosineljänneksittäin (2006-2024)')
lines(fin_exportsQ2$Value, col = 2)
lines(fin_exportsQ_SPX1$Value, col = 3)
legend('topleft', legend=c('Total services', 'Commercial services', 'Other Services'), col = 1:3, lty = 1)

####



# ----
#### WTO percentage subset ####

fin_importsMP <- subset(wto_percentage_imports, ReportingEconomy=='Finland' & ProductOrSectorCode == 'S') # M = monthly & P = percentage
fin_exportsMP <- subset(wto_percentage_exports, ReportingEconomy=='Finland' & ProductOrSectorCode == 'S')

####
# ----
# monthly service imports and exports Finland ####
plot.ts(fin_importsMP$Value, xaxt = 'n', ylim = c(-30, 110), xlab = 'Vuosi', 
        ylab = 'Change over the same period of the previous year (%)',
        main = 'Finland total monthly service imports and exports (2006-2024)')
abline(h=0, col = 'blue', lty = 4)
lines(fin_exportsMP$Value, col = 2)
legend('topleft', legend=c('Total service imports', 'Total service exports'), col = 1:2, lty = 1)
axis(1, at = seq(1, 211, by = 12), labels = 2007:2024)

####

# calculate trade balances (exports less imports )

# ----
# trade balance visuals ####

# monthly trade balance Finland (total services)
plot.ts(fin_exportsM$Value - fin_importsM$Value)
abline(h=0, col = 'blue', lty = 4)

# monthly trade balance percentage change (total services)
plot.ts(fin_exportsMP$Value - fin_importsMP$Value)
abline(h=0, col = 'blue', lty = 4)

# monthly trade balance Finland (total services): past 4 years
plot.ts(tail(fin_exportsM$Value, 48) - tail(fin_importsM$Value, 48))
abline(h=0, col = 'blue', lty = 4)

# trade balance's 12-month moving average
plot.ts(rollmean(fin_exportsM$Value - fin_importsM$Value, k = 12, fill = NA, align = 'right'), ylim = c(-800, 100))
abline(h=0, col = 'blue', lty = 4)

####

# ----
# monthly total service imports and exports of other countries ####

# monthly total service imports
swe_importsM <- subset(wto_monthly_imports, ReportingEconomy=='Sweden' & ProductOrSectorCode == 'S') # SOX and SPX1 doesn't exist
ger_importsM <- subset(wto_monthly_imports, ReportingEconomy=='Germany' & ProductOrSectorCode == 'S') # SOX and SPX1 doesn't exist
usa_importsM <- subset(wto_monthly_imports, ReportingEconomy=='United States of America' & ProductOrSectorCode == 'S') # SOX and SPX1 doesn't exist
chn_importsM <- subset(wto_monthly_imports, ReportingEconomy=='China' & ProductOrSectorCode == 'S') # SOX and SPX1 doesn't exist

# monthly total service exports 
swe_exportsM <- subset(wto_monthly_exports, ReportingEconomy=='Sweden' & ProductOrSectorCode == 'S') # SOX and SPX1 doesn't exist
ger_exportsM <- subset(wto_monthly_exports, ReportingEconomy=='Germany' & ProductOrSectorCode == 'S') # SOX and SPX1 doesn't exist
usa_exportsM <- subset(wto_monthly_exports, ReportingEconomy=='United States of America' & ProductOrSectorCode == 'S') # SOX and SPX1 doesn't exist
chn_exportsM <- subset(wto_monthly_exports, ReportingEconomy=='China' & ProductOrSectorCode == 'S') # SOX and SPX1 doesn't exist

# visual checks
plot.ts(fin_importsM$Value, ylim = c(500, 4500))
lines(fin_exportsM$Value, col = 2)

plot.ts(swe_importsM$Value, ylim = c(3000, 11000))
abline(h=0, col = 'blue', lty = 4)
lines(swe_exportsM$Value, col = 2)

plot.ts(ger_importsM$Value)
lines(ger_exportsM$Value, col = 2)

plot.ts(usa_importsM$Value, ylim = c(20000, 90000))
lines(usa_exportsM$Value, col = 2)

plot.ts(chn_importsM$Value, ylim = c(0, 60000))
lines(chn_exportsM$Value, col = 2)

####
# ----
# monthly percentage change of other countries (total services) ####

# percentage change of monthly total service imports
swe_importsMP <- subset(wto_percentage_imports, ReportingEconomy=='Sweden' & ProductOrSectorCode == 'S') # SOX and SPX1 doesn't exist
ger_importsMP <- subset(wto_percentage_imports, ReportingEconomy=='Germany' & ProductOrSectorCode == 'S') # SOX and SPX1 doesn't exist
usa_importsMP <- subset(wto_percentage_imports, ReportingEconomy=='United States of America' & ProductOrSectorCode == 'S') # SOX and SPX1 doesn't exist
chn_importsMP <- subset(wto_percentage_imports, ReportingEconomy=='China' & ProductOrSectorCode == 'S') # SOX and SPX1 doesn't exist

# percentage change of monthly total service exports
swe_exportsMP <- subset(wto_percentage_exports, ReportingEconomy=='Sweden' & ProductOrSectorCode == 'S') # SOX and SPX1 doesn't exist
ger_exportsMP <- subset(wto_percentage_exports, ReportingEconomy=='Germany' & ProductOrSectorCode == 'S') # SOX and SPX1 doesn't exist
usa_exportsMP <- subset(wto_percentage_exports, ReportingEconomy=='United States of America' & ProductOrSectorCode == 'S') # SOX and SPX1 doesn't exist
chn_exportsMP <- subset(wto_percentage_exports, ReportingEconomy=='China' & ProductOrSectorCode == 'S') # SOX and SPX1 doesn't exist

# visual checks

# Sweden
plot.ts(swe_exportsMP$Value, col = 2, xlab ='Vuosi', xaxt = 'n',
        ylab ='Change over the same period of the previous year (%)',
        main = 'Sweden total monthly service imports and exports (2009-2024)')
lines(swe_importsMP$Value, col = 1)
abline(h=0, col = 'blue', lty = 4)
legend('topleft', legend=c('Total service imports', 'Total service exports'), col = 1:2, lty = 1)
axis(1, at = seq(1, 187, by = 12), labels = 2009:2024)

# Germany
plot.ts(ger_exportsMP$Value, col = 2, xlab ='Vuosi', xaxt = 'n', ylim = c(-40, 50),
        ylab ='Change over the same period of the previous year (%)',
        main = 'Germany total monthly service imports and exports (2006-2024)')
lines(ger_importsMP$Value, col = 1)
abline(h=0, col = 'blue', lty = 4)
legend('topleft', legend=c('Total service imports', 'Total service exports'), col = 1:2, lty = 1)
axis(1, at = seq(1, 223, by = 12), labels = 2006:2024)

# USA
plot.ts(usa_exportsMP$Value, col = 2, xlab ='Vuosi', xaxt = 'n', ylim = c(-40, 50),
        ylab ='Change over the same period of the previous year (%)',
        main = 'USA total monthly service imports and exports (2006-2024)')
lines(usa_importsMP$Value, col = 1)
abline(h=0, col = 'blue', lty = 4)
legend('topleft', legend=c('Total service imports', 'Total service exports'), col = 1:2, lty = 1)
axis(1, at = seq(1, 223, by = 12), labels = 2006:2024)

# China
plot.ts(chn_exportsMP$Value, col = 2, xlab ='Vuosi', xaxt = 'n', ylim = c(-40, 70),
        ylab ='Change over the same period of the previous year (%)',
        main = 'China total monthly service imports and exports (2016-2024)')
lines(chn_importsMP$Value, col = 1)
abline(h=0, col = 'blue', lty = 4)
legend('topleft', legend=c('Total service imports', 'Total service exports'), col = 1:2, lty = 1)
axis(1, at = seq(1, 103, by = 12), labels = 2016:2024)

####

# ----
# Finland: commercial services by sub sector quarterly ####

# Finland imports
fin_imports_subsector <- subset(wto_subsector_imports, ReportingEconomy == 'Finland')
fin_imports_subsector <- subset(fin_imports_subsector, select = c(ProductOrSectorCode, ProductOrSector, Value, Date))

# Finland exports
fin_exports_subsector <- subset(wto_subsector_exports, ReportingEconomy == 'Finland')
fin_exports_subsector <- subset(fin_exports_subsector, select = c(ProductOrSectorCode, ProductOrSector, Value, Date))

# Finland imports
fin_imports_subsector_sector <- fin_imports_subsector %>%
  pivot_wider(
    id_cols = Date,
    names_from = ProductOrSector,
    values_from = Value
  )

# Finland exports
fin_exports_subsector_sector <- fin_exports_subsector %>%
  pivot_wider(
    id_cols = Date,
    names_from = ProductOrSector,
    values_from = Value
  )


# Finland imports visual checks
plot.ts(fin_imports_subsector_sector$`Telecommunications, computer, and information services`, xaxt = 'n')
axis(1, at=seq(1, 73, by = 4), labels=2006:2024)

# Finnish exports visual checks
plot.ts(fin_exports_subsector_sector$`Telecommunications, computer, and information services`, xaxt = 'n')
axis(1, at=seq(1, 73, by = 4), labels=2006:2024)

# Finnish imports and exports
plot.ts(fin_imports_subsector_sector$`Telecommunications, computer, and information services`, xaxt = 'n', ylim = c(0, 5000),
        main = 'Imports and exports of Finnish ICT-services',
        ylab = 'MEUR', xlab = 'Vuosi')
lines(fin_exports_subsector_sector$`Telecommunications, computer, and information services`, col = 2)
axis(1, at=seq(1, 73, by = 4), labels=2006:2024)
legend('topleft', legend = c('Imports', 'Exports'), col = 1:2, lty = 1)

####
# ----
# Sweden commercial services by sub sector quarterly ####

# Sweden imports
swe_imports_subsector <- subset(wto_subsector_imports, ReportingEconomy == 'Sweden')
swe_imports_subsector <- subset(swe_imports_subsector, select = c(ProductOrSectorCode, ProductOrSector, Value, Date))

# Sweden exports
swe_exports_subsector <- subset(wto_subsector_exports, ReportingEconomy == 'Sweden')
swe_exports_subsector <- subset(swe_exports_subsector, select = c(ProductOrSectorCode, ProductOrSector, Value, Date))

# Sweden imports
swe_imports_subsector_sector <- swe_imports_subsector %>%
  pivot_wider(
    id_cols = Date,
    names_from = ProductOrSector,
    values_from = Value
  )

# Sweden exports
swe_exports_subsector_sector <- swe_exports_subsector %>%
  pivot_wider(
    id_cols = Date,
    names_from = ProductOrSector,
    values_from = Value
  )

# Sweden imports visual checks
plot.ts(swe_imports_subsector_sector$`Telecommunications, computer, and information services`, xaxt = 'n')
axis(1, at=seq(1, 73, by = 4), labels=2006:2024)

# Sweden exports visual checks
plot.ts(swe_exports_subsector_sector$`Telecommunications, computer, and information services`, xaxt = 'n')
axis(1, at=seq(1, 73, by = 4), labels=2006:2024)

# Swedish imports and exports
plot.ts(swe_imports_subsector_sector$`Telecommunications, computer, and information services`, xaxt = 'n', ylim = c(0, 8000),
        main = 'Imports and exports of Swedish ICT-services',
        ylab = 'MEUR', xlab = 'Vuosi')
lines(swe_exports_subsector_sector$`Telecommunications, computer, and information services`, col = 2)
axis(1, at=seq(1, 73, by = 4), labels=2006:2024)
legend('topleft', legend = c('Imports', 'Exports'), col = 1:2, lty = 1)

####
# ----
# monthly global service demand and supply (commercial and total services) ####

# monthly imports: s and sox
wto_monthly_global_imports_s <- subset(wto_monthly_global_imports, ProductOrSectorCode == 'S')
wto_monthly_global_imports_sox <- subset(wto_monthly_global_imports, ProductOrSectorCode == 'SOX')

# monthly exports: s and sox
wto_monthly_global_exports_s <- subset(wto_monthly_global_exports, ProductOrSectorCode == 'S')
wto_monthly_global_exports_sox <- subset(wto_monthly_global_exports, ProductOrSectorCode == 'SOX')

# monthly world imports: s
wto_monthly_global_imports_s_aggregate <- aggregate(
  Value ~ Date,
  data = subset(wto_monthly_global_imports, ProductOrSectorCode == 'S'),
  sum
  )

# add growth rate for s imports
wto_monthly_global_imports_s_aggregate$growth_rate <- (wto_monthly_global_imports_s_aggregate$Value - lag(wto_monthly_global_imports_s_aggregate$Value)) / lag(wto_monthly_global_imports_s_aggregate$Value)

# monthly world exports: s
wto_monthly_global_exports_s_aggregate <- aggregate(
  Value ~ Date,
  data = subset(wto_monthly_global_exports, ProductOrSectorCode == 'S'),
  sum
)

# add growth rate for s exports
wto_monthly_global_exports_s_aggregate$growth_rate <- (wto_monthly_global_exports_s_aggregate$Value - lag(wto_monthly_global_exports_s_aggregate$Value)) / lag(wto_monthly_global_exports_s_aggregate$Value)

# monthly world imports: sox
wto_monthly_global_imports_sox_aggregate <- aggregate(
  Value ~ Date,
  data = subset(wto_monthly_global_imports, ProductOrSectorCode == 'SOX'),
  sum
)

# add growth rate for sox imports
wto_monthly_global_imports_sox_aggregate$growth_rate <- (wto_monthly_global_imports_sox_aggregate$Value - lag(wto_monthly_global_imports_sox_aggregate$Value)) / lag(wto_monthly_global_imports_sox_aggregate$Value)

# monthly world exports: sox
wto_monthly_global_exports_sox_aggregate <- aggregate(
  Value ~ Date,
  data = subset(wto_monthly_global_exports, ProductOrSectorCode == 'SOX'),
  sum
)

#add growth rate for sox exports
wto_monthly_global_exports_sox_aggregate$growth_rate <- (wto_monthly_global_exports_sox_aggregate$Value - lag(wto_monthly_global_exports_sox_aggregate$Value)) / lag(wto_monthly_global_exports_sox_aggregate$Value)

####

# visual checks for monthly global service demand ####

# visual check: s
plot.ts(wto_monthly_global_imports_s_aggregate$Value[-length(wto_monthly_global_imports_s_aggregate$Value)], ylim = c(0, 500000))
lines(wto_monthly_global_exports_s_aggregate$Value[-length(wto_monthly_global_exports_s_aggregate$Value)], col = 2)

# Finland for comparison
plot.ts(head(wto_monthly_global_imports_s_aggregate$Value, -1), ylab = 'MEUR', xaxt = 'n',
        main = 'Global service demand v. Finnish exports'); par(new = TRUE)
plot.ts(fin_exportsM$Value, yaxt = 'n', ylab = '', xaxt = 'n', col = 2); axis(4)
legend('topleft', legend = c('World services demand (left scale)', 'Finland services exports (right scale)'), col = 1:2, lty = 1)
axis(1, at = seq(1, 223, by = 12), labels = 2006:2024)

# visual check: sox
plot.ts(wto_monthly_global_imports_sox_aggregate$Value[-length(wto_monthly_global_imports_sox_aggregate$Value)], ylim = c(0, 500000))
lines(wto_monthly_global_exports_sox_aggregate$Value[-length(wto_monthly_global_exports_sox_aggregate$Value)], col = 2)

# growth rates for s imports and exports
plot.ts(head(wto_monthly_global_imports_s_aggregate$growth_rate, -1), xaxt = 'n')
axis(1, at = seq(1, 224, by = 12), labels = 2006:2024)

plot.ts(head(wto_monthly_global_exports_s_aggregate$growth_rate, -1), xaxt = 'n')
axis(1, at = seq(1, 224, by = 12), labels = 2006:2024)


# ... same in barplot format
barplot(head(wto_monthly_global_imports_s_aggregate$growth_rate, -1), ylim = c(-0.2, 0.2),
        names.arg = head(wto_monthly_global_imports_s_aggregate$Date, -1), las = 2); grid()

barplot(head(wto_monthly_global_exports_s_aggregate$growth_rate, -1), ylim = c(-0.2, 0.2))

# growth rates for sox imports and exports
plot.ts(head(wto_monthly_global_imports_sox_aggregate$growth_rate, -1))
plot.ts(head(wto_monthly_global_exports_sox_aggregate$growth_rate, -1))

# ... same in barplot format
barplot(head(wto_monthly_global_imports_sox_aggregate$growth_rate, -1), ylim = c(-0.2, 0.2))
barplot(head(wto_monthly_global_exports_sox_aggregate$growth_rate, -1), ylim = c(-0.2, 0.2))


####

# ----

