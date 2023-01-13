#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(quantmod)
library(ggplot2)
library(tidyverse)
library(plyr)
library(dplyr)
library(assertive.base)
library(RColorBrewer)



## AUFBEREITUNG DER GRUNDDATEN
##geschieht außerhalb der Anwendung für die Performance und weil all das nur einmal durchgegangen werden muss
data<-read.csv2("./Data/RKI_COVID19_Berlin.csv", header=T, sep=",")
##Transformiert diese beiden Spalten in Datumsformat, damit Vergleiche angesellt werden können:
data$Meldedatum<-as.Date(data$Meldedatum)
data$Refdatum<-as.Date(data$Refdatum) 
## Die daten werden nun nach dem referenzdatum sortiert
data <- data[order(data$Refdatum),]

bevoelkerung<-read.csv2("./Data/bevoelkerungs.csv", header = T, sep = ";")
bevoelkerung$Geschlecht <- factor(bevoelkerung$Geschlecht, levels = c("M", "W")) 
bevoelkerung$Nationalität<-factor(bevoelkerung$Nationalität, levels=c("D","A"))
bevoelkerung$Landkreis<-factor(bevoelkerung$Landkreis, levels= rev(c("SK Berlin Mitte","SK Berlin Friedrichshain-Kreuzberg","SK Berlin Pankow", 
                                                                     "SK Berlin Charlottenburg-Wilmersdorf", "SK Berlin Spandau",  
                                                                     "SK Berlin Steglitz-Zehlendorf","SK Berlin Tempelhof-Schöneberg", 
                                                                     "SK Berlin Neukölln","SK Berlin Treptow-Köpenick","SK Berlin Marzahn-Hellersdorf",
                                                                     "SK Berlin Lichtenberg", "SK Berlin Reinickendorf",
                                                                     "Berlin")))
summary(bevoelkerung)


##Daten über die Impfkampangne vom RKI, sortiert nach bundesländern:
##https://github.com/robert-koch-institut/COVID-19-Impfungen_in_Deutschland/blob/master/Aktuell_Deutschland_Bundeslaender_COVID-19-Impfungen.csv

impfungen<-read.csv2("./Data/impfungen.csv", header = T, sep = ",")

##aus den zurückgehenden Todesfällen v.a. bei Delta im Vergleich zu den weniger letalen Vorgängern ergibt sich ein Interesse für die Impfkampagne:

##Impfungen in Berlin:
##hier wird nur der Landkreis des Impfortes angegeben, nicht der Meldeadresse der geimpften Person. (Vermutlich aus Datenschutzgründen)
impfungen_b<-impfungen[impfungen$BundeslandId_Impfort==11,]
## return: data als alle covid-daten (mit dates als typ date), impfungen als alle impfdaten, impfungen_b als alle impfdaten für berlin (die für uns relevanten daten)


##Datums, Monats- und Jahresspalte gemäß des New Data Approaches
cWeeks_i <- strftime(impfungen_b$Impfdatum, format = "%V")
cWeeks_i <- sub("^0+", "", cWeeks_i)
cWeeks_i <- as.numeric(cWeeks_i)
impfungen_b <- cbind(impfungen_b, Woche = cWeeks_i)
cMonths_i <- strftime(impfungen_b$Impfdatum, format = "%m")
cMonths_i <- sub("^0+", "", cMonths_i)
cMonths_i <- as.numeric(cMonths_i)
cMonths_i <- month.abb[cMonths_i]
impfungen_b <- cbind(impfungen_b, Monat = cMonths_i)
cYears_i <- strftime(impfungen_b$Impfdatum, format = "%Y")
cYears_i <- sub("^0+", "", cYears_i)
cYears_i <- as.numeric(cYears_i)
impfungen_b  <- cbind(impfungen_b , Jahr = cYears_i)
##kumuliere verabreichte impfdosen
impfdosenTotal<-cumsum(impfungen_b$Anzahl)
impfungen_b <-cbind(impfungen_b, Impfungen_Gesamt=impfdosenTotal)




## GRUNDSAETZLICHE FESTLEGUNGEN:
## Wenn wir Covid Faelle untersuchen, gehen wir immer vom Refdatum aus
## Wenn wir Impfungen untersuchen, danach aggregieren etc. verwenden wir immer die totalen Impfdosen verabreicht, nicht die Anzahl der erhaltenen Impfungen












##Ist das nach New Data Approach noch nötig=

## UNTERSUCHUNG: IMPFUNGEN PRO TAG/WOCHE FUER 20/21/22 (+ Ermittlung von Maximalwerten):
## 2020
aggImpfsPerDay20 <- aggregate(Anzahl ~ Impfdatum, FUN = sum, data = impfungen_b[impfungen_b$Jahr==2020,])
aggImpfsPerWeek20 <- aggregate(Anzahl ~ Woche, FUN = sum, data = impfungen_b[impfungen_b$Jahr==2020,])

maxImpfsPerDay20 <- max(aggImpfsPerDay20$Anzahl)
maxImpfsPerWeek20 <- max(aggImpfsPerWeek20$Anzahl)

## 2021
aggImpfsPerDay21 <- aggregate(Anzahl ~ Impfdatum, FUN = sum, data = impfungen_b[impfungen_b$Jahr==2021,])
aggImpfsPerWeek21 <- aggregate(Anzahl ~ Woche, FUN = sum, data = impfungen_b[impfungen_b$Jahr==2021,])

maxImpfsPerDay21 <- max(aggImpfsPerDay21$Anzahl)
maxImpfsPerWeek21 <- max(aggImpfsPerWeek21$Anzahl)

## 2022
aggImpfsPerDay22<-aggregate(Anzahl ~ Impfdatum, FUN = sum, data = impfungen_b[impfungen_b$Jahr==2022,])
aggImpfsPerWeek22<-aggregate(Anzahl ~ Woche, FUN = sum, data = impfungen_b[impfungen_b$Jahr==2022,])

maxImpfsPerDay22 <- max(aggImpfsPerDay22$Anzahl)
maxImpfsPerWeek22 <- max(aggImpfsPerWeek22$Anzahl)

## 2023
aggImpfsPerDay23<-aggregate(Anzahl ~ Impfdatum, FUN = sum, data = impfungen_b[impfungen_b$Jahr==2023,])
aggImpfsPerWeek23<-aggregate(Anzahl ~ Woche, FUN = sum, data = impfungen_b[impfungen_b$Jahr==2023,])

maxImpfsPerDay23 <- max(aggImpfsPerDay22$Anzahl)
maxImpfsPerWeek23 <- max(aggImpfsPerWeek22$Anzahl)

##diese hier könnten genügen
aggImpfsPerDayTotal<-aggregate(Anzahl~Impfdatum,  FUN = sum, data = impfungen_b)
aggImpfsPerWeekTotal <- aggregate(Anzahl ~ Woche, FUN = sum, data = impfungen_b)
maxImpfsPerDayTotal <- max(aggImpfsPerDayTotal$Anzahl)
maxImpfsPerWeekTotal <- max(aggImpfsPerWeekTotal$Anzahl)











## UNTERTEILUNG DER COVID DATEN IN DIE VERSCHIEDENEN COVID-VARIANTEN (ausgehend von den Zeitraeumen, in denen jeweilige Variante vorherrschend war)
## gemäß
## https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Daten/VOC_VOI_Tabelle.xlsx?__blob=publicationFile
## wird folgendes angenommen:
## KW09/2021-KW24/2021: Alpha ist vorherrschend
## KW25/2021-KW51/2021: Delta ist vorherrschend
## ab KW52/2021: Omikron- vorerst keine weitere Unterscheidung der Subtypen
##vorherrschend heißt >50%

data_urtyp<-data[data$Refdatum < '2021-03-01',]
data_alpha<-data[data$Refdatum > '2021-02-28' & data$Refdatum < '2021-06-21',]
data_delta<-data[data$Refdatum > '2021-06-21' & data$Refdatum < '2021-12-27',]
data_ominkron<-data[data$Refdatum > '2021-12-26',]







## UNTERSUCHUNG: DISKREPANZ ZWISCHEN MELDEDATUM UND REFERENZDATUM (ERKRANKUNGSBEGINN)
meldeRefDiscrepency <- data$Meldedatum - data$Refdatum
## maxDiscrepency
maxMeldeRefDiscrepency <- max(meldeRefDiscrepency)





# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ----------------------------------------------------------
#
#                   - New Data Approach -
#
# ----------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Originaldaten als "base"
base <- data

# base auf die wichtigen Merkmale begrenzen
base <- base[,c('Refdatum', 'Landkreis', 'Geschlecht', 'Altersgruppe', 'AnzahlFall', 'AnzahlTodesfall')]

# char-columns zu factors umwandeln. wie und warum? -> siehe: UMWANDELN VON CHARACTERSPALTEN ZU FACTORS
base$Geschlecht <- factor(base$Geschlecht, levels = c("unbekannt", "M", "W")) 
# ich habe landkreis custom geordnet, da auf die Weise, die nach dem Landkreis aggregierten Fall-Daten (und andere)
# so nun automatisch und ohne Weiteres in der richtigen, Reihenfolge abgebildet werden (beim späteren barPlotFallTot) (stimmt aber tz nicht immer)
base$Landkreis <- factor(base$Landkreis, levels = rev(c("SK Berlin Mitte", "SK Berlin Neukölln", "SK Berlin Tempelhof-Schöneberg", 
                                                        "SK Berlin Friedrichshain-Kreuzberg", "SK Berlin Charlottenburg-Wilmersdorf", 
                                                        "SK Berlin Pankow", "SK Berlin Reinickendorf", "SK Berlin Spandau", 
                                                        "SK Berlin Steglitz-Zehlendorf", "SK Berlin Lichtenberg", 
                                                        "SK Berlin Treptow-Köpenick", "SK Berlin Marzahn-Hellersdorf")))

impf <- impfungen

# ------------------------------------------------------------------------------------- WEITERE RELEVANTE MERKMALE ALS SPALTEN HINZUFÜGEN
# ------------------------------------------------------------------------------------- wochenspalte
# week-column für base erstellen (c = column)
cWeeks <- strftime(base$Refdatum, format = "%V")

# Nullen vor den week-chars löschen
cWeeks <- sub("^0+", "", cWeeks)

# weeks zu numeric values umwandeln
cWeeks <- as.numeric(cWeeks)

# cWeeks an base ranhängen
base <- cbind(base, Woche = cWeeks)

# für impf
cWeeksImpf <- strftime(impf$Impfdatum, format = "%V")
cWeeksImpf <- sub("^0+", "", cWeeksImpf)
cWeeksImpf <- as.numeric(cWeeksImpf)
impf <- cbind(impf, Woche = cWeeksImpf)

# ------------------------------------------------------------------------------------- monatsspalte
# month-column für base erstellen
cMonths <- strftime(base$Refdatum, format = "%m")

# Nullen vor den month-chars löschen
cMonths <- sub("^0+", "", cMonths)

# months zu numeric values umwandeln
cMonths <- as.numeric(cMonths)

# Monatszahlen zu Monatsnamen (in abgekürzter Weise als String) konvertieren
# siehe dafür: https://stackoverflow.com/questions/22058393/convert-a-numeric-month-to-a-month-abbreviation
cMonths <- month.abb[cMonths]

# cMonths an base ranhängen
base <- cbind(base, Monat = cMonths)

# für impf
cMonthsImpf <- strftime(impf$Impfdatum, format = "%m")
cMonthsImpf <- sub("^0+", "", cMonthsImpf)
cMonthsImpf <- as.numeric(cMonthsImpf)
cMonthsImpf <- month.abb[cMonthsImpf]
impf <- cbind(impf, Monat = cMonthsImpf)

# ------------------------------------------------------------------------------------- jahresspalte
# year-column für base erstellen
cYears <- strftime(base$Refdatum, format = "%Y")

# Nullen vor den year-chars löschen
cYears <- sub("^0+", "", cYears)

# years zu numeric values umwandeln
cYears <- as.numeric(cYears)

# cYears an base ranhängen
base <- cbind(base, Jahr = cYears)

# für impf
cYearsImpf <- strftime(impf$Impfdatum, format = "%Y")
cYearsImpf <- sub("^0+", "", cYearsImpf)
cYearsImpf <- as.numeric(cYearsImpf)
impf <- cbind(impf, Jahr = cYearsImpf)

# ------------------------------------------------------------------------------------- covid-variante-spalte
# ausgehend von den Zeiträumen, in denen jeweilige Variante vorherrschend war
## gemäß
## https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Daten/VOC_VOI_Tabelle.xlsx?__blob=publicationFile
## wird folgendes angenommen:
## KW09/2021-KW24/2021: Alpha ist vorherrschend
## KW25/2021-KW51/2021: Delta ist vorherrschend
## ab KW52/2021: Omikron- vorerst keine weitere Unterscheidung der Subtypen
##vorherrschend heißt >50%
## für folgenden Code, siehe: https://www.marsja.se/r-add-column-to-dataframe-based-on-other-columns-conditions-dplyr/
base <- base %>%
  mutate(Variante = case_when(
    Refdatum <= '2021-03-01' ~ "urtyp",
    Refdatum > '2021-03-01' & Refdatum <= '2021-06-20' ~ "alpha",
    Refdatum > '2021-06-20' & data$Refdatum <= '2021-12-26' ~ "delta",
    Refdatum > '2021-12-26' ~ "omikron"
  ))

# ------------------------------------------------------------------------------------- spalte für impf die das wochenproblem fixt
impf <- impf %>%
  mutate(WochenJahr = case_when(
    Impfdatum >= '2019-12-30' & Impfdatum < '2021-01-03' ~ "2020",
    Impfdatum >= '2021-01-04' & Impfdatum < '2022-01-02' ~ "2021",
    Impfdatum >= '2022-01-03' & Impfdatum < '2023-01-01' ~ "2022",
    Impfdatum >= '2023-01-02' & Impfdatum < '2023-12-31' ~ "2023"
  ))

# ------------------------------------------------------------------------------------- spalte für impf die impfstoffe zusammenfasst
impf <- impf %>%
  mutate(ImpfstoffGruppiert = case_when(
    Impfstoff == "Comirnaty" | Impfstoff == "Comirnaty-Kleinkinder" | Impfstoff == "Comirnaty bivalent (Original/Omikron)" ~ "Comirnaty",
    Impfstoff == "Jcovden" ~ "Jcovden",
    Impfstoff == "Nuvaxovid" ~ "Nuvaxovid",
    Impfstoff == "Spikevax" | Impfstoff == "Spikevax bivalent (Original/Omikron)" ~ "Spikevax",
    Impfstoff == "Valneva" ~ "Valneva",
    Impfstoff == "Vaxzevria" ~ "Vaxzevria"
  ))

# ------------------------------------------------------------------------------------- EINTEILUNG IN ZEITRÄUME
# Unterteilung in einen Datensatz für den gesamten Zeitraum, durch welchen nach Tagen/Monaten aggregiert werden kann (d = data)
# und einen für die Wochen eines Jahres, denn:
# die erste/letzte Woche eines Jahres beginnt/endet nicht zwingend im selben Jahr
# siehe ISO-week-date: https://en.wikipedia.org/wiki/ISO_week_date
# um ISO-week-dates für jedes Jahr zu bekommen, siehe: https://www.epochconverter.com/weeks/2020

# ------------------------------------------------------------------------------------- 2020
d20 <- base[base$Refdatum >= '2020-01-01' & base$Refdatum < '2021-01-01',]
d20weeks <- base[base$Refdatum >= '2019-12-30' & base$Refdatum <= '2021-01-03',]

impf20 <- impf[impf$Jahr == 2020,]
impf20weeks <- impf[impf$WochenJahr == "2020",]
# ------------------------------------------------------------------------------------- 2021
d21 <- base[base$Refdatum >= '2021-01-01' & base$Refdatum < '2022-01-01',]
d21weeks <- base[base$Refdatum >= '2021-01-04' & base$Refdatum <= '2022-01-02',]

impf21 <- impf[impf$Jahr == 2021,]
impf21weeks <- impf[impf$WochenJahr == "2021",]
# ------------------------------------------------------------------------------------- 2022
d22 <- base[base$Refdatum >= '2022-01-01' & base$Refdatum < '2023-01-01',]
d22weeks <- base[base$Refdatum >= '2022-01-03' & base$Refdatum <= '2023-01-01',]

impf22 <- impf[impf$Jahr == 2022,]
impf22weeks <- impf[impf$WochenJahr == "2022",]
# ------------------------------------------------------------------------------------- 2023
d23 <- base[base$Refdatum >= '2023-01-01' & base$Refdatum < '2024-01-01',]
d23weeks <- base[base$Refdatum >= '2023-01-02' & base$Refdatum <= '2023-12-31',]

impf23 <- impf[impf$Jahr == 2023,]
impf23weeks <- impf[impf$WochenJahr == "2023",]





# ------------------------------------------------------------------------------------- Covid-Varianten
# Die Daten zu Covid Varianten gehen über Zeiträume, die ggf. in der Mitte einer Woche/eines Monats starten bzw. aufhören
# bezieht man die abgeschnittenen Wochen/Monate ein, erhält man dadurch ein weirdes Bild vom jeweiligen Woche/Monat (weil sie/er verkürzt ist)
# ich kann mir vorstellen, dass wir das nicht zwingend verhindern müssen, weil es dennoch gewisse sachen aussagen kann aber theoretisch
# könnten wir uns dabei noch was ausdenken
#
# da eine variante theoretisch über ein jahr hinweg dominieren kann, könnte sie auch zweimal den monat mit dem value = "Jan" umfassen
# nämlich einmal in bspw. 2021 und einmal in 2022 (bei wochen dasselbe), da dies bei einer späteren aggregation zu problemen führt
# müssen wochen und monate einen im bezug aufs jeweilige jahr einzigartigen wert haben, was wie folgt sichergestellt wird
# Code entnommen aus: https://www.marsja.se/how-to-concatenate-two-columns-or-more-in-r-stringr-tidyr/
# und aus: https://search.r-project.org/CRAN/refmans/assertive.base/html/parenthesize.html
# und aus: https://stackoverflow.com/questions/62000584/r-paste-two-strings-without-space
# und aus: https://www.digitalocean.com/community/tutorials/substring-function-in-r

# urtyp
dUrtyp <- base[base$Variante == "urtyp",]
dUrtyp$Woche <- paste(as.character(dUrtyp$Woche), parenthesize(as.character(dUrtyp$Jahr)), sep = "-")
dUrtyp$Monat <- paste(as.character(dUrtyp$Monat), substring(as.character(dUrtyp$Jahr), 3, 4), sep = "")
# kleine adjustments wegen der weirden wochendynamik (53. Woche von 20 liegt zum Teil in 21, 
# dieser bekommt deshalb 53-(2021) und die daten werden an den stellen richtig weird)
# für den code, siehe: https://sparkbyexamples.com/r-programming/replace-values-in-r/#:~:text=To%20replace%20a%20column%20value,single%20column%20use%20df%24column_name%20.
dUrtyp$Woche[dUrtyp$Refdatum >= '2020-12-28' & dUrtyp$Refdatum <= '2021-01-03'] <- "53-(2020)"

# alpha
dAlpha <- base[base$Variante == "alpha",]
dAlpha$Woche <- paste(as.character(dAlpha$Woche), parenthesize(as.character(dAlpha$Jahr)), sep = "-")
dAlpha$Monat <- paste(as.character(dAlpha$Monat), substring(as.character(dAlpha$Jahr), 3, 4), sep = "")

# delta
dDelta <- base[base$Variante == "delta",]
dDelta$Woche <- paste(as.character(dDelta$Woche), parenthesize(as.character(dDelta$Jahr)), sep = "-")
dDelta$Monat <- paste(as.character(dDelta$Monat), substring(as.character(dDelta$Jahr), 3, 4), sep = "")

# omikron
dOmikron <- base[base$Variante == "omikron",]
dOmikron$Woche <- paste(as.character(dOmikron$Woche), parenthesize(as.character(dOmikron$Jahr)), sep = "-")
dOmikron$Monat <- paste(as.character(dOmikron$Monat), substring(as.character(dOmikron$Jahr), 3, 4), sep = "")
# wochen-weirdness hops nehmen
dOmikron$Woche[dOmikron$Refdatum >= '2021-12-27' & dOmikron$Refdatum <= '2022-01-02'] <- "52-(2021)"

# ------------------------------------------------------------------------------------- UMWANDELN VON CHARACTERSPALTEN ZU FACTORS
# belassen wir die spalten so wie sie jetzt sind, ordnet ggplot sie später alphabetisch
# aus diesem grund müssen wir die characterspalten nun zu factors machen, damit sie ihre ordnung behalten
# zu diesem problem, siehe: https://stackoverflow.com/questions/20041136/avoid-ggplot-sorting-the-x-axis-while-plotting-geom-bar
# diesen ansatz verwende ich hier, allerdings hat das levels setzen so nicht funktioniert
# daher die lösung dafür, unter: https://www.r-bloggers.com/2021/12/how-to-find-unique-values-in-r/
# more about factors: https://r4ds.had.co.nz/factors.html#:~:text=In%20R%2C%20factors%20are%20used,to%20work%20with%20than%20characters.
#
# ich setze jetzt hier die factors für alle bisherigen dataframes ein, man hätte das sicherlich auch schon früher machen können
# aber da ich schon halb wahnsinnig bin und sonst noch was kaputt mache, hole ich das jetzt alles übersichtlich hier nach
# (kann man nicht einfach schon bei base machen weil später noch character spalten für einzigartigkeit zusammengefügt werden)

base$Monat <- factor(base$Monat, levels = unique(base$Monat))
base$Variante <- factor(base$Variante, levels = unique(base$Variante))

d20$Monat <- factor(d20$Monat, levels = unique(d20$Monat))
d20$Variante <- factor(d20$Variante, levels = unique(d20$Variante))

d21$Monat <- factor(d21$Monat, levels = unique(d21$Monat))
d21$Variante <- factor(d21$Variante, levels = unique(d21$Variante))

d22$Monat <- factor(d22$Monat, levels = unique(d22$Monat))
d22$Variante <- factor(d22$Variante, levels = unique(d22$Variante))

d23$Monat <- factor(d23$Monat, levels = unique(d23$Monat))
d23$Variante <- factor(d23$Variante, levels = unique(d23$Variante))

dUrtyp$Woche <- factor(dUrtyp$Woche, levels = unique(dUrtyp$Woche))
dUrtyp$Monat <- factor(dUrtyp$Monat, levels = unique(dUrtyp$Monat))
dUrtyp$Variante <- factor(dUrtyp$Variante, levels = unique(dUrtyp$Variante))

dAlpha$Woche <- factor(dAlpha$Woche, levels = unique(dAlpha$Woche))
dAlpha$Monat <- factor(dAlpha$Monat, levels = unique(dAlpha$Monat))
dAlpha$Variante <- factor(dAlpha$Variante, levels = unique(dAlpha$Variante))

dDelta$Woche <- factor(dDelta$Woche, levels = unique(dDelta$Woche))
dDelta$Monat <- factor(dDelta$Monat, levels = unique(dDelta$Monat))
dDelta$Variante <- factor(dDelta$Variante, levels = unique(dDelta$Variante))

dOmikron$Woche <- factor(dOmikron$Woche, levels = unique(dOmikron$Woche))
dOmikron$Monat <- factor(dOmikron$Monat, levels = unique(dOmikron$Monat))
dOmikron$Variante <- factor(dOmikron$Variante, levels = unique(dOmikron$Variante))

impf$Monat <- factor(impf$Monat, levels = unique(impf$Monat))
impf20$Monat <- factor(impf20$Monat, levels = unique(impf20$Monat))
impf21$Monat <- factor(impf21$Monat, levels = unique(impf21$Monat))
impf22$Monat <- factor(impf22$Monat, levels = unique(impf22$Monat))
impf23$Monat <- factor(impf23$Monat, levels = unique(impf23$Monat))



##wird hierher verschoben, da normierte Datensätze d20weekws etc verwendet werden
## FUNKTIONSWEISE AGGREGATION:
##(https://stackoverflow.com/questions/10202480/aggregate-rows-by-shared-values-in-a-variable)






## UNTERSUCHUNG: FAELLE PRO TAG/WOCHE FUER 20/21/22 (+ Ermittlung von Maximalwerten):
## 2020
aggCasesPerDay20 <- aggregate(AnzahlFall ~ Refdatum, FUN = sum, data = d20weeks)
aggCasesPerWeek20 <- aggregate(AnzahlFall ~ Woche, FUN = sum, data = d20weeks)
##Problem: ich versuche hiermit die wochen tabelle zu ordnen, klappt auch irgendwie
## aber im server-code (habe ich auskommentiert oder at this point schon gelöscht), ist der barplot den ich daraus mache immernoch fcking falsch sortiert... hääääääääääää?????
testSortedCpW20 <- aggCasesPerWeek20[order(as.numeric(as.character(aggCasesPerWeek20$Woche))),]

maxCasesPerDay20 <- max(aggCasesPerDay20$AnzahlFall)
maxCasesPerWeek20 <- max(aggCasesPerWeek20$AnzahlFall)

## 2021
aggCasesPerDay21 <- aggregate(AnzahlFall ~ Refdatum, FUN = sum, data = d21weeks)
aggCasesPerWeek21 <- aggregate(AnzahlFall ~ Woche, FUN = sum, data = d21weeks)

maxCasesPerDay21 <- max(aggCasesPerDay21$AnzahlFall)
maxCasesPerWeek21 <- max(aggCasesPerWeek21$AnzahlFall)

## 2022
aggCasesPerDay22<-aggregate(AnzahlFall ~ Refdatum, FUN = sum, data = d22weeks)
aggCasesPerWeek22<-aggregate(AnzahlFall ~ Woche, FUN = sum, data = d22weeks)

maxCasesPerDay22 <- max(aggCasesPerDay22$AnzahlFall)
maxCasesPerWeek22 <- max(aggCasesPerWeek22$AnzahlFall)

##2023
## 2022
aggCasesPerDay23<-aggregate(AnzahlFall ~ Refdatum, FUN = sum, data = d23weeks)
aggCasesPerWeek23<-aggregate(AnzahlFall ~ Woche, FUN = sum, data = d23weeks)

maxCasesPerDay23 <- max(aggCasesPerDay23$AnzahlFall)
maxCasesPerWeek23 <- max(aggCasesPerWeek23$AnzahlFall)






## UNTERSUCHUNG: TODE PRO TAG/WOCHE FUER 20/21/22 (+ Ermittlung von Maximalwerten):
## 2020
aggDeathsPerDay20 <- aggregate(AnzahlTodesfall ~ Refdatum, FUN = sum, data = d20weeks)
aggDeathsPerWeek20 <- aggregate(AnzahlTodesfall ~ Woche, FUN = sum, data = d20weeks)

maxDeathsPerDay20 <- max(aggDeathsPerDay20$AnzahlTodesfall)
maxDeathsPerWeek20 <- max(aggDeathsPerWeek20$AnzahlTodesfall)

## 2021
aggDeathsPerDay21 <- aggregate(AnzahlTodesfall ~ Refdatum, FUN = sum, data = d21weeks)
aggDeathsPerWeek21 <- aggregate(AnzahlTodesfall ~ Woche, FUN = sum, data = d21weeks)

maxDeathsPerDay21 <- max(aggDeathsPerDay21$AnzahlTodesfall)
maxDeathsPerWeek21 <- max(aggDeathsPerWeek21$AnzahlTodesfall)

## 2022
aggDeathsPerDay22<-aggregate(AnzahlTodesfall ~ Refdatum, FUN = sum, data = d22weeks)
aggDeathsPerWeek22<-aggregate(AnzahlTodesfall ~ Woche, FUN = sum, data = d22weeks)

maxDeathsPerDay22 <- max(aggDeathsPerDay22$AnzahlTodesfall)
maxDeathsPerWeek22 <- max(aggDeathsPerWeek22$AnzahlTodesfall)

## 2022
aggDeathsPerDay23<-aggregate(AnzahlTodesfall ~ Refdatum, FUN = sum, data = d23weeks)
aggDeathsPerWeek23<-aggregate(AnzahlTodesfall ~ Woche, FUN = sum, data = d23weeks)

maxDeathsPerDay23 <- max(aggDeathsPerDay23$AnzahlTodesfall)
maxDeathsPerWeek23 <- max(aggDeathsPerWeek23$AnzahlTodesfall)

# ------------------------------------------------------------------------------------- 


##wird benötigt, um die y-Achse in der Auswertung der Todesfälle im Verhältnis zu den
##verabreichten Impfungen zu skalieren
helper20<-aggregate(cbind(AnzahlFall, AnzahlTodesfall)~Woche, FUN = sum, data = d20weeks)
value_helper20<-max(helper20$AnzahlTodesfall/helper20$AnzahlFall)
helper21<-aggregate(cbind(AnzahlFall, AnzahlTodesfall)~Woche, FUN = sum, data = d21weeks)
value_helper21<-max(helper21$AnzahlTodesfall/helper21$AnzahlFall)
helper22<-aggregate(cbind(AnzahlFall, AnzahlTodesfall)~Woche, FUN = sum, data = d22weeks)
value_helper22<-max(helper22$AnzahlTodesfall/helper22$AnzahlFall)
helper23<-aggregate(cbind(AnzahlFall, AnzahlTodesfall)~Woche, FUN = sum, data = d23weeks)
value_helper23<-max(helper23$AnzahlTodesfall/helper23$AnzahlFall)






# ⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿
# (random zeichen, damit ich beim scrollen direkt sehe, dass ich jetzt bei der ui bin)
## SHINY LOGIC:
# Define UI for application that draws a histogram
ui <- fluidPage( 
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ----------------------------------------------------------
  #
  #                  - New UI Approach -
  #
  # ----------------------------------------------------------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # Application title
  titlePanel("Covid19 - Berlin"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("varUntersuchungsMerkmal", label = "Sollen Fälle oder Todesfälle untersucht werden?",
                   choices = list("Fälle" = 1, "Todesfälle" = 2), selected = 1),
      selectInput("varZeitraumArt", label = "Art des Zeitraums:",
                  choices = list("Jahr" = 1, "Covid-Variante" = 2), selected = 1),
      # conditional panels, siehe: https://shiny.rstudio.com/reference/shiny/1.6.0/conditionalpanel
      conditionalPanel(
        condition = "input.varZeitraumArt == 1",
        radioButtons("varJahr", label = "Welches Jahr soll betrachtet werden?",
                     choices = list("2020" = 1, "2021" = 2, "2022" = 3, "2023" = 4), selected = 1)
      ),
      conditionalPanel(
        condition = "input.varZeitraumArt == 2",
        radioButtons("varVariante", label = "Welche Covid-Variante soll betrachtet werden?",
                     choices = list("Urtyp" = 1, "Alpha" = 2, "Delta"= 3, "Omikron" = 4), selected = 1)
      ),
      selectInput("varBetrachtungsArt", label = "Zeitlichen Verlauf oder bestimmtes Merkmal analysieren?",
                  choices = list("Zeit. Verlauf" = 1, "Merkmal" = 2)),
      conditionalPanel(
        condition = "input.varBetrachtungsArt == 1",
        radioButtons("varZeitEinheit", label = "Unterscheidung nach...",
                     choices = list("Wochen" = 1, "Monate" = 2, "Jahre" = 3), selected = 1)
      ),
      conditionalPanel(
        condition = "input.varBetrachtungsArt == 2",
        radioButtons("varMerkmalEinheit", label = "Unterscheidung nach...",
                     choices = list("Landkreis" = 1, "Geschlecht" = 2, "Altersgruppe" = 3, "Variante" = 4), selected = 1)
      ),
      radioButtons("varUnterteilungsArt", label = "Wonach sollen die Ausprägungen unterteilt sein?",
                   choices = list("Landkreis" = 1, "Geschlecht" = 2, "Altersgruppe" = 3, "Variante" = 4), selected = 1),
      h5("━━━━━━━━━━"),
      checkboxInput("varPercPlotBool", label = "Anteile der Unterteilungen abbilden", value = FALSE),
      checkboxInput("varFlipBool", label = "Flip Diagramm (kann Beschriftungen besser sichtbar machen)", value = FALSE)
    ),
    mainPanel(
      # für tabsetPanel und tabPanel, siehe: https://shiny.rstudio.com/reference/shiny/0.14/tabsetpanel
      tabsetPanel(
        tabPanel("Diagramm", plotOutput("barPlotFallTot")),
        tabPanel("Zusammenfassung", verbatimTextOutput("zusammenfassung")),
        tabPanel("Beschreibung", textOutput("beschreibung"))
      )
    ),
    position = c("left", "right"),
    fluid = FALSE
  ),
  # br() aus: https://community.rstudio.com/t/spacing-between-plots/2356
  br(),
  br(),
  h2("Impfungen in Berlin - Covid19"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("varImpfJahr", label = "Jahr (2000er)",
                  min = 20, max = 23, value = 21),
      radioButtons("varImpfZeitEinheit", label = "Pro...",
                   choices = list("Woche" = 1, "Monat" = 2, "Jahr" = 3), selected = 1),
      checkboxInput("varImpfPercPlotBool", label = "Anteile der Unterteilungen abbilden", value = FALSE)
    ),
    mainPanel(plotOutput("barPlotImpf"))
  ),
  br(),
  br(),
  h2("Relation: Anz. Infektionen und Anz. Todesfälle - im Bezug auf die Altersgruppe"),
  
  # ----------------------------------------------------------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ----------------------------------------------------------
  
  
  
  
  sidebarPanel(
    selectInput("Altersgruppe", label="Wählen Sie eine Altersgruppe", choices = list("unbekannt"=1, "0 bis 4 Jahre"=2,"5 bis 14 Jahre"=3, "15 bis 34 Jahre"=4,"35 bis 59 Jahre"=5,"60 bis 79 Jahre"=6,"über 80 Jahre"=7, "Gesamt"=8), selected = 8),
    radioButtons("Variante", label="Welche Virusvariante soll betrachtet werden?", choices = list("Urtyp"=1, "Alpha"=2, "Delta"=3, "Omikron"=4, "Alle zusammen"=5), selected = 5)
    
  ),
  mainPanel(
    textOutput("TextAlter"),
    plotOutput("VerhaeltnisAlter")
  ),
  
  br(),
  br(),
  h2("Relation zwischen total verabreichten IMPFDOSEN und prozentualer TODESRATE nach Infektion"),
  sidebarPanel(      sliderInput(inputId="Woche", label="KalenderWoche", min=1, max=52, value=1),
                     radioButtons("Jahr", label="Welches Kalenderjahr soll betrachtet werden?", choices=list("2021"=1, "2022"=2), selected=1)),
  
  mainPanel(
    fluidRow(
      column(6, align="right",
             plotOutput("impfungen_Woche")
      ),
      column(6, align="right",
             plotOutput("tode_Woche")))
    
    
  )
)





# ⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿⦿
server <- function(input, output) {
  
  var_Woche<-reactive({
    return(input$Woche)
  })
  
  
  ##gibt einen vektor mit 3 Summen zurück:
  ##var_jahr()[1]: die Fälle pro Woche
  ##var_jahr()[2]: die todesfälle pro Woche
  ##var_jahr()[3]: kumulierte Anzahl der Impfungen 2 Woche vor der gewählten Kalenderwoche 
  ##2 Wochen vorher, weil eine Wirksamkeit der Impfstoffe erst nach 2 Wochen angenommen wird
  ##dies wurde kurz mit Prof. Spott besprochen, auf weitere Belege wird an dieser Stelle verzichtet
  var_jahr<-reactive({
    
    if (var_Woche()==1)(
      switch (as.character(input$Jahr),
              "1" = return (c(sum(d21weeks$AnzahlFall[d21weeks$Woche==var_Woche()]), (helper21$AnzahlTodesfall[helper21$Woche==var_Woche()]/helper21$AnzahlFall[helper21$Woche==var_Woche()]), max(impfungen_b$Impfungen_Gesamt[(impfungen_b$Woche==max(impfungen_b$Woche)-1)&(impfungen_b$Jahr==2020)]))),
              "2" = return (c(sum(d22weeks$AnzahlFall[d22weeks$Woche==var_Woche()]), (helper22$AnzahlTodesfall[helper22$Woche==var_Woche()]/helper22$AnzahlFall[helper22$Woche==var_Woche()]),max(impfungen_b$Impfungen_Gesamt[(impfungen_b$Woche==max(impfungen_b$Woche)-2)&(impfungen_b$Jahr==2021)])))
      )
    )
    if (var_Woche()==2)(
      switch (as.character(input$Jahr),
              "1" = return (c(sum(d21weeks$AnzahlFall[d21weeks$Woche==var_Woche()]), (helper21$AnzahlTodesfall[helper21$Woche==var_Woche()]/helper21$AnzahlFall[helper21$Woche==var_Woche()]),impfungen_b$Impfungen_Gesamt[(impfungen_b$Woche==max(impfungen_b$Woche))&(impfungen_b$Jahr==2020)])),
              "2" = return (c(sum(d22weeks$AnzahlFall[d22weeks$Woche==var_Woche()]), (helper22$AnzahlTodesfall[helper22$Woche==var_Woche()]/helper22$AnzahlFall[helper22$Woche==var_Woche()]),max(impfungen_b$Impfungen_Gesamt[(impfungen_b$Woche==max(impfungen_b$Woche)-1)&(impfungen_b$Jahr==2021)])))
      ))
    else(
      switch(
        as.character(input$Jahr),
        "1" = return (c(sum(d21weeks$AnzahlFall[d21weeks$Woche==var_Woche()]), (helper21$AnzahlTodesfall[helper21$Woche==var_Woche()]/helper21$AnzahlFall[helper21$Woche==var_Woche()]), impfungen_b$Impfungen_Gesamt[(impfungen_b$Woche==var_Woche()-2)&(impfungen_b$Jahr==2021)])),
        "2" = return (c(sum(d22weeks$AnzahlFall[d22weeks$Woche==var_Woche()]), (helper22$AnzahlTodesfall[helper22$Woche==var_Woche()]/helper22$AnzahlFall[helper22$Woche==var_Woche()]),impfungen_b$Impfungen_Gesamt[(impfungen_b$Woche==var_Woche()-2)&(impfungen_b$Jahr==2022)]))
      )
    )
  })
  
  
  ##die y-Achsen sind hier unterschiedlich! Ich weiß nicht so recht, wie damit umzugehen - aktuell ist das 
  ##maximum immer der maximal vorkommende wert - so verhalten sich immerhin alle 3 Plots zu ihrem maximum (also quasi zu 100%)
  output$impfungen_Woche<-renderPlot(barplot(main="Anzahl der INSGESAMT verabreichten IMPFDOSEN bis vor 2 Wochen",var_jahr()[3],ylim=c(0,max(impfungen_b$Impfungen_Gesamt)), col="#c5f587"))
  output$tode_Woche<-renderPlot(barplot(main="Prozentualer Anteil TODESFÄLLE ",var_jahr()[2],ylim=c(0,max(value_helper21, value_helper22)), col="#f58787"))
  
  
  ##gibt reduzierte Datensätze zurück - im jeweiligen Zeitraum war die Variante mit >50% vertreten
  var_variant<-reactive({
    switch(
      as.character(input$Variante),
      "1" = return (dUrtyp),
      "2" = return (dAlpha),
      "3" = return (dDelta),
      "4" = return (dOmikron),
      "5" = return (base)
    )
  })
  
  
  ##dieser Funktion gibt Werte in Abhängigkeit des  gewählten Altes aus - ich schlage vor, dass weitere altersabhängige Werte ebenfalls hier erfasst werden - solange die Reihenfolge erhalten bleibt, sollte das kein Problem sein.
  ##aktuell gibt var_age()[1] die Fallzahl abhängig vom Alter, var_age()[2] die Anzahl der Todesfälle abhängig vom Alter zurück. Ggf. in diesem Stil weiter dokumentieren
  var_age<-reactive({
    
    digitVal <- 3
    
    if(input$Altersgruppe == 1)({
      digitVal <- 6
    })
    
    if(input$Altersgruppe == 8)({
      return (c(sum(var_variant()$AnzahlFall),sum(var_variant()$AnzahlTodesfall), signif((sum(var_variant()$AnzahlTodesfall)/sum(var_variant()$AnzahlFall))*100,digits=digitVal)))
    })
    
    stringAltersgruppe <- switch(
      as.character(input$Altersgruppe),
      "1" = "unbekannt",
      "2" = "A00-A04",
      "3" = "A05-A14",
      "4" = "A15-A34",
      "5" = "A35-A59",
      "6" = "A60-A79",
      "7" = "A80+"
    )
    
    return (c(sum(var_variant()$AnzahlFall[var_variant()$Altersgruppe==stringAltersgruppe]),sum(var_variant()$AnzahlTodesfall[var_variant()$Altersgruppe==stringAltersgruppe]),signif((sum(var_variant()$AnzahlTodesfall[var_variant()$Altersgruppe==stringAltersgruppe])/sum(var_variant()$AnzahlFall[var_variant()$Altersgruppe==stringAltersgruppe]))*100,digits=digitVal)))
    
  })
  
  output$TextAlter<-renderText({paste0("Anzahl der Infektionen in der gewählten Altersgruppe insgesamt:", var_age()[1], " davon Todesfälle: ",  var_age()[2], " (",var_age()[3],"%)") })
  
  
  plotAlter<-reactive({
    
    zustand<-rep(c( "davon Todesfälle", "Infektionen"))
    zustand<-factor(zustand, levels = c("Infektionen", "davon Todesfälle" ))
    
    minuend <- var_age()[2] ## vor dem return wird innerhalb der variable zahlen, minus diese variable gerechnet
    
    if (input$Altersgruppe == 1) ({
      minuend <- 0; ## im Fall Altergruppse == 1 wird nichts Minusgerechnet (war in Antons code zumindest so)
    })
    
    stringAltersrange <- switch(
      as.character(input$Altersgruppe),
      "1" = "Alter unbekannt",
      "2" = "0 - 4",
      "3" = "05-14",
      "4" = "15 - 34",
      "5" = "35-59",
      "6" = "60 bis 79",
      "7" = "80+",
      "8" = "Gesamt"
    )
    
    zahlen<-rep(c(var_age()[2], var_age()[1]-minuend)) ## für minuend, siehe variable oben
    frame<-data.frame(zustand, zahlen)
    return (ggplot(frame, aes(fill=zustand, y=zahlen, x=stringAltersrange))+  geom_bar(position='stack', stat='identity') + xlab("Anzahl Fälle")+ylab("Altersgruppe")+scale_fill_manual(values=c("#649be8", "#f58787")))
    
  })
  
  output$VerhaeltnisAlter<-renderPlot({plotAlter()})
  
  
  
  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ----------------------------------------------------------
  #
  #                 - New Server Approach -
  #
  # ----------------------------------------------------------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # ------------------------------------------------------------------------------------- UPDATE-SUMMARY
  updateSummary <- reactive({
    # renderPrint() für summarys, nach: https://stackoverflow.com/questions/24701806/r-shiny-output-summary-statistics
    output$zusammenfassung <- renderPrint({summary(getDataFrame())})
  })
  
  # ------------------------------------------------------------------------------------- GET-COLOR-PALETTE
  # Erstellt und returns eine Farbpalette, die für die farbliche bar-Unterteilung im barPlotFallTot genutzt wird
  getColorPalette <- reactive({
    
    # da im späteren barPlotFallTot die bars farblich unterteilt werden, braucht es bei vielen unterteilungen color-paletts
    # dafür nutze ich die brewer-paletten, siehe: https://rdrr.io/cran/RColorBrewer/man/ColorBrewer.html
    #
    # speziell nutze ich die "Blues"-Palette für Fälle und die "Reds"-Palette für Tode
    # beide sind allerdings nur 9 Farben lang, beim Merkmal "Landkreis" gibt es allerdings 12 Ausprägungen
    # in diesem Fall verlängere ich die Paletten mit der Funktion "colorRampPalette", siehe:
    # https://www.datanovia.com/en/blog/easy-way-to-expand-color-palettes-in-r/
    
    # aus: https://htmlcolorcodes.com/
    if(input$varUnterteilungsArt == 2){return(c("#B7EA4B", "#4BD3EA", "#EA754B"))}
    
    # für switch-case, siehe: https://www.geeksforgeeks.org/switch-case-in-r/
    # warum mache ich alles immer erst zum character? --> weil es bei mir anders warum auch immer nicht funktioniert
    colorType <- switch(as.character(input$varUntersuchungsMerkmal),
                        "1" = "Blues",
                        "2" = "Reds")
    
    numOfSubdivs <- switch(as.character(input$varUnterteilungsArt),
                           "1" = 12,
                           "2" = 3,
                           "3"= 7,
                           "4" = 4)
    
    colorPalette <- colorRampPalette(brewer.pal(9, colorType))(as.numeric(numOfSubdivs))
    
    colorPalette[1] <- "#E3E3E3" #da die erste farbe nicht sichtbar genug ist
      
    return(colorPalette)
    
  })
  
  # ------------------------------------------------------------------------------------- GET-DATA-FRAME
  getDataFrame <- reactive({
    
    if(input$varZeitraumArt == 1) { #falls jahre untersucht werden
      if(input$varBetrachtungsArt == 1 & input$varZeitEinheit == 1) { #falls x-achse nach wochen (dann ist der Zeitraum nicht genau das Jahr)
        switch(as.character(input$varJahr),
               "1" = return(d20weeks),
               "2" = return(d21weeks),
               "3" = return(d22weeks),
               "4" = return(d23weeks))
      }
      else { #wenn x-achse nicht nach wochen aufgeteilt ist
        switch(as.character(input$varJahr),
               "1" = return(d20),
               "2" = return(d21),
               "3" = return(d22),
               "4" = return(d23))
      }
    }
    else { #wenn covid-varianten untersucht werden
      switch(as.character(input$varVariante),
             "1" = return(dUrtyp),
             "2" = return(dAlpha),
             "3" = return(dDelta),
             "4" = return(dOmikron))
    }
  })
  
  # ------------------------------------------------------------------------------------- GET-X-AXIS-ATTRIBUTE
  getXatt <- reactive({ #att = attribute
    df <- getDataFrame()
    if(input$varBetrachtungsArt == 1) { #betrachtung von zeiteinheiten (wochen, monate, jahre)
      switch(as.character(input$varZeitEinheit),
             "1" = return(df$Woche),
             "2" = return(df$Monat),
             "3" = return(df$Jahr))
    }
    else { #betrachtung von merkmalen
      switch(as.character(input$varMerkmalEinheit),
             "1" = return(df$Landkreis),
             "2" = return(df$Geschlecht),
             "3" = return(df$Altersgruppe),
             "4" = return(df$Variante))
    }
  })
  
  # ------------------------------------------------------------------------------------- GET-Y-AXIS-ATTRIBUTE
  getYatt <- reactive({
    df <- getDataFrame()
    switch(as.character(input$varUntersuchungsMerkmal),
           "1" = return(df$AnzahlFall),
           "2" = return(df$AnzahlTodesfall))
  })
  
  # ------------------------------------------------------------------------------------- GET-UNTERTEILUNGS-ATT
  getUnterteilungsAtt <- reactive({ #att = attribute
    df <- getDataFrame()
    switch(as.character(input$varUnterteilungsArt),
           "1" = return(df$Landkreis),
           "2" = return(df$Geschlecht),
           "3" = return(df$Altersgruppe),
           "4" = return(df$Variante))
  })
  
  # ------------------------------------------------------------------------------------- BUILD PLOT
  barPlotFallTot <- reactive({
    
    updateSummary()
    
    # für folgendes code-Verständnis: siehe https://www.youtube.com/watch?v=n_ACYLWUmos
    # und unter: http://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization
    #
    # ich nutze einen "coord_flip", da ein flip der x- und y-Achse des bar plots, bei vielen Einträgen auf der x-Achse deutlich übersichtlicher ist
    # leider reversed der coord flip die x-achse, was zwar fixable mit "forcats::fct_rev(spalte)" ist, siehe: 
    # https://stackoverflow.com/questions/34227967/reversed-order-after-coord-flip-in-r
    # aber nur bei factors funktioniert (also bei uns bei Monaten und Varianten)
    # ich weiß nicht ob es sinn macht alles plötzlich deshalb zu factors umzuwandeln, da der flip nicht so schlimm ist, lasse ich es daher so
    
    if(input$varFlipBool == TRUE) { #falls diagramm geflippt werden soll
      if(input$varPercPlotBool == TRUE) { #falls die anteile abgebildet werden sollen
        return(getDataFrame() %>% 
                 ggplot(aes(x = getXatt(), y = getYatt(), fill = getUnterteilungsAtt())) +
                 # für geom_col, siehe: https://r-graphics.org/recipe-bar-graph-proportional-stacked-bar
                 geom_col(position = "fill") + #diese line macht bei verzweigung 2 den unterschied (sie ersetzt "geom_bar")
                 scale_y_continuous(labels = scales::percent) +
                 coord_flip() + #diese line macht bei verzweigung-1 den unterschied
                 theme_minimal() +
                 labs(x = "x",
                      y = "y", 
                      title = "Title") + 
                 scale_fill_manual(values = getColorPalette()))
      }
      else { #falls nicht die anteile abgebildet werden sollen
        return(getDataFrame() %>% 
                 ggplot(aes(x = getXatt(), y = getYatt(), fill = getUnterteilungsAtt())) +
                 geom_bar(stat = "identity") +
                 coord_flip() + #diese line macht den unterschied
                 theme_minimal() +
                 labs(x = "x",
                      y = "y", 
                      title = "Title") + 
                 scale_fill_manual(values = getColorPalette()))
      }
    }
    else { #wenn das Diagramm nicht geflippt sein soll
      if(input$varPercPlotBool == TRUE) { #falls die anteile abgebildet werden sollen
        return(getDataFrame() %>% 
                 ggplot(aes(x = getXatt(), y = getYatt(), fill = getUnterteilungsAtt())) +
                 geom_col(position = "fill") + #diese line macht bei verzweigung 2 den unterschied (sie ersetzt "geom_bar")
                 theme_minimal() +
                 labs(x = "x",
                      y = "y", 
                      title = "Title") + 
                 scale_fill_manual(values = getColorPalette()))
      }
      else { #falls nicht die anteile abgebildet werden sollen
        return(getDataFrame() %>% 
                 ggplot(aes(x = getXatt(), y = getYatt(), fill = getUnterteilungsAtt())) +
                 geom_bar(stat = "identity") +
                 theme_minimal() +
                 labs(x = "x",
                      y = "y", 
                      title = "Title") + 
                 scale_fill_manual(values = getColorPalette()))
      }
    }
    
    # es gibt leider bei den stacked percentage barplots manchmal probleme (die bars extenden zu -100%)
    # nach: https://stackoverflow.com/questions/13734368/ggplot2-and-a-stacked-bar-chart-with-negative-values
    # glaube ich, dass es an negativen values innerhalb der y-spalte liegt
    # gibt man "base[base$AnzahlFall < 0,]" (bei untersuchung von anzahl fall relevant) in die konsole ein, sieh man,
    # dass es viele von eben solchen negativen Werten gibt, die das problem verursachen könnten
    # ich habe bisher noch keine lösung gefunden, allerdings ist das problem aus meiner sicht nicht gravieren,
    # man kann im percentage bar plot trotzdem die wichtigen sachen ablesen
    
  })
  
  # was hat es mit "height" auf sich? siehe: https://stackoverflow.com/questions/17838709/scale-and-size-of-plot-in-rstudio-shiny}
  output$barPlotFallTot <- renderPlot({return(barPlotFallTot())}, height = 670)
  
  
  
  
  getImpfDF <- reactive({
    switch(as.character(input$varImpfJahr),
           "20" = return(impf20),
           "21" = return(impf21),
           "22" = return(impf22),
           "23" = return(impf23))
  })
  
  getImpfX <- reactive({
    switch(as.character(input$varImpfZeitEinheit),
           "1" = return(getImpfDF()$Woche),
           "2" = return(getImpfDF()$Monat),
           "3" = return(getImpfDF()$Jahr))
  })
  
  barPlotImpf <- reactive({
    if(input$varImpfPercPlotBool == TRUE) {
      return(getImpfDF() %>% 
               ggplot(aes(x = getImpfX(), y = getImpfDF()$Anzahl, fill = getImpfDF()$ImpfstoffGruppiert)) +
               geom_col(position = "fill") +
               theme_minimal() +
               labs(x = "x",
                    y = "y", 
                    title = "Title") + 
               scale_fill_manual(values = rev(c("#006d2c", "#238b45", "#41ab5d", "#74c476", "#a1d99b", "#c7e9c0"))))
    }
    else {
      return(getImpfDF() %>% 
               ggplot(aes(x = getImpfX(), y = getImpfDF()$Anzahl, fill = getImpfDF()$ImpfstoffGruppiert)) +
               geom_bar(stat = "identity") +
               theme_minimal() +
               labs(x = "x",
                    y = "y", 
                    title = "Title") + 
               scale_fill_manual(values = rev(c("#006d2c", "#238b45", "#41ab5d", "#74c476", "#a1d99b", "#c7e9c0"))))
    }
    
  })
  
  output$barPlotImpf <- renderPlot({return(barPlotImpf())})
  
}

# Run the application 
shinyApp(ui = ui, server = server)