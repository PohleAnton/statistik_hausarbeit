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

##Daten über die Impfkampangne vom RKI, sortiert nach bundesländern:
##https://github.com/robert-koch-institut/COVID-19-Impfungen_in_Deutschland/blob/master/Aktuell_Deutschland_Bundeslaender_COVID-19-Impfungen.csv

impfungen<-read.csv2("./Data/impfungen.csv", header = T, sep = ",")

##aus den zurückgehenden Todesfällen v.a. bei Delta im Vergleich zu den weniger letalen Vorgängern ergibt sich ein Interesse für die Impfkampagne:

##Impfungen in Berlin:
##hier wird nur der Landkreis des Impfortes angegeben, nicht der Meldeadresse der geimpften Person. (Vermutlich aus Datenschutzgründen)
impfungen_b<-impfungen[impfungen$BundeslandId_Impfort==11,]
## return: data als alle covid-daten (mit dates als typ date), impfungen als alle impfdaten, impfungen_b als alle impfdaten für berlin (die für uns relevanten daten)






## GRUNDSAETZLICHE FESTLEGUNGEN:
## Wenn wir Covid Faelle untersuchen, gehen wir immer vom Refdatum aus
## Wenn wir Impfungen untersuchen, danach aggregieren etc. verwenden wir imemr die totalen Impfdosen verabreicht, nicht die Anzahl der erhaltenen Impfungen







## AUFBEREITEN VON GRUNDDATEN FUER 2020 - (hinzufuegen einer Wochen-spalte)
##in 2020
impf20<-impfungen_b[impfungen_b$Impfdatum < '2021-01-01',]
##fügt Wochenspalte hinzu
impf20$Woche<-strftime(impf20$Impfdatum, format="%V")
##remove leading zeros von "Woche":
impf20$Woche<-sub("^0+","",impf20$Woche)
## Daten gesamt in 2020
data20 <- data[data$Refdatum < '2021-01-01',]
data20$Woche <- strftime(data20$Refdatum, format = "%V")
## fordere Nullen von "Woche" löschen:
data20$Woche<-sub("^0+","",data20$Woche)
## return: impf20 als alle impfdaten für 2020, data20 als alle covid daten für 2020 - (beide mit Wochenangaben)






## AUFBEREITEN VON GRUNDDATEN FUER 2021 - (hinzufuegen einer Wochen-spalte)
##in 2021 
impf21<-impfungen_b[impfungen_b$Impfdatum>'2020-12-31' & impfungen_b$Impfdatum<'2022-01-01',]
##fügt Wochenspalte hinzu
impf21$Woche<-strftime(impf21$Impfdatum, format="%V")
##remove leading zeros von "Woche":
impf21$Woche<-sub("^0+","",impf21$Woche)
##Daten gesamt in 2021
data21<-data[data$Refdatum>'2020-12-31' & data$Refdatum<'2022-01-01',]
data21$Woche<-strftime(data21$Refdatum, format="%V")
##remove leading zeros von "Woche":
data21$Woche<-sub("^0+","",data21$Woche)
## return: impf21 als alle impfdaten für 2021, data21 als alle covid daten für 2021 - (beide mit Wochenangaben)






## AUFBEREITEN VON GRUNDDATEN FUER 2022 - (hinzufuegen einer Wochen-spalte)
##in 2022
impf22<-impfungen_b[impfungen_b$Impfdatum>'2021-12-31',]
##fügt eine Wochenspaltehinzu
impf22$Woche<-strftime(impf22$Impfdatum, format="%V")
##remove leading zeros von "Woche":
impf22$Woche<-sub("^0+","",impf22$Woche)
##Daten gesamt in 2022
data22<-data[data$Refdatum>'2021-12-31' & data$Refdatum<'2023-01-01',]
##fügt eine Wochenspalte hinzu
data22$Woche<-strftime(data22$Refdatum, format="%V")
##remove leading zeros von "Woche":
data22$Woche<-sub("^0+","",data22$Woche)
## return: impf22 als alle impfdaten für 2022, data22 als alle covid daten für 2022 - (beide mit Wochenangaben)






## FUNKTIONSWEISE AGGREGATION:
##(https://stackoverflow.com/questions/10202480/aggregate-rows-by-shared-values-in-a-variable)






## UNTERSUCHUNG: FAELLE PRO TAG/WOCHE FUER 20/21/22 (+ Ermittlung von Maximalwerten):
## 2020
aggCasesPerDay20 <- aggregate(AnzahlFall ~ Refdatum, FUN = sum, data = data20)
aggCasesPerWeek20 <- aggregate(AnzahlFall ~ Woche, FUN = sum, data = data20)
##Problem: ich versuche hiermit die wochen tabelle zu ordnen, klappt auch irgendwie
## aber im server-code (habe ich auskommentiert), ist der barplot den ich daraus mache immernoch fcking falsch sortiert... hääääääääääää?????
testSortedCpW20 <- aggCasesPerWeek20[order(as.numeric(as.character(aggCasesPerWeek20$Woche))),]

maxCasesPerDay20 <- max(aggCasesPerDay20$AnzahlFall)
maxCasesPerWeek20 <- max(aggCasesPerWeek20$AnzahlFall)

## 2021
aggCasesPerDay21 <- aggregate(AnzahlFall ~ Refdatum, FUN = sum, data = data21)
aggCasesPerWeek21 <- aggregate(AnzahlFall ~ Woche, FUN = sum, data = data21)

maxCasesPerDay21 <- max(aggCasesPerDay21$AnzahlFall)
maxCasesPerWeek21 <- max(aggCasesPerWeek21$AnzahlFall)

## 2022
aggCasesPerDay22<-aggregate(AnzahlFall ~ Refdatum, FUN = sum, data = data22)
aggCasesPerWeek22<-aggregate(AnzahlFall ~ Woche, FUN = sum, data = data22)

maxCasesPerDay22 <- max(aggCasesPerDay22$AnzahlFall)
maxCasesPerWeek22 <- max(aggCasesPerWeek22$AnzahlFall)







## UNTERSUCHUNG: TODE PRO TAG/WOCHE FUER 20/21/22 (+ Ermittlung von Maximalwerten):
## 2020
aggDeathsPerDay20 <- aggregate(AnzahlTodesfall ~ Refdatum, FUN = sum, data = data20)
aggDeathsPerWeek20 <- aggregate(AnzahlTodesfall ~ Woche, FUN = sum, data = data20)

maxDeathsPerDay20 <- max(aggDeathsPerDay20$AnzahlTodesfall)
maxDeathsPerWeek20 <- max(aggDeathsPerWeek20$AnzahlTodesfall)

## 2021
aggDeathsPerDay21 <- aggregate(AnzahlTodesfall ~ Refdatum, FUN = sum, data = data21)
aggDeathsPerWeek21 <- aggregate(AnzahlTodesfall ~ Woche, FUN = sum, data = data21)

maxDeathsPerDay21 <- max(aggDeathsPerDay21$AnzahlTodesfall)
maxDeathsPerWeek21 <- max(aggDeathsPerWeek21$AnzahlTodesfall)

## 2022
aggDeathsPerDay22<-aggregate(AnzahlTodesfall ~ Refdatum, FUN = sum, data = data22)
aggDeathsPerWeek22<-aggregate(AnzahlTodesfall ~ Woche, FUN = sum, data = data22)

maxDeathsPerDay22 <- max(aggDeathsPerDay22$AnzahlTodesfall)
maxDeathsPerWeek22 <- max(aggDeathsPerWeek22$AnzahlTodesfall)







## UNTERSUCHUNG: IMPFUNGEN PRO TAG/WOCHE FUER 20/21/22 (+ Ermittlung von Maximalwerten):
## 2020
aggImpfsPerDay20 <- aggregate(Anzahl ~ Impfdatum, FUN = sum, data = impf20)
aggImpfsPerWeek20 <- aggregate(Anzahl ~ Woche, FUN = sum, data = impf20)

maxImpfsPerDay20 <- max(aggImpfsPerDay20$Anzahl)
maxImpfsPerWeek20 <- max(aggImpfsPerWeek20$Anzahl)

## 2021
aggImpfsPerDay21 <- aggregate(Anzahl ~ Impfdatum, FUN = sum, data = impf21)
aggImpfsPerWeek21 <- aggregate(Anzahl ~ Woche, FUN = sum, data = impf21)

maxImpfsPerDay21 <- max(aggImpfsPerDay21$Anzahl)
maxImpfsPerWeek21 <- max(aggImpfsPerWeek21$Anzahl)

## 2022
aggImpfsPerDay22<-aggregate(Anzahl ~ Impfdatum, FUN = sum, data = impf22)
aggImpfsPerWeek22<-aggregate(Anzahl ~ Woche, FUN = sum, data = impf22)

maxImpfsPerDay22 <- max(aggImpfsPerDay22$Anzahl)
maxImpfsPerWeek22 <- max(aggImpfsPerWeek22$Anzahl)







## JUST TO CHECK
abh<-lm(aggDeathsPerWeek21$AnzahlTodesfall~aggImpfsPerWeek21$Anzahl)
summary(abh)
##R-squared ist 0.17 - das ist weniger als ich dachte, aber auch irgendwie keine kleinigkeit








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
# so nun automatisch und ohne Weiteres in der richtigen, Reihenfolge abgebildet werden (beim späteren barPlotFallTot)
base$Landkreis <- factor(base$Landkreis, levels = rev(c("SK Berlin Mitte", "SK Berlin Neukölln", "SK Berlin Tempelhof-Schöneberg", 
                                                    "SK Berlin Friedrichshain-Kreuzberg", "SK Berlin Charlottenburg-Wilmersdorf", 
                                                    "SK Berlin Pankow", "SK Berlin Reinickendorf", "SK Berlin Spandau", 
                                                    "SK Berlin Steglitz-Zehlendorf", "SK Berlin Lichtenberg", 
                                                    "SK Berlin Treptow-Köpenick", "SK Berlin Marzahn-Hellersdorf")))

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

# ------------------------------------------------------------------------------------- jahresspalte
# year-column für base erstellen
cYears <- strftime(base$Refdatum, format = "%Y")

# Nullen vor den year-chars löschen
cYears <- sub("^0+", "", cYears)

# years zu numeric values umwandeln
cYears <- as.numeric(cYears)

# cYears an base ranhängen
base <- cbind(base, Jahr = cYears)

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

# ------------------------------------------------------------------------------------- EINTEILUNG IN ZEITRÄUME
# Unterteilung in einen Datensatz für den gesamten Zeitraum, durch welchen nach Tagen/Monaten aggregiert werden kann (d = data)
# und einen für die Wochen eines Jahres, denn:
# die erste/letzte Woche eines Jahres beginnt/endet nicht zwingend im selben Jahr
# siehe ISO-week-date: https://en.wikipedia.org/wiki/ISO_week_date
# um ISO-week-dates für jedes Jahr zu bekommen, siehe: https://www.epochconverter.com/weeks/2020

# ------------------------------------------------------------------------------------- 2020
d20 <- base[base$Refdatum >= '2020-01-01' & base$Refdatum < '2021-01-01',]
d20weeks <- base[base$Refdatum >= '2019-12-30' & base$Refdatum <= '2021-01-03',]

# ------------------------------------------------------------------------------------- 2021
d21 <- base[base$Refdatum >= '2021-01-01' & base$Refdatum < '2022-01-01',]
d21weeks <- base[base$Refdatum >= '2021-01-04' & base$Refdatum <= '2022-01-02',]

# ------------------------------------------------------------------------------------- 2022
d22 <- base[base$Refdatum >= '2022-01-01' & base$Refdatum < '2023-01-01',]
d22weeks <- base[base$Refdatum >= '2022-01-03' & base$Refdatum <= '2023-01-01',]

# ------------------------------------------------------------------------------------- 2023
d23 <- base[base$Refdatum >= '2023-01-01' & base$Refdatum < '2024-01-01',]
d23weeks <- base[base$Refdatum >= '2023-01-02' & base$Refdatum <= '2023-12-31',]

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
      checkboxInput("varFlipBool", label = "Flip Diagramm (Kann z.B. Balkenbeschriftungen sichtbarer machen)", value = FALSE)
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
  br(),
  br(),
  br(),

  # ----------------------------------------------------------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ----------------------------------------------------------
  
  
  
  
    sidebarPanel(
      selectInput("Altersgruppe", label="Wählen Sie eine Altersgruppe", choices = list("unbekannt"=1, "0 bis 4 Jahre"=2,"5 bis 14 Jahre"=3, "15 bis 34 Jahre"=4,"35 bis 59 Jahre"=5,"60 bis 79 Jahre"=6,"über 80 Jahre"=7, "Gesamt"=8), selected = 8),
      radioButtons("Variante", label="Welche Virusvariante soll betrachtet werden?", choices = list("Urtyp"=1, "Alpha"=2, "Delta"=3, "Omikron"=4, "Alle zusammen"=5), selected = 5),
      sliderInput(inputId="Woche", label="KalenderWoche", min=1, max=53, value=1),
      radioButtons("Jahr", label="Welches Kalenderjahr soll betrachtet werden?", choices=list("2021"=1, "2022"=2), selected=1)
      ),
    
  mainPanel(
    textOutput("TextAlter"),
    plotOutput("VerhaeltnisAlter"),
    plotOutput("impfungen_Woche"),
    plotOutput("faelle_Woche"),
    plotOutput("tode_Woche")
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
  ##var_jahr()[3]: die impfungen pro Wochen
  var_jahr<-reactive({
    switch(
      as.character(input$Jahr),
      "1" = return (c(sum(data21$AnzahlFall[data21$Woche==var_Woche()]), sum (data21$AnzahlTodesfall[data21$Woche==var_Woche()]), sum(impf21$Anzahl[impf21$Woche==var_Woche()]))),
      "2" = return (c(sum(data22$AnzahlFall[data22$Woche==var_Woche()]), sum (data22$AnzahlTodesfall[data22$Woche==var_Woche()]), sum(impf22$Anzahl[impf22$Woche==var_Woche()])))
    )
  })
  
  
  ##die y-Achsen sind hier unterschiedlich! Ich weiß nicht so recht, wie damit umzugehen - aktuell ist das 
  ##maximum immer der maximal vorkommende wert - so verhalten sich immerhin alle 3 Plots zu ihrem maximum (also quasi zu 100%)
  output$impfungen_Woche<-renderPlot(barplot(main="Anzahl Impfungen in dieser KW",var_jahr()[3],ylim=c(0,max(maxImpfPerWeek21, maxImpfPerWeek22))))
  output$tode_Woche<-renderPlot(barplot(main="Anzahl Todesfälle in dieser KW",var_jahr()[2],ylim=c(0,max(maxDeathsPerWeek21, maxDeathsPerWeek22))))
  output$faelle_Woche<-renderPlot(barplot(main="Anzahl Infektionen in dieser KW",var_jahr()[1],ylim=c(0,max(maxCasesPerWeek21, maxCasesPerWeek22))))
      
                         
  ##gibt reduzierte Datensätze zurück - im jeweiligen Zeitraum war die Variante mit >50% vertreten
  var_variant<-reactive({
    switch(
      as.character(input$Variante),
      "1" = return (data_urtyp),
      "2" = return (data_alpha),
      "3" = return (data_delta),
      "4" = return (data_ominkron),
      "5" = return (data)
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
    return (ggplot(frame, aes(fill=zustand, y=zahlen, x=stringAltersrange))+  geom_bar(position='stack', stat='identity'))

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
    
    if(input$varFlipBool == TRUE) {
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
    else {
      return(getDataFrame() %>% 
               ggplot(aes(x = getXatt(), y = getYatt(), fill = getUnterteilungsAtt())) +
               geom_bar(stat = "identity") +
               theme_minimal() +
               labs(x = "x",
                    y = "y", 
                    title = "Title") + 
               scale_fill_manual(values = getColorPalette()))
    }
    
  })
  
  # was hat es mit "height" auf sich? siehe: https://stackoverflow.com/questions/17838709/scale-and-size-of-plot-in-rstudio-shiny}
  output$barPlotFallTot <- renderPlot({return(barPlotFallTot())}, height = 670)
  
}

# Run the application 
shinyApp(ui = ui, server = server)
