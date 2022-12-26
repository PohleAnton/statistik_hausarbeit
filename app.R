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
#                     - New Approach -
#
# ----------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Originaldaten als "base"
base <- data

# base auf die wichtigen Merkmale begrenzen
base <- base[,c('Refdatum', 'Landkreis', 'Geschlecht', 'AnzahlFall', 'AnzahlTodesfall')]

# ------------------------------------------------------------------------------------- WOCHENSPALTE
# week-column für base erstellen (c = column)
cWeeks <- strftime(base$Refdatum, format = "%V")

# Nullen vor den week-chars löschen
cWeeks <- sub("^0+", "", cWeeks)

# weeks zu numeric values umwandeln
cWeeks <- as.numeric(cWeeks)

# cWeeks an base ranhängen
base <- cbind(base, Woche = cWeeks)

# ------------------------------------------------------------------------------------- MONATSSPALTE
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

# ------------------------------------------------------------------------------------- JAHRESSPALTE
# year-column für base erstellen
cYears <- strftime(base$Refdatum, format = "%Y")

# Nullen vor den year-chars löschen
cYears <- sub("^0+", "", cYears)

# years zu numeric values umwandeln
cYears <- as.numeric(cYears)

# cYears an base ranhängen
base <- cbind(base, Jahr = cYears)

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
d22weeks <- base[base$Refdatum >= '2021-01-04' & base$Refdatum <= '2022-01-02',]




## SHINY LOGIC:
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Covid19 in Berlin"),
  
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
  ),
  
  
  
  sidebarPanel(
    sliderInput(inputId = "bins",
                label = "Number of bins:",
                min = 1,
                max = 52,
                value = 52)
  ),
  
  mainPanel(
    plotOutput("UltimateDings")
  )
  
  
  
  
  

)





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
  
  # # returns den dataFrame aus dem später der allumfängliche bar plot erstellt werden soll
  # advBarPlotDataFrame <- reactive({
  # 
  #   return(aggCasesPerDay20) ## abhängig von der Auswahl, muss hier möglicherweise ein anderer data frame eingespeist werden
  #   
  # })
  # 
  # # returns die height und width die der advBarPlot in der UI haben soll
  # advBarPlotHeightWidth <- reactive({
  # 
  #  
  #   # für round_any-Funktion, siehe: https://stackoverflow.com/questions/6461209/how-to-round-up-to-the-nearest-10-or-100-or-x
  #   return(setNames(c(round_any(nrow(advBarPlotDataFrame) * 2, 10), ## erstellt abhängig von der Anzahl an rows eine größere oder kleinere Grafik
  #           round_any(max(advBarPlotDataFrame) / 4, 10)), c("Height", "Width"))) ## und abhängig vom maximalwert eine bestimmte breite
  # })
  
  output$UltimateDings <- renderPlot({
    
    dataFrame <- aggCasesPerDay20
  
    # für folgendes code-Verständnis: siehe https://www.youtube.com/watch?v=n_ACYLWUmos
    dataFrame %>% 
      ggplot(aes(x = Refdatum, y = AnzahlFall))+
      geom_bar(stat = "identity", color = "#97B3C6", fill = "#97B3C6")+
      coord_flip()+ ## flip der x- und y-Achse des bar plots, da dies bei vielen Einträgen auf der x-Achse deutlich übersichtlicher ist
      theme_bw()+
      ylim(c(-1, round_any(max(dataFrame[2]), 10, f = ceiling)))+ ## "f = ceiling" - damit nach oben gerundet wird
      labs(x = "Zeitraum",
           y = "Faelle", ## abhängig davon, was untersucht wird, muss hier möglicherweise etwas anderes stehen
           title = "Faelle pro Tag für 2020") ## auch hier könnte anderes untersucht und der Zeitraum ausgetauscht werden
  }, height = round_any(nrow(aggCasesPerDay20) * 3, 10), width = round_any(max(aggCasesPerDay20[2]) / 3.5, 10)) ## das ist nur gebruteforced
  
  # Gibt einen BarPlot zu einem beliebigem DataFrame des Formats [Date, Anzahl] zurück, 
  # bei dem man beliebige, Histogramm-ähnliche bins bzw. breaks erstellen kann.
  #
  # @param advDF - ein formatgerechter DataFrame
  # @param binSeq - die Breite bzw. Anzahl an Tagen die ein bin (bar) umfassen soll
  #
  # @return advBarPlot - der gewünschte BarPlot
  # 
  # advBarPlot <- ({ ## "adv" steht für "advanced" (hört sich cooler an - und hebt die input$variablen von den anderen ab, dann sehe ich mehr durch).
  #   
  #   advDF <- aggCasesPerDay20 ## Muss im Format: [Date, Numeric Values] kommen. Für anderes könnte man diese Funktion sicherlich abwandeln.
  #   
  #   dfLength <- nrow(advDF)
  #   
  #   indexVec <- 1:dfLength
  #   binSequenz <- seq(from = 1, to = dfLength, by = binWidth)
  #   avgsVec <- c()
  #   
  #   for (i in)
  # })
  
  
  # Funktionierender BarPlot zu CasesPerDay20 ggplot(aggCasesPerDay20, aes(x = Refdatum, y = AnzahlFall)) + geom_bar(stat = "identity", color = "#288BA8", fill = "#288BA8")
  
  ## Wochen-Problem, welches oben beschrieben wurde: ggplot(testSortedCpW20 , aes(x = Woche, y = AnzahlFall)) + geom_bar(stat = "identity", color = "#288BA8", fill = "#288BA8")
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
