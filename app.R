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



## AUFBEREITUNG DER GRUNDDATEN
##geschieht außerhalb der Anwendung für die Performance und weil all das nur einmal durchgegangen werden muss
data<-read.csv2("./Data/RKI_COVID19_Berlin.csv", header=T, sep=",")
##Transformiert diese beiden Spalten in Datumsformat, damit Vergleiche angesellt werden können:
data$Meldedatum<-as.Date(data$Meldedatum)
data$Refdatum<-as.Date(data$Refdatum) 

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
  


))

server <- function(input, output) {
  
  var_Woche<-reactive({
    return(input$Woche)
  })
  
  
  ##gibt einen vektor mit 3 Summen zurück:
  ##var_jahr()[1]: die Fälle pro Woche
  ##var_jahr()[2]: die todesfälle pro Woche
  ##var_jahr()[3]: die impfungen pro Wochen
  var_jahr<-reactive({
    if(input$Jahr==1)({
      return (c(sum(data21$AnzahlFall[data21$Woche==var_Woche()]), sum (data21$AnzahlTodesfall[data21$Woche==var_Woche()]), sum(impf21$Anzahl[impf21$Woche==var_Woche()])))
    })
    if(input$Jahr==2)({
      return (c(sum(data22$AnzahlFall[data22$Woche==var_Woche()]), sum (data22$AnzahlTodesfall[data22$Woche==var_Woche()]), sum(impf22$Anzahl[impf22$Woche==var_Woche()])))
    })
  })
  
  ##die y-Achsen sind hier unterschiedlich! Ich weiß nicht so recht, wie damit umzugehen - aktuell ist das 
  ##maximum immer der maximal vorkommende wert - so verhalten sich immerhin alle 3 Plots zu ihrem maximum (also quasi zu 100%)
  output$impfungen_Woche<-renderPlot(barplot(main="Anzahl Impfungen in dieser KW",var_jahr()[3],ylim=c(0,max(maxImpfPerWeek21, maxImpfPerWeek22))))
  output$tode_Woche<-renderPlot(barplot(main="Anzahl Todesfälle in dieser KW",var_jahr()[2],ylim=c(0,max(maxDeathsPerWeek21, maxDeathsPerWeek22))))
  output$faelle_Woche<-renderPlot(barplot(main="Anzahl Infektionen in dieser KW",var_jahr()[1],ylim=c(0,max(maxCasesPerWeek21, maxCasesPerWeek22))))
                             
  ##gibt reduzierte Datensätze zurück - im jeweiligen Zeitraum war die Variante mit >50% vertreten
  var_variant<-reactive({
    if(input$Variante==1)({
      return (data_urtyp)
    })
    if(input$Variante==2)({
      return (data_alpha)
    })
    if(input$Variante==3)({
      return (data_delta)
    })
    if(input$Variante==4)({
      return (data_ominkron)
    })
    if(input$Variante==5)({
      return (data)
    })
  })

  
  ##dieser Funktion gibt Werte in Abhängigkeit des  gewählten Altes aus - ich schlage vor, dass weitere altersabhängige Werte ebenfalls hier erfasst werden - solange die Reihenfolge erhalten bleibt, sollte das kein Problem sein.
  ##aktuell gibt var_age()[1] die Fallzahl abhängig vom Alter, var_age()[2] die Anzahl der Todesfälle abhängig vom Alter zurück. Ggf. in diesem Stil weiter dokumentieren
  var_age<-reactive({
    if(input$Altersgruppe ==1)({
    return (c(sum(var_variant()$AnzahlFall[var_variant()$Altersgruppe=="unbekannt"]),sum(var_variant()$AnzahlTodesfall[var_variant()$Altersgruppe=="unbekannt"]),signif((sum(var_variant()$AnzahlTodesfall[var_variant()$Altersgruppe=="unbekannt"])/sum(var_variant()$AnzahlFall[var_variant()$Altersgruppe=="unbekannt"]))*100,digits=6)))
    })
    if(input$Altersgruppe ==2)({
      return (c(sum(var_variant()$AnzahlFall[var_variant()$Altersgruppe=="A00-A04"]),sum(var_variant()$AnzahlTodesfall[var_variant()$Altersgruppe=="A00-A04"]),signif((sum(var_variant()$AnzahlTodesfall[var_variant()$Altersgruppe=="A00-A04"])/sum(var_variant()$AnzahlFall[var_variant()$Altersgruppe=="A00-A04"]))*100,digits=3)))
    })
    if(input$Altersgruppe ==3)({
      return (c(sum(var_variant()$AnzahlFall[var_variant()$Altersgruppe=="A05-A14"]),sum(var_variant()$AnzahlTodesfall[var_variant()$Altersgruppe=="A05-A14"]),signif((sum(var_variant()$AnzahlTodesfall[var_variant()$Altersgruppe=="A05-A14"])/sum(var_variant()$AnzahlFall[var_variant()$Altersgruppe=="A05-A14"]))*1000,digits=3)))
    })
    if(input$Altersgruppe ==4)({
      return (c(sum(var_variant()$AnzahlFall[var_variant()$Altersgruppe=="A15-A34"]),sum(var_variant()$AnzahlTodesfall[var_variant()$Altersgruppe=="A15-A34"]),signif((sum(var_variant()$AnzahlTodesfall[var_variant()$Altersgruppe=="A15-A34"])/sum(var_variant()$AnzahlFall[var_variant()$Altersgruppe=="A15-A34"]))*1000,digits=3)))
    })
    if(input$Altersgruppe ==5)({
      return (c(sum(var_variant()$AnzahlFall[var_variant()$Altersgruppe=="A35-A59"]),sum(var_variant()$AnzahlTodesfall[var_variant()$Altersgruppe=="A35-A59"]),signif((sum(var_variant()$AnzahlTodesfall[var_variant()$Altersgruppe=="A35-A59"])/sum(var_variant()$AnzahlFall[var_variant()$Altersgruppe=="A35-A59"]))*100,digits=3)))
    })
    if(input$Altersgruppe ==6)({
      return (c(sum(var_variant()$AnzahlFall[var_variant()$Altersgruppe=="A60-A79"]),sum(var_variant()$AnzahlTodesfall[var_variant()$Altersgruppe=="A60-A79"]), signif((sum(var_variant()$AnzahlTodesfall[var_variant()$Altersgruppe=="A60-A79"])/sum(var_variant()$AnzahlFall[var_variant()$Altersgruppe=="A60-A79"]))*100,digits=3)))
    })
    if(input$Altersgruppe ==7)({
      return (c(sum(var_variant()$AnzahlFall[var_variant()$Altersgruppe=="A80+"]),sum(var_variant()$AnzahlTodesfall[var_variant()$Altersgruppe=="A80+"]), signif((sum(var_variant()$AnzahlTodesfall[var_variant()$Altersgruppe=="A80+"])/sum(var_variant()$AnzahlFall[var_variant()$Altersgruppe=="A80+"]))*100,digits=3)))
    })
    if(input$Altersgruppe ==8)({
      return (c(sum(var_variant()$AnzahlFall),sum(var_variant()$AnzahlTodesfall), signif((sum(var_variant()$AnzahlTodesfall)/sum(var_variant()$AnzahlFall))*100,digits=3)))
    })
  })
  
  output$TextAlter<-renderText({paste0("Anzahl der Infektionen in der gewählten Altersgruppe insgesamt:", var_age()[1], " davon Todesfälle: ",  var_age()[2], " (",var_age()[3],"%)") })
  

  plotAlter<-reactive({
    zustand<-rep(c( "davon Todesfälle", "Infektionen"))
    zustand<-factor(zustand, levels = c("Infektionen", "davon Todesfälle" ))
    if(input$Altersgruppe==1) ({
      zahlen<-rep(c(var_age()[2], var_age()[1]))
      frame<-data.frame(zustand, zahlen)
      return (ggplot(frame, aes(fill=zustand, y=zahlen, x="Alter unbekannt"))+  geom_bar(position='fill', stat='identity'))
    })
    if(input$Altersgruppe==2) ({
      zahlen<-rep(c(var_age()[2], var_age()[1]-var_age()[2]))
      frame<-data.frame(zustand, zahlen)
      return (ggplot(frame, aes(fill=zustand, y=zahlen, x="0 - 4"))+  geom_bar(position='stack', stat='identity'))
    })
    if(input$Altersgruppe==3) ({
      zahlen<-rep(c(var_age()[2], var_age()[1]-var_age()[2]))
      frame<-data.frame(zustand, zahlen)
      return (ggplot(frame, aes(fill=zustand, y=zahlen, x="05-14"))+  geom_bar(position='stack', stat='identity'))
    })
    if(input$Altersgruppe==4) ({
      zahlen<-rep(c(var_age()[2], var_age()[1]-var_age()[2]))
      frame<-data.frame(zustand, zahlen)
      return (ggplot(frame, aes(fill=zustand, y=zahlen, x="15 - 34"))+  geom_bar(position='stack', stat='identity'))
    })
    if(input$Altersgruppe==5) ({
      zahlen<-rep(c(var_age()[2], var_age()[1]-var_age()[2]))
      frame<-data.frame(zustand, zahlen)
      return (ggplot(frame, aes(fill=zustand, y=zahlen, x="35-59"))+  geom_bar(position='stack', stat='identity'))
    })
    if(input$Altersgruppe==6) ({
      zahlen<-rep(c(var_age()[2], var_age()[1]-var_age()[2]))
      frame<-data.frame(zustand, zahlen)
      return (ggplot(frame, aes(fill=zustand, y=zahlen, x="60 bis 79 "))+  geom_bar(position='stack', stat='identity'))
    })
    if(input$Altersgruppe==7) ({
      zahlen<-rep(c(var_age()[2], var_age()[1]-var_age()[2]))
      frame<-data.frame(zustand, zahlen)
      return (ggplot(frame, aes(fill=zustand, y=zahlen, x="80+"))+  geom_bar(position='stack', stat='identity'))
    })
    if(input$Altersgruppe==8) ({
      zahlen<-rep(c(var_age()[2], var_age()[1]-var_age()[2]))
      frame<-data.frame(zustand, zahlen)
      return (ggplot(frame, aes(fill=zustand, y=zahlen, x="Gesamt"))+  geom_bar(position='stack', stat='identity'))
    })

  })
  
  
  output$VerhaeltnisAlter<-renderPlot({plotAlter()})
}

# Run the application 
shinyApp(ui = ui, server = server)
