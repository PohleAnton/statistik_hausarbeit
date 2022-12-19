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


##geschieht außerhalb der Anwendung für die Performance
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

##in 2021 
impf21<-impfungen_b[impfungen_b$Impfdatum>'2020-12-31' & impfungen_b$Impfdatum<'2022-01-01',]
##fügt wochenspalte hinzu
impf21$woche<-strftime(impf21$Impfdatum, format="%V")
##remove leading zeros von "woche":
impf21$woche<-sub("^0+","",impf21$woche)
##Daten gesamt in 2021
data21<-data[data$Meldedatum>'2020-12-31' & data$Meldedatum<'2022-01-01',]
data21$woche<-strftime(data21$Meldedatum, format="%V")
##remove leading zeros von "woche":
data21$woche<-sub("^0+","",data21$woche)



##in 2022
impf22<-impfungen_b[impfungen_b$Impfdatum>'2021-12-31',]
##fügt eine wochenspaltehinzu
impf22$woche<-strftime(impf22$Impfdatum, format="%V")
##remove leading zeros von "woche":
impf22$woche<-sub("^0+","",impf22$woche)
##Daten gesamt in 2022
data22<-data[data$Meldedatum>'2021-12-31',]
##fügt eine Wochenspalte hinzu
data22$woche<-strftime(data22$Meldedatum, format="%V")
##remove leading zeros von "woche":
data22$woche<-sub("^0+","",data22$woche)

sum(impf21$Anzahl[])
barplot(sum(impf21$Anzahl[impf21$woche==15]),ylim=c(0,300000))

sum(impf21$Anzahl[impf21$woche=="05"])

##es wird im weiteren nach totalen Impfdosen verabreicht ermittelt, nicht nach Anzahl der erhaltenen Impfungen
##2020 wird wegen den quasi noch nicht verfügbaren Impfdosen nicht beachtet



## gemäß
## https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Daten/VOC_VOI_Tabelle.xlsx?__blob=publicationFile
## wird folgendes angenommen:
## KW09/2021-KW24/2021: Alpha ist vorherrschend
## KW25/2021-KW51/2021: Delta ist vorherrschend
## ab KW52/2021: Omikron- vorerst keine weitere Unterscheidung der Subtypen
##vorherrschend heißt >50%

data_urtyp<-data[data$Meldedatum < '2021-03-01',]
data_alpha<-data[data$Meldedatum > '2021-02-28' & data$Meldedatum < '2021-06-21',]
data_delta<-data[data$Meldedatum > '2021-06-21' & data$Meldedatum < '2021-12-27',]
data_ominkron<-data[data$Meldedatum > '2021-12-26',]


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Covid19 in Berlin"),
  
    sidebarPanel(
      selectInput("Altersgruppe", label="Wählen Sie eine Altersgruppe", choices = list("unbekannt"=1, "0 bis 4 Jahre"=2,"5 bis 14 Jahre"=3, "15 bis 34 Jahre"=4,"35 bis 59 Jahre"=5,"60 bis 79 Jahre"=6,"über 80 Jahre"=7, "Gesamt"=8), selected = 8),
      radioButtons("Variante", label="Welche Virusvariante soll betrachtet werden?", choices = list("Urtyp"=1, "Alpha"=2, "Delta"=3, "Omikron"=4, "Alle zusammen"=5), selected = 5),
      sliderInput(inputId="Woche", label="Kalenderwoche", min=1, max=53, value=1),
      radioButtons("Jahr", label="Welches Kalenderjahr soll betrachtet werden?", choices=list("2021"=1, "2022"=2), selected=1)
    ),

mainPanel(
  plotOutput("VerhaeltnisAlter"),
  textOutput("TextAlter"),
  plotOutput("impfungen_woche"),
  plotOutput("faelle_woche")
  


))

server <- function(input, output) {
  
  var_woche<-reactive({
    return(input$Woche)
  })
  
  ##gibt den Datensatz für das gewählte Kalenderjahr zurück:
  ##gibt einen vektor mit 2 datensätzen zurück:
  ##var_jahr()[1]: der gesamtdatensatz mit wochen
  ##var_jahr()[2]: die impfdaten mit wochen
  var_jahr<-reactive({
    if(input$Jahr==1)({
      return (c(sum(data21$AnzahlFall[data21$woche==var_woche()]), sum(impf21$Anzahl[impf21$woche==var_woche()])))
    })
    if(input$Jahr==2)({
      return (c(sum(data22$AnzahlFall[data22$woche==var_woche()]), sum(impf22$Anzahl[impf22$woche==var_woche()])))
    })
  })
  
  output$impfungen_woche<-renderPlot(barplot(main="Anzahl Impfungen in dieser KW",var_jahr()[2],ylim=c(0,270000)))
  output$faelle_woche<-renderPlot(barplot(main="Anzahl Infektionen in dieser KW",var_jahr()[1],ylim=c(0,90000)))
                             
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
