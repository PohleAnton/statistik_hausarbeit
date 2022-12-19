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
  

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      
        sidebarPanel(
          selectInput("Altersgruppe", label="Wählen Sie eine Altersgruppe", choices = list("unbekannt"=1, "0 bis 4 Jahre"=2,"5 bis 14 Jahre"=3, "15 bis 34 Jahre"=4,"35 bis 59 Jahre"=5,"60 bis 79 Jahre"=6,"über 80 Jahre"=7, "Gesamt"=8), selected = 8),
          radioButtons("Variante", label="Welche Virusvariante soll betrachtet werden?", choices = list("Urtyp"=1, "Alpha"=2, "Delta"=3, "Omikron"=4, "Alle zusammen"=5), selected = 5)
        ),

        # Show a plot of the generated distribution
        mainPanel(
          textOutput("TextAlter"),
          plotOutput("VerhaeltnisAlter"),


        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

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
