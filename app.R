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

data<-read.csv2("./Data/RKI_COVID19_Berlin.csv", header=T, sep=",")
##den folgenden code will ich als Plot rendern. hier tut er genau, was er soll - der output funktioniert aber noch nicht...
zustand<-rep(c( "davon Todesfälle", "Infektionen"))
zustand<-factor(zustand, levels = c("Infektionen", "davon Todesfälle" ))
tod<-sum(data$AnzahlTodesfall[data$Altersgruppe=="A80+"])
inf<-sum(data$AnzahlFall[data$Altersgruppe=="A80+"])
zahlen<-rep(c(tod, inf-tod))
frame<-data.frame(zustand, zahlen)
ploty<-ggplot(frame, aes(fill=zustand, y=zahlen, x="A80+"))+  geom_bar(position='stack', stat='identity')
ploty


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),
  

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      
        sidebarPanel(
          selectInput("Altersgruppe", label="Wählen Sie eine Altersgruppe", choices = list("unbekannt"=1, "0 bis 4 Jahre"=2,"5 bis 14 Jahre"=3, "15 bis 34 Jahre"=4,"35 bis 59 Jahre"=5,"60 bis 79 Jahre"=6,"über 80 Jahre"=7)),
        ),

        # Show a plot of the generated distribution
        mainPanel(
          textOutput("AlterString"),
          plotOutput("test")

        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
 
  data<-read.csv2("./Data/RKI_COVID19_Berlin.csv", header=T, sep=",")
  

  output$AlterString<-reactive({
  if(input$Altersgruppe ==1) ({paste("Anzahl der Infektionen in der gewählten Altersgruppe insgesamt:", sum(data$AnzahlFall[data$Altersgruppe == "unbekannt"]), "davon Todesfälle: ",  sum(data$AnzahlTodesfall[data$NeuerTodesfall==0 & data$Altersgruppe=="unbekannt"]))})
  else if(input$Altersgruppe ==2) ({paste("Anzahl der Infektionen in der gewählten Altersgruppe insgesamt:", sum(data$AnzahlFall[data$Altersgruppe == "A00-A04"]), "davon Todesfälle: ",  sum(data$AnzahlTodesfall[data$NeuerTodesfall==0 & data$Altersgruppe=="A00-A04"]))})
  else if(input$Altersgruppe ==3) ({paste("Anzahl der Infektionen in der gewählten Altersgruppe insgesamt:", sum(data$AnzahlFall[data$Altersgruppe == "A05-A14"]), "davon Todesfälle: ",  sum(data$AnzahlTodesfall[data$NeuerTodesfall==0 & data$Altersgruppe=="A05-A14"]))})
  else if(input$Altersgruppe ==4) ({paste("Anzahl der Infektionen in der gewählten Altersgruppe insgesamt:", sum(data$AnzahlFall[data$Altersgruppe == "A15-A34"]), "davon Todesfälle: ",  sum(data$AnzahlTodesfall[data$NeuerTodesfall==0 & data$Altersgruppe=="A15-A34"]))})
  else if(input$Altersgruppe ==5) ({paste("Anzahl der Infektionen in der gewählten Altersgruppe insgesamt:", sum(data$AnzahlFall[data$Altersgruppe == "A35-A59"]), "davon Todesfälle: ",  sum(data$AnzahlTodesfall[data$NeuerTodesfall==0 & data$Altersgruppe=="A35-A59"]))})
  else if(input$Altersgruppe ==6) ({paste("Anzahl der Infektionen in der gewählten Altersgruppe insgesamt:", sum(data$AnzahlFall[data$Altersgruppe == "A60-A79"]), "davon Todesfälle: ",  sum(data$AnzahlTodesfall[data$NeuerTodesfall==0 & data$Altersgruppe=="A60-A79"]))})
  else if(input$Altersgruppe ==7) ({paste("Anzahl der Infektionen in der gewählten Altersgruppe insgesamt:", sum(data$AnzahlFall[data$Altersgruppe == "A80+"]), "davon Todesfälle: ",  sum(data$AnzahlTodesfall[data$NeuerTodesfall==0 & data$Altersgruppe=="A80+"]))})
  })
    
  plotAlter<-reactive({
    if(input$Altersgruppe==7) ({
      zustand<-rep(c( "davon Todesfälle", "Infektionen"))
      zustand<-factor(zustand, levels = c("Infektionen", "davon Todesfälle" ))
      tod<-sum(data$AnzahlTodesfall[data$Altersgruppe=="A80+"])
      inf<-sum(data$AnzahlFall[data$Altersgruppe=="A80+"])
      zahlen<-rep(c(tod, inf-tod))
      frame<-data.frame(zustand, zahlen)
      ploty<-ggplot(frame, aes(fill=zustand, y=zahlen, x="A80+"))+  geom_bar(position='stack', stat='identity')
      return(ploty)
    })
  output$test<-renderPlot({plotAlter()})
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
