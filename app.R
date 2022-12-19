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

plot(zahlen)

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
          textOutput("TextAlter"),
          plotOutput("VerhaeltnisAlter"),


        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
 library(ggplot2)
  data<-read.csv2("./Data/RKI_COVID19_Berlin.csv", header=T, sep=",")
  

  var_age<-reactive({
    if(input$Altersgruppe ==1)({
    return (c(sum(data$AnzahlFall[data$Altersgruppe=="unbekannt"]),sum(data$AnzahlTodesfall[data$Altersgruppe=="unbekannt"]),signif((sum(data$AnzahlTodesfall[data$Altersgruppe=="unbekannt"])/sum(data$AnzahlFall[data$Altersgruppe=="unbekannt"]))*100),digits=1))
    })
    if(input$Altersgruppe ==2)({
      return (c(sum(data$AnzahlFall[data$Altersgruppe=="A00-A04"]),sum(data$AnzahlTodesfall[data$Altersgruppe=="A00-A04"]),round((sum(data$AnzahlTodesfall[data$Altersgruppe=="A00-A04"])/sum(data$AnzahlFall[data$Altersgruppe=="A00-A04"]))*100)))
    })
    if(input$Altersgruppe ==3)({
      return (c(sum(data$AnzahlFall[data$Altersgruppe=="A05-A14"]),sum(data$AnzahlTodesfall[data$Altersgruppe=="A05-A14"]),round((sum(data$AnzahlTodesfall[data$Altersgruppe=="A05-A14"])/sum(data$AnzahlFall[data$Altersgruppe=="A05-A14"]))*100)))
    })
    if(input$Altersgruppe ==4)({
      return (c(sum(data$AnzahlFall[data$Altersgruppe=="A15-A34"]),sum(data$AnzahlTodesfall[data$Altersgruppe=="A15-A34"]),round((sum(data$AnzahlTodesfall[data$Altersgruppe=="A15-A34"])/sum(data$AnzahlFall[data$Altersgruppe=="A15-A34"]))*100)))
    })
    if(input$Altersgruppe ==5)({
      return (c(sum(data$AnzahlFall[data$Altersgruppe=="A35-A59"]),sum(data$AnzahlTodesfall[data$Altersgruppe=="A35-A59"]),round((sum(data$AnzahlTodesfall[data$Altersgruppe=="A35-A59"])/sum(data$AnzahlFall[data$Altersgruppe=="A35-A59"]))*100)))
    })
    if(input$Altersgruppe ==6)({
      return (c(sum(data$AnzahlFall[data$Altersgruppe=="A60-A79"]),sum(data$AnzahlTodesfall[data$Altersgruppe=="A60-A79"]), round((sum(data$AnzahlTodesfall[data$Altersgruppe=="A60-A79"])/sum(data$AnzahlFall[data$Altersgruppe=="A60-A79"]))*100)))
    })
    if(input$Altersgruppe ==7)({
      return (c(sum(data$AnzahlFall[data$Altersgruppe=="A80+"]),sum(data$AnzahlTodesfall[data$Altersgruppe=="A80+"]), signif((sum(data$AnzahlTodesfall[data$Altersgruppe=="A80+"])/sum(data$AnzahlFall[data$Altersgruppe=="A80+"]))*100),digits=3))
    })
  })
  
  output$TextAlter<-renderText({paste0("Anzahl der Infektionen in der gewählten Altersgruppe insgesamt:", var_age()[1], " davon Todesfälle: ",  var_age()[2], " (",var_age()[3],"%)") })
  

  plotAlter<-reactive({
    zustand<-rep(c( "davon Todesfälle", "Infektionen"))
    zustand<-factor(zustand, levels = c("Infektionen", "davon Todesfälle" ))
    if(input$Altersgruppe==1) ({
      zahlen<-rep(c(var_age()[2], var_age()[1]-var_age()[2]))
      frame<-data.frame(zustand, zahlen)
      return (ggplot(frame, aes(fill=zustand, y=zahlen, x="Alter unbekannt"))+  geom_bar(position='stack', stat='identity'))
    })
    if(input$Altersgruppe==2) ({
      zahlen<-rep(c(var_age()[2], var_age()[1]-var_age()[2]))
      frame<-data.frame(zustand, zahlen)
      return (ggplot(frame, aes(fill=zustand, y=zahlen, x="Alter unbekannt"))+  geom_bar(position='stack', stat='identity'))
    })
    if(input$Altersgruppe==3) ({
      zahlen<-rep(c(var_age()[2], var_age()[1]-var_age()[2]))
      frame<-data.frame(zustand, zahlen)
      return (ggplot(frame, aes(fill=zustand, y=zahlen, x="Alter unbekannt"))+  geom_bar(position='stack', stat='identity'))
    })
    if(input$Altersgruppe==4) ({
      zahlen<-rep(c(var_age()[2], var_age()[1]-var_age()[2]))
      frame<-data.frame(zustand, zahlen)
      return (ggplot(frame, aes(fill=zustand, y=zahlen, x="Alter unbekannt"))+  geom_bar(position='stack', stat='identity'))
    })
    if(input$Altersgruppe==5) ({
      zahlen<-rep(c(var_age()[2], var_age()[1]-var_age()[2]))
      frame<-data.frame(zustand, zahlen)
      return (ggplot(frame, aes(fill=zustand, y=zahlen, x="Alter unbekannt"))+  geom_bar(position='stack', stat='identity'))
    })
    if(input$Altersgruppe==6) ({
      zahlen<-rep(c(var_age()[2], var_age()[1]-var_age()[2]))
      frame<-data.frame(zustand, zahlen)
      return (ggplot(frame, aes(fill=zustand, y=zahlen, x="Alter unbekannt"))+  geom_bar(position='stack', stat='identity'))
    })
    if(input$Altersgruppe==7) ({
      zahlen<-rep(c(var_age()[2], var_age()[1]-var_age()[2]))
      frame<-data.frame(zustand, zahlen)
      return (ggplot(frame, aes(fill=zustand, y=zahlen, x="A80+"))+  geom_bar(position='stack', stat='identity'))
    })

  })
  
  
  output$VerhaeltnisAlter<-renderPlot({plotAlter()})
}

# Run the application 
shinyApp(ui = ui, server = server)
