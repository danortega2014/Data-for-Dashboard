library(semantic.dashboard)
library(quantreg)
library(shiny)
data <- read.csv("https://raw.githubusercontent.com/danortega2014/Data-for-Dashboard/main/realestatedata1.csv")
attach(data)

ui <- dashboardPage(
    dashboardHeader(color = "teal", title = "Daniel's Dashboard"),
    dashboardSidebar(color="teal",
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboardx", icon = icon("dashboard")),
            menuItem("Additional information", tabName = "widgets", icon = icon("th"))
        )
    ),
    dashboardBody(
        tabItems(
            selected=1,
            tabItem(
                tabName = "dashboard",
        fluidRow(
            box(plotOutput("Plot1", height = 225)),
            box(plotOutput("Plot2", height = 225)),
            box(plotOutput("Plot3", height = 225)),
            box(plotOutput("Plot4", height = 225)),
            box(title = "Coefficients and Confidence Interval for Each Variable",status = "primary", solidHeader=TRUE, color="orange",verbatimTextOutput("Print")),
            box(title = "Tau Adjuster","Change to see the impact of the variables on different Price Percentiles", solidHeader=TRUE, color="blue", sliderInput("bins", "Percentile Group", 0, 1, .50, width = 700))
            )
        ),
    tabItem(
        tabName = "widgets",
        fluidRow(box(title = "Additional information",color="red","R Code for this dashboard can found at github.com/danortega2014",br(), "Data set can be found at https://www.kaggle.com/arslanali4343/real-estate-dataset",
                     br(), "Refresh to see dashboard."))
        )
        )
    )
)



server <- function(input, output) {

    output$Plot1 <- renderPlot({
        plot(RM,MEDV,main="Quantile Impact of # of Rooms on Housing Prices ",  xlab="Number of Rooms",ylab="Price of House")
        abline(rq(MEDV~RM, tau=input$bins), col="blue")
    })
    output$Plot2 <- renderPlot({
        plot(TAX,MEDV,main="Quantile Impact of Tax on Housing prices ",  xlab="Tax",ylab="Price of House")
        abline(rq(MEDV~TAX, tau=input$bins), col="green")
    })
    output$Plot3 <- renderPlot({
        plot(CRIM,MEDV,main="Quantile Impact of Crime on Housing prices ",  xlab="Per Capita Crime Rate",ylab="Price of House")
        abline(rq(MEDV~CRIM, tau=input$bins), col="red")
    })
    output$Plot4 <- renderPlot({
        plot(CRIM,MEDV,main="Quantile Impact of Nitric Oxide Conc. on Housing Prices ",  xlab="Nitric Oxides Conc. (parts per 10 million)",ylab="Price of House")
        abline(rq(MEDV~NOX, tau=input$bins), col="magenta")
    })
    
    output$Print <- renderPrint({
        summary(rq(MEDV~TAX+RM+CRIM+NOX, tau=input$bins))
    })
}

shinyApp(ui, server)