#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

#' @title no contribution function
#' @description calculate how much money you expect to get at the end of a several year period with a certain return rate
#' @param amount amount of money put in (numeric)
#' @param rate return rate (numeric)
#' @param years number of years (numeric)
#' @return computed final return

no_contrib = function(amount, rate, years){
  amount1 = amount * (1 + (rate/100)) ^ years
  return(amount1)
}

#' @title fixed contribution function
#' @description calculate future value of annuity
#' @param contrib how much you deposit at the end of each year (numeric)
#' @param rate anuual rate of return (numeric)
#' @param years number of years (numeric)
#' @return compute future value of annuity

fixed_contrib = function(contrib, rate, years){
  return(contrib * (((1 + (rate/100)) ^ years - 1)/(rate/100)))
}

#' @title growing contribution function
#' @description calculate future value of growing annuity
#' @param contrib how much you deposit at the end of each year (numeric)
#' @param rate anuual rate of return (numeric)
#' @param grwoth annual growth rate
#' @param years number of years (numeric)
#' @return compute future value of annuity

growing_contrib = function(contrib, rate, growth, years){
  return(contrib * (((1 + (rate/100)) ^ years - (1 + (growth/100)) ^ years)/((rate - growth)/100)))
}

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Savings Simulation"),
   
   # Sidebar with a slider input for number of bins 
   
      fluidRow(
        column(4,
               sliderInput("initial",
                     "Initial Amount:",
                     min = 1000,
                     max = 1000000,
                     value = 1000,
                     step = 500)),
        column(4,
         sliderInput("return",
                     "Return Rate (in %):",
                     min = 0,
                     max = 20,
                     value = 5,
                     step = 0.1)),
        column(4,
         sliderInput("years",
                     "Years:",
                     min = 0,
                     max = 50,
                     value = 20,
                     step = 1)),

          column(4, 
         sliderInput("contribution",
                     "Annual Contribution:",
                     min = 0,
                     max = 500000,
                     value = 2000,
                     step = 500)),
         column(4,
         sliderInput("growth",
                     "Growth rate (in %)",
                     min = 0,
                     max = 20,
                     value = 2,
                     step = 0.1)),
         column(4, 
         selectInput("facet",
                     "Facet",
                     c("Yes", "No")))
      ),
      
      # Show a plot of the generated distribution
<<<<<<< HEAD
      mainPanel(column(4,h4("Timelines")),
         plotOutput("distPlot"),
         column(4,h4("Balance")),
=======
      mainPanel(
         plotOutput("distPlot"),
>>>>>>> 945b71af48775ce187f74a366a67177e69f19903
         dataTableOutput("distdatatable")
      )
   )


# Define server logic required to draw a histogram
server <- function(input, output) {
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
     no_contrib = rep(0, input$years + 1)
     for (i in 0:input$years) {
       no_contrib[i + 1] = no_contrib(input$initial, input$return, i) 
     }
  
     fixed_contrib = rep(0, input$years + 1)
    for (j in 0:input$years) {
      fixed_contrib[j + 1] = fixed_contrib(input$contribution, input$return, j + 1)
    }
     
     growing_contrib = rep(0, input$years + 1)
     for (k in 0:input$years) {
       growing_contrib[k + 1] = growing_contrib(input$contribution, input$return, input$growth, k + 1)
     }
     
     dat = data.frame(years = 0:input$years, no_contrib = no_contrib, fixed_contrib = fixed_contrib, growing_contrib = growing_contrib)
     dat1 = data.frame(years = rep(0:input$years, 3), values = c(no_contrib, fixed_contrib, growing_contrib), 
                       modality = factor(c(rep("no_contrib",input$years+1), rep("fixed_contrib", input$years+1), rep("growing_contrib", input$years+1)), 
                                         levels = c("no_contrib", "fixed_contrib", "growing_contrib"))) 
     if(input$facet == "Yes"){
     ggplot(data = dat1) + 
       #geom_line(data = dat1[1:(input$years+1),], aes(x = years, y = values, color = "no_contrib")) + 
       geom_area(data = dat1[(input$years+2):((input$years+1)*2),], aes(x = years, y = values, fill = "fixed_contrib"),alpha = 0.5) + 
       geom_area(data = dat1[((input$years+1)*2+1):(3*(input$years + 1)),], aes(x = years, y = values, fill = "growing_contrib"), alpha = 0.5) +
       geom_area(data = dat1[1:(input$years+1),], aes(x = years, y = values, fill = "no_contrib"), alpha = 0.5) +
       geom_line(data = dat1[(input$years+2):((input$years+1)*2),], aes(x = years, y = values, color = "fixed_contrib")) + 
       geom_point(data = dat1[(input$years+2):((input$years+1)*2),], aes(x = years, y = values, color = "fixed_contrib")) +
       geom_point(data = dat1[((input$years+1)*2+1):(3*(input$years + 1)),], aes(x = years, y = values, color = "growing_contrib")) +
       geom_line(data = dat1[((input$years+1)*2+1):(3*(input$years + 1)),], aes(x = years, y = values, color = "growing_contrib")) + 
       geom_point(data = dat1[1:(input$years+1),], aes(x = years, y = values, color = "no_contrib")) +
       geom_line(data = dat1[1:(input$years+1),], aes(x = years, y = values, color = "no_contrib")) + 
       facet_grid(~modality) + 
<<<<<<< HEAD
       labs(color = "variable", fill = "variable") + ggtitle("Three modes of investing")
=======
       labs(color = "variable", fill = "variable")
>>>>>>> 945b71af48775ce187f74a366a67177e69f19903
       } else{
      # draw the histogram with the specified number of bins
    ggplot(data = dat) +
      geom_line(aes(x = years, y = no_contrib, color = "no_contrib")) + 
      geom_line(aes(x = years, y = fixed_contrib, color = "fixed contrib")) + 
<<<<<<< HEAD
      geom_line(aes(x = years, y = growing_contrib, color = "growing_contrib")) + 
      ggtitle("Three modes of investing")
=======
      geom_line(aes(x = years, y = growing_contrib, color = "growing_contrib"))
>>>>>>> 945b71af48775ce187f74a366a67177e69f19903
       }})
   output$distdatatable = renderDataTable({
       # generate bins based on input$bins from ui.R
       no_contrib = rep(0, input$years + 1)
       for (i in 0:input$years) {
         no_contrib[i + 1] = no_contrib(input$initial, input$return, i) 
       }
       
       fixed_contrib = rep(0, input$years + 1)
       for (j in 0:input$years) {
         fixed_contrib[j + 1] = fixed_contrib(input$contribution, input$return, j + 1)
       }
       
       growing_contrib = rep(0, input$years + 1)
       for (k in 0:input$years) {
         growing_contrib[k + 1] = growing_contrib(input$contribution, input$return, input$growth, k + 1)
       }
       
       dat = data.frame(years = 0:input$years, no_contrib = no_contrib, fixed_contrib = fixed_contrib, growing_contrib = growing_contrib)
   })
}  
# Run the application 
shinyApp(ui = ui, server = server)
