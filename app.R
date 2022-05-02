install.packages("shiny")
install.packages("shinyWidgets")
install.packages("rsconnect")
library("shiny")
library("shinyWidgets")
library("rsconnect")


#Create UI elements
ui <- fluidPage(
  setBackgroundColor(
    color = c("#E6E6FA", "#FFFAFA"),
    gradient = "linear",
    direction = "bottom"
  ),
  


  #Application title
  titlePanel("Teen Birth Rate Classification Model"),
  h4("Input information about the demographics in your area. Use your city, state, country, etc."),
  h4("Note: format all percentages as a decimal."),
  
  #Sidebar layout
  sidebarLayout(
    
    #Sidebar with inputs and submit button
    sidebarPanel(
      numericInput(inputId = "immigration", 
                   label="Percent Foreign Born Population
                   (Range 0.02-0.27):",
                   value= 0,
                   min=0.02,
                   max=0.27),
      numericInput(inputId="poverty",
                   label="Percent Below Poverty Line
                   (Range 0.05-0.19):",
                   value=0,
                   min=0.05,
                   max=0.19),
      selectInput(inputId = "adultpop",
                  label= "Percent Age 18+ in Population:
                  'Low': = 0.78,
                  'High': > 0.78",
                  choices = list("Low"=0,"High"=1)),
      actionButton(inputId = "submit",
                   label="Submit")
    ),
    #Main panel display 
    mainPanel(
      verbatimTextOutput("result"),
      h5("This model was constructed in Spring 2022 using 2019 data taken from the CDC, World Health Organization (WHO), US Census Bureau, Pew Research Center, US Bureau of Labor Statistics, and Guttmacher Institute."),
      h5("'The U.S. Census Bureau uses the term foreign born to refer to anyone who is not a U.S. citizen at birth.'"),
      h5("'The Census Bureau uses a set of money income thresholds that vary by family size and composition to determine who is in poverty. If a family's total income is less than the family's threshold, then that family and every individual in it is considered in poverty.'"),
      HTML("<p>For more information, see <a href='https://www.census.gov/'> The U.S. Census Bureau </a></p>"),
    )
  )
)

#Define server logic
server <- function(input, output){
  
  #Coefficients for the model
  b <- c(20.4,37.099,-238.272,4.195)
  
  #User Inputs
  x <- eventReactive(input$submit,
                     {c(1, input$immigration, input$poverty, as.numeric(input$adultpop))})
  output$result <- renderPrint({
    
    logodds <- b %*% x()
    result <-prob <- 1/(1+exp(-logodds))
    
    ifelse(prob > 0.52, "High Teen Birth Rate (ABOVE 1,565 per 100,000 girls)", "Low Teen Birth Rate (BELOW 1,565 per 100,000 girls)")
    
  })
}

# Run the application 
shinyApp(ui=ui, server=server)


rsconnect::deployApp()
