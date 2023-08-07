############################################
# App to Convert Temperature               #
############################################

library(shiny)
library(shinythemes)

# fahrenheit to Celsius
F_to_C <- function(F_temp){
  C_temp <- (F_temp - 32) * 5/9;
  return(C_temp);
}

# Celsius to fahrenheit
C_to_F <- function(C_temp){
  F_temp <- (C_temp * 9/5) + 32;
  return(F_temp);
}

####################################
# User Interface                   #
####################################
ui <- fluidPage(theme = shinytheme("united"),
                navbarPage("Temperature Converter:",

                           tabPanel("Home",
                                    # Input values
                                    sidebarPanel(
                                      HTML("<h3>Input parameters</h3>"),
                                      sliderInput(inputId = "f_temp",
                                                  label = "Fahreinhet (℉)",
                                                  value = 32,
                                                  min = 0,
                                                  max = 212),
                                      sliderInput(inputId = "c_temp",
                                                  label = "Celsius (℃)",
                                                  value = 0,
                                                  min = 0,
                                                  max = 100),

                                      actionButton("submitbutton",
                                                   "Convert",
                                                   class = "btn btn-primary")
                                    ),

                                    mainPanel(
                                      tags$label(h3('Status/Output(☔ ☼ ☁ 💦 ☔ )☃')), # Status/Output Text Box
                                      verbatimTextOutput('contents'),
                                      tableOutput("Fahrenheit"),
                                      tableOutput('Celsius'), # Results table
                                    ) # mainPanel()

                           ), #tabPanel(), Home

                           tabPanel("About",
                                    titlePanel("About"),
                                    div(includeMarkdown("about.md"),
                                        align="justify")
                           ) #tabPanel(), About

                ) # navbarPage()
) # fluidPage()


####################################
# Server                           #
####################################
server <- function(input, output, session) {

  # Input Data
  celsius <- reactive({
    results <- C_to_F(input$c_temp)
    cf_results <- data.frame(results)
    names(cf_results) <- "Celsius_2_Fahrenheit"
    print(cf_results)
  })

  fahrenheit <- reactive({
    results <- F_to_C(input$f_temp)
    fc_results <- data.frame(results)
    names(fc_results) <- "Fahrenheit_2_Celsius"
    print(fc_results)
  })

  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) {
      isolate("Calculation complete.")
    } else {
      return("Server is ready for calculation.")
    }
  })

  # Prediction results table
  output$Celsius <- renderTable({
    if (input$submitbutton > 0) {
      isolate(celsius())
    }
  })

  output$Fahrenheit <- renderTable({
    if (input$submitbutton > 0) {
      isolate(fahrenheit())
    }
  })


}


####################################
# Create Shiny App                 #
####################################
shinyApp(ui = ui, server = server)
