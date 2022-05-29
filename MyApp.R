library(shiny)
library(shinydashboard)
library(plotly)
library(plyr)
library(DT)

# Loads all Data sets in R
datasets <- ls("package:datasets")
selectedDataset <- ""
selectedColumn <- ""
selectedValue <- ""
inSelect <- ""
checkBoxTxt <- ""
graphPlot <- ""
filePath <- ls("package:datasets")
newFilename <- ""
status <- "A"

ui <- dashboardPage(skin = 'black',
  dashboardHeader(title = "Descriptive Statistics"),
  dashboardSidebar(width = 0),
  dashboardBody(box(fluidRow(width=12, column(width = 12, fileInput("file1", "Choose CSV File", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))), column(width=6, selectInput("datasets", "Select Data Set:", choices = c("",datasets), width = "400")),
                           column(width=6, selectInput("inSelect", "Select Column:", choices = c("",""), width = "400")), 
                           column(width=6, checkboxGroupInput("inCheckboxGroup", "Find:", c("Mean Value", "Median Value", "Variance Value", "Standard Deviation", "Five Number Summary"))),
                           column(width=12, verbatimTextOutput("codeb"), 
                                  column(width = 12, textOutput("ans"))))), 
                box(fluidRow(width=12, column(width=12, checkboxGroupInput("inCheckboxGroup2", "Plot:", c("Histogram", "Box Plot"))), column(width=12), verbatimTextOutput("codec"), 
                             column(width=12), plotOutput(outputId = "distPlot"))),
                box(fluidRow(column(width=12, DT::dataTableOutput("code"))))))


server <- function(input, output, session) {
  observe({
    # Assigns Graph types inCheckboxgroup text value
    graphPlot <- input$inCheckboxGroup2
    if (is.null(graphPlot) || graphPlot == "" || length(graphPlot) == 0){
      return()
    }
    else{
      if(length(graphPlot) == 1){
        # Plot histogram
        if(graphPlot == "Histogram"){
          output$distPlot <- renderPlot({
            if(input$inSelect != "" & status != "B"){
              dataValue <- eval(as.symbol(c(input$datasets)))
              colValue <- as.symbol(c(input$inSelect))
              x <- dataValue[[colValue]]
              
              if(is.null(x))
                return()
              
              par(bg = 'white')
              hist(x, xlab = paste(c(input$datasets), "$", toString(colValue), sep = ""),
                   main = paste("Histogram of ", paste(c(input$datasets), "$", toString(colValue), sep = "")))
            }
            
            # Plot Loaded Data Histogram  
            if(status == "B"){
              colValue <- as.symbol(c(input$inSelect))
              x <- filePath[[colValue]]
              
              if(is.null(x))
                return()
              
              par(bg = 'white')
              hist(x, xlab = paste(newFilename, "$", toString(colValue), sep = ""),
                   main = paste("Histogram of ", newFilename, "$", toString(colValue), sep = ""))
            }
          })
          # if(input$inSelect != "" & status != "B"){
          #   dataValue <- eval(as.symbol(c(input$datasets)))
          #   colValue <- as.symbol(c(input$inSelect))
          #   output$codec <- renderPrint({
          #     # Get the frequencies of data values used for the histogram
          #     table(dataValue[[colValue]])
          #   })
          # }
          # if(status == "B"){
          #   colValue <- as.symbol(c(input$inSelect))
          #   output$codec <- renderPrint({
          #     # Print Read Data Set
          #     table(filePath[[colValue]])
          #   })
          # }
        }
        # Plot Box Plot
        else if(graphPlot == "Box Plot"){
          output$distPlot <- renderPlot({
            if(input$inSelect != "" & status != "B"){
              dataValue <- eval(as.symbol(c(input$datasets)))
              colValue <- as.symbol(c(input$inSelect))
              x    <- dataValue[[colValue]]
              
              if(is.null(x))
                return()
              
              par(bg = 'white')
              boxplot(x, xlab = paste(c(input$datasets), "$", toString(colValue), sep = ""),
                      main = paste("Box Plot of", paste(c(input$datasets), "$", toString(colValue), sep = "")))
            }
            
            # Plot Loaded Data Boxplot  
            if(status == "B"){
              colValue <- as.symbol(c(input$inSelect))
              x <- filePath[[colValue]]
              
              if(is.null(x))
                return()
              
              par(bg = 'white')
              boxplot(x, xlab = paste(newFilename, "$", toString(colValue), sep = ""),
                      main = paste("Box Plot of ", newFilename, "$", toString(colValue), sep = ""))
            }
          })
          if(input$inSelect != "" & status != "B"){
            dataValue <- eval(as.symbol(c(input$datasets)))
            colValue <- as.symbol(c(input$inSelect))
            output$codec <- renderPrint({
              # Print Read Data Set
              summary(dataValue[[colValue]])
            })
          }
          if(status == "B"){
            colValue <- as.symbol(c(input$inSelect))
            output$codec <- renderPrint({
              # Print Read Data Set
              summary(filePath[[colValue]])
            })
          }
        }
      }
      else{
        updateCheckboxGroupInput(session, "inCheckboxGroup2", "Plot:", c("Histogram", "Box Plot"), selected = NULL)
        graphPlot <- ""
      }
    }
  })
  observe({
    checkBoxTxt <<- input$inCheckboxGroup
    if (is.null(checkBoxTxt) || checkBoxTxt == "" || length(checkBoxTxt) == 0){
      return()
    }
    else{
      if(length(checkBoxTxt) == 1){
        if(checkBoxTxt == "Mean Value"){
          output$ans <- renderText({
            
            if(input$inSelect != "" & status != "B"){
              dataValue <- eval(as.symbol(c(input$datasets)))
              colValue <- as.symbol(c(input$inSelect))
              paste(checkBoxTxt, " = ", mean(dataValue[[colValue]], na.rm = TRUE))
            }
            if(status == "B"){
              colValue <- as.symbol(c(input$inSelect))
              paste(checkBoxTxt, " = ", mean(filePath[[colValue]], na.rm = TRUE))
            }
          })
        }
        else if(checkBoxTxt == "Median Value"){
          output$ans <- renderText({
            
            if(input$inSelect != "" & status != "B"){
              dataValue <- eval(as.symbol(c(input$datasets)))
              colValue <- as.symbol(c(input$inSelect))
              paste(checkBoxTxt, " = ", median(dataValue[[colValue]], na.rm = TRUE))
            }
            if(status == "B"){
              colValue <- as.symbol(c(input$inSelect))
              paste(checkBoxTxt, " = ", median(filePath[[colValue]], na.rm = TRUE))
            }
          })
        }
        else if(checkBoxTxt == "Variance Value"){
          output$ans <- renderText({
            
            if(input$inSelect != "" & status != "B"){
              dataValue <- eval(as.symbol(c(input$datasets)))
              colValue <- as.symbol(c(input$inSelect))
              paste(checkBoxTxt, " = ", var(dataValue[[colValue]]))
            }
            if(status == "B"){
              colValue <- as.symbol(c(input$inSelect))
              paste(checkBoxTxt, " = ", var(filePath[[colValue]]))
            }
          })
        }
        else if(checkBoxTxt == "Standard Deviation"){
          output$ans <- renderText({
            
            if(input$inSelect != "" & status != "B"){
              dataValue <- eval(as.symbol(c(input$datasets)))
              colValue <- as.symbol(c(input$inSelect))
              paste(checkBoxTxt, " = ", sd(dataValue[[colValue]]))
            }
            if(status == "B"){
              colValue <- as.symbol(c(input$inSelect))
              paste(checkBoxTxt, " = ", sd(filePath[[colValue]]))
            }
          })
        }
        else if(checkBoxTxt == "Five Number Summary"){
    
          if(input$inSelect != "" & status != "B"){
            dataValue <- eval(as.symbol(c(input$datasets)))
            colValue <- as.symbol(c(input$inSelect))
            output$codeb <- renderPrint({
              # Print Read Data Set
              summary(dataValue[[colValue]])
            })
          }
          if(status == "B"){
            colValue <- as.symbol(c(input$inSelect))
            output$codeb <- renderPrint({
              # Print Read Data Set
              summary(filePath[[colValue]])
            })
          }
        }
      }
      else{
        updateCheckboxGroupInput(session, "inCheckboxGroup", "Find:",
                                 c("Mean Value", "Median Value", "Variance Value", 
                                   "Standard Deviation", "Five Number Summary"), selected = NULL)
        checkBoxTxt <- ""
      }
    }
  })
  
  # Print Read Data Set
  output$code <- renderTable({
    # Save the selected data set
    selectedDataset = input$datasets
    
    if(selectedDataset != ""){
      # Save the selected data set
      selectedDataset = input$datasets
      
      # Convert the string value to an object
      selectedValue <- eval(as.symbol(selectedDataset))
      
      # Extract the column names from the selected data set
      x <- colnames(selectedValue)
      
      # Update the column select input list
      updateSelectInput(session, "inSelect", choices = c("", x), selected = NULL)
      
      # View the data set columns and rows
      head(selectedValue)
    }
  })
  
  observe({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    status <<- "B"
    
    # Save the selected data set
    filePath <<- read.csv(inFile$datapath)
    
    newFilename <<- sub('\\.csv$', '', inFile$name)
    
    # Extract the column names from the selected data set
    x <- colnames(filePath)
    
    # Update the column select input list
    updateSelectInput(session, "inSelect", choices = c("", x), selected = NULL)
    
    # Update the column select input list
    updateSelectInput(session, "datasets", choices = c("", newFilename), selected = 1)
    
    # View the data set columns and rows
    head(selectedValue)
    
    output$code = DT::renderDataTable(
      filePath, options = list(
        lengthChange = TRUE,
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#42f', 'color': '#fff'});",
          "}"),
        autowidth = TRUE,
        columnDefs = list(list(width = '70%', targets = 1))
      )
    )
  })
}
shinyApp(ui, server)