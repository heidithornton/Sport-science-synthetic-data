library(shiny)
library(synthpop)
library(DT)
library(tidyverse)
library(readxl)
library(data.table)
library(rsconnect)
library(fontawesome)
library(htmltools)
library(shinythemes)
library(RcppRoll)
library(dplyr)
library(reactable)
library(shinydashboard)
library(shinydashboardPlus)
library(formattable)
library(dashboardthemes)
library(readr)
library(deidentifyr)
library(anonymizer)
library(digest)
library(shinycssloaders)
suppressWarnings(suppressPackageStartupMessages(library(ggplot2)))
library(ggsci)
library(ggbeeswarm)
library(ggpubr)
library(sdamr)
library(cowplot)
library(kableExtra)


# User interface
ui <- fluidPage(theme = shinytheme("cosmo"),
  
                navbarPage("Synthetic data",
                           tabPanel("What is synthetic data?", 
                                    fluidRow(
                                      column(width = 4,
                                             box(width= 12, h3(strong("What is synthetic data?"))),
                                             box(width = 12, h4(HTML(paste0("<div style='text-align: justify;'>Synthetic datasets contain simulated data which replace some or all of all the original observed values, with values which are sampled from the underlying distribution of the data.",tags$sup("1"),
                                                                            " This ensures that the essential features (e.g., distributional shape, skewness, missing data etc.) of the original dataset are replicated.",tags$sup("2,3"),
                                                                            " As the individual data points which could be de-identifiable in the original data are replaced, privacy and confidentiality concerns are alleviated, with the synthetic dataset able to then be shared freely.</div>"))),
                                                 br(),
                                                 br(),
                                                 actionButton(inputId='link', label="Link to published paper", icon = icon("th"), 
                                                              onclick ="window.open('https://journals.humankinetics.com/view/journals/ijspp/aop/issue.xml', 
                                                              '_blank')"),
                                                 br(),
                                                 br(),
                                                 br(),
                                                 br(),
                                                 h5(style = "margin-top: 10px;", strong("References")),
                                                 h6(HTML(paste0(tags$sup("1"), " Nowok B, Raab GM, Dibben C. Providing bespoke synthetic data for the UK Longitudinal Studies and other sensitive data with the synthpop package for R 1. ", tags$i("Stat J IAOS"), ". 2017;33(3):785-796"))),
                                                 h6(HTML(paste0(tags$sup("2"), " Warmenhoven J, Harrison A, Quintana D, Hooker G, Gunning E, Bargary N. Unlocking sports medicine research data while maintaining participant privacy via synthetic datasets. ", tags$i("SportRxiv"), ". 2020"))),
                                                 h6(HTML(paste0(tags$sup("3"), " Quintana DS. A synthetic dataset primer for the biobehavioural sciences to promote reproducibility and hypothesis generation. ", tags$i("Elife"), ". 2020;9"))),
                                                 h6(HTML(paste0(tags$sup("4"), " Wiewelhove T, Raeder C, Meyer T, Kellmann M, Pfeiffer M, Ferrauti A. Markers for routine assessment of fatigue and recovery in male and female team sport athletes during high-intensity interval training. ", tags$i("Plos One"), ". 2015;10(10):e0139801"))))
                                             
                                             ),
                                             
                                             column(width = 8, align="center",
                                                    br(),
                                                    box(width = 12, h4(HTML(paste0(
                                                      "<div style='text-align: justify;'>",
                                                      "<strong>Example.</strong> Raincloud plot comparison between the original observed and synthetic datasets for delayed onset muscle soreness at pre, immediately post (Post 1) and 72 hours post (Post 2) the completion of a six-day training intervention. ",
                                                      "Each dot represents an individual data point, and the single vertical line indicates the mean. Figure adapted from previous research.",
                                                      tags$sup("4"), " The table below presents the descriptive data, as well a statistical comparison using Welch's t-test.",
                                                      "</div>"))),
                                                      br(),
                                                      br(),
                                                      plotOutput(outputId = "domsPlot", height = 400, width = "62%"), 
                                                      br(),
                                                      br(),
                                                      br(),
                                                      dataTableOutput("doms"), style = "font-size:89%")))
      ),

      tabPanel("Using this app", 
               
               fluidRow(
                 column(width = 6, 
                        h3(strong("Instructions on how to use this app")), 
                        br(),
                        h4("This application creates a synthetic dataset using the", strong("synthpop"), "package. 
                        Either download the dataset provided from the table on the right, or upload your own data to create a synthetic dataset"), 
                        br(),
                        h4(strong("1."), "Your file must be in csv format to upload"),
                        br(),
                        h4(strong("2."), "There is a 15 mb size limit on csv file uploads"),
                        br(),
                        h4(strong("3."), "The synthpop package recommends your dataset have a modest number of variables (between 8 to 12). If any larger, it may take a long time to process, or simply not work"),
                        br(),
                        h4(strong("4."), "Each variable should be in a different column, and each observation should be in a different row"), 
                        br(),  
                        h4(strong("5."), "The first row in the data should be the column headings. See the example on the right side of the page"),
                        br(),
                        h4(strong("6."), "Athlete identification (i.e., Name) should be text only and not include numerical values. You can anonomise athlete identifications using this app"),
                        br(),
                        h4(strong("7."), "Use the tabs at the top of the page to upload your data, view and download the synthetic data, and to visualise the original and synthetic dataset")
                 ),
                 
                 column(width = 6, tags$div(
                   style = "width: 900px;",
                        h3(strong("Example dataset")),
                   br(),
                   downloadButton("download", "Download csv."),
                   br(),
                        dataTableOutput("exampleData"), style = "font-size:80%")))),
   
      # Upload data tab
      tabPanel("Upload data",

               sidebarLayout(
                 
                 sidebarPanel(width = 3,
                   
                     h4(strong("Upload original data")),
                    br(),
                     
                     fileInput(inputId = "datafile", 
                               label = HTML("1. Upload a csv file (your own or the example provided on the 'Using this app' tab) then press 'Update' below.<br><br>Note, the larger your dataset, the longer it will take to load"), multiple = FALSE, placeholder = "No file selected", 
                               accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                 br(),
                 br(),
                 uiOutput(outputId = "variables"),
                 br(),
                 actionButton(inputId = "update", label = "Update", icon = icon("fas fa-sync")), 
                 br(),
                 br(),
                 br(),
                 h5(strong("2. To view and download the synthesised dataset, click on the 'Synthetic data' tab at the top", 
                           HTML("<br><br>"),
                           "The synthesized variables have 'SYNTH' at the beginning of the heading"))), 
               
                 mainPanel(
                     tags$div(
                       style = "overflow-x: scroll;",
                       dataTableOutput("table"), style = "font-size:85%"))
                 )),

      # Synthetic data download       
      tabPanel("Synthetic data",
               
               sidebarLayout(
                 
                 sidebarPanel(width = 3,
                    
                    h4(strong("Download synthetic dataset")),
                    br(),
                    h5(strong("1. Download the synthetic data using the button below. Select this if you don't wish to anonomise your dataset")), 
                    br(),
                    downloadButton("downloadSynth", "Download synthetic dataset"), 
                    br(),
                    br(),
                    br(),
                    h4(strong("Anonomise data?")),           
                    br(),
                    uiOutput(outputId = "anon"),
                    br(),
                    actionButton(inputId = "update2", label = "Update", icon = icon("fas fa-sync")), 
                    br(),
                    br(),
                    downloadButton("downloadAnonSynth", "Download anonomised synthetic dataset")), 
                    
                mainPanel(tags$div(
                          style = "overflow-x: scroll;",
                          h3(strong("Simulated synthetic dataset")),
                          br(),
                          withSpinner(
                          dataTableOutput("synth")), style = "font-size:85%")))
        ),
                  
      # Plot page  
      tabPanel("Comparison plots",          
               
                   column(12, h3(strong("View comparison plots"))), 
                   br(),
                   column(12, uiOutput(outputId = "selection1")),
                   column(12, 
                          withSpinner(
                            plotOutput(outputId = "plot", height = 530))))
          ))
          


#  Server function
server <- function(input, output, session) {
  
      # 15 mb file limit
      options(shiny.maxRequestSize=15*1024^2)
  
        inputData <- read_csv("Comparison.csv", show_col_types = FALSE) %>%
          select("SYN_DOMS_Post2","DOMS_Post2","SYN_DOMS_Post1","DOMS_Post1","SYN_DOMS_Pre","DOMS_Pre")
        
        # Change column names
        colnames(inputData) <- c('Synthetic - Post 2','Original - Post 2','Synthetic - Post 1','Original - Post 1','Synthetic - Pre','Original - Pre')
        
        # Reformat the data for ggplot
        inputData <- gather(inputData, condition, value, colnames(inputData), factor_key = TRUE) %>% filter(value != "")

      
      # Read in required datasets
      exampleDataset <- read_csv("RugbySyntheticData.csv", show_col_types = FALSE)
      
      my_data <- read_excel("DOMS comparison data.xlsx")
      my_data$Synthetic <- paste(my_data$Syn_mean, "\u00B1", my_data$syn_SD)
      my_data$Observed <- paste(my_data$Obs_mean, "\u00B1", my_data$Obs_SD)
      my_data$Mean_diff <- paste(my_data$Mean_diff, "(",my_data$CI_lower, "to", my_data$CI_upper,")")
      my_data <- select(my_data, c(Time_point, Synthetic, Observed, Mean_diff, T_stat, P_val))
      
      my_data$Synthetic <- as.character(my_data$Synthetic)
      my_data$Observed <- as.character(my_data$Observed)
      my_data$Mean_diff <- as.character(my_data$Mean_diff)

      
    
      
    ## Download handler for rugby example data  
     output$download <- downloadHandler(
        filename = function() {
          paste("RugbySyntheticDatat", "csv", sep = ".")
        },
        content = function(file) {
          # Write the df data frame to a csv file
          write.csv(exampleDataset, file, row.names = FALSE)
        }
      )
      
      
  
      # Generate DOMS plot
      output$domsPlot <- renderPlot({
          ggplot(inputData, aes(x = condition, y = value, fill = condition, color = condition)) +
          
          ylab(bquote(bold("Delayed onset muscle soreness"))) +
          xlab("") +
          
          scale_shape_identity() +
          scale_y_continuous(limits = c(-0.3, 6.3), expand = c(0, 0), breaks = seq(0, 6, by = 1)) +
          theme_classic() +
          
          theme(legend.position = "none",
                axis.title.y = element_text(size = 14, colour = "black", hjust = 0.5, vjust = 0.5),
                axis.title.x = element_text(size = 13, colour = "black"),
                axis.text = element_text(size = 12, colour = "black", hjust = 0.5, vjust = 0.5)) +
          
          scale_color_igv() +
          scale_fill_igv() +
          geom_beeswarm(size = 1.8, alpha = 1,
                        aes(shape = 16)) +
          
          geom_flat_violin(position = position_nudge(x = 0.2, y = 0),
                           adjust = 2, alpha = 0.6,
                           trim = TRUE, scale = "width") +
          stat_summary(fun.min = mean, fun.max = mean,
                       geom = "errorbar", width = 0.5,
                       position = position_nudge(x = 0.2, y = 0), size = 0.2,
                       color = "black") +
          coord_flip() 
      })
      
      
      ## Raw statistical data for DOMS synthetic vs raw
      output$doms <- renderDataTable({
        DT::datatable(my_data,
                      rownames = FALSE,
                      colnames = c("Time", 
                                   paste0("Synthetic", " (mean ", "\u00B1", " SD)"), 
                                   paste0("Observed", " (mean ", "\u00B1", " SD)"), 
                                   "Mean difference (95% CI)", 
                                   "T-statistic", 
                                   "P value"),
                options = list(
                               searching = FALSE, paging = FALSE, info = FALSE,
                               columnDefs = list(list(className = 'dt-center', targets = '_all')),
                               dom = 'frtip'))
        
      }) 

      
      # Original dataset
      uploadedData <- reactive({
        inFile <- input$datafile
        if (is.null(inFile))
          return(NULL)
        read_csv(inFile$datapath)
      })
      
      # Create a table of the data
      output$table1 <- renderTable({
            downloadDataset()
      })
      
 
        # Original dataset table
      output$table <- renderDataTable({
        withProgress(message = 'Loading data...', value = 0, {
          
          # Return the table
          DT::datatable(uploadedData(), 
                        caption = htmltools::tags$caption(
                          style = 'caption-side: top; text-align: left; color: black; font-size:140%',
                          h3(strong("Original data"))),
                        rownames = FALSE,
                        options = list(scrollX = TRUE, scrollY = FALSE,
                                       lengthMenu = list(c(17)), searching = FALSE,
                                       columnDefs = list(list(className = 'dt-center', targets = '_all')),
                                       dom = 'frtip'))
        })
        })
      

      # Download the synthetic dataset
      output$downloadSynth <- downloadHandler(
        filename = function() {
          paste("syntheticDataset", "csv", sep = ".")
        },
        content = function(file) {
          # Write the df data frame to a csv file
          write.csv(df, file, row.names = FALSE)
        }
      )
  
      ## Original data
      output$table <- renderDataTable({
        withProgress(message = 'Loading data...', value = 0, {
          
          # Return the table
          DT::datatable(uploadedData(), 
                        caption = htmltools::tags$caption(
                          style = 'caption-side: top; text-align: left; color: black; font-size:140%',
                          h3(strong("Original data"))),
                        rownames = FALSE,
                        options = list(scrollX = TRUE, scrollY = FALSE,
                                       lengthMenu = list(c(17)), searching = FALSE,
                                       columnDefs = list(list(className = 'dt-center', targets = '_all')),
                                       dom = 'frtip'))
    })
  })  
  
      # Select Input
      output$variables <- renderUI({
        validate(
          need(!is.null(uploadedData()) && ncol(uploadedData()) > 0 && any(sapply(uploadedData(), is.numeric)), 
               "Please ensure you have read the instructions on the 'Using this app page")
        )
        original <- uploadedData() %>% dplyr::select(where(is.numeric))
        
        selectInput(inputId = "variables", 
                    label = "2. Select the variables you'd like to synthesize",
                    choices = colnames(original),
                    multiple = TRUE)
      })
      
      
      # Example data
      output$exampleData <- renderDataTable(exampleDataset, 
                                            server = FALSE, 
                                            rownames = FALSE, 
                                            options = list(lengthMenu = list(c(17)), searching = FALSE,
                                                           columnDefs = list(list(className = 'dt-center', targets = '_all')),
                                                           dom = 'frtip'))
      
     
  # Define an observer for the update button
  observeEvent(input$update, {
    if(!is.null(input$datafile)){
      
      # Check if file size is greater than 15 MB - CHANGE BACK TO 15
      if(file.size(input$datafile$datapath) > 15000000) {
        showNotification("File size is greater than 15 MB. Please upload a smaller file and refer to instructions on the 'Using this app' tab", type = "error", duration = NULL)
        return(NULL)
      }
      
      # Create original file for uploaded data
      original <- read_csv(input$datafile$datapath)
      
      print(original)
      
      # Subset the original data to only keep numeric columns
      original_numeric <- original %>% select(where(is.numeric))
      
      # Get the selected variables for synthesis
      variables_to_synthesize <- input$variables

      my.seed <- 20000000
      synResult <- syn(original_numeric %>% select(all_of(variables_to_synthesize)),
                       seed = my.seed, maxfaclevels = 200)
      
      # Combine synthesized variables with the remaining variables in the original data
      synthesized_data <- synResult$syn %>% mutate(rowid = row_number())
      original_not_selected <- original %>% select(-all_of(variables_to_synthesize))
      synth <- bind_cols(original_not_selected, synthesized_data) %>% select(-rowid)
      
      colnames(synth)[-(1:length(original_not_selected))] <- paste("SYNTH_", colnames(synth)[-(1:length(original_not_selected))], sep="")
      df <- synth 
      assign("df", df, envir = .GlobalEnv)
      
      ## print
      df1 <- df
      colnames(df1) <- gsub("SYNTH_", "", colnames(df1))
      df1 <- df1[, intersect(variables_to_synthesize, colnames(df1))]

      contentsrea <- reactive({
        inFile <- input$datafile
        if(is.null(inFile))
          return(NULL)
        dataset <- read_csv(inFile$datapath)
      })
      
      # Variable dropdown to anonomise data
      output$anon <- renderUI({
        
        selectInput(inputId = "anon", 
                    label="2. Select the variable you'd like to anonomise (i.e., athlete name). If not necessary, leave as blank. Please ensure the values are text only",
                    choices = c(" ", colnames(df)),
                    selected = NULL)
      })
      
      
      # Synthetic dataset
      output$synth <- DT::renderDataTable({

        Sys.sleep(3)
        DT::datatable(df, rownames = FALSE, 
                        options = list(dom = 'frtip',
                        scrollX = TRUE, scrollY = FALSE,
                        pageLength = 18, lengthMenu = list(c(18)),
                        searching = FALSE,
                        columnDefs = list(list(className = 'dt-center', targets = '_all'))))
      })
      
      # Select Input
      output$selection1 <- renderUI({
        
        synthesized_datadf <- subset(synthesized_data, select = -rowid)
        
        selectInput(inputId = "selection1", 
                    label=HTML("Select the variable you'd like to plot.<br><br>Synthesized variables only will be displayed here"),
                    br(),
                    choices = colnames(synthesized_data)[colnames(synthesized_data) != "rowid"], 
                    multiple = FALSE)
      })
      
      # Final plots
      output$plot <- renderPlot({
          # Check if input$selection1 is not NULL
          req(input$selection1)
          Sys.sleep(3)
          
          # Filter datasets
          original1 <- original %>% select(!!!input$selection1)
          original1 <- as.data.frame(lapply(original1, as.numeric))
          
          df1 <- df1 %>% select(!!!input$selection1)
          df1 <- as.data.frame(lapply(df1, as.numeric))
          
          compare(df1, original1, print.coef = TRUE,
                  vars= colnames(original1), cols = c("#62B6CB", "#1B4965"), stat = "counts")

      
    })}  
  
  
  # Synthetic dataset with update for anon
  observeEvent(input$update2, {
    if(!is.null(input$datafile)){
      
      # Update the dfanon data frame
      selection <- input$anon 
      dfanon <<- as_tibble(df) %>%
                  select(!!as.name(selection)) %>%
                  unique() %>%
                  rowwise() %>%
                  mutate(anonID = digest(!!as.name(selection), algo = 'crc32'))
      
      dfanon <<- merge(dfanon, df, by= selection)
      dfanon <<- dfanon[,!(names(dfanon) %in% selection)]
      
      # Render the table
      output$synth <- DT::renderDataTable({
        datatable(dfanon,
                  rownames=FALSE, 
                  options = list(dom = 'frtip', scrollX = TRUE, scrollY = FALSE, pageLength = 18,
                                 lengthMenu = list(c(18)), searching = FALSE,
                                 columnDefs = list(list(className = 'dt-center', targets = '_all'))))
            })
          }
        })
  
          # Download the anonomised synthetic dataset
          output$downloadAnonSynth <- downloadHandler(
            filename = function() {
              paste("anonSyntheticDataset", "csv", sep = ".")
            },
            content = function(file) {
              # Write the dfanon data frame to a csv file
              write.csv(dfanon, file, row.names = FALSE)
            }
          )
        })  
}

# Run the app ----

shinyApp(ui, server)
