library(shiny)
library(shinydashboard)
library(rsvg)
library(DiagrammeRsvg)
library(DiagrammeR)

source("functions.R")


roses_pdf <- function(x, filename = "roses.pdf") {
  utils::capture.output({
    rsvg::rsvg_pdf(svg = charToRaw(DiagrammeRsvg::export_svg(x)),
                   file = filename)
  })
  invisible()
}
roses_png <- function(x, filename = "roses.png") {
  utils::capture.output({
    rsvg::rsvg_png(svg = charToRaw(DiagrammeRsvg::export_svg(x)),
                   file = filename)
  })
  invisible()
}

# Define UI for application that draws a histogram
ui <- shinyUI(navbarPage("ROSES Flow Chart",

    # Tab 1 ----
    tabPanel("Home",
         fluidRow(
            column(10, offset = 1,
                   tags$a(href="https://www.roses-reporting.com/", tags$img(height = "80px", src = "https://github.com/nealhaddaway/ROSES_flowchart/blob/master/inst/extdata/roses.png?raw=true")),br(),
                   br(),
                   'Systematic reviews should be described in a high degree of methodological detail. ', tags$a(href="http://www.roses-reporting.com/", "The ROSES reporting standards"), 
                   'call for a high level of reporting detail in systematic reviews and systematic maps. An integral part of the methodological description of a review 
                   is a flow diagram/chart.',
                   br(),
                   br(),
                   'This tool allows you to produce a flow chart for your own review that conforms to ', tags$a(href="http://www.roses-reporting.com/", "ROSES reporting standards."), 
                   'You can provide the numbers for the boxes in the input fields in the \'Create flow chart\' tab. Select whether your synthesis is a \'review\' or a \'map\' and whether title and abstract 
                   screening were \'combined\' or performed \'separately\'.',
                   br(),
                   br(),
                   'Please let us know if you have any feedback or if you encounter an error by sending an email to ', tags$a(href="mailto:neal.haddaway@sei.org", "neal.haddaway@sei.org."),
                   br(),
                   br(),
                   '***TO BEGIN***',
                   br(),
                   'Navigate to the \'Create flow chart\' tab and enter your data.'),
    ),

        # Show a plot of the generated distribution
        fluidRow(
            column(10, offset = 1,
            br(),
            hr(),
            'Credits:',
            br(),
            'Neal R Haddaway (creator)', br(),
            br(),
            tags$a(href="https://github.com/nealhaddaway/ROSES_flowchart", tags$img(height = 40, width = 40, src = "https://pngimg.com/uploads/github/github_PNG40.png")), 
            'Created November 2020', br(),
            br(),
            'Cite as:', br(),
            'Haddaway, N. R. (2020) ROSES_flowchart(): An R package and ShinyApp. doi: 10.5281/zenodo.4294810.'
            )
        )
    ),
    
    # Tab 2 ----
    tabPanel("Create flow chart",
             column(12,
                    column(4,
                           h3("Options"), 
                           selectInput("type", "Synthesis type:", choices=c('review', 'map')), 
                           selectInput("combined", "Title and abstract screening:", choices=c('combined', 'separately')),
                           hr()),
                    column(7,
                           tags$a(href="https://www.roses-reporting.com/", tags$img(height = "80px", src = "https://github.com/nealhaddaway/ROSES_flowchart/blob/master/inst/extdata/roses.png?raw=true")),
                           hr())
             ),
             sidebarLayout(  
             
              sidebarPanel(style = "overflow-y:scroll; max-height: 900px; position:relative;",
                h3("Data sources"),
                splitLayout(textInput("dbresults", label = 'Database results:', value = ''),
                            textInput("otherresults", label = "Other sources results:", value = '')),
                textInput("prescreened", label = "Prescreened records:", value = ''),
                h3("Deduplication"),
                splitLayout(textInput("deduped", label = "Deduplicated records:", value = ''),
                            textInput("dupesremoved", label = "Duplicates removed:", value = '')),
                h3("Screening"),
                
                conditionalPanel(
                       condition = "input.combined == 'combined'",
                       
                       splitLayout(textInput("tandaincl", label = "Included titles and abstracts:", value = ''),
                                   textInput("tandaexcl", label = "Excluded titles and abstracts:", value = ''))
                       ),     
                conditionalPanel(
                  condition = "input.combined == 'separately'",
                  splitLayout(textInput("titleincl", label = "Included titles:", value = ''),
                              textInput("titleexcl", label = "Excluded titles:", value = '')),
                  splitLayout(textInput("abstractincl", label = "Included abstracts:", value = ''),
                              textInput("abstractexcl", label = "Excluded abstracts:", value = ''))),
              
                textInput("ftretr", label = "Retrieved full texts:", value = ''), 
                splitLayout(textInput("ftnotretr_na", label = "Inaccessible full texts:", value = ''),
                           textInput("ftnotretr_nf", label = "Full texts not found:", value = '')),
                splitLayout(textInput("ftincl", label = "Included full texts:", value = ''),
                           textInput("ftexcl", label = "Excluded full texts, with reasons:", value = '', placeholder = 'Reason1, n; Reason2, n; Reason3, n')),
                h3("Articles/studies"),
                splitLayout(textInput("studart1", label = "Articles in the review:", value = ''),
                           textInput("studart2", label = "Studies in the review:", value = '')),
               
               conditionalPanel(
                 condition = "input.type == 'review'",
                 h3("Critical appraisal and synthesis"),
                 splitLayout(textInput("caincl", label = "Studies included after CA:", value = ''),
                             textInput("caexcl", label = "Studies excluded after CA:", value = '')),
                 textInput("narrincl", label = "Studies included in synthesis:", value = ''),
                 splitLayout(textInput("finalincl", label = "Final included studies:", value = ''),
                             textInput("finalexcl", label = "Studies not included:", value = ''))
                 
                 ),
               conditionalPanel(
                 condition = "input.type == 'map'",
                 h3("Synthesis"),
                 textInput("finalmapincl", label = "Studies included in the map:", value = ''))
               ),
             mainPanel(
               DiagrammeR::grVizOutput(outputId = "plot1", width = "90%", height = "800px"),
               hr(),
               h3("Download"),
               downloadButton('ROSESflowchartPDF', 'Download PDF'),
               downloadButton('ROSESflowchartPNG', 'Download PNG')
             )
              ))
    )
    )
    


# Define server logic required to draw a histogram
server <- function(input, output) {
    
# Reactive plot ----
    
    # Create plot
    plot <- reactive({
      if(input$combined == 'combined'){
        combinedinput <- TRUE
      } else {
        combinedinput <- FALSE
      } 
      ftnotretr <- data.frame(reason = c('Not accessible', 'Not found'), n = c(input$ftnotretr_na, input$ftnotretr_nf))
      ftexcl <- data.frame(reason = gsub(",.*$", "", unlist(strsplit(input$ftexcl, split = '; '))), 
                 n = gsub(".*,", "", unlist(strsplit(input$ftexcl, split = '; '))))
      studart <- c(input$studart1, input$studart2)
      plot <- ROSES_flowchart(dbresults = input$dbresults,
                              otherresults = input$otherresults,
                              deduped = input$deduped,
                              dupesremoved = input$dupesremoved,
                              tandaincl = input$tandaincl,
                              tandaexcl = input$tandaexcl,
                              titleincl = input$titleincl,
                              titleexcl = input$titleexcl,
                              abstractincl = input$abstractincl,
                              abstractexcl = input$abstractexcl,
                              ftretr = input$ftretr,
                              ftnotretr = ftnotretr,
                              ftincl = input$ftincl,
                              ftexcl = ftexcl,
                              prescreened = input$prescreened,
                              studart = studart,
                              caincl = input$caincl,
                              caexcl = input$caexcl,
                              narrincl = input$narrincl,
                              finalincl = input$finalincl,
                              finalexcl = input$finalexcl,
                              finalmapincl = input$finalmapincl,
                              interactive = FALSE,
                              type = input$type,
                              combined = combinedinput)
    })
    
    
    # Display plot
    output$plot1 <- DiagrammeR::renderDiagrammeR({
        plot <- plot()
    })
    
    
# Handle downloads ----
    output$ROSESflowchartPDF <- downloadHandler(
        filename = "roses.pdf",
        content = function(file){
            roses_pdf(plot(), 
                       file)
            }
        )
    output$ROSESflowchartPNG <- downloadHandler(
        filename = "roses.png",
        content = function(file){
            roses_png(plot(), 
                       file)
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)


