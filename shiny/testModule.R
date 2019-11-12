library(shiny)

## UI #####################################
sliderTextUI <- function(id){
	ns <- NS(id)
	tagList(
		sliderInput(ns('slider'), "Slide me", 0, 100, 1),
		textOutput(ns('number'))
	)
}

checkboxGroupUI <- function(id) {
	ns <- NS(id)
	tagList(
		checkboxGroupInput(ns("checkGroup"), label = h3("Checkbox group"), 
			choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),
			selected = 1),
		textOutput(ns('checkGroupValue'))
	)
}

actionButtonUI <- function(id) {
	ns <- NS(id)
	tagList(
		actionButton(ns("actionButton"), label = "Action"),
		textOutput(ns('actionButtonValue'))
	)
}

fileInputUI <- function(fileId) {
	tagList(
		fileInput(fileId, h3("Choose CSV File"),
			multiple = F,
			accept = c(
			  "text/csv",
			  "text/comma-separated-values,text/plain",
			  ".csv")
		),
		tags$hr(),
		checkboxInput("header", "Header", TRUE),
		selectInput("file1Rows", label="Read N Rows", 
					choices=c("All"=Inf, "20"=20), 
					selected = "All", multiple = FALSE,
					selectize = TRUE, width = NULL, size = NULL)
	)
}

tableOutputUI <- function(id) {
	tagList(
		dataTableOutput(id)
	)
}

tableOutputSummaryUI <- function(id) {
	tagList(
		h4("Table Summary"),
		verbatimTextOutput(id)
	)
}

#tableOutputSummaryUI <- function(id) {
#	tagList(
#		tableOutput(id)
#	)
#}
#
#tableOutputPlotUI <- function(id) {
#	tagList(
#		plotOutput(id)
#	)
#}


## SERVER ###################################
sliderTextServer <- function(input, output, session){
  output$number <- renderText({
    input$slider
  })
}

checkboxGroupServer <- function(input, output, session){
  output$checkGroupValue <- renderText({
    input$checkGroup
  })
}

actionButtonServer <- function(input, output, session){
  output$actionButtonValue <- renderText({
    input$actionButton
  })
}

#fileInputServer <- function(input, output, session) {
#  # You can access the value of the widget with input$file, e.g.
#  output$file1Table <- renderDataTable({
#    # input$file1 will be NULL initially. After the user selects
#    # and uploads a file, it will be a data frame with 'name',
#    # 'size', 'type', and 'datapath' columns. The 'datapath'
#    # column will contain the local filenames where the data can
#    # be found.
#    inFile <- input$file1
#
#    if (is.null(inFile))
#      return(NULL)
#
#	read.csv(inFile$datapath, header = input$header, stringsAsFactors=FALSE)
#  })
#  
#}
