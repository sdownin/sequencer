library(shiny)
source('testModule.R')


ui <- fluidPage(
	titlePanel("Test App Title"),
	sidebarLayout(
		sidebarPanel(
			fileInputUI("file1")
		),
		mainPanel(
			tableOutputUI("file1Table"),
			tableOutputSummaryUI("file1TableSummary")
		),		
	),
	hr(),
	sidebarLayout(
		sidebarPanel(
			sliderTextUI("one"),
			a("Test Link (Google)", href="https://google.com", target="_blank")
		),
		mainPanel(
			sliderTextUI("two"),
			hr(),
			actionButtonUI("one"),
			checkboxGroupUI("one")
		)
	),
	hr()
)


server <- function(input, output, session) {
	# server logic
	callModule(sliderTextServer, "one")
	callModule(sliderTextServer, "two")
	callModule(actionButtonServer, "one")
	callModule(checkboxGroupServer, "one")

	# data table input #TODO: find why cannot call from external module?
	output$file1Table <- renderDataTable({
		inFile <- input$file1
		if (is.null(inFile)) 
			return(NULL)
		read.csv(inFile$datapath, header = input$header, 
				nrows=as.numeric(input$file1Rows), stringsAsFactors=FALSE)
	}, options = list(lengthMenu = c(5, 15, 50), pageLength = 5))
 
	# Generate a summary of the dataset ----
	output$file1TableSummary <- renderPrint({
		inFile <- input$file1
		if (is.null(inFile)) 
			return(NULL)
		df <- read.csv(inFile$datapath, header = input$header, 
						nrows=as.numeric(input$file1Rows), stringsAsFactors=FALSE)
		summary(df)
	})
 
	# close the R session when Chrome closes
	session$onSessionEnded(function() { 
		stopApp()
		q("no") 
	})
}

# ## SORCE CODE FOR session$onSessionEnded
# onSessionEnded = function(callback) {
#   "Registers the given callback to be invoked when the session is closed
#       (i.e. the connection to the client has been severed). The return value
#       is a function which unregisters the callback. If multiple callbacks are
#       registered, the order in which they are invoked is not guaranteed."
#   return(.closedCallbacks$register(callback))
# }

## RUN
shinyApp(ui, server)
