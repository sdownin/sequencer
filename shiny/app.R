library(shiny)
#library(shinydashboard)
source('testModule.R')

####################################################################################
##                                  VIEW 
####################################################################################

ui <-  navbarPage("SequenceR",

	## Page 1 - Analysis ====================================
	tabPanel("Analysis", 
		
		tabsetPanel(
			
			## 1. Tab 1. Data
			tabPanel("Data", 
				
					titlePanel("Sequnce Dictionary"),
					sidebarLayout(
						sidebarPanel(
							fileInputUI("analysis_file_dictionary")
						),
						mainPanel(
							tableOutputSummaryUI("analysis_file_dictionary_summary", "Dictionary Summary")
						),		
					),
					#hr(),
					titlePanel("Sequnce Data"),
					sidebarLayout(
						sidebarPanel(
							fileInputUI("analysis_file_data")
						),
						mainPanel(
							tableOutputUI("analysis_file_data_table"),
							tableOutputSummaryUI("analysis_file_data_table_summary", "Data Summary")
						),		
					),
					hr()
				
				
			), 
			
			## 1. Tab 2. Measures
			tabPanel("Measures", 
					titlePanel("Select Measures to Compute"),
					actionButtonUIcheckdata("datacheck", "Data Check"),
					hr(),
					sidebarLayout(
						sidebarPanel(
							sliderTextUI("one"),
						),
						mainPanel(
							sliderTextUI("two"),
							hr(),
							actionButtonUI("one"),
							checkboxGroupUI("one")
						)
					),
					hr()
			), 
			
			## 1. Tab 3. Outputs
			tabPanel("Outputs", 
				"Outputs"
			)
			
		)
			
		
	), 
	
		
	## Page 2 - Inference ===================================
	tabPanel("Inference", 
		"Coming soon..."
	),
	
	## Pages Other - Info ===================================
	navbarMenu("Info",
		tabPanel("FAQ", 
			"Where is the project code repository?",
			a("https://github.com/sdownin/sequencer", href="https://github.com/sdownin/sequencer", target="_blank")
		),
		tabPanel("Version", 
			"[0.2.0] - 2019-11-12"
		)
	)

)


### Pages, Other  =======================================
#navbarMenu("Info",
#	tabPanel("FAQ",
#		"coming soon"
#	),
#	tabPanel("Version",
#		"coming soon"
#	)
#)



####################################################################################
##                                  SERVER 
####################################################################################
server <- function(input, output, session) {

	model_file <- './../R-Portable/tmp_sequencer_data_model.rds'

	if (file.exists(model_file)) {
		model <- readRDS(model_file)
	} else {
		model <- list(analysis_dictionary=list(data=NA, datapath=''), 
					  analysis_data=list(data=NA, datapath=''))
	}

	# server logic
	callModule(sliderTextServer, "one")
	callModule(sliderTextServer, "two")
	callModule(actionButtonServer, "one")
	callModule(checkboxGroupServer, "one")

	# dictionary table input #TODO: find why cannot call from external module?
	output$analysis_file_dictionary_summary <- renderPrint({
		inFile <- input$analysis_file_dictionary
		
		## NONE
		if (is.null(inFile)) 
			return(NULL)
		
		## SAVED
		if (inFile$datapath == model$analysis_dictionary$datapath)
			return(str(model$analysis_dictionary$data))
		
		## INPUT
		df <- read.csv(inFile$datapath, header = input$header, na.strings=c('','""'), stringsAsFactors=TRUE)
		li <- apply(df, 2, function(col){
			as.factor(unique(col[which(!is.null(col) & !is.nan(col) & !is.na(col))]))
		})
		
		model$analysis_dictionary$datapath <- inFile$datapath
		model$analysis_dictionary$data <- li
		saveRDS(model, file=model_file)
		
		str(li)
	})

	## data table input #TODO: find why cannot call from external module?
	#output$analysis_file_data_table <- renderDataTable({
	#	inFile <- input$analysis_file_data
	#	if (is.null(inFile)) 
	#		return(NULL)
	#	read.csv(inFile$datapath, header = input$header, sep=',', fill=TRUE, stringsAsFactors=TRUE)
	#}, options = list(lengthMenu = c(5, 15, 50), pageLength = 5)) 
 
	# Generate a summary of the dataset ----
	output$analysis_file_data_table_summary <- renderPrint({
		inFile <- input$analysis_file_data
		
		## NONE
		if (is.null(inFile)) 
			return(NULL)
		
		## SAVED
		if (inFile$datapath == model$analysis_data$datapath)
			return(summary(model$analysis_data$data))
		
		## INPUT
		df <- read.csv(inFile$datapath, header = input$header, sep=',', fill=TRUE, stringsAsFactors=TRUE)

		model$analysis_data$datapath <- inFile$datapath
		model$analysis_data$data <- df
		saveRDS(model, file=model_file)

		summary(df)
	})
 
	## Measures
	output$datacheck <- renderText({
		sprintf('Loaded Dictionary %s; Data %s', 
			    !all(is.na(model$analysis_data$data)), !all(is.na(model$analysis_data$data)))
	})
 
 
	# close the R session when Chrome closes
	session$onSessionEnded(function() { 
		stopApp()
		if (file.exists(model_file))
			file.remove(model_file)
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
shinyApp(ui, server, enableBookmarking = "url")  ## enableBookmarking = "server", "url"
