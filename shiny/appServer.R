
####################################################################################
##                                  SERVER 
####################################################################################
.server <- function()
{

	## MODEL FILE
	MODEL_FILE <- './../R-Portable/tmp_sequencer_data_model.rds'
	## INPUT FILE
	INPUT_FILE <- './../R-Portable/tmp_sequencer_input.rds'

	##
	# Init Model Object
	##
	initModel <- function()
	{
		return(list(
			## BOOL flag indicating if model was saved/updated during session
			updated=FALSE,
			## Sequence Alphabet
			analysis_alphabet = list(x=NA, xpath=''), 
			## Substitution Cost Matrix
			analysis_subcostmat = list(x=NA, xpath=''),
			## Sequence Data Series
			analysis_data = list(x=NA, xpath='')
		))
	}

	##
	# LOAD MODEL
	##
	loadModel <- function()
	{
		if (file.exists(MODEL_FILE)) 
			return(readRDS(MODEL_FILE))
		return(initModel())
	}

	##
	# SAVE MODEL TO DISK (TEMP FILE)
	##
	saveModel <- function(model, xname, x, xpath)
	{
		model[[xname]]$x <- x
		model[[xname]]$xpath <- xpath
		model$updated <- TRUE
		saveRDS(model, file=MODEL_FILE)
	}

	##
	#
	##
	saveInput <- function(input)
	{
		saveRDS(input, file=INPUT_FILE)
	}

	##
	# Check for all falseys except number zero
	##
	exists <- function(x)
	{
		if(any(is.null(x))) return(FALSE)
		if(all(is.nan(x))) return(FALSE)
		if(all(is.na(x))) return(FALSE)
		if(all(x == '')) return(FALSE)
		return(TRUE)
	}

	subcostmatPlotServer <- function(input, output, session){
		output$analysis_file_subcostmat_plot <- renderPlot({
			model <- loadModel()
			# if (!exists(model$analysis_subcostmat$x)) {
			# 	return()
			# }
			image(model$analysis_subcostmat$x)
		})
	}


	##
	# Main Server Function
	##
	server <- function(input, output, session) {

		model <- loadModel()
		cat('\nSERVER:  MODEL STRUCT\n')
		print(model)

		# server logic
		# callModule(sliderTextServer, "one")
		# callModule(sliderTextServer, "two")
		# callModule(actionButtonServer, "one")
		# callModule(checkboxGroupServer, "one")

		# alphabet table input #TODO: find why cannot call from external module?
		output$analysis_file_alphabet_summary <- renderPrint({
			inFile <- input$analysis_file_alphabet
			
			## NONE
			if (is.null(inFile)) 
				return(NULL)
			
			## INPUT
			df <- read.csv(inFile$datapath, header = input$header, na.strings=c('','""'), stringsAsFactors=TRUE)
			li <- apply(df, 2, function(col){
				as.factor(unique(col[which(!is.null(col) & !is.nan(col) & !is.na(col))]))
			})

			## BREAK SCOPE TO SAVE
			saveModel(loadModel(), xname='analysis_alphabet', x=li, xpath=inFile$datapath)
			
			str(li)
		})



		# alphabet table input #TODO: find why cannot call from external module?
		output$analysis_file_subcostmat_summary <- renderPrint({

			inFile <- input$analysis_file_subcostmat

			## NONE
			if (is.null(inFile)) 
				return(NULL)
			
			## INPUT
			df <- read.csv(inFile$datapath, header = input$header, na.strings=c('','""'), stringsAsFactors=TRUE)
			
			if (input$rownames) {
				row.names <- df[,1]
				df <- df[,-1]
				row.names(df) <- row.names
			} else {
				row.names <- names(df)
			}

			mat <- as.matrix(df)

			## SAVE
			saveModel(loadModel(), xname='analysis_subcostmat', x=mat, xpath=inFile$datapath)
			
			print(mat)
		})


		# # alphabet table input #TODO: find why cannot call from external module?
		# output$analysis_file_subcostmat_plot <- renderPlot({

		# 	inFile <- input$analysis_file_subcostmat

		# 	## NONE
		# 	if (is.null(inFile)) 
		# 		return(NULL)
			
		# 	model <- loadModel()
		# 	image(model$analysis_subcostmat$x)
		# })


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

			model <- loadModel()
			
			## SAVED
			if (inFile$datapath == model$analysis_data$xpath)
				return(summary(model$analysis_data$x))
			
			## INPUT
			df <- read.csv(inFile$datapath, header = input$header, sep=',', fill=TRUE, stringsAsFactors=TRUE)

			## BREAK SCOPE TO SAVE
			saveModel(model, xname='analysis_data', x=df, xpath=inFile$datapath)

			summary(df)
		})

		## Measures 
		# output$actionButtonDataCheckValue <- renderText({
		# 	model <- loadModel()
		# 	print("CHECK FROM actionButtonDataCheckValue renderText")
		# 	print(model)
		# 	if (model$updated != 'FALSE' | model$updated) {
		# 	    return(sprintf('Loaded Alphabet %s; Data %s', 
		# 			    !all(is.na(model$analysis_data$x)), !all(is.na(model$analysis_data$x))))
		# 	} else {
		# 		return('model not updated yet.')
		# 	}
		# })

		# output$actionButtonDataCheckValue <- renderText({
		# 	input$actionButtonDataCheck
		# })
		# output$actionButtonDataCheckValue <- renderText({
		# 	if(input$actionButtonDataCheck) {
		# 		model <- loadModel()
		# 		if (model$updated != 'FALSE' | model$updated) {
		# 		    return(sprintf('Loaded Alphabet %s;  Substitution Costs %s;  Data %s', 
		# 				    !all(is.na(model$analysis_alphabet$x)), 
		# 				    !all(is.na(model$analysis_subcostmat$x)),
		# 				    !all(is.na(model$analysis_data$x))))
		# 		} else {
		# 			return('model not updated yet.')
		# 		}
		# 	} 
		# 	NULL
		# })

	 
		# close the R session when Chrome closes
		session$onSessionEnded(function() { 
			stopApp()
			if (file.exists(MODEL_FILE))
				file.remove(MODEL_FILE)
			if (file.exists(INPUT_FILE))
				file.remove(INPUT_FILE)
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

	return(server)
}
