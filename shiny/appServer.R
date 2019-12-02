
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
			## Sequence Alphabet
			analysis_alphabet = list(x=NA, xpath=''), 
			## Substitution Cost Matrix
			analysis_subcostmat = list(x=NA, xpath=''),
			## Sequence Data Series
			analysis_data = list(x=NA, xpath=''),
			##------------------------------
			## BOOL flag indicating if model was saved/updated during session
			updated=FALSE,
			## Sequence distance settings
			analysis_dist_settings = list(
				method='OM',
				norm='none',
				indel=1.0
			)
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
	saveModelDistSettings <- function(model, xname, x)
	{
		model$analysis_dist_settings[[xname]] <- x
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
		if(class(x) %in% c('list','data.frame')) return(TRUE)
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
	# Create list of sequences by `seqname` attribute (e.g., firm)
	# for sequence listed in `varname` column (e.g., action)
	##
	longDf2SeqList <- function(df, seqnames, seqname='firm', varname='action')
	{
	  l <- list()
	  for (i in 1:length(seqnames)) {
	    seq_i <- as.character(seqnames[i])
	    rows <- which(df[,seqname]==seq_i)
	    l[[seq_i]] <- as.character(df[rows, varname])
	  }
	  return(l)
	}

	##
	# Transform named list of sequences to wide dataframe of sequence rows
	#   paddings NAs at end to match seq lengths 
	##
	seqList2Df <- function(l)
	{
	  maxlen <- max(sapply(l,length))
	  for (i in 1:length(l)) {
	    xi <- c(l[[i]], rep(NA, maxlen - length(l[[i]])) )
	    if (i == 1) {
	      ldf <- data.frame(xi)
	      names(ldf) <- names(l)[i]
	    } else {
	      ldf <- cbind(ldf, xi)
	      names(ldf)[i] <- names(l)[i]
	    }
	  }
	  return(t(ldf))
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
			df <- read.csv(inFile$datapath, header = T, na.strings=c('','""'), stringsAsFactors=TRUE)
			
			# if (input$rownames) {
				row.names <- df[,1]
				df <- df[,-1]
				row.names(df) <- row.names
			# } else {
			# 	row.names <- names(df)
			# }

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
			df <- read.csv(inFile$datapath, header = T, sep=',', fill=TRUE, stringsAsFactors=TRUE)

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

		## METHOD 
		output$analysis_distance_function_value <- renderText({
			if(exists(input$analysis_distance_function)) {
				saveModelDistSettings(loadModel(), xname='method', x=input$analysis_distance_function)
			}
			return()
		})
		## NORM
		output$analysis_distance_norm_value <- renderText({
			if(exists(input$analysis_distance_norm)) {
				saveModelDistSettings(loadModel(), xname='norm', x=input$analysis_distance_norm)
			}
			return()
		})
		## INDEL
		output$analysis_indel_cost_value <- renderText({
			if(exists(input$analysis_indel_cost)) {
				saveModelDistSettings(loadModel(), xname='indel', x=input$analysis_indel_cost)
			}
			return()
		})

		##
		## seqdef - define missing values on right (NA or DEL)
		##   --> compute dist from OM
		## simplicity - HHI
		## unpredictability - levdist from own firm previous period
		## grouping - mean { gamma analysis separation score } across all actions
		## motif - variance of the averages in the pair-wise gamma analysis precedence scores across all action types
		##
		## MAIN ANALYSIS FUNCTION CALLED FROM MEASURES TAB
		output$analysis_run_value <- renderPrint({
			if (input$analysis_run) {
				library(TraMineR)
				library(reshape2)

				model <- loadModel()
				# model$analysis_run <- 'CHECKPOINT BEFORE RUN'
				# saveRDS(model, file=MODEL_FILE)

				dat <- model$analysis_data$x
				alphabet <- model$analysis_alphabet$x
				method <- model$analysis_dist_settings$method
				indel <- as.numeric(model$analysis_dist_settings$indel)
				norm <- model$analysis_dist_settings$norm
				sm <- model$analysis_subcostmat$x
				storage.mode(sm) <- "numeric"

				actionCol <- 'action'
				periodCol <- 'period'
				firmCol <- 'firm'

				right <- 'DEL'  # left <- 'NA' # gaps <- 'NA'

				periods <- unique(dat[,periodCol])
				firms <- as.character(alphabet[[firmCol]])
				actionAlphabet <- as.character(alphabet[[actionCol]])

				dists <- list() # period list
				seqdefs <- list() # period list
				for (t in 1:length(periods))  #length(periods)
				{
					pd <- periods[t]
					tidx <- which(dat[,periodCol] == pd)
					t.dat <- dat[tidx, ]
					t.l <- longDf2SeqList(t.dat, firms, 'firm', 'action')
					t.ldf <- seqList2Df(t.l)
					t.xseqdef <- seqdef(t.ldf, alphabet=actionAlphabet, right=right)
					t.xdist <- seqdist(t.xseqdef, 
						method = method, indel = indel, norm = norm, sm = sm)
					dimnames(t.xdist) <-  list(firms, firms)
					seqdefs[[pd]] <- t.xseqdef
					dists[[pd]] <- t.xdist
				}

				model$dists <- dists
				model$seqdefs <- seqdefs
				model$analysis_run <- 'ANALYSIS RUN COMPLETED'
				saveRDS(model, file=MODEL_FILE)

				return(print(list(
					Sequences=model$seqdefs,
					Distance_Method=method,
					Distances=model$dists
				)))
			}
			return()
		})

		output$analysis_output_plots <- renderPlot({
			if(input$analysis_output_plots_button) {
				model <- readRDS(MODEL_FILE)
				measuresAll <- c('seqdefs', 'dists')
				modelNames <- names(model)
				measures <- measuresAll[measuresAll %in% modelNames]
				if (length(measures) > 0) {
					nall <- sum(sapply(measures, function(x) length(model[[x]]) ))
					par(mfrow=c(ceiling(nall/3),3), mar=c(2,3,2,1))
					# #DEBUG
					# plot(model[[measures[1]]][[1]], main=sprintf('%s: period %s',measures[1],1))
					# for (measure in measures) {
						measure <- measures[1]
						for (i in 1:length(model[[measure]])) {
							plot(model[[measure]][[i]], main=sprintf('%s: period %s',measure,i))
						}
					# }
				} else {
					return('Notion to plot.')
				}
			} else {
				return()
			}
		})
	 
		# output$analysis_output_dists_plot <- renderPlot({
		#  	if(input$analysis_output_dists_plot_button) {
		#  		model <- readRDS(MODEL_FILE)
		#  		if ('dists' %in% names(model)) {
		#  			nplotcols <- ifelse(length(model$dists)>1, 2, 1)
		#  			par(mfrow=c(length(model$dists),nplotcols))
		#  			for (i in 1:length(model$dists)) {
		#  				# heatmap(model$dists[[i]])
		#  				image(model$dists[[i]], main=sprintf('Distances: Period %s',i))
		#  				# image(zlim=range(c(model$dists[[i]])), legend.only=T, horizontal=F)
		#  			}
		#  		}
		#  	} else {
		#  		return()
		#  	}
		# })

		output$analysis_output_download <- downloadHandler(
			filename = function() {
			    sprintf('sequence_analysis_results-%s.zip', 
			    	as.integer(Sys.time())
			    )
		    # file.choose()
			},
			content = function(con) {
				library(reshape2)
			  	model <- loadModel()
			    # saveRDS(model, con)
			    files <- NULL;
			    ts <- as.integer(Sys.time())

			    dat <- model$dists
			    df <- data.frame()
			    for (t in 1:length(dat)) {
			    	dft <- melt(dat[[t]], varnames = c('firm1','firm2'), value.name = 'distances')
			    	dft$period <- names(dat)[t]
			    	df <- rbind(df, dft)
			    }
			    file <- sprintf('dists-%s.csv',ts)
			    write.csv(df, file=file, row.names = F)
			    files <- c(files, file)

			    # dat <- model$seqdefs
			    # df <- data.frame()
			    # for (t in 1:length(dat)) {
			    # 	dft <- melt(dat[[t]], varnames = c('firm1','firm2'), value.name = 'se')
			    # 	dft$period <- names(dat)[t]
			    # 	df <- rbind(df, dft)
			    # }
			    # file <- sprintf('dists-%s.csv',ts)
			    # write.csv(df, file=file)
			    # files <- c(files, file)

		    	zip(con,files)

			}
		)

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
