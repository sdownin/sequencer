
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
	# Concordant and Discordant Pairs
	# @see DescTools https://github.com/AndriSignorell/DescTools
	##
	ConDisPairs <- function(x) 
	{
	    n <- nrow(x)
	    m <- ncol(x)
	    pi.c <- pi.d <- matrix(0, nrow = n, ncol = m)
	    row.x <- row(x)
	    col.x <- col(x)
	    for (i in 1:n) {
	        for (j in 1:m) {
	            pi.c[i, j] <- sum(x[row.x < i & col.x < j]) + sum(x[row.x > i & col.x > j])
	            pi.d[i, j] <- sum(x[row.x < i & col.x > j]) + sum(x[row.x > i & col.x < j])
	        }
	    }
	    C <- sum(pi.c * x)/2
	    D <- sum(pi.d * x)/2
	    return(list(pi.c = pi.c, pi.d = pi.d, C = C, D = D))
	}


	##
	# Goodman Kruskall Gamma precedence measure 
	# @see DescTools https://github.com/AndriSignorell/DescTools
	##
	GoodmanKruskalGamma <- function (x, y = NULL, conf.level = NA, ...) 
	{
	    if (!is.null(y)) 
	        tab <- table(x, y, ...)
	    else tab <- as.table(x)
	    x <- ConDisPairs(tab)
	    psi <- 2 * (x$D * x$pi.c - x$C * x$pi.d)/(x$C + x$D)^2
	    sigma2 <- sum(tab * psi^2) - sum(tab * psi)^2
	    gamma <- (x$C - x$D)/(x$C + x$D)
	    if (all(is.na(conf.level))) {
	        result <- gamma
	    } else {
	        pr2 <- 1 - (1 - conf.level)/2
	        ci <- qnorm(pr2) * sqrt(sigma2) * c(-1, 1) + gamma
	        result <- c(gamma = gamma, lwr.ci = max(ci[1], -1), upr.ci = min(ci[2], 1))
	    }
	    return(result)
	}

	##
	# Herfindahl-Hirschman Index
	#   0 < hhi <= 1
	##
	hhi <- function(x){
		## market share in decimals (not percentages)
		ms <-  x / sum(x) 
		## HHI is sum of sqared market shares
		hhi <- sum(ms^2)
		return(hhi)
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

			library(TraMineR)
			library(reshape2)

			model <- loadModel()
			
			## INPUT
			df <- read.csv(inFile$datapath, header = T, sep=',', fill=TRUE, stringsAsFactors=TRUE)
			model$analysis_data <- list(x=df, xpath=inFile$datapath)
			saveRDS(model, file=MODEL_FILE)
			# saveModel(model, xname='analysis_data', x=df, xpath=inFile$datapath)

			alphabet <- model$analysis_alphabet$x
			periodCol <- 'period'
			firmCol <- 'firm'
			actionCol <- 'action'
			periods <- unique(df[,periodCol])
			firms <- as.character(alphabet[[firmCol]])
			actionAlphabet <- as.character(alphabet[[actionCol]])

			seqdefs <- list()
			for (t in 1:length(periods))  #length(periods)
			{
				pd <- periods[t]
				tidx <- which(df[,periodCol] == pd)
				t.df <- df[tidx, ]
				t.l <- longDf2SeqList(t.df, firms, 'firm', 'action')
				t.ldf <- seqList2Df(t.l)
				right <- 'DEL'  # left <- 'NA' # gaps <- 'NA'
				seqdefs[[pd]] <- seqdef(t.ldf, alphabet=actionAlphabet, right=right)
			}
			model$seqdefs <- seqdefs
			saveRDS(model, file=MODEL_FILE)

			print(model$seqdefs)
		})


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
				library(tidyverse)

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

				periods <- unique(dat[,periodCol])
				firms <- as.character(alphabet[[firmCol]])
				actionAlphabet <- as.character(alphabet[[actionCol]])

				seqdefs <- model$seqdefs
				distances <- list() # squence distance mesaures period list
				groupings <- list() # gamma list ('grouping' measure avg. precedence scores)
				motifs <- list() # gamma list ('grouping' measure avg. precedence scores)
				predictabilities <- list()
				simplicities <- list()

				for (t in 1:length(periods))  #length(periods)
				{
					pd <- periods[t]
					if ('distances' %in% input$analysis_measures_group) {
						t.xdist <- seqdist(seqdefs[[t]], 
							method = method, indel = indel, norm = norm, sm = sm)
						dimnames(t.xdist) <- list(firms, firms)
						distances[[pd]] <- t.xdist
					}
					if ('grouping' %in% input$analysis_measures_group) {
						## separation score from gamma analysis
						## To calculate the extent to which the entire sequence exhibits grouping, 
						## we calculated the mean of the separation scores across all action types. 
						## High scores indicate that the sequence contains elements 
						## that were not ordinally proximate to one another; 
						## low scores indicate that the sequence exhibits groups of actions. 
						## We reversed the direction of the scores (multiplying them by −1.0) 
						## so that higher scores indicate higher levels of grouping among actions in a sequence.
					}
					if ('motif' %in% input$analysis_measures_group) {
						## precedence score from gamma analysis
						## To determine the extent to which the entire sequence exhibits internal structuredness, 
						## we calculated the variance of the averages in the pair-wise precedence scores across all action types. 
						## Averaging across action types indicates the relative position—beginning or 
						## end—in the overall sequence in which the particular type of action appears. 
						## Variance in average precedence scores captures the ordinal specificity and 
						## stability of elements in a sequence.
					}
					if ('simplicities' %in% input$analysis_measures_group) {
						## simplicity HHI score

						t.dat <- dat[dat[,periodCol]==pd, ]
						t.simp <- data.frame()
						for (i in 1:length(firms)) {
							t.i.cnt <- plyr::count( t.dat[ t.dat[,firmCol]==firms[i] , actionCol] )
							t.i.cntdf <- data.frame(firm=firms[i], hhi=hhi(t.i.cnt$freq))
							t.simp <- rbind(t.simp,  t.i.cntdf)
						}
						simplicities[[pd]] <- t.simp

					}
					if ('predictabilities' %in% input$analysis_measures_group) {
						## based on OM of firm to previous period
					}
				}

				model$distances <- distances
				model$groupings <- groupings
				model$motifs <- motifs
				model$predictabilities <- predictabilities
				model$simplicities <- simplicities
				model$analysis_run <- 'ANALYSIS RUN COMPLETED'
				saveRDS(model, file=MODEL_FILE)

				return(print(list(
					Distance_Method=method,
					Distances=model$distances,
					Groupings=model$groupings,
					Motifs=model$motifs,
					Predictability=model$predictabilities,
					Simplicity=model$simplicities
				)))
			}
			return()
		})

		output$analysis_output_plots <- renderPlot({
			if(input$analysis_output_plots_button) {
				library(tidyverse)
				library(ggpubr)
				library(reshape2)
				model <- readRDS(MODEL_FILE)
				measuresAll <- c('seqdefs', 'distances')
				modelNames <- names(model)
				measures <- measuresAll[measuresAll %in% modelNames]
				if (length(measures) > 0) {
					nall <- sum(sapply(measures, function(x) length(model[[x]]) ))
					par(mfrow=c(ceiling(nall/3),3), mar=c(2,3,2,1))
					# #DEBUG
					# plot(model[[measures[1]]][[1]], main=sprintf('%s: period %s',measures[1],1))
					# for (measure in measures) {
						measure <- 'distances'
						plots <- list()
						for (i in 1:length(model[[measure]])) {
							dflong <- melt(model[[measure]][[i]], varnames = c('firm1','firm2'), value.name = 'distances')
							plt <- ggplot(data = dflong, aes(x = firm1, y = firm2)) +
								geom_tile(aes(fill = distances))  + 
								geom_text(aes(label = round(distances, 1)), colour='#FFFFFF') +
								scale_fill_continuous(high = "#132B43", low = "#56B1F7") + 
								ggtitle(sprintf('%s: period %s',measure,i))
							plots[[length(plots)+1]] <- plt
						}

					# }
						ncols <- 3
						nrows <- ceiling(length(plots) / ncols)
						ggarrange(plotlist = plots, ncol=ncols, nrow = nrows #,
					        	# labels = c("A", "B", "C")
					        )
				} else {
					return('Notion to plot.')
				}
			} else {
				return()
			}
		})
	 
		# output$analysis_output_distances_plot <- renderPlot({
		#  	if(input$analysis_output_distances_plot_button) {
		#  		model <- readRDS(MODEL_FILE)
		#  		if ('distances' %in% names(model)) {
		#  			nplotcols <- ifelse(length(model$distances)>1, 2, 1)
		#  			par(mfrow=c(length(model$distances),nplotcols))
		#  			for (i in 1:length(model$distances)) {
		#  				# heatmap(model$distances[[i]])
		#  				image(model$distances[[i]], main=sprintf('Distances: Period %s',i))
		#  				# image(zlim=range(c(model$distances[[i]])), legend.only=T, horizontal=F)
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
			  	measuresAll <- c('distances','groupings','motifs','simplicities','unpredictability')
			  	measures <- measuresAll[measuresAll %in% names(model)]
			    files <- NULL;
			    # temp dir
			    owd <- setwd(tempdir())
      			on.exit(setwd(owd))
      			# timestamp
			    ts <- as.integer(Sys.time())

			    ## DISTANCES
			    if ('distances' %in% measures) {
				    dat <- model$distances
				    df <- data.frame()
				    for (t in 1:length(dat)) {
				    	dft <- melt(dat[[t]], varnames = c('firm1','firm2'), value.name = 'distances')
				    	dft$period <- ifelse(length(names(dat))>0, names(dat)[t], t)
				    	df <- rbind(df, dft)
				    }
				    file <- sprintf('distances-%s.csv',ts)
				    write.csv(df, file=file, row.names = F)
				    files <- c(files, file)
			    }

			    ## Simplicities
			    if ('simplicities' %in% measures) {
				    df <- data.frame()
				    for (t in 1:length(dat)) {
				    	dft <- model$simplicities[[t]]
				    	dft$period <- ifelse(length(names(dat))>0, names(dat)[t], t)
				    	df <- rbind(df, dft)
				    }
				    file <- sprintf('simplicities-%s.csv',ts)
				    write.csv(df, file=file, row.names = F)
				    files <- c(files, file)
			    }

			    # dat <- model$seqdefs
			    # df <- data.frame()
			    # for (t in 1:length(dat)) {
			    # 	dft <- melt(dat[[t]], varnames = c('firm1','firm2'), value.name = 'se')
			    # 	dft$period <- names(dat)[t]
			    # 	df <- rbind(df, dft)
			    # }
			    # file <- sprintf('distances-%s.csv',ts)
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
