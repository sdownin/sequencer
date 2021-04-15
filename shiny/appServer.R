
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
			analysis_alphabet = list(x=NA, xpath='', varnamemap=NA, actionCol=NA, firmCol=NA, periodCol=NA), 
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
				indel=1.0   ## deprecated -- using indel substitution cost matrix instead
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
		if(class(x) %in% c('list','data.frame') & length(x)==0) return(FALSE)
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
	# Get list of action name abbreviations 
	#  (for compact display of subcostmat)
	##
	getNameAbbrevList <- function(actions) {
	  num.categories <- length(actions)
	  out <- as.list(actions)
	  abbs <- LETTERS
	  i <- 1
	  while( length(abbs) < num.categories) {
	    i <- i+1
	    abbs <- c(abbs, sapply(LETTERS, function(x)paste(c(x,i),collapse = '')))
	  }
	  names(out) <- abbs[1:length(out)]
	  
	  return(out)
	}

	##
	#
	##
	printNamedList <- function(li){
	  names <- names(li)
	  if (length(names)==0 | is.null(names) | all(is.na(names))) {
	    names(li) <- as.character(1:length(li))
	  }
	  items <- sapply(1:length(li), function(i)paste(c(names(li)[i],li[[i]]),collapse = ' = '))
	  return(paste(items, collapse = '; '))
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
	# Compute table input for pairwaiseGammaMatrix()
	##
	getTable4GKgamma <- function(seqx, a, b) {
	  z <- matrix(c(  
	    seqx == a, 
	    seqx == b 
	  ), nrow=2, byrow = T)
	  return( z * 1 ) #convert bool to integer
	}

	##
	# GET Pairwise Gamma
	#   for `seqx` sequence of actions (a character vector)
	##
	pairwiseGammaMatrix <- function(seqx, alphabet, na.fill=NA) {
	  ## GET 1 firms sequence pairwise gamma table
	  mat <- matrix(NA,nrow=length(alphabet), ncol=length(alphabet),
	                dim=list(alphabet, alphabet))
	  for (i in 1:length(alphabet)) {
	    a <- alphabet[i]
	    for (j in 1:length(alphabet)) {
	      if (i == j) 
	        next
	      b <- alphabet[j]
	      gammainput<- getTable4GKgamma(seqx, a, b)
	      ## CHECK if any row is empty (one action type not in seq)
	      mat[i,j] <- if (all(rowSums(gammainput) > 0 )) {
	        GoodmanKruskalGamma(gammainput)
	      } else {
	        NA
	      }
	    }
	  }
	  if (all(!is.na(na.fill))) {
	    mat[is.na(mat)] <- na.fill
	  }
	  return(mat)
	}



	##
	# Main Server Function
	##
	server <- function(input, output, session) {

		model <- loadModel()
		cat('\nSERVER:  MODEL STRUCT\n')
		print(model)

		# alphabet table input #TODO: find why cannot call from external module?
		output$analysis_file_alphabet_summary <- renderPrint({
			inFile <- input$analysis_file_alphabet
			
			## NONE
			if (is.null(inFile)) 
				return(NULL)
			
			## INPUT
			df <- read.csv(inFile$datapath, header = input$header, na.strings=c('','""'), stringsAsFactors=FALSE, 
				fileEncoding=input$alphabet_fileEncoding, #'UTF-8',
				# encoding= 'UTF-8', fileEncoding="UTF-8-BOM", #'UTF-8',
				check.names=TRUE, strip.white=TRUE)
			li <- apply(df, 2, function(col){
				# as.factor(unique(col[which(!is.null(col) & !is.nan(col) & !is.na(col))]))
				unique(col[which(!is.null(col) & !is.nan(col) & !is.na(col))])
			})
			
			## UPDATE SELECTION OF ACTION COLUMN
			updateSelectInput(session, "alphabet_selectActionColumn", label = "Category Column", choices = colnames(df), 
				selected = ifelse(exists(input$alphabet_selectActionColumn) & input$alphabet_selectActionColumn %in% colnames(df), input$alphabet_selectActionColumn, colnames(df)[1])
				# selected = input$alphabet_selectActionColumn
				)

			## add var name abbreviations
			varnamemap <- if (input$alphabet_selectActionColumn %in% names(li)) {
					getNameAbbrevList(li[[input$alphabet_selectActionColumn]])
				} else {
					x <- li[[1]]
					names(x) <- li[[1]]
					x
				}

			## OUTPUT: list of alphabet items (action, firm, etc.) 
			str(li)
			if (length(varnamemap) > 0) {
				model <- loadModel()
				model[['analysis_alphabet']]$actionCol <- input$alphabet_selectActionColumn
				model[['analysis_alphabet']]$varnamemap <- varnamemap
				saveRDS(model, file=MODEL_FILE)
				cat(sprintf('\nAbbreviations for Column: `%s`\n %s', 
					input$alphabet_selectActionColumn, printNamedList(varnamemap)))
			}

			## REPLACE ACTION NAMES WITH ABBREVIATIONS
			if (input$alphabet_selectActionColumn %in% names(li)) {
				for (i in 1:length(varnamemap)) {
					idx <- which(li[[input$alphabet_selectActionColumn]] == varnamemap[[i]])
					li[[input$alphabet_selectActionColumn]][idx] <- names(varnamemap)[i]
				}
			}
			## BREAK SCOPE TO SAVE (both alphabet and var name mapping for alphabet)
			saveModel(loadModel(), xname='analysis_alphabet', x=li, xpath=inFile$datapath)

		})



		# alphabet table input #TODO: find why cannot call from external module?
		output$analysis_file_subcostmat_summary <- renderPrint({

			inFile <- input$analysis_file_subcostmat

			## NONE
			if (is.null(inFile)) 
				return(NULL)
			
			model <- loadModel()

			## INPUT
			df <- read.csv(inFile$datapath, header = input$subcostmat_header, na.strings=c('','""'), stringsAsFactors=FALSE,
				# row.names=input$subcostmat_rownames,
				fileEncoding=input$subcostmat_fileEncoding, #'UTF-8',
				check.names=TRUE, strip.white=TRUE)

			alphabet <- model$analysis_alphabet$x
			actionCol <- model$analysis_alphabet$actionCol
			varnamemap <- model$analysis_alphabet$varnamemap
			ncats <- length(varnamemap)
			# print(alphabet)
			# print(actionCol)
			# print(varnamemap)

			## automatically determine column names
			nrows <- nrow(df)
			ncols <- ncol(df)
			hasColNames <- nrows > ncats ## if 1 more row than #cats --> has col name
			hasRowNames <- ncols > ncats ## if 1 more col than #cats --> has row name
			## remove name row or col if present, and then assign rownames(), colnames()
			df <- if (hasColNames & hasRowNames) { # is Square
				# .rownames <-  df[,1]; # .colnames <-  df[1,]
					df <- df[-1,-1]
				} else if (nrows > ncols) {
					## varnames in 1st row  (as column headers)
					df <- df[-1, ]
				} else if (ncols > nrows) {
					## varnames in 1st col (as row names)
					df <- df[ ,-1]
				} else {
					df ## NO CHANGE (no col names and no row names)
				}
			colnames(df) <- names(varnamemap)
			rownames(df) <- names(varnamemap)

			mat <- as.matrix(df)

			# if (nrow(mat) != ncol(mat)) {
			# 	print('Rows and Columns are different dimensions. Are you sure you selected a substitution cost matrix?')
			# 	return()
			# }

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
		#	read.csv(inFile$datapath, header = input$header, sep=',', fill=TRUE, stringsAsFactors=FALSE)
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
			df <- read.csv(inFile$datapath, header = TRUE, sep=',', fill=TRUE, stringsAsFactors=FALSE,
				fileEncoding=input$seqdata_fileEncoding, #'UTF-8',
				check.names=TRUE, strip.white=TRUE)

			## Input
			model$analysis_data <- list(x=df, xpath=inFile$datapath)
			saveRDS(model, file=MODEL_FILE)
			# saveModel(model, xname='analysis_data', x=df, xpath=inFile$datapath)

			## Main model attributes
			alphabet <- model$analysis_alphabet$x
			varnamemap <- model$analysis_alphabet$varnamemap
			ncats <- length(varnamemap)
			## Main variable names
			actionCol <- model$analysis_alphabet$actionCol
			firmCol <- names(alphabet)[ ! names(alphabet) %in%  actionCol ][1] ## 'firm'
			.remainingCols <- names(df)[! names(df) %in% c(actionCol,firmCol) ]
			periodCol <- .remainingCols[length(.remainingCols)] ##'period'

			## UPDATE MODEL
			model[['analysis_alphabet']]$firmCol <- firmCol
			model[['analysis_alphabet']]$periodCol <- periodCol
			saveRDS(model, file=MODEL_FILE)

			# print(actionCol)
			# print(firmCol)
			# print(periodCol)
			# print(varnamemap)
			# print(df)
			# return();
			# if ( ! actionCol %in% names(df)) {
			# 	print('actionCol not in names(df)');
			# 	return();
			# }

			## REPLACE ACTION NAMES WITH ABBREVIATIONS
			for (i in 1:length(varnamemap)) {
				idx <- which(df[,actionCol] == varnamemap[[i]])
				df[idx,actionCol] <- names(varnamemap)[i]
			}

			# print(df)
			# return();

			## Data based on df, alphabet, and main variable names
			periods <- unique(df[,periodCol])
			firms <- as.character(alphabet[[firmCol]])
			actionAlphabet <- as.character(alphabet[[actionCol]])

			seqdefs <- list()
			for (t in 1:length(periods))  #length(periods)
			{
				pd <- periods[t]
				tidx <- which(df[,periodCol] == pd)
				t.df <- df[tidx, ]
				t.l <- longDf2SeqList(t.df, firms, firmCol, actionCol)
				t.ldf <- seqList2Df(t.l)
				right <- 'DEL'  # left <- 'NA' # gaps <- 'NA'
				seqdefs[[pd]] <- seqdef(t.ldf, alphabet=actionAlphabet, right=right)
			}
			names(seqdefs) <- periods
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

		# output$analysis_num_plots <- renderText({
		# 	model <- loadModel()
		# 	if (length(model) > 0)
		# 			return(length(model$analysis_alphabet$x$firms) + length(model$) )
		# })

		##
		## seqdef - define missing values on right (NA or DEL)
		##   --> compute dist from OM  (or selected dist metric)
		## simplicity - HHI
		## unpredictability - levdist from own firm previous period(s)
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

				actionCol <- model$analysis_alphabet$actionCol
				firmCol <- model$analysis_alphabet$firmCol
				periodCol <- model$analysis_alphabet$periodCol
				right <- 'DEL'  # left <- 'NA' # gaps <- 'NA' ## seqdef parameter

				periods <- unique(dat[,periodCol])
				firms <- as.character(alphabet[[firmCol]])
				actionAlphabet <- as.character(alphabet[[actionCol]])

				npds <- length(periods)
				nfirms <- length(firms)

				seqdefs <- model$seqdefs
				gamma <- list()    # list of pairwise gamma matrices per period and per firm
				distance <- list() # squence distance mesaures period list
				grouping <- list() # gamma list ('grouping' measure avg. precedence scores)
				motif <- list() # gamma list ('grouping' measure avg. precedence scores)
				predictability <- list()
				simplicity <- list()

				measures <- input$analysis_measures_group

				## clear existing measures from model (in case user reruns analysis with different subset of measures selected)
				for (measure in c('distance','simplicity','grouping','predictability','motif', 'gamma')) {
					if (measure %in% names(model))
						model[[measure]] <- NULL
				}

				##-----------------------------------
				## COMPUTE SELECTED MEASURES BY PERIOD
				##-----------------------------------
				## loop over periods (per firm)
				for (t in 1:length(periods))  #length(periods)
				{
					pd <- periods[t]

					## FIRST compute pairwise G-K gamma
					gamma[[pd]] <- list()
					for (i in 1:length(firms)) {
						firm <- firms[i]
						gamma[[pd]][[firm]] <- pairwiseGammaMatrix(seqdefs[[t]][i,], actionAlphabet, na.fill=0)
					}
					# names(gamma) <- firms

					## loop over firms  (per period)
					if ('distance' %in% measures) 
					{
						t.xdist <- seqdist(seqdefs[[t]], method=method, norm=norm, sm=sm, with.missing=TRUE)
						dimnames(t.xdist) <- list(firms, firms)
						distance[[pd]] <- t.xdist
					}

					if ('grouping' %in% measures) 
					{
						t.dat <- dat[dat[,periodCol]==pd, ]
						t.group <- data.frame()
						for (i in 1:length(firms)) {
							firm <- firms[i]
							## separation score for a given type of element is calculated as 
							## the mean of the absolute value of its pair-wise gamma values 
							## (Holmes, 1995; Holmes and Sykes, 1993). 
							separation <- rowMeans(abs(gamma[[pd]][[firm]]), na.rm = T)
							## Grouping
							## To calculate the extent to which the entire sequence exhibits grouping, 
							## we calculated the mean of the separation scores across all action types.
							## We reversed the direction of the scores (multiplying them by −1.0) 
							## so that higher scores indicate higher levels of grouping 
							## among actions in a sequence.
							groupingMeasure <- mean(separation, na.rm = T) * -1 
							t.i.group <- data.frame(firm=firm, grouping = groupingMeasure)
							t.group <- rbind(t.group, t.i.group)
						}
						grouping[[pd]] <- t.group
					}

					if ('motif' %in% measures) 
					{
						t.dat <- dat[dat[,periodCol]==pd, ]
						t.motif <- data.frame()
						for (i in 1:length(firms)) {
							firm <- firms[i]
							## the precedence score for a given element type is calculated as
							## the mean of all of its pair-wise gamma values 
							## (Holmes, 1995; Holmes and Sykes, 1993).
							precedence <- rowMeans(gamma[[pd]][[firm]], na.rm = T)
							## Motif
							## determine the extent to which the entire sequence 
							## exhibits internal structuredness, we calculated 
							## **the variance of the averages in the pair-wise precedence scores 
							## across all action types. Averaging across action types indicates 
							## the relative position—beginning or end—in the overall sequence 
							## in which the particular type of action appears. 
							## Variance in average precedence scores captures the ordinal specificity 
							## and stability of elements in a sequence.
							motifMeasure <- var(precedence, na.rm = TRUE)
							t.i.motif <- data.frame(firm=firm, motif = motifMeasure)
							t.motif <- rbind(t.motif, t.i.motif)
						}
						motif[[pd]] <- t.motif
					}

					if ('simplicity' %in% measures) 
					{
						## simplicity HHI score
						t.dat <- dat[dat[,periodCol]==pd, ]
						t.simp <- data.frame()
						for (i in 1:length(firms)) {
							t.i.cnt <- plyr::count( t.dat[ t.dat[,firmCol]==firms[i] , actionCol] )
							simplicityMeasure <- hhi(t.i.cnt$freq)
							t.i.cntdf <- data.frame(firm=firms[i], simplicity = simplicityMeasure)
							t.simp <- rbind(t.simp,  t.i.cntdf)
						}
						simplicity[[pd]] <- t.simp

					}
				} ##/end period loop

				## loop over firms  (per period)
				if ('predictability' %in% measures) 
				{
					seqlaglist <- list()
					for (i in 1:length(firms)) {
					  firm <- firms[i]
					  for (t in 1:length(periods)) {
					    seqlaglist[[firm]] <- lapply(seqdefs, function(x) {
					      xseq <- as.character(unlist( x[i,]))
					      return( xseq[which(xseq %in% actionAlphabet)] ) #remove filled-in "%" for empty levels
					    })
					  }
					  tlagseq <- seqList2Df(seqlaglist[[firm]])
					  seqdeflag <- seqdef(tlagseq, alphabet=actionAlphabet, right=right)
					  predictability[[firm]] <- seqdist(seqdeflag,  method=method, norm=norm, sm=sm, with.missing = TRUE) * -1
					  dimnames(predictability[[firm]]) <- list(periods, periods)
					}
				}

				## list names by time period
				if (length(gamma) == npds)  		names(gamma) <- periods
				if (length(distance) == npds) 		names(distance) <- periods
				if (length(grouping) == npds) 		names(grouping) <- periods
				if (length(motif) == npds) 			names(motif) <- periods
				if (length(simplicity) == npds) 	names(simplicity) <- periods
				## list names by firm
				if (length(predictability) == nfirms) names(predictability) <- firms

				## add selected measures to model object
				if('distance' %in% measures)  		model$distance <- distance
				if('grouping' %in% measures) 		model$grouping <- grouping
				if('motif' %in% measures) 			model$motif <- motif
				if('predictability' %in% measures)  model$predictability <- predictability
				if('simplicity' %in% measures)  	model$simplicity <- simplicity
				## add gamma if used by other measure
				if (any(c('grouping','motif') %in% measures))	model$gamma <- gamma
				
				## Save Model
				model$analysis_run <- 'ANALYSIS RUN COMPLETED'
				saveRDS(model, file=MODEL_FILE)

				# ## SAVE NUM PLOTS
				# numplots <- length(model$seqdefs) * length(model$analysis_alphabet$x$firm)
				# saveRDS(list(num_plots=numplots), file=NUM_PLOTS_FILE)

				## Print selected measures
				printModel <- list()
				for (measure in measures) {
					printModel[[measure]] <- model[[measure]]
				}
				return(print(printModel))

			}
			return()
		})


		# output$analysis_num_plots <- renderPrint({
		# 	if (input$analysis_measures_goto) {
		# 		numplots <- readRDS(NUM_PLOTS_FILE)
		# 		return(print(numplots))
		# 	}
		# 	return()
		# })

		# output$analysis_output_plot_distance <- renderPlot({

		# 	if(input$analysis_output_plots_button) {

		# 		require(tidyverse)
		# 		require(ggpubr)
		# 		require(reshape2)
		# 		model <- readRDS(MODEL_FILE)
		# 		if (! 'plots' %in% names(model))
		# 			model$plots <- list()
		# 		measuresAll <- c('distance')
		# 		modelNames <- names(model)
		# 		measures <- measuresAll[measuresAll %in% modelNames]

		# 		if ( length(measures) == 0 ) {
		# 			return('Nothing to plot.')
		# 		}

		# 		plots <- list()

		# 		if ('distance' %in% measures) 
		# 		{
		# 			# #DEBUG
		# 			# plot(model[[measures[1]]][[1]], main=sprintf('%s: period %s',measures[1],1))
		# 			# for (measure in measures) {
		# 			measure <- 'distance'
		# 			for (i in 1:length(model[[measure]])) {
		# 				dflong <- melt(model[[measure]][[i]], varnames = c('firm1','firm2'), value.name = 'distance')
		# 				plt <- ggplot(data = dflong, aes(x = firm1, y = firm2)) +
		# 					geom_tile(aes(fill = distance))  + 
		# 					geom_text(aes(label = round(distance, 1)), colour='#FFFFFF') +
		# 					scale_fill_continuous(high = "#132B43", low = "#56B1F7") + 
		# 					ggtitle(sprintf('%s: period %s',measure,i))
		# 				plots[[length(plots)+1]] <- plt
		# 			}
		# 		} 

		# 		model$plots <- c(model$plots, plots)
		# 		saveRDS(model, file=MODEL_FILE)

		# 		## PLOT
		# 		nall <- sum(sapply(measures, function(x) length(model[[x]]) ))
		# 		ncols <- 3
		# 		nrows <- ceiling(length(plots) / ncols)
		# 		par(mfrow=c(ceiling(nall / ncols), ncols), mar=c(2,3,2,1))
		# 		ggarrange(plotlist = plots, ncol=ncols, nrow = nrows)

		# 	} 

		# })
	 
	#  output$analysis_output_plot_predictability <- renderPlot({

	# 	if(input$analysis_output_plots_button) {

	# 		require(tidyverse)
	# 		require(ggpubr)
	# 		require(reshape2)
	# 		model <- readRDS(MODEL_FILE)
	# 		if (! 'plots' %in% names(model))
	#  			model$plots <- list()
	# 		measuresAll <- c('predictability')
	# 		modelNames <- names(model)
	# 		measures <- measuresAll[measuresAll %in% modelNames]

	# 		if ( length(measures) == 0 ) {
	# 			return('Nothing to plot.')
	# 		}

	# 		plots <- list()

	# 		if ('predictability' %in% measures) 
	# 		{
	# 			# #DEBUG
	# 			# plot(model[[measures[1]]][[1]], main=sprintf('%s: period %s',measures[1],1))
	# 			# for (measure in measures) {
	# 			measure <- 'predictability'
	# 			itemnames <- names(model[[measure]])
	# 			for (i in 1:length(model[[measure]])) {
	# 				dflong <- melt(model[[measure]][[i]], varnames = c('period1','period2'), value.name = 'predictability')
	# 				plt <- ggplot(data = dflong, aes(x = period1, y = period2)) +
	# 					geom_tile(aes(fill = predictability))  + 
	# 					geom_text(aes(label = round(predictability, 1)), colour='#FFFFFF') +
	# 					scale_fill_continuous(high = "#B10026", low = "#FFFFB2") + 
	# 					ggtitle(sprintf('%s: firm %s',measure, itemnames[i] ))
	# 				plots[[length(plots)+1]] <- plt
	# 			}
	# 		} 

	# 		model$plots <- c(model$plots, plots)
	# 		saveRDS(model, file=MODEL_FILE)

	# 		## PLOT
	# 		nall <- sum(sapply(measures, function(x) length(model[[x]]) ))
	# 		ncols <- 3
	# 		nrows <- ceiling(length(plots) / ncols)
	# 		par(mfrow=c(ceiling(nall / ncols), ncols), mar=c(2,3,2,1))
	# 		ggarrange(plotlist = plots, ncol=ncols, nrow = nrows)

	# 	} 

	# })

	output$analysis_output_plots <- renderPlot({

	 	if(input$analysis_output_plots_button) {
	 		require(tidyverse)
	 		require(ggpubr)
	 		require(reshape2)

	 		model <- readRDS(MODEL_FILE)

	 		measuresAll <- c('distance','predictability','simplicity','grouping','motif')
	 		modelNames <- names(model)
	 		measures <- measuresAll[measuresAll %in% modelNames]

	 		if ( length(measures) == 0 ) {
	 			return();
	 		}

	 		plots <- list()

	 		if ('distance' %in% measures) 
	 		{
	 			# #DEBUG
	 			# plot(model[[measures[1]]][[1]], main=sprintf('%s: period %s',measures[1],1))
	 			# for (measure in measures) {
	 			measure <- 'distance'
	 			for (i in 1:length(model[[measure]])) {
	 				dflong <- melt(model[[measure]][[i]], varnames = c('firm1','firm2'), value.name = 'distance')
	 				plt <- ggplot(data = dflong, aes(x = firm1, y = firm2)) +
	 					geom_tile(aes(fill = distance))  + 
	 					geom_text(aes(label = round(distance, 1)), colour='#FFFFFF') +
	 					scale_fill_continuous(high = "#132B43", low = "#56B1F7") + 
	 					ggtitle(sprintf('%s: period %s',measure,i))
	 				plots[[length(plots)+1]] <- plt
	 			}
	 		} 

	 		if ('predictability' %in% measures) 
	 		{
	 			# #DEBUG
	 			# plot(model[[measures[1]]][[1]], main=sprintf('%s: period %s',measures[1],1))
	 			# for (measure in measures) {
	 			measure <- 'predictability'
	 			itemnames <- names(model[[measure]])
	 			for (i in 1:length(model[[measure]])) {
	 				dflong <- melt(model[[measure]][[i]], varnames = c('period1','period2'), value.name = 'predictability')
	 				plt <- ggplot(data = dflong, aes(x = period1, y = period2)) +
	 					geom_tile(aes(fill = predictability))  + 
	 					geom_text(aes(label = round(predictability, 1)), colour='#FFFFFF') +
	 					scale_fill_continuous(high = "#B10026", low = "#FFFFB2") + 
	 					ggtitle(sprintf('%s: firm %s',measure, itemnames[i] ))
	 				plots[[length(plots)+1]] <- plt
	 			}
	 		} 

	 		if ('motif' %in% measures)
	 		{
	 			measure <- 'motif'
	 			dat <- model[[measure]]
	 		    dflong <- data.frame()
	 		    for (t in 1:length(dat)) {
	 		    	dft <- model[[measure]][[t]]
	 		    	dft$period <- ifelse(length(names(dat))>0, names(dat)[t], t)
	 		    	dflong <- rbind(dflong, dft)
	 		    }
	 		    plt <- ggplot(data = dflong, aes(x = as.integer(period), y = motif, colour=firm)) +
	 		    	geom_line()  + 	geom_point() +
	 		    	xlab('Period') + theme_bw() + 
	 		    	ggtitle(sprintf('%s',measure))
	 		    plots[[length(plots)+1]] <- plt
	 		}

	 		if ('grouping' %in% measures)
	 		{
	 			measure <- 'grouping'
	 			dat <- model[[measure]]
	 		    dflong <- data.frame()
	 		    for (t in 1:length(dat)) {
	 		    	dft <- model[[measure]][[t]]
	 		    	dft$period <- ifelse(length(names(dat))>0, names(dat)[t], t)
	 		    	dflong <- rbind(dflong, dft)
	 		    }
	 		    plt <- ggplot(data = dflong, aes(x = as.integer(period), y = grouping, colour=firm)) +
	 		    	geom_line()  + 	geom_point() +
	 		    	xlab('Period') + theme_bw() + 
	 		    	ggtitle(sprintf('%s',measure))
	 		    plots[[length(plots)+1]] <- plt
	 		}

	 		if ('simplicity' %in% measures)
	 		{
	 			measure <- 'simplicity'
	 			dat <- model[[measure]]
	 		    dflong <- data.frame()
	 		    for (t in 1:length(dat)) {
	 		    	dft <- model[[measure]][[t]]
	 		    	dft$period <- ifelse(length(names(dat))>0, names(dat)[t], t)
	 		    	dflong <- rbind(dflong, dft)
	 		    }
	 		    plt <- ggplot(data = dflong, aes(x = as.integer(period), y = simplicity, colour=firm)) +
	 		    	geom_line()  + 	geom_point() +
	 		    	xlab('Period') + theme_bw() + 
	 		    	ggtitle(sprintf('%s',measure))
	 		    plots[[length(plots)+1]] <- plt
	 		}



	 		## SAVE
	 		model$plots <- plots
	 		saveRDS(model, file=MODEL_FILE)

	 		## PLOT
	    	nall <- length(model$plots)
	    	ncols <- floor(sqrt(nall))
	    	nrows <- ceiling(nall / ncols)
	 		par(mar=c(2,3,2,1))
	 		ggarrange(plotlist = plots, ncol=ncols, nrow = nrows)

	 	} 

	})


		# output$analysis_output_distance_plot <- renderPlot({
		#  	if(input$analysis_output_distance_plot_button) {
		#  		model <- readRDS(MODEL_FILE)
		#  		if ('distance' %in% names(model)) {
		#  			nplotcols <- ifelse(length(model$distance)>1, 2, 1)
		#  			par(mfrow=c(length(model$distance),nplotcols))
		#  			for (i in 1:length(model$distance)) {
		#  				# heatmap(model$distance[[i]])
		#  				image(model$distance[[i]], main=sprintf('Distances: Period %s',i))
		#  				# image(zlim=range(c(model$distance[[i]])), legend.only=T, horizontal=F)
		#  			}
		#  		}
		#  	} else {
		#  		return()
		#  	}
		# })

      	# timestamp
		systime <- Sys.time()
	    ts <- as.integer(systime)


		output$analysis_output_download <- downloadHandler(
			filename = function() {
			    sprintf('sequence_analysis_results-%s.zip', ts)
		    # file.choose()
			},
			content = function(con) {
				library(reshape2)
			  	model <- loadModel()
			  	measuresAll <- c('distance','grouping','motif','simplicity','predictability')
			  	measures <- measuresAll[measuresAll %in% names(model)]
			  	# if (any(c('grouping','motif') %in% measures))
			  	# 	measures <- c(measures, 'gamma')
			    files <- NULL;
			    # temp dir
			    owd <- setwd(tempdir())
      			on.exit(setwd(owd))

			    ## write txt output summarizing the analyses called
				file <- sprintf('analysis_summary-%s.txt',ts)
				summarydf <- data.frame(summary=rbind(
					sprintf('Time: %s', as.character(systime)),
					sprintf('Measures: %s', paste(measures, collapse=', ')),
					sprintf('Notes: %s', ifelse(any(c('grouping','motif') %in% measures), 'gamma values used in motif and/or grouping are also saved', ' '))
				))
			    write.table(summarydf, file=file, row.names=FALSE, col.names=FALSE)  ## skip first row (name placehoder) when saving summary txt file
			    files <- c(files, file)

			    ## GAMMAS
			    if (any(c('grouping','motif') %in% measures)) {
			    	measure <- 'gamma'
				    dat <- model[[measure]]
				    df <- data.frame()
				    for (t in 1:length(dat)) {
				    	dft <- melt(dat[[t]], varnames = c('action1','action2'), value.name = measure)
				    	dft$period <- ifelse(length(names(dat))>0, names(dat)[t], t)
				    	df <- rbind(df, dft)
				    }
				    names(df)[which(names(df)=='L1')] <- 'firm'  ## list name at Level 1 is the firm name; rename column 'L1' to firmname
				    file <- sprintf('%s-%s.csv', measure,ts)
				    write.csv(df, file=file, row.names = F)
				    files <- c(files, file)
			    }

			    ## DISTANCES
			    if ('distance' %in% measures) {
			    	measure <- 'distance'
				    dat <- model[[measure]]
				    df <- data.frame()
				    for (t in 1:length(dat)) {
				    	dft <- melt(dat[[t]], varnames = c('firm1','firm2'), value.name = measure)
				    	dft$period <- ifelse(length(names(dat))>0, names(dat)[t], t)
				    	df <- rbind(df, dft)
				    }
				    file <- sprintf('%s-%s.csv',measure,ts)
				    write.csv(df, file=file, row.names = F)
				    files <- c(files, file)
			    }

		        ## GROUPING
		        if ('grouping' %in% measures) {
		        	measure <- 'grouping'
				    df <- data.frame()
				    for (t in 1:length(dat)) {
				    	dft <- model[[measure]][[t]]
				    	dft$period <- ifelse(length(names(dat))>0, names(dat)[t], t)
				    	df <- rbind(df, dft)
				    }
				    file <- sprintf('%s-%s.csv',measure,ts)
				    write.csv(df, file=file, row.names = F)
				    files <- c(files, file)
		        }

	            ## MOTIFS
	            if ('motif' %in% measures) {
	            	measure <- 'motif'
				    df <- data.frame()
				    for (t in 1:length(dat)) {
				    	dft <- model[[measure]][[t]]
				    	dft$period <- ifelse(length(names(dat))>0, names(dat)[t], t)
				    	df <- rbind(df, dft)
				    }
				    file <- sprintf('%s-%s.csv',measure,ts)
				    write.csv(df, file=file, row.names = F)
				    files <- c(files, file)
	            }

			    ## Simplicities
			    if ('simplicity' %in% measures) {
			    	measure <- 'simplicity'
				    df <- data.frame()
				    for (t in 1:length(dat)) {
				    	dft <- model[[measure]][[t]]
				    	dft$period <- ifelse(length(names(dat))>0, names(dat)[t], t)
				    	df <- rbind(df, dft)
				    }
				    file <- sprintf('%s-%s.csv',measure,ts)
				    write.csv(df, file=file, row.names = F)
				    files <- c(files, file)
			    }

		        ## Predictability
		        if ('predictability' %in% measures) {
		        	measure <- 'predictability'
				    dat <- model[[measure]]
				    df <- data.frame()
				    for (i in 1:length(dat)) {
				    	dfi <- melt(dat[[i]], varnames = c('period1','period2'), value.name = measure)
				    	dfi$firm <- names(dat)[i] # ifelse(length(names(dat))>0, names(dat)[t], t)
				    	df <- rbind(df, dfi)
				    }
				    file <- sprintf('%s-%s.csv',measure,ts)
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
			    # file <- sprintf('distance-%s.csv',ts)
			    # write.csv(df, file=file)
			    # files <- c(files, file)

		    	zip(con,files)

			}
		)



		output$analysis_save_plots_button <- downloadHandler(
			filename = function() {
			    sprintf('sequence_analysis_plots-%s.png', ts)
			},
			content = function(file) {
			  	model <- loadModel()
			    # temp dir
			    owd <- setwd(tempdir())
      			on.exit(setwd(owd))
			    if ('plots' %in% names(model)) {
			    	## PLOT
			    	nall <- length(model$plots)
			    	ncols <- floor(sqrt(nall))
			    	nrows <- ceiling(nall / ncols)
			    	grid <- ggarrange(plotlist = model$plots, ncol=ncols, nrow = nrows)
			    	ggsave(file, plot=grid, width = ncols*5.5, height = nrows*4, dpi = 200, units = "in")  ## height='10', width='10', units='in', res=200
			    }
			}
		)

		# output$savePlotValue <- renderText({
		#     input$savePlots
		# })

		# output$analysis_save_plots_button <- downloadHandler(
		# 	filename = function() {
		# 	    sprintf('sequence_measure_plots-%s.png', ts)
		#     # file.choose()
		# 	},
		# 	content = function(file) {
		# 		ggsave(file,plot=data$plot)
		# 	}
		# )


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
