
####################################################################################
##                                  SERVER 
####################################################################################
.server <- function()
{
  # require(TraMineR)
  # require(reshape2)
  # require(ggplot2)
  # require(tibble)
  # require(tidyr)
  # require(stringr)
  # require(ggpubr)
  
  
	## MODEL FILE
	MODEL_FILE <- './tmp_sequencer_data_model.rds'
	# MODEL_FILE <- './../R-Portable/tmp_sequencer_data_model.rds'

	## INPUT FILE
	INPUT_FILE <- './tmp_sequencer_input.rds'
	# INPUT_FILE <- './../R-Portable/tmp_sequencer_input.rds'

	##
	# Init Model Object
	##
	initModel <- function()
	{
		return(list(
			## Sequence Alphabet
			analysis_alphabet = list(x=NA, xpath='', varnamemap=NA, actionCol=NA, actorCol=NA, periodCol=NA), 
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
  #cats <- strsplit("Agreement, Challenge, Elaboration, New Idea, Proceeding, 
	#                     Questioning, Relational Integration", ', ')[[1]]
	##
	getNameAbbrevList <- function(categories) {
	  num.cats <- length(categories)
	  out <- as.list(categories)
	  abbs <- LETTERS
	  i <- 1
	  while( length(abbs) < num.cats) {
	    i <- i+1
	    abbs <- c(abbs, 
	      sapply(LETTERS, function(x)paste(c(x,i),collapse = ''))
	    )
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
	# Create list of sequences by `seqname` attribute (e.g., actor)
	# for sequence listed in `varname` column (e.g., action)
	##
	longDf2SeqList <- function(df, seqnames, seqname='actor', varname='action')
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
	seqList2Df <- function(l, missing.to.end=FALSE)
	{
	  maxlen <- max(sapply(l,length))
	  
	  for (i in 1:length(l)) {
	    xi <- l[[i]]
	    xilen <- length(l[[i]])
	    
	    if(missing.to.end) { ## move missing to end
	      idx.val <- which( !is.null(xi) & !is.na(xi) & !is.nan(xi) & xi !='' )
	      idx.mis <- (1:xilen)[ -idx.val ]
	      xi <- c( xi[idx.val], rep(NA, length(idx.mis) ) ) #replace empty strings,NULL,NaN with NAs at end
	    }
	    
	    ## pad ending with NAs (to length of longest sequence)
	    if (xilen < maxlen) {
	      xi <- c(xi, rep(NA, maxlen - xilen) ) 
	    }
	    
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

	# ##
	# # Transform named list of sequences to wide dataframe of sequence rows
	# #   paddings NAs at end to match seq lengths 
	# ##
	# seqList2Df <- function(l, categories)
	# {
	#   ## get vector of lengths for each item(vector) in input list
	#   l.len <- plyr::laply(l, length) 
	#   ## filter l to only items with elements (length > 0)
	#   l <- l[ which(l.len > 0) ]
	#   ##
	#   maxlen <- max(l.len)
	#   for (i in 1:length(l)) {
	#     n.xi.orig <- length( l[[i]] )
	#     xi <- l[[i]][ which( l[[i]] %in% categories ) ]
	#     xi <- c(xi, rep(NA, maxlen - n.xi.orig) )
	#     if (i == 1) {
	#       ldf <- data.frame(xi)
	#       names(ldf) <- names(l)[i]
	#     } else {
	#       ldf <- cbind(ldf, xi)
	#       names(ldf)[i] <- names(l)[i]
	#     }
	#   }
	#   return(t(ldf))
	# }

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

	# ##
	# # Herfindahl-Hirschman Index
	# #   0 < hhi <= 1
	# ##
	# hhi <- function(x){
	# 	## share in decimals (not percentages)
	# 	ms <-  x / sum(x) 
	# 	## HHI is sum of squared shares
	# 	hhi <- sum(ms^2)
	# 	return(hhi)
	# }
	
	##
	# Shannon entropy (Shannon diversity index)
	# (replacing HHI for complexity measure)
	# @see Connelly et al. 2017 SMJ
	#
	##
	shannonEntropy <- function(x, logbase=exp(1)) {
	  # proportions by category
	  vec <- x / sum(x)
	  #drop 0 to avoid NaN resulting from log2
	  vec <- vec[vec>0]
	  #compute entropy
	  ent <-  -sum(vec * log(vec, base=logbase))
	  # because shannon entropy is negatively related to HHI (e.g., simplicity)
	  # take the negative of shannon measure to proxy simplicity
	  return( -ent )
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
	  ## GET 1 actors sequence pairwise gamma table
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
	  require(TraMineR)
	  require(reshape2)
	  require(ggplot2)
	  require(tibble)
	  require(tidyr) ## TODO: check if this lib is used or can be removed
	  require(stringr)
	  require(ggpubr)
	  
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
			df <- read.csv(inFile$datapath, header = input$header, na.strings=c('','""','NA'), stringsAsFactors=FALSE, 
				fileEncoding=input$alphabet_fileEncoding, #'UTF-8',
				# encoding= 'UTF-8', fileEncoding="UTF-8-BOM", #'UTF-8',
				# quote = c('"'),
				check.names=TRUE, 
				strip.white=TRUE
				)
			# print(c('class df:',class(df)))
			# print(c('alphabet df:',df))
			# print(c('colnames alphabet df:',colnames(df)))
			
			li <- list()
			for (i in 1:ncol(df)) {
			  colnm <- names(df)[i]
			  .x <- df[[i]] ## get vector from i'th col
			  li[[colnm]] <- .x[which( !is.null(.x) & !is.nan(.x) & !is.na(.x) & .x != '' )]
			}
			# names(li)
			# li <- plyr::dlply(df function(col){
			# 	# as.factor(unique(col[which(!is.null(col) & !is.nan(col) & !is.na(col))]))
			# 	unique(col[which(!is.null(col) & !is.nan(col) & !is.na(col))])
			# })
			# print(c('li: ', li))
			
			## UPDATE SELECTION OF ACTION COLUMN
			updateSelectInput(session, "alphabet_selectCategoryColumn", label = "Category Column", choices = colnames(df), 
				selected = ifelse(exists(input$alphabet_selectCategoryColumn) & input$alphabet_selectCategoryColumn %in% colnames(df), 
				                  input$alphabet_selectCategoryColumn, 
				                  colnames(df)[1])
				# selected = input$alphabet_selectCategoryColumn
				)

			## add var name abbreviations
			varnamemap <- if (input$alphabet_selectCategoryColumn %in% names(li)) {
					getNameAbbrevList(li[[input$alphabet_selectCategoryColumn]])
				} else {
					x <- li[[1]]
					names(x) <- li[[1]]
					x
				}

			## OUTPUT: list of alphabet items (action, actor, etc.) 
			str(li)
			if (length(varnamemap) > 0) {
				model <- loadModel()
				model[['analysis_alphabet']]$actionCol <- input$alphabet_selectCategoryColumn
				model[['analysis_alphabet']]$varnamemap <- varnamemap
				saveRDS(model, file=MODEL_FILE)
				cat(sprintf('\nAbbreviations for Column: `%s`\n %s', 
					input$alphabet_selectCategoryColumn, printNamedList(varnamemap)))
			}

			## REPLACE ACTION NAMES WITH ABBREVIATIONS
			if (input$alphabet_selectCategoryColumn %in% names(li)) {
				for (i in 1:length(varnamemap)) {
					idx <- which(li[[input$alphabet_selectCategoryColumn]] == varnamemap[[i]])
					li[[input$alphabet_selectCategoryColumn]][idx] <- names(varnamemap)[i]
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
			df <- read.csv(inFile$datapath, header = FALSE, ## header=FALSE for handling row/colnames
			               na.strings=c('','""'), stringsAsFactors=FALSE,  
              		  # header = input$subcostmat_header
              			# row.names=input$subcostmat_rownames,
              			fileEncoding=input$subcostmat_fileEncoding, #'UTF-8',
              			# quote = c('"'),
              			check.names=TRUE,
              			strip.white=TRUE
              			)
      
      # print('DEBUG subcost mat df:')
      # print(df)
      
    	alphabet <- model$analysis_alphabet$x
    	actionCol <- model$analysis_alphabet$actionCol
    	varnamemap <- model$analysis_alphabet$varnamemap
    	ncats <- length(varnamemap)
    
    	
    	nrows <- nrow(df)
    	ncols <- ncol(df)
    	
    	## Assume must have row names or column names
    	## Only keep subcost mat rows/cols in square matrix (excluding header or rownames)
    	## 
    	if (nrows == ncols) {
    	  headvec <- df[1,] ## either first row or column should work
    	  df2 <- df[-1,-1]
    	} else if (nrows > ncols) {
    	  headvec <- df[1, ]
    	  df2 <- df[-1, ]
    	} else {
    	  headvec <- df[ ,1]
    	  df2 <- df[ ,-1]
    	}
    	
    	idxs <- which(headvec %in% varnamemap)
    	
    	mat <- as.matrix( df2[idxs,idxs] )
    	rownames(mat) <- headvec[idxs]
    	colnames(mat) <- headvec[idxs]
    	storage.mode(mat) <- "numeric"
    	
    	
    	# ## ASSUME ROW NAMES OR COLUMN NAMES ARE INCLUDED (REQUIRED)
    	# if (nrows == ncols) {
    	#   row.idxs <- which( df[,1] %in% varnamemap ) ## first col of df for rownames
    	#   col.idxs <- which( df[1,] %in% varnamemap ) ## first row of df for colnames 
    	#   mat <- as.matrix( df[row.idxs, col.idxs] )
    	#   rownames(mat) <- varnamemap[ row.idxs ]
    	#   colnames(mat) <- varnamemap[ col.idxs ]
    	#   
    	# } else {
    	#   
    	#   headvec <- if (nrows > ncols) { df[1,] } else { df[,1] }
    	#   ## take the square matrix of what rows/cols have in common
    	#   ## regardless missing 1 row or 1 col
    	#   nsq <- min( nrows, ncols )
    	#   ## bottom right square of input table (in case it's rectangular with header row, etc.)
    	#   df2 <- df[ (nrows-nsq+1):nrows, (ncols-nsq+1):ncols ]
    	#   mat <- as.matrix( df2 )
    	#   
    	#   .cnames <- headvec[which(headvec %in% varnamemap)]
    	#   .cnames <- if( length(.cnames) < nsq ) {
    	#     ## pad missing names with letters as placeholders
    	#     .cnames <- c(.cnames, paste0('v', (length(.cnames)+1):nsq ) )  
    	#   } else if () {
    	#     
    	#   } else {
    	#     
    	#   }
    	#   
    	#   rownames(mat) <- 
    	#   colnames(mat) <- 
    	#   
    	# }
    	
  		# ## Use only the square matrix of common rows/columns
  		# ## reading in the table without header...
  		# ##  (if ncols > nrows --> has header; no rownames)
  		# ##  (if nrows < ncols --> has rownames; no header )
  		# ##  --> keep only square matrix
  		# df2 <- df[ (nrows-n+1):nrows, (ncols-n+1):ncols ]
  		# as.matrix( df2 )
  		# # .catnames <- if ( length(varnamemap) >= n) {
  		# #   names(varnamemap)[1:n]
  		# # } else {
  		# #   c()
  		# # }
     
  		# mat <- as.matrix( df )
			

			nvarnms <- length(varnamemap)
			
			if ( nvarnms != nrow(mat) ) {
			  cat(sprintf('\nNumber of category names %s does not equal the subcost matrix dimensions [%s x %s], transformed from input table dimensions [%s x %s]\n', 
			                nvarnms, dim(df)[1], dim(df)[2], nrows, ncols))
			  return()
			}
			
			
			# # print(c('DEBUG varnamemap:',varnamemap))
			# # print(c('DEBUG subcostmat df str: ', str(df)))
			
			##-----------------------------------------
			# ## automatically determine column names
			# nrows <- nrow(df)
			# ncols <- ncol(df)
			# hasColNames <- nrows > ncats ## if 1 more row than #cats --> has col name
			# hasRowNames <- ncols > ncats ## if 1 more col than #cats --> has row name
			# ## remove name row or col if present, and then assign rownames(), colnames()
			# df <- if (hasColNames & hasRowNames) { # is Square
			# 	# .rownames <-  df[,1]; # .colnames <-  df[1,]
			# 		df <- df[-1,-1]
			# 	} else if (nrows > ncols) {
			# 		## varnames in 1st row  (as column headers)
			# 		df <- df[-1, ]
			# 	} else if (ncols > nrows) {
			# 		## varnames in 1st col (as row names)
			# 		df <- df[ ,-1]
			# 	} else {
			# 		df ## NO CHANGE (no col names and no row names)
			# 	}
			# colnames(df) <- names(varnamemap)
			# rownames(df) <- names(varnamemap)
			# 
			# mat <- as.matrix(df)
			##------------------------

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

			## run seqdef() in parallel
			require(foreach)
			
			model <- loadModel()
			
			## INPUT
			df <- read.csv(inFile$datapath, header = input$seqdata_header, sep=',', fill=TRUE, stringsAsFactors=FALSE,
				fileEncoding=input$seqdata_fileEncoding, #'UTF-8',
				# quote = c('"'),
				check.names=TRUE,
				strip.white=TRUE
				)
			
			## Input
			model$analysis_data <- list(x=df, xpath=inFile$datapath)
			saveRDS(model, file=MODEL_FILE)
			# saveModel(model, xname='analysis_data', x=df, xpath=inFile$datapath)

			
			# print('colnames df:')
			# print(names(df))
			
			## Main model attributes
			alphabet <- model$analysis_alphabet$x
			varnamemap <- model$analysis_alphabet$varnamemap
			ncats <- length(varnamemap)
			
			## Main variable names
			actionCol <- model$analysis_alphabet$actionCol
			actorCol <- names(alphabet)[ ! names(alphabet) %in%  actionCol ][1] ## e.g., 'actor', 'Team', ...
			aCols <- c(actionCol,actorCol)
			aColIdxs <- which( names(df) %in% aCols)
			naColIdxs <- which( ! names(df) %in% aCols)
			
			cnames <- names(df)
			# periodCol <- cnames[ (max(aColIdxs)+1) ]
			possiblePdCols <- cnames[ naColIdxs ]
			
			# print('DEBUG 6')
			# print('possiblePdCols:')
			# print(possiblePdCols)
			
			# possiblePdCols <- names(df)[! names(df) %in% aCols ]
			# periodCol <- possiblePdCols[length(.remainingCols)] ##'period'
			
			# # nuni <- apply(df,2,function(x)length(unique(x, na.action='na'))) ## unique elements per column
			# nuni <- foreach(i=1:ncol(df), .combine='c') %do% { length(unique(df[[i]])) } ## extract i'th column as a vector
		
			# print(c('NAMES nuni:',names(nuni)))
			# print(c('nuni:',nuni))
			# min.col.idx <- which.min( nuni[naColIdxs] )
			maxaColIdxs <- max(aColIdxs)
			pdColIdx <- if ( !is.infinite(maxaColIdxs) & maxaColIdxs < ncol(df) ) {
			  max(aColIdxs)+1 ## the next column after the last of actor/action columns
			} else {
			  length(cnames) ## just use last column
			}
			periodCol <- cnames[pdColIdx]
			# cat(c('\n\nDEBUG periodCol:', periodCol))
			
			## UPDATE MODEL
			model[['analysis_alphabet']]$actorCol <- actorCol
			model[['analysis_alphabet']]$periodCol <- periodCol
			saveRDS(model, file=MODEL_FILE)
			# print(model)
			
			## UPDATE SELECTION OF PERIOD COLUMN
			updateSelectInput(session, "alphabet_selectPeriodColumn", label = "Period Column", 
			                  choices = possiblePdCols, 
			                  selected = ifelse(exists(input$alphabet_selectPeriodColumn) & input$alphabet_selectPeriodColumn %in% cnames, 
			                                    input$alphabet_selectPeriodColumn, 
			                                    periodCol
			                  )
			                  # selected = input$alphabet_selectCategoryColumn
			)
			
			periodCol <- input$alphabet_selectPeriodColumn
			model[['analysis_alphabet']]$periodCol <- periodCol
			saveRDS(model, file=MODEL_FILE)
			
			## REPLACE ACTION NAMES WITH ABBREVIATIONS
			for (i in 1:length(varnamemap)) {
				idx <- which(df[,actionCol] == varnamemap[[i]])
				df[idx,actionCol] <- names(varnamemap)[i]
			}

			## Data based on df, alphabet, and main variable names
			periods <- unique(df[,periodCol])
			actors <- as.character(alphabet[[actorCol]])
			actionAlphabet <- as.character(alphabet[[actionCol]])
			
			## remove null actor rows (e.g., empty last CSV row)
			df <- df[which(df[[actorCol]] %in% actors), ]
			
			##-----------------------------------------------
			## LIMIT ACTORS TO THOSE WITH ACTIONS IN DATAFRAME
			# if (input$seqdata_drop_empty_actors) {
			  df <- df[complete.cases(df), ]
			  # df <- df[which(df[[actorCol]] %in% actors), ]
			  idx.actions <- which( ! is.null(df[[actionCol]]) & !is.na(df[[actionCol]]) & df[[actionCol]] != '')
			  actorsNonempty <- sort(unique(df[idx.actions,actorCol]))
			  actors <- actorsNonempty #sort(unique(df[[actorCol]]))
			  ##
			  model$analysis_alphabet$x$actorsSubset <- actors
			  # model$analysis_data$x <- df
			# }
			##-----------------------------------------------
			
			# seqdefs <- list()
			# for (t in 1:length(periods))  #length(periods)
			# {
			# 	pd <- periods[t]
			# 	tidx <- which(df[,periodCol] == pd)
			# 	t.df <- df[tidx, ]
			# 	t.l <- longDf2SeqList(t.df, actors, actorCol, actionCol)
			# 	t.ldf <- seqList2Df(t.l)
			# 	right <- 'DEL'  # left <- 'NA' # gaps <- 'NA'
			# 	seqdefs[[pd]] <- seqdef(t.ldf, alphabet=actionAlphabet, right=right)
			# }
			seqdefs <- foreach(t=1:length(periods)) %do% {
			  pd <- periods[t]
			  tidx <- which(df[,periodCol] == pd)
			  t.df <- df[tidx, ]
			  t.l <- longDf2SeqList(t.df, actors, actorCol, actionCol)
			  # ##
			  # model$DEBUG <- list(t.l=t.l) ## DEBUG
			  # saveRDS(model, file = MODEL_FILE) ##DEBUG
			  # print(t.l)
			  # return()
			  # ##
			  t.ldf <- seqList2Df(t.l, missing.to.end=TRUE) ## <-- Fixes error(?) TODO: more testing with different data
			  
			  # model$DEBUG <- list(t.l=t.l) ## DEBUG
			  # saveRDS(model, file = MODEL_FILE) ##DEBUG
			  # print(actionAlphabet)
			  # print(t.l)
			  # # return()
			  
			  ## REMOVE ACTORS WITHOUT ACTIONS
			  # if (input$seqdata_drop_empty_actors) {
  		    t.ldf.lns <- sapply(t.ldf, length)
  		    if(any(t.ldf.lns == 0)) {
  		      cat('\nNote: Removing actors without actions from sequences list.\n\n')
  		      t.ldf <- t.ldf[ which(t.ldf.lns > 0) ]
  		    }
			  # }
			  
			  ## this block moved to seqList2Df() logic
			  # ## MOVE missing actions (NA's or empty '' ) to end of each actor's sequence
			  # t.ldf <- plyr::llply(t.ldf, function(x){
			  #   idx.val <- which( !is.null(x) & !is.na(x) & !is.nan(x) & x!='' )
			  #   idx.mis <- (1:length(x))[ -idx.val ]
			  #   c( x[idx.val], x[idx.mis] ) # move missing to end for now (this is a constraint to be loosened in future development)
			  # })
			  
			  
			  ##
			  right <- 'DEL'  # left <- 'NA' # gaps <- 'NA'
			  seqdef(t.ldf, alphabet=actionAlphabet, right=right)  # missing=''
			  
			  # return()
			  
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
		# 			return(length(model$analysis_alphabet$x$actors) + length(model$) )
		# })

		##
		## seqdef - define missing values on right (NA or DEL)
		##   --> compute dist from OM  (or selected dist metric)
		## simplicity - HHI
		## unpredictability - levdist from own actor previous period(s)
		## grouping - mean { gamma analysis separation score } across all actions
		## motif - variance of the averages in the pair-wise gamma analysis precedence scores across all action types
		##
		## MAIN ANALYSIS FUNCTION CALLED FROM MEASURES TAB
		output$analysis_run_value <- renderPrint({

			if (input$analysis_run) {

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
				actorCol <- model$analysis_alphabet$actorCol
				periodCol <- model$analysis_alphabet$periodCol
				right <- 'DEL'  # left <- 'NA' # gaps <- 'NA' ## seqdef parameter

				periods <- unique(dat[,periodCol])
				## use subset of actors with actors, else use original alphabet of all actors
				actors <- if ('actorsSubset' %in% names(alphabet) ){ 
				    as.character( alphabet[['actorsSubset']] )
				  } else { 
				    as.character( alphabet[[actorCol]] )
				  }
				actionAlphabet <- as.character(alphabet[[actionCol]])

				npds <- length(periods)
				
				## Filter Actors to those kept 
				##  (MOVED TO DATA INPUT STEP)
				nactors <- length(actors)
				nactpds <- npds * nactors

				seqdefs <- model$seqdefs
				gamma <- list()    # list of pairwise gamma matrices per period and per actor
				distance <- list() # squence distance mesaures period list
				grouping <- list() # gamma list ('grouping' measure avg. precedence scores)
				motif <- list() # gamma list ('grouping' measure avg. precedence scores)
				predictability <- list()
				simplicity <- list()

				## clear existing measures from model (in case user reruns analysis with different subset of measures selected)
				for (measure in c('distance','simplicity','grouping','predictability','motif', 'gamma')) {
					if (measure %in% names(model))
						model[[measure]] <- NULL
				}
				
				measures <- input$analysis_measures_group
				model$analysis_measures_group <- measures
				saveRDS(model, file=MODEL_FILE)
				
				##-----------------------------------
				## COMPUTE SELECTED MEASURES BY PERIOD
				##-----------------------------------
				
				
				# withProgress(message = 'Computing measures by period...', value=0, {
				#   npds <- length(periods)
				#   				    ## TODO: find best place to update progress (beginning, middle, end)
				#     incProgress(1/npds, detail=sprintf('Processing period: %s',pd))
				# }) ## /end withProgress()
				
				    
		    ## Pairwise distances over actors  (per period)
		    if ('distance' %in% measures) 
		    {
		      withProgress(message = 'Computing distance...', value=0, {
		        model <- loadModel()
  		      for (t in 1:npds)  #length(periods)
  		      {
  		        pd <- periods[t]
				      t.xdist <- seqdist(seqdefs[[t]], method=method, norm=norm, sm=sm, with.missing=input$seqdata_run_with_missing)
				      dimnames(t.xdist) <- list(actors, actors)
				      distance[[pd]] <- t.xdist
				      incProgress(1/npds, detail=sprintf('Period: %s',pd))
  		      }
		        model$distance <- distance
		        saveRDS(model, file=MODEL_FILE)
		      }) ## /end withProgress()
		    } else {
		    	model$distance <- NULL
		    }
		  
				# print('DEBUG 1')
				# return()
				
				
		    ## GK Gamma is used in motif and grouping -- compute it once first for either/both
		    if (any(c('motif','grouping') %in% measures)) 
		    {
		      withProgress(message = 'Computing gamma...', value=0, {
		        model <- loadModel()
		        for (t in 1:npds)  #length(periods)
		        {
		          pd <- periods[t]
    		      # # FIRST compute pairwise G-K gamma
    		      # gamma[[pd]] <- list()
    		      # for (i in 1:length(actors)) {
    		      # 	actor <- actors[i]
    		      # 	gamma[[pd]][[actor]] <- pairwiseGammaMatrix(seqdefs[[t]][i,], actionAlphabet, na.fill=0)
    		      # }
    		      # 
    		      # print(gamma)
  
  		        ## FIRST compute pairwise G-K gamma
  		        gamma[[pd]] <- foreach(i=1:length(actors)) %do% {
  		          actor <- actors[i]
  		          incProgress(1/nactpds, detail=sprintf('Period: %s - Actor: %s',pd,actor))
  		          # gamma[[pd]][[actor]] <- pairwiseGammaMatrix(seqdefs[[t]][i,], actionAlphabet, na.fill=0)
  		          pairwiseGammaMatrix(seqdefs[[t]][i,], actionAlphabet, na.fill=0)
  		        }
  		        names(gamma[[pd]]) <- actors
		        }
		        
		        model$gamma <- gamma
		        saveRDS(model, file=MODEL_FILE)
		        
		      }) ## /end withProgress()
		 
		      # names(gamma) <- actors
		      # print(c('DEBUG gamma:',gamma))
		    } else {
		    	model$gamma <- NULL
		    }
				# print(loadModel())
				# return()
				
		    
		    if ('grouping' %in% measures) 
		    {
		      withProgress(message = 'Computing grouping...', value=0, {
		        model <- loadModel()
		        for (t in 1:npds)  #length(periods)
		        {
		          pd <- periods[t]
      		      # # t.dat <- dat[dat[,periodCol]==pd, ]
      		      # t.group <- data.frame()
      		      # for (i in 1:length(actors)) {
      		      # 	actor <- actors[i]
      		      # 	## separation score for a given type of element is calculated as
      		      # 	## the mean of the absolute value of its pair-wise gamma values
      		      # 	## (Holmes, 1995; Holmes and Sykes, 1993).
      		      # 	separation <- rowMeans(abs(gamma[[pd]][[actor]]), na.rm = T)
      		      # 	## Grouping
      		      # 	## To calculate the extent to which the entire sequence exhibits grouping,
      		      # 	## we calculated the mean of the separation scores across all action types.
      		      # 	## We reversed the direction of the scores (multiplying them by −1.0)
      		      # 	## so that higher scores indicate higher levels of grouping
      		      # 	## among actions in a sequence.
      		      # 	groupingMeasure <- mean(separation, na.rm = T) * -1
      		      # 	t.i.group <- data.frame(actor=actor, grouping = groupingMeasure)
      		      # 	t.group <- rbind(t.group, t.i.group)
      		      # }
      		      # grouping[[pd]] <- t.group
      		      
      		      grouping[[pd]] <- foreach(i=1:length(actors), .combine='rbind') %do% {
      		        actor <- actors[i]
      		        ## separation score for a given type of element is calculated as
      		        ## the mean of the absolute value of its pair-wise gamma values
      		        ## (Holmes, 1995; Holmes and Sykes, 1993).
      		        separation <- rowMeans(abs(gamma[[pd]][[actor]]), na.rm = T)
      		        ## Grouping
      		        ## To calculate the extent to which the entire sequence exhibits grouping,
      		        ## we calculated the mean of the separation scores across all action types.
      		        ## We reversed the direction of the scores (multiplying them by −1.0)
      		        ## so that higher scores indicate higher levels of grouping
      		        ## among actions in a sequence.
      		        groupingMeasure <- mean(separation, na.rm = T) * -1
      		        data.frame(actor=actor, grouping = groupingMeasure)
      		      }
      		      
      		      incProgress(1/npds, detail=sprintf('Period: %s',pd))
      		      
		        }
      		      
		        model$grouping <- grouping
		        saveRDS(model, file=MODEL_FILE)
		        
		      }) ## /end withProgress()

		    } else {
		    	model$grouping <- NULL
		    }
		    
		    
		    if ('motif' %in% measures) 
		    {
		      withProgress(message = 'Computing motif...', value=0, {
		        model <- loadModel()
		        for (t in 1:npds)  #length(periods)
		        {
		          pd <- periods[t]
    		      # # t.dat <- dat[dat[,periodCol]==pd, ]
    		      # t.motif <- data.frame()
    		      # for (i in 1:length(actors)) {
    		      # 	actor <- actors[i]
    		      # 	## the precedence score for a given element type is calculated as
    		      # 	## the mean of all of its pair-wise gamma values
    		      # 	## (Holmes, 1995; Holmes and Sykes, 1993).
    		      # 	precedence <- rowMeans(gamma[[pd]][[actor]], na.rm = T)
    		      # 	## Motif
    		      # 	## determine the extent to which the entire sequence
    		      # 	## exhibits internal structuredness, we calculated
    		      # 	## **the variance of the averages in the pair-wise precedence scores
    		      # 	## across all action types. Averaging across action types indicates
    		      # 	## the relative position—beginning or end—in the overall sequence
    		      # 	## in which the particular type of action appears.
    		      # 	## Variance in average precedence scores captures the ordinal specificity
    		      # 	## and stability of elements in a sequence.
    		      # 	motifMeasure <- var(precedence, na.rm = TRUE)
    		      # 	t.i.motif <- data.frame(actor=actor, motif = motifMeasure)
    		      # 	t.motif <- rbind(t.motif, t.i.motif)
    		      # }
    		      # motif[[pd]] <- t.motif
    		      
    		      motif[[pd]] <- foreach(i=1:length(actors), .combine='rbind') %do% {
    		        actor <- actors[i]
    		        ## the precedence score for a given element type is calculated as
    		        ## the mean of all of its pair-wise gamma values
    		        ## (Holmes, 1995; Holmes and Sykes, 1993).
    		        precedence <- rowMeans(gamma[[pd]][[actor]], na.rm = T)
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
    		        data.frame(actor=actor, motif = motifMeasure)
    		      }
    		      
    		      incProgress(1/npds, detail=sprintf('Period: %s',pd))
    		      
		        }
		        
		        model$motif <- motif
		        saveRDS(model, file=MODEL_FILE)
		        
		      }) ## /end withProgress()
		    } else {
		    	model$motif <- NULL
		    }
		    # print(motif)
		    
		    if ('simplicity' %in% measures) 
		    {
		      withProgress(message = 'Computing simplicity...', value=0, {
		        model <- loadModel()
		        for (t in 1:npds)  #length(periods)
		        {
		          pd <- periods[t]
    		      ## simplicity HHI score
    		      t.dat <- dat[dat[,periodCol]==pd, ]
    		      t.simp <- data.frame()
    		      for (i in 1:length(actors)) {
    		      	t.i.cnt <- plyr::count( t.dat[ t.dat[,actorCol]==actors[i] , actionCol] )
    		      	#simplicityMeasure <- hhi(t.i.cnt$freq)
    		      	simplicityMeasure <- shannonEntropy(t.i.cnt$freq)
    		      	t.i.cntdf <- data.frame(actor=actors[i], simplicity = simplicityMeasure)
    		      	t.simp <- rbind(t.simp,  t.i.cntdf)
    		      }
    		      simplicity[[pd]] <- t.simp
    		      # simplicity[[pd]] <- foreach (i=1:length(actors), .combine='rbind') %do% {
    		      #   t.i.cnt <- plyr::count( t.dat[ t.dat[,actorCol]==actors[i] , actionCol] )
    		      #   #simplicityMeasure <- hhi(t.i.cnt$freq)
    		      #   simplicityMeasure <- shannonEntropy(t.i.cnt$freq)
    		      #   data.frame(actor=actors[i], simplicity = simplicityMeasure)
    		      # }
    		      
    		      incProgress(1/npds, detail=sprintf('Period: %s',pd))
    		      
		        }
		        
		        model$simplicity <- simplicity
		        saveRDS(model, file=MODEL_FILE)
		        
		      }) ## /end withProgress()
		    } else {
		    	model$simplicity <- NULL
		    }
		    

				  
				
				
				## loop over actors  (per period)
				## TODO: Convert to foreach (parallel)
				if ('predictability' %in% measures) 
				{
				  withProgress(message = 'Computing predictability...', value=0, {
				    model <- loadModel()
  					seqlaglist <- list()
  					for (i in 1:length(actors)) 
  					{
  					  actor <- actors[i]
  					  for (t in 1:length(periods)) {
  					    seqlaglist[[actor]] <- lapply(seqdefs, function(x) {
  					      xseq <- as.character(unlist( x[i,]))
  					      return( xseq[which(xseq %in% actionAlphabet)] ) #remove filled-in "%" for empty levels
  					    })
  					  }
  					  tlagseq <- seqList2Df(seqlaglist[[actor]])
  					  seqdeflag <- seqdef(tlagseq, alphabet=actionAlphabet, right=right)
  					  predictability[[actor]] <- seqdist(seqdeflag,  method=method, norm=norm, sm=sm, with.missing = TRUE) * -1
  					  dimnames(predictability[[actor]]) <- list(periods, periods)
  					  
  					  ## update progress
  					  incProgress(1/length(actors), detail=sprintf('Actor: %s',actor))
  					  
  					}
  					
  					model$predictability <- predictability
  					saveRDS(model, file=MODEL_FILE)
  					
				  })
				}else {
		    	model$predictability <- NULL
		    }

				# ## list names by time period
				# # if (length(gamma) == npds)  		names(gamma) <- periods
				# if (length(distance) == npds) 		names(distance) <- periods
				# if (length(grouping) == npds) 		names(grouping) <- periods
				# if (length(motif) == npds) 			names(motif) <- periods
				# if (length(simplicity) == npds) 	names(simplicity) <- periods
				# ## list names by actor
				# if (length(predictability) == nactors) names(predictability) <- actors

				# ## add selected measures to model object
				# if('distance' %in% measures)  		model$distance <- distance
				# if('grouping' %in% measures) 		model$grouping <- grouping
				# if('motif' %in% measures) 			model$motif <- motif
				# if('predictability' %in% measures)  model$predictability <- predictability
				# if('simplicity' %in% measures)  	model$simplicity <- simplicity
				# ## add gamma if used by other measure
				# # if (any(c('grouping','motif') %in% measures))	model$gamma <- gamma
				
				# model <- loadModel()
				
				## Save Model
				model$analysis_run <- 'ANALYSIS RUN COMPLETED'
				saveRDS(model, file=MODEL_FILE)

				# ## SAVE NUM PLOTS
				# numplots <- length(model$seqdefs) * length(model$analysis_alphabet$x$actor)
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
		# 				dflong <- melt(model[[measure]][[i]], varnames = c('actor1','actor2'), value.name = 'distance')
		# 				plt <- ggplot(data = dflong, aes(x = actor1, y = actor2)) +
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
	# 					ggtitle(sprintf('%s: actor %s',measure, itemnames[i] ))
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
		
	## Clear existing plots before re-plotting 
	## (TODO: check if this is superfluous)
	output$analysis_output_plots <- renderPlot({
	    model <- loadModel()
	    model$plots <- NULL
	    return()
	})

	output$analysis_output_plots <- renderPlot({

	 	if(input$analysis_output_plots_button) {
	 		# model <- readRDS(MODEL_FILE)
	 		model <- loadModel()
	 		
	 		if ('plots' %in% names(model)) {
	 		  model$plots <- NULL
	 		}
	 		
	 		measuresAll <- c('distance','predictability','simplicity','grouping','motif')
	 		modelNames <- names(model)
	 		
	 		measures <- model$analysis_measures_group ## use saved measures
	 		# measures <- measuresAll[measuresAll %in% modelNames]

	 		if ( length(measures) == 0 ) {
	 			return()
	 		}
	 		
	 		n.measures <- length(measures) + 1 ## treat load/save/arrange overhead as another plot for progress bar
	 		
	 		withProgress(message = 'Plotting measures...', value=0, {

  	 		plots <- list()
  	 		
        counter <- 0
  
  	 		MEASURE <- 'distance'
  	 		if (MEASURE %in% measures) 
  	 		{
  	 			# #DEBUG
  	 			# plot(model[[measures[1]]][[1]], main=sprintf('%s: period %s',measures[1],1))
  	 			# for (measure in measures) {
  	 			for (i in 1:length(model[[MEASURE]])) {
  	 				dflong <- melt(model[[MEASURE]][[i]], varnames = c('actor1','actor2'), value.name = MEASURE)
  	 				plt <- ggplot(data = dflong, aes(x = actor1, y = actor2)) +
  	 					geom_tile(aes(fill = distance))  + 
  	 					geom_text(aes(label = round(distance, 1)), colour='#FFFFFF') +
  	 					scale_fill_continuous(high = "#132B43", low = "#56B1F7") + 
  	 					ggtitle(sprintf('%s: period %s',MEASURE,i))
  	 				plots[[length(plots)+1]] <- plt
  	 			}
  	 		  counter <- counter + 1
  	 			incProgress(1/n.measures)
  	 		} 
  	 		
  	 		MEASURE <- 'predictability'
  	 		if (MEASURE %in% measures) 
  	 		{
  	 			# #DEBUG
  	 			# plot(model[[measures[1]]][[1]], main=sprintf('%s: period %s',measures[1],1))
  	 			# for (measure in measures) {
  	 			itemnames <- names(model[[MEASURE]])
  	 			for (i in 1:length(model[[MEASURE]])) {
  	 				dflong <- melt(model[[MEASURE]][[i]], varnames = c('period1','period2'), value.name = MEASURE)
  	 				plt <- ggplot(data = dflong, aes(x = period1, y = period2)) +
  	 					geom_tile(aes(fill = predictability))  + 
  	 					geom_text(aes(label = round(predictability, 1)), colour='#FFFFFF') +
  	 					scale_fill_continuous(high = "#B10026", low = "#FFFFB2") + 
  	 					ggtitle(sprintf('%s: actor %s',MEASURE, itemnames[i] ))
  	 				plots[[length(plots)+1]] <- plt
  	 			}
  	 			counter <- counter + 1
  	 			incProgress(1/n.measures)
  	 		} 
  
  	 		MEASURE <- 'motif'
  	 		if (MEASURE %in% measures)
  	 		{
  	 			dat <- model[[MEASURE]]
  	 		    dflong <- data.frame()
  	 		    for (t in 1:length(dat)) {
  	 		    	dft <- model[[MEASURE]][[t]]
  	 		    	dft$period <- ifelse(length(names(dat))>0, names(dat)[t], t)
  	 		    	dflong <- rbind(dflong, dft)
  	 		    }
  	 		    plt <- ggplot(data = dflong, aes(x = as.integer(period), y = motif, colour=actor)) +
  	 		    	geom_line()  + 	geom_point() +
  	 		    	xlab('Period') + theme_bw() + 
  	 		    	ggtitle(sprintf('%s',MEASURE))
  	 		    plots[[length(plots)+1]] <- plt
  	 		    counter <- counter + 1
  	 		    incProgress(1/n.measures)
  	 		}
  
  	 		MEASURE <- 'grouping'
  	 		if (MEASURE %in% measures)
  	 		{
  	 			dat <- model[[MEASURE]]
  	 		    dflong <- data.frame()
  	 		    for (t in 1:length(dat)) {
  	 		    	dft <- model[[MEASURE]][[t]]
  	 		    	dft$period <- ifelse(length(names(dat))>0, names(dat)[t], t)
  	 		    	dflong <- rbind(dflong, dft)
  	 		    }
  	 		    plt <- ggplot(data = dflong, aes(x = as.integer(period), y = grouping, colour=actor)) +
  	 		    	geom_line()  + 	geom_point() +
  	 		    	xlab('Period') + theme_bw() + 
  	 		    	ggtitle(sprintf('%s',MEASURE))
  	 		    plots[[length(plots)+1]] <- plt
  	 		    counter <- counter + 1
  	 		    incProgress(1/n.measures)
  	 		}
  
  	 		MEASURE <- 'simplicity'
  	 		if (MEASURE %in% measures)
  	 		{
  	 			dat <- model[[MEASURE]]
  	 		    dflong <- data.frame()
  	 		    for (t in 1:length(dat)) {
  	 		    	dft <- model[[MEASURE]][[t]]
  	 		    	dft$period <- ifelse(length(names(dat))>0, names(dat)[t], t)
  	 		    	dflong <- rbind(dflong, dft)
  	 		    }
  	 		    plt <- ggplot(data = dflong, aes(x = as.integer(period), y = simplicity, colour=actor)) +
  	 		    	geom_line()  + 	geom_point() +
  	 		    	xlab('Period') + theme_bw() + 
  	 		    	ggtitle(sprintf('%s',MEASURE))
  	 		    plots[[length(plots)+1]] <- plt
  	 		    counter <- counter + 1
  	 		    incProgress(1/n.measures)
  	 		}
  
  	 		model$plots <- plots
  	 		saveRDS(model, file=MODEL_FILE)
  	 		
  	 		counter <- counter + 1
  	 		incProgress(1/n.measures)
  	 		
  	 		if (length(plots) > 0)
  	 		{
  	 		  ## PLOT
  	 		  nall <- length(model$plots)
  	 		  ncols <- floor(sqrt(nall))
  	 		  nrows <- ceiling(nall / ncols)
  	 		  
  	 		  par(mar=c(2,3,2,1))
  	 		  ggarrange(plotlist = plots, ncol=ncols, nrow = nrows)
  	 		}
  	 		
  	 		
	 		})
	 		
# 	 		model <- loadModel()
# 
# 	 		## PLOT
#     	nall <- length(model$plots)
#     	ncols <- floor(sqrt(nall))
#     	nrows <- ceiling(nall / ncols)
# 	 		par(mar=c(2,3,2,1))
# 	 		ggarrange(plotlist = plots, ncol=ncols, nrow = nrows)

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
				require(reshape2)
			  	model <- loadModel()
			  	measuresAll <- c('distance','grouping','motif','simplicity','predictability')
			  	# measures <- measuresAll[measuresAll %in% names(model)]
			  	measures <- model$analysis_measures_group
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
				    names(df)[which(names(df)=='L1')] <- 'actor'  ## list name at Level 1 is the actor name; rename column 'L1' to actorname
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
				    	dft <- melt(dat[[t]], varnames = c('actor1','actor2'), value.name = measure)
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
	        	dat <- model[[measure]]
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
	           dat <- model[[measure]]
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
			    	dat <- model[[measure]]
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
				    	dfi$actor <- names(dat)[i] # ifelse(length(names(dat))>0, names(dat)[t], t)
				    	df <- rbind(df, dfi)
				    }
				    file <- sprintf('%s-%s.csv',measure,ts)
				    write.csv(df, file=file, row.names = F)
				    files <- c(files, file)
		        }

			    # dat <- model$seqdefs
			    # df <- data.frame()
			    # for (t in 1:length(dat)) {
			    # 	dft <- melt(dat[[t]], varnames = c('actor1','actor2'), value.name = 'se')
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
