
library(shiny)
#library(shinydashboard)

fileEncodingVec <- c(
	"latin1"="latin1",
	"UTF-8"="UTF-8",
	"UTF-8-BOM"="UTF-8-BOM",
	"(none)"=NA,
	"UCS-2LE"="UCS-2LE", 
	"UTF-16LE"="UTF-16LE", 
	"UCS-2"="UCS-2", 
	"UTF-16"="UTF-16"
)

## UI #####################################
sliderTextUI <- function(id){
	ns <- NS(id)
	tagList(
		sliderInput(ns('slider'), "Slide me", 0, 100, 1),
		textOutput(ns('number'))
	)
}

sliderTextUItenths <- function(id, label){
	tagList(
		sliderInput(id, label, min=0, max=10, value=1, step=.1),
		textOutput(paste0(id,'_value'))
	)
}

selectInputUI <- function(id, label, optList) {
	tagList(
		selectInput(id, label, optList, 
			selected = NULL, multiple = FALSE,
			selectize = TRUE, width = NULL, size = NULL
		),
		textOutput(paste0(id,'_value'))
	)
}

selectInputUIdistance <- function(id, label) {
	optList <- list(
		`Edit Distances`=list(
			 `Optimal Matching (OM)`='OM'
			# ,`Localized OM`='OMloc'
			# ,`Spell-length-sensitive OM`='OMslen'
			# ,`OM of Spell Sequences`='OMspell'
			# ,`OM of Transition Sequences`='OMstran'
			# ,`Hamming`='OMstran'
			# ,`Hamming`='HAM'
			# ,`Dynamic Hamming`='DHD'
			# ,`Time Warp`='TWED'
		),
		`Common Attriute Count Metrics`=list(
			 `Longest Common Subsequence`='LCS'
			,`Longest Common Prefix`='LCP'
			,`Longest Common Suffix`='RLCP'
			# ,`Number of Matching Subsequences (NMS)`='NMS'
			# ,`NMS Weighted by Minimum Shared Time`='NMSMST'
			# ,`Subsequence Vectorial Representation`='SVRspell'
		),
		`State Distributions Distances`=list(
			# `Euclidean`='EUCLID'
			# ,`Chi-squared`='CHI2'
		)
	)
	selectInputUI(id, label, optList)
}


selectInputUInorm <- function(id, label) {
	optList <- list(`None` = "none", `Auto`= "auto")
	selectInputUI(id, label, optList)
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

	## seqdef - define missing values on right (NA or DEL)
		##   --> compute dist from OM
		## simplicity - HHI
		## unpredictability - levdist from own firm previous period
		## grouping - mean { gamma analysis separation score } across all actions
		## motif - variance of the averages in the pair-wise gamma analysis precedence scores across all action types
		##

checkboxGroupUImeasures <- function(id, label) {
	tagList(
		checkboxGroupInput(id, label = h3(label), 
			choices = list(
				  'Distance' = 'distance' 
				, 'Predictability' = 'predictability'
				, 'Simplicity' = 'simplicity' 
				, 'Grouping' = 'grouping'
				, 'Motif' = 'motif'
			),
			selected = c('distance','predictability','simplicity','grouping','motif')),
		textOutput(paste0(id,'_value'))
	)
}

actionButtonUI <- function(id) {
	ns <- NS(id)
	tagList(
		actionButton(ns("actionButton"), label = "Action"),
		textOutput(ns('actionButtonValue'))
	)
}

actionButtonUIrun <- function(id, label, ...) {
	# ns <- NS(id)
	tagList(
		actionButton(id, label = label, ...)
	)
}

actionButtonUIcheckdata <- function(id, label) {
	ns <- NS(id)
	tagList(
		actionButton("actionButtonDataCheck", label = label),
		textOutput('actionButtonDataCheckValue')
	)
}

fileInputUI <- function(fileId, heading, fileEncodingId) {
	tagList(
		fileInput(fileId, h3(heading),
			multiple = F,
			accept = c(
			  "text/csv",
			  "text/comma-separated-values,text/plain",
			  ".csv")
		),
		# selectInput(fileEncodingId "File Encoding", fileEncodingVec)
	)
}

fileInputUIAlphabet <- function(fileId, heading) {
	tagList(
		fileInput(fileId, h3(heading),
			multiple = F,
			accept = c(
			  "text/csv",
			  "text/comma-separated-values,text/plain",
			  ".csv")
		),
		#tags$hr(),
		checkboxInput("header", "Header", TRUE),
		# selectInput("alphabet_encoding", "Encoding", c()),
		selectInput("alphabet_fileEncoding", "File Encoding", fileEncodingVec)
	)
}

fileInputUIrownames <- function(fileId) {
	tagList(
		fileInput(fileId, h3("Choose CSV File"),
			multiple = F,
			accept = c(
			  "text/csv",
			  "text/comma-separated-values,text/plain",
			  ".csv")
		),
		#tags$hr(),
		checkboxInput("rownames", "Rownames", TRUE),
		checkboxInput("header", "Header", TRUE),
	)
}

fileInputUInrows <- function(fileId, nrowsID) {
	tagList(
		fileInput(fileId, h3("Choose CSV File"),
			multiple = F,
			accept = c(
			  "text/csv",
			  "text/comma-separated-values,text/plain",
			  ".csv")
		),
		#tags$hr(),
		checkboxInput("header", "Header", TRUE),
		selectInput(nrowsID, label="Read N Rows", 
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

tableOutputSummaryUI <- function(id, title) {
	tagList(
		h4(title),
		verbatimTextOutput(id) 
	) 
}


tableOutputSummaryUIwithSpinner <- function(id, title) {
	tagList(
		h4(title),
		verbatimTextOutput(id) %>% withSpinner(color="#0dc5c1")
	) 
}

plotOutputUI <- function(id, title) {
	# tagList(
	# 	h4(title),
		plotOutput(id, width = "300px", height = "300px")
	# )
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
