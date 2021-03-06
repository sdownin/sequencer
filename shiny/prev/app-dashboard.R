library(shiny)
library(shinydashboard)
source('testModule.R')

####################################################################################
##                                  VIEW 
####################################################################################

## header
dsh.header <- dashboardHeader(dropdownMenuOutput("messageMenu"), title="SequenceR")
#dsh.header <- dashboardHeader(
#	dashboardHeader(dropdownMenuOutput("messageMenu"))
#, title="SequenceR")



## sidebar - processes (analysis, inference, etc.)
dsh.sidebar <- dashboardSidebar(
	## Sidebar content
	sidebarMenu(
	  menuItem("Analysis", tabName = "Analysis", icon = icon("dashboard")),
	  menuItem("Inference", tabName = "Inference", icon = icon("th"))
	)

)

## body
dsh.body <- dashboardBody(
  
  navbarPage("Analysis",

		## Page 1 - data ====================================
		tabPanel("Data", 
			
				
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
				
				
		),
		
		## Page 2 - Measures ===================================
		tabPanel("Measures", 
		
		),
		
		## Page 3 - Outputs ===================================
		tabPanel("Outputs", 
		
		)
		
	),
  
    # Boxes need to be put in a row (or column)
	
	    tabItems(
		  # First tab content
		  tabItem(tabName = "Analysis",
			fluidRow(
			  box(plotOutput("plot1", height = 250)),

			  box(
				title = "Controls",
				sliderInput("slider", "Number of observations:", 1, 100, 50)
			  )
			)
		  ),

		  # Second tab content
		  tabItem(tabName = "Inference",
			h2("Widgets tab content")
		  )
		),
	
    fluidRow(
      box(plotOutput("plot1", height = 250)),

      box(
        title = "Controls",
        sliderInput("slider", "Number of observations:", 1, 100, 50)
      )
    )
)

## UI dashboard page
ui <- dashboardPage(dsh.header, dsh.sidebar, dsh.body)


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
 
# 	set.seed(122)
#	histdata <- rnorm(500)
#	output$plot1 <- renderPlot({
#		data <- histdata[seq_len(input$slider)]
#		hist(data)
#	})
 
#	 output$messageMenu <- renderMenu({
#	  # Code to generate each of the messageItems here, in a list. This assumes
#	  # that messageData is a data frame with two columns, 'from' and 'message'.
#	  msgs <- apply(messageData, 1, function(row) {
#		messageItem(from = row[["from"]], message = row[["message"]])
#	  })
#
#	  # This is equivalent to calling:
#	  #   dropdownMenu(type="messages", msgs[[1]], msgs[[2]], ...)
#	  dropdownMenu(type = "messages", .list = msgs)
#	})
 
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
shinyApp(ui, server, enableBookmarking = "url")  ## enableBookmarking = "server", "url"
