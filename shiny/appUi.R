####################################################################################
##                                  VIEW 
####################################################################################
.ui <- function()
{

	ui <-  navbarPage("SequenceR",

		## Page 1 - Analysis ====================================
		tabPanel("Analysis", 

			tags$head(tags$script(src="js/script.js")), ## load javascript
			
			tabsetPanel(

				## 1. Tab 1. Data --------------
				tabPanel("Data", 
					
						titlePanel("Alphabet"),
						sidebarLayout(
							sidebarPanel(
								fileInputUI("analysis_file_alphabet")
							),
							mainPanel(
								tableOutputSummaryUI("analysis_file_alphabet_summary", "Alphabet Summary")
							),		
						),
						titlePanel("Substitution Cost Matrix"),
						sidebarLayout(
							sidebarPanel(
								fileInputUIrownames("analysis_file_subcostmat")
							),
							mainPanel(
								tableOutputSummaryUI("analysis_file_subcostmat_summary", "Substitution Costs")
								# plotOutputUI("analysis_file_subcostmat_plot", "Substitution Cost Heatmap")
							),		
						),
						hr(),
						titlePanel("Sequence Data"),
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
				
				## 1. Tab 2. Measures ------------
				tabPanel("Measures", 
						titlePanel("Select Measures to Compute"),
						wellPanel(fluidRow(
						      column(width = 6,
						        # h4("Distance Metric"),
								# checkboxGroupInput("distance", label = h3("Checkbox group"), 
								# 	choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),
								# 	selected = 1),
								selectInputUI('distance_function', 'Distance Function')
						      ),
						      column(width = 6,
						      	h4("Clicked points"),
						        verbatimTextOutput("plot_clickinfo")
						      )
						 )),
						fluidRow(
						    column(width = 4, offset=8,
						        actionButton("analysis_run", "Run Analysis", class='btn btn-primary btn-lg btn-block', icon=icon('play-circle'))
						    )
						)
						# actionButtonUIcheckdata("datacheck", "Data Check"),
						# hr(),
						# sidebarLayout(
						# 	sidebarPanel(
						# 		sliderTextUI("one"),
						# 	),
						# 	mainPanel(
						# 		sliderTextUI("two"),
						# 		hr(),
						# 		actionButtonUI("one"),
						# 		checkboxGroupUI("one")
						# 	)
						# ),
						# hr()
				), 
				
				## 1. Tab 3. Outputs -------------
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

	return(ui)
	### Pages, Other  =======================================
	#navbarMenu("Info",
	#	tabPanel("FAQ",
	#		"coming soon"
	#	),
	#	tabPanel("Version",
	#		"coming soon"
	#	)
	#)	
}
