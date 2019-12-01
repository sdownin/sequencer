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
						titlePanel("Load Sequence Alphabet, Substitution Costs, and Data (CSV Files)"),
						sidebarLayout(
							sidebarPanel(
								fileInputUIheader("analysis_file_alphabet", '1. Sequence Alphabet')
							),
							mainPanel(
								tableOutputSummaryUI("analysis_file_alphabet_summary", "Alphabet Summary")
							),		
						),
						hr(),
						sidebarLayout(
							sidebarPanel(
								fileInputUI("analysis_file_subcostmat", '2. Substitution Costs')
							),
							mainPanel(
								tableOutputSummaryUI("analysis_file_subcostmat_summary", "Substitution Cost Matrix")
								# plotOutputUI("analysis_file_subcostmat_plot", "Substitution Cost Heatmap")
							),		
						),
						hr(),
						sidebarLayout(
							sidebarPanel(
								fileInputUI("analysis_file_data", '3. Sequence Data')
							),
							mainPanel(
								tableOutputUI("analysis_file_data_table"),
								tableOutputSummaryUI("analysis_file_data_table_summary", "Sequence Data Summary")
							),		
						),
						hr(),
					
				icon=icon('table')	
				), 
				
				## 1. Tab 2. Measures ------------
				tabPanel("Measures", 
						titlePanel("Select Measures to Compute"),
						wellPanel(fluidRow(
						      column(width = 6,
						      	h3("Sequence Distance Measurement Settings"),
								selectInputUIdistance('analysis_distance_function', 'Distance Function'),
								selectInputUInorm('analysis_distance_norm', 'Normalization'),
								sliderTextUItenths('analysis_indel_cost', "INDEL Cost")
						      ),
						      column(width = 6,
						      	checkboxGroupUImeasures('analysis_measures_group', 'Select Measures to Compute')
						      )
						 )),
						fluidRow(
						    column(width = 6, offset=6,
						        actionButtonUIrun("analysis_run", "Run Analysis", class='btn btn-primary btn-lg btn-block', icon=icon('play-circle'))
						    )
						),
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
				icon=icon('sliders')	
				), 
				
				## 1. Tab 3. Outputs -------------
				tabPanel("Outputs", 
					fluidRow(
				    	column(width = 6,
				    		h3("Sequence Distance Measurement Settings"),
				    		verbatimTextOutput('analysis_run_value')
				    	),
				    	column(width = 6,
				    		h3("Sequence Plots"),
				    		plotOutput('analysis_run_seq_plot')
				    	),
					),
				icon=icon('chart-bar')	
				)
				
			),
				
		icon=icon('sitemap')
		), 
		
			
		## Page 2 - Inference ===================================
		tabPanel("Inference", 
			"Coming soon...",
		icon=icon('magic')	
		),
		
		## Pages Other - Info ===================================
		navbarMenu("Info",
			tabPanel("FAQ", 
				"Where is the project code repository?",
				a("https://github.com/sdownin/sequencer", href="https://github.com/sdownin/sequencer", target="_blank")
			),
			tabPanel("Version", 
				"[0.2.0] - 2019-11-12"
			),
		icon=icon('info-circle')	
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
