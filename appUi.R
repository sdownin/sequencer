####################################################################################
##                                  VIEW 
####################################################################################
.ui <- function()
{

	ui <-  navbarPage("SequenceR",

		## Page 1 - Analysis ====================================
		tabPanel("Analysis", 

			tags$head(tags$script(src="nav.js")), ## load javascript
			
			tabsetPanel(

				## 1. Tab 1. Data --------------
				tabPanel("Data", 
						titlePanel("Load Sequence Alphabet, Substitution Costs, and Data (CSV Files)"),
						sidebarLayout(
							sidebarPanel(
								fileInputUIAlphabet("analysis_file_alphabet", '1. Sequence Alphabet')
							),
							mainPanel(
								tableOutputSummaryAlphabet("analysis_file_alphabet_summary", "Alphabet Summary") 
							),		
						),
						# hr(),
						sidebarLayout(
							sidebarPanel(
								fileInputUISubcostmat("analysis_file_subcostmat", '2. Substitution Costs', 'subcostmat_header', 'subcostmat_fileEncoding')
							),
							mainPanel(
								tableOutputSummaryUI("analysis_file_subcostmat_summary", "Substitution Cost Matrix")
								# plotOutputUI("analysis_file_subcostmat_plot", "Substitution Cost Heatmap")
							),		
						),
						# hr(),
						sidebarLayout(
							sidebarPanel(
								fileInputUI("analysis_file_data", '3. Sequence Data', 'seqdata_header', 'seqdata_fileEncoding')
							),
							mainPanel(
								tableOutputUI("analysis_file_data_table"),
								tableOutputSummaryUIwithSpinner("analysis_file_data_table_summary", "Sequence Data Summary") 
							),		
						),
						# hr(),
						fluidRow(
						    column(width = 6, offset=6,
						        actionButtonUIrun("analysis_measures_goto", "Select Measures", class='btn btn-primary btn-lg btn-block', icon=icon('sliders'))
						    )
						),
						hr(),
						# div(id='analysis_num_plots', style="display:none;", value=0), ## initial value of num_plots
				icon=icon('table')	
				), 
				
				## 1. Tab 2. Measures ------------
				tabPanel("Measures", 
						titlePanel("Select Measures"),
						wellPanel(fluidRow(
						      column(width = 6,
						      	checkboxGroupUImeasures('analysis_measures_group', 'Select Measures to Compute')
						      ),
						      column(width = 6,
						      	conditionalPanel(condition = "input.analysis_measures_group.includes('distance')",
							      	h3("Sequence Distance Measurement Settings"),
							      	tagList(
							      		icon('question-circle'),
										a("Sequence distance function details", href="https://rdrr.io/cran/TraMineR/man/seqdist.html", target="_blank"),
							      		br(),br()
							      	),
									selectInputUIdistance('analysis_distance_function', 'Distance Function')#,
									# selectInputUInorm('analysis_distance_norm', 'Normalization') #,
									# sliderTextUItenths('analysis_indel_cost', "INDEL Cost")
						      	)
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
				
				## 1. Tab 3. Results -------------
				tabPanel("Results", 
					fluidRow(
				    	column(width = 12,
				    		h3("Sequence Analysis Results Summary"),
				    		verbatimTextOutput('analysis_run_value') %>% withSpinner(color="#0dc5c1")
				    	),
					),
					fluidRow(column(width=12,
						''
					)),
					# hr(),
					# fluidRow(
					#     column(width = 12,
					#     	h3("Save Results"),
					#         downloadButton('analysis_output_download', class='btn btn-default btn-lg', label = "Download") # class = NULL, ...
					#     )
					# ),
					hr(),
					# h3("Save Results"),
					fluidRow(
						column(width = 6,
						    downloadButton('analysis_output_download', class='btn btn-default btn-lg btn-block', label = "Save Results") # class = NULL, ...
						),
					    column(width = 6, #offset=6,
					        actionButtonUIrun("analysis_output_plots_button", "Visualize Results", class='btn btn-primary btn-lg btn-block', icon=icon('chart-pie'))
					    )
					),
					hr(),
				icon=icon('poll-h')	
				),


				## 1. Tab 4. Plots -------------
				tabPanel("Plots", 
					fluidRow(
						column(width = 12,
							h3("Sequence Analysis Plots"),
							h5("(Resize the figure by click-dragging the plot region's bottom right corner.)"),
							jqui_resizable(
								plotOutput('analysis_output_plots', height=550, width='95%') 
							) %>% withSpinner(color="#0dc5c1") 
    			    		# plotOutput('analysis_output_plot_distance', height=270, width='100%'),
    			    		# plotOutput('analysis_output_plot_predictability', height=270, width='100%'),
    			    		# plotOutput('analysis_output_plot_singles', height=270, width='100%')
						)
					),
					hr(),
					# h3("Save Plot"),
					fluidRow(column(width=6,
						downloadButton("analysis_save_plots_button",  "Save Plot", class='btn btn-default btn-lg btn-block', icon=icon('save'))
					)),
					br(),
					br(),
					br(),
					br(),
				icon=icon('chart-pie')	
				)
				
			),
				
		icon=icon('sitemap')
		), 
		
			
		# ## Page 2 - Inference ===================================
		# tabPanel("Inference", 
		# 	"Coming soon...",
		# icon=icon('magic')	
		# ),
		
		## Pages Other - Info ===================================
		navbarMenu("Info",
			tabPanel("FAQ", 
				"Where is the project code repository?",
				a("https://github.com/sdownin/sequencer", href="https://github.com/sdownin/sequencer", target="_blank")
			),
			# tabPanel("Version", 
			# 	"[0.4.0] - 2020-06-20"
			# ),
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
