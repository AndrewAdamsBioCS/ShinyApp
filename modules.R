library(shinyjs)
library(dplyr)
library(shinydashboard)
#library(shinydashboardPlus)
library(ggplot2)
library(zip)

### Data selector module ---
dataSelect_mod_ui <- function(id) {
	
	ns <- NS(id)
	useShinyjs()

	tagList(
		tabsetPanel(
			tabPanel("Demo", style = "padding-top: 10px;",
				# Input: Select a demo dataset
				selectInput(ns("demo_file"), "Choose Demo Dataset",
					choices = c("Demo datasets..." = "none", "mtcars", "iris", "ToothGrowth"),
					selected = "None")
			), # End "Demo" tabPanel

			tabPanel("Custom", style = "padding-top: 10px;",
				# User selects their own data file for upload
				fileInput(ns("file1"), "Choose CSV File",
					multiple = FALSE,
					accept = c("text/csv",
								"text/comma-separated-values,text/plain",
								".csv"),
					width = "100%"),

				# User selects specifications of their data file
				hidden(div(id = ns("custom_file_specs"),
					checkboxInput(ns("header"), "Header", TRUE),

					radioButtons(ns("sep"), "Separator:",
						choices = c(Comma = ",",
									Semicolon = ";",
									Tab = "\t"),
						selected = ",",
						inline = TRUE),

					radioButtons(ns("quote"), "Text qualifier (quotes):",
						choices = c(None = "",
									"Double" = '"',
									"Single" = "'"),
						selected = '"',
						inline = TRUE)
					) # End "custom_file_specs" div
				)
			) # End "Custom" tabPanel
		),

		hidden(div(id = ns("previewDiv"),
			style = "margin-top: 10px; padding-top: 20px; border-top: 1px solid;",
			#p(style = "font-style: italic;", "Data Preview"),
			div(style="font-style: italic; text-align: center;", "Data Preview"),

			# Input: Select number of rows to display
			radioButtons(ns("disp"), "",
				choices = c(Head = "head",
							All = "all"),
				selected = "head", inline = TRUE),

			div(style = "height: 25vh; margin-bottom: 10px; overflow-x: scroll; overflow-y: scroll",
				tableOutput(ns("fileDataDisplay"))
			), hr(),

			actionButton(ns("launchBtn"), "Launch Analysis Tools")
		))
	)
}

### File selection module server ---
dataSelect_mod_server <- function(input, output, session) {
	
	rv <- reactiveValues(file_select_df=NULL)

	# Update user file dataframe specifications on change of user input
	usr_file_change <- reactive({
		list(input$header, input$sep, input$quote)
	})

	# Observe selection of demo file
	observeEvent(input$demo_file, {
		if(input$demo_file != "none") {
			demo_data <- get(input$demo_file)
			demo_file_df <- reactive({
				data.frame(demo_data)
			})

			hideElement("custom_file_specs")
			updatePanel(demo_file_df)
		} else {
			#hideElement("previewDiv")
		}
	})

	# Observe upload of custom user file
	observeEvent(input$file1, {
		# Create user file dataframe based on user inputs
		usr_file_df <- eventReactive(usr_file_change(), {
			read.csv(input$file1$datapath,
				header = input$header,
				sep = input$sep,
				quote = input$quote)
		})

		updateSelectInput(session, "demo_file",
			selected = "none")
		showElement("custom_file_specs")
		updatePanel(usr_file_df)
	})

	# Update file selector panel based on user selections
	updatePanel <- function(selected_data) {
		output$fileDataDisplay <- renderTable({
			if(input$disp == "head") {
				return(head(selected_data()))
			} else {
				return(selected_data())
			}
		}, bordered = TRUE, align = "c") # End renderTable

		showElement("previewDiv")
		observeEvent(selected_data(), {
			rv$file_select_df <- selected_data()
		})
		
	}


	return(
		list(
			df = reactive({rv$file_select_df}),
			toolsBtn = reactive({input$launchBtn})
		)
	)
}

### summaryTools module UI ---
summaryTools_mod_ui <- function(id) {
	ns <- NS(id)
	useShinyjs()

	tagList(
		fluidRow(style = "padding-top: 10px",
			column(width = 2,
				hidden(div(id = ns("subset_control"),
					p("Subset by categorical variable:"),
					checkboxGroupInput(ns("subset_vars"),
						"",	choices = "")
					)
				)
			),

			column(width= 5,
				h4(textOutput(ns("summary_header"))),
				tableOutput(ns("numeric_summary")),
				tableOutput(ns("cat_summary"))
			)
		)
	)
}

### summaryTools module server ---
summaryTools_mod_server <- function(input, output, session, data_rv) {

	# Update summary imputs with dataset variables
	observe({
		resetDisplay()

		updateCheckboxGroupInput(session, "subset_vars",
			label = data_rv$cat_var[1],
			choices = data_rv$cat_val,
			selected = data_rv$cat_val)
	})

	# Summary table header
	output$summary_header <- renderText({
		summary_label <- "Summary statistics"

		if(data_rv$has_categorical) {
			summary_label <- paste(summary_label, " for ", toString(input$subset_vars))
		}
	})

	# Summary table data
	output$numeric_summary <- renderTable({
		summary_levels <- input$subset_vars
		
		if(data_rv$has_categorical){
			showElement("subset_control")
			disp_data <- subset(data_rv$user_data,
				data_rv$user_data[[data_rv$cat_var]] %in% summary_levels)

			render_data <- select_if(disp_data, is.numeric)

			showElement("cat_summary")
			output$cat_summary <- renderTable({
				cat_data <- select_if(disp_data, is.factor)
				sapply(cat_data, summary)
			}, rownames=TRUE)

		} else {
			render_data <- select_if(data_rv$user_data, is.numeric)
		}

		sapply(render_data, summary)
	}, bordered=TRUE, rownames=TRUE)

	resetDisplay <- function() {
		hideElement("subset_control")
		hideElement("cat_summary")
	}
}

### plotTools module UI ---
plotTools_mod_ui <- function(id) {

	ns <- NS(id)
	useShinyjs()

	tagList(div(style = "padding-top: 10px",
		tabsetPanel(
			tabPanel("Scatter",
				fluidRow(style = "padding-top: 10px",
					column(width = 2,
						hidden(uiOutput(ns("scatter_cat_ui"))),
						uiOutput(ns("scatter_var_ui")),
						checkboxInput(ns("lin_reg"), "Linear regression", FALSE),
						
						div(id = ns("add_scat_result_div"), style = "border-top: 1px solid; padding-top: 3px",
							actionButton(ns("add_scat_result"), "Save to Results"),
							textOutput(ns("scat_result_added_msg"))
						)
					),

					column(width = 10,
						hidden(textOutput(ns("data_error_msg"))),
						plotOutput(ns("scatter_plot")),
						div(id = ns("regression_summary"), style = "border-top: 1px solid; padding-top: 3px",
							verbatimTextOutput(ns("reg_summ"))
						)
					)
				)
			), # End "Scatter" plot tabPanel

			tabPanel("Histogram",
				fluidRow(style = "padding-top: 10px",
					column(width = 2,
						uiOutput(ns("hist_cat_ui")),
						uiOutput(ns("hist_var_ui")),
						
						div(id = ns("add_hist_result_div"), style = "border-top: 1px solid; padding-top: 3px",
							actionButton(ns("add_hist_result"), "Add to results"),
							textOutput(ns("hist_result_added_msg"))
						)
					),

					column(width = 10,
						plotOutput(ns("hist_plot"))						
					)
				)
			), # End "Histogram" plot tabPanel

			id = ns("plots")
		), # End plot type selection tabsetPanel
	)) # End div/tagList
}

### plotTools module server
plotTools_mod_server <- function(input, output, session, data_rv) {
	
	ns <- session$ns
	current_plots <- reactiveValues()
	saved_plot <- reactiveVal()
	data_change <- reactive({data_rv$user_data})

	observeEvent(data_change(), {
		reset_display()
	})

	# Scatter plot inputs
	output$scatter_cat_ui <- renderUI({
		
		if(data_rv$has_categorical) {
			cat_choices <- data_rv$cat_val
		} else {
			cat_choices <- c()
		}

		tagList(
			p("Subset by categorical variable:"),

			checkboxGroupInput(ns("scatter_cat"),
				label = data_rv$cat_var[1],
				choices = cat_choices,
				selected = data_rv$cat_val)
		)
	})

	output$scatter_var_ui <- renderUI({
		tagList(
			selectInput(ns("X"),
				"Select X variable",
				choices = data_rv$num_var,
				selected = data_rv$num_var[1]),

			selectInput(ns("Y"),
				"Select Y variable",
				choices = data_rv$num_var,
				selected = data_rv$num_var[2])
		)
	})

	# Histogram inputs
	output$hist_cat_ui <- renderUI({
		if(data_rv$has_categorical) {
			tagList(
				p("Subset by categorical variable:"),

				checkboxGroupInput(ns("hist_cat"),
					label = data_rv$cat_var[1],
					choices = data_rv$cat_val,
					selected = data_rv$cat_val)
			)
		}
	})

	output$hist_var_ui <- renderUI ({
		tagList(
			selectInput(ns("hist_var"),
				"Select distribution variable",
				choices = data_rv$num_var,
				selected = data_rv$num_var[1]),

			sliderInput(ns("bins"),
					  label = "Number of bins:",
					  min = 1,
					  max =  ceiling(log2(nrow(data_rv$user_data)) + 1) * 2,
					  value = ceiling(log2(nrow(data_rv$user_data)) + 1))
		)
	})
	
	# Scatter plot
	output$scatter_plot <- renderPlot({
		# Get plot details and status of controls based on categorical variables
		if (data_rv$has_categorical) {
			plot_title <- toString(input$scatter_cat)
			if (length(input$scatter_cat) < 1) {
				disable("add_scat_result")
				disable("lin_reg")
				if (input$lin_reg) {
					hideElement("regression_summary")
				}
			} else {
				enable("add_scat_result")
				enable("lin_reg")
			}
		} else {
			plot_title <- ""
		}

		# Check for X and Y variable being set to same
		req(input$X, input$Y)
		if (input$X == input$Y) {
			disable("add_scat_result")
			disable("lin_reg")
			hideElement("regression_summary")
			showElement("data_error_msg")
		} else {
			hideElement("data_error_msg")
			if (!data_rv$has_categorical) {
				enable("add_scat_result")
				enable("lin_reg")
			}
		}
	
		# Get subset of data based on user selected of categorical variables
		plot_data <- cat_subset("scatter")

		# Construct plot
		req(input$X, input$Y, all(c(input$X, input$Y) %in% data_rv$num_var), input$X != input$Y)

		scatgg <- ggplot(data = plot_data, aes(x = .data[[input$X]], y = .data[[input$Y]])) + geom_point()
		if (length(input$scatter_cat) > 1) {
			scatgg <- scatgg + geom_point(aes(colour = .data[[data_rv$cat_var]]))
		}
		scatgg <- scatgg + labs(title = plot_title) + theme(
			plot.title = element_text(color="blue", size=14, face="bold.italic")
		)

		# Add linear regression details
		if(input$lin_reg) {
			lm_formula <- paste0(input$Y, "~", input$X)
			fit <- lm(lm_formula, plot_data)

			scatgg <- scatgg + stat_smooth(method = "lm", geom = "smooth")
			scatgg <- scatgg + labs(subtitle = paste0("Adj R2 = ", signif(summary(fit)$adj.r.squared, 5),
                     "; Intercept = ", signif(fit$coef[[1]], 5),
                     "; Slope = ", signif(fit$coef[[2]], 5),
                     "; P = ", signif(summary(fit)$coef[2,4], 5)))

			showElement("regression_summary")
			output$reg_summ <- renderPrint({
				# Format ouput of regression summary
				to_print <- capture.output(summary(fit))				
				cat(to_print[-(1:4)], sep="\n")
			})
		} else {
			hideElement("regression_summary")
		}
		
		current_plots$scatter <- scatgg
		return(scatgg)
	})

	# Histogram plot
	observeEvent(input$hist_cat, {
		obs <- nrow(cat_subset("hist"))
		opt_bins <- ceiling(log2(obs) + 1)
		updateSliderInput(session, "bins", max = opt_bins*2, value = opt_bins)
	})

	output$hist_plot <- renderPlot({
		if(data_rv$has_categorical) {
			plot_title <- toString(input$hist_cat)
			if (length(input$hist_cat) < 1) {
				disable("add_hist_result")
			} else {
				enable("add_hist_result")
			}
		} else {
			plot_title <- ""
		}

		# Subset data based on user selection of categorical variable
		plot_data <- cat_subset("hist")

		# Construct plot
		req(input$hist_var, input$hist_var %in% data_rv$num_var)
		
		histgg <- ggplot(data = plot_data, aes(x = .data[[input$hist_var]]))
		histgg <- histgg + geom_histogram(bins = input$bins, color = "white", fill = "blue")
		histgg <- histgg + labs(title = plot_title) + theme(
			plot.title = element_text(color="blue", size=14, face="bold.italic")
		)

		current_plots$hist <- histgg
		return(histgg)
	})

	# Save plots on click of button
	timed_msg <- reactiveVal()
	observeEvent(input$add_scat_result, {
		saved_plot(NULL)
		saved_plot(current_plots$scatter)
		
		timed_msg("Plot saved...")
		output$scat_result_added_msg <- renderText({
			timed_msg()
		})
		delay(ms = 2000, timed_msg(NULL))
	})

	observeEvent(input$add_hist_result, {
		saved_plot(NULL)
		saved_plot(current_plots$hist)

		timed_msg("Plot saved...")
		output$hist_result_added_msg <- renderText({
			timed_msg()
		})
		delay(ms = 2000, timed_msg(NULL))
	})

	output$data_error_msg <- renderText({
		"Invalid data selection"
	})

	# HELPER FUNCTIONS
	reset_display <- function() {

		enable("add_scat_result")
		enable("add_hist_result")
		
		enable("lin_reg")
		updateCheckboxInput(session, "lin_reg", value = FALSE)
		hideElement("regression_summary")
		hideElement("data_error")

		if (data_rv$has_categorical) {
			showElement("scatter_cat_ui")
		} else {
			hideElement("scatter_cat_ui")
		}
	}
	
	cat_subset <- function(plot_type) {
		if(data_rv$has_categorical) {
			input_str <- paste0(plot_type, "_cat")
			input_cat <- input[[input_str]]

			req(input_cat %in% data_rv$cat_val)
			return(
				subset(data_rv$user_data,
					data_rv$user_data[[data_rv$cat_var]] %in% input_cat)
			)
		} else {
			return(data_rv$user_data)
		}
	} # End cat_subset

	return(saved_plot)

} # End plotTools_mod_server


results_mod_ui <- function(id) {
	ns <- NS(id)
	useShinyjs()

	tagList(
		fluidRow(style = "padding-top: 10px",
			column(width = 2,
				hidden(div(id = ns("dl-btns"), style = "padding: 10px",
					downloadButton(ns("dl_zip"), "Download ZIP"),
					br(), br(),
					downloadButton(ns("dl_pdf"), "Download PDF")
					)
				)	
			),

			column(width = 10,
				div(id = ns("results_disp"),
					p(id = ns("no_results"), "No saved results")
				)				
			)
		)
	)
} # End results_mod_ui

results_mod_server <- function(input, output, session, new_result) {

	ns <- session$ns
	curr_id <- 0
	results_list <- list()
	remove_btn_obs <- list()
	saved_plots <- list()

	observeEvent(new_result(), {
		hideElement("no_results")
		showElement("dl-btns")
		curr_id <<- curr_id + 1
		result_id <- paste0("result", curr_id)

		results_list[[result_id]] <<- new_result()
		btn_id <- paste0(curr_id)

		if (is.null(remove_btn_obs[[btn_id]])) {
			remove_btn_obs[[btn_id]] <<- observeEvent(input[[btn_id]], {
				removeUI(
					selector = paste0("#", "result", btn_id, "_div")
				)

				results_list <<- results_list[names(results_list) != paste0("result", btn_id)]
				if (length(results_list) == 0) {
					showElement("no_results")
					hideElement("dl-btns")
				}
			})
		}

		insertUI(
			selector = paste0("#", ns("results_disp")),
			ui = tags$div(id = paste0(result_id, "_div"), style = "padding: 20px",
				plotOutput(ns(result_id)),
				actionButton(ns(btn_id), "Remove result")				
			)
		)

		output[[result_id]] <- renderPlot({
			results_list[[result_id]]
		})

	}, ignoreInit = TRUE) # End observeEvent on new_result()

	output$dl_zip <- downloadHandler(
		filename = function() {
			"results.zip"
		},
		content = function(file) {
			# Set temp working dir
			owd <- setwd(tempdir())
			on.exit(setwd(owd))
		
			filenames <- c()
			for (i in 1:length(results_list)) {
				this_filename <- paste0("result", i, ".png")
				filenames <- append(filenames, this_filename)
				ggsave(this_filename, results_list[[i]], device = "png")
			}

			zip(file, filenames)
		},
		contentType = "application/zip"
	)

	output$dl_pdf <- downloadHandler(
		filename = function() {
			"results.pdf"
		},
		content = function(file) {
			pdf(file)
			for (i in 1:length(results_list)) {
				print(results_list[[i]])
			}
			dev.off()
		}
	)

} # End results_mod_server