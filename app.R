library(shiny)
library(shinyjs)
library(shinydashboard)

source("modules.R")

customHeader <- dashboardHeader()
customHeader$children[[3]]$children <-  div(style="text-align:center; font-style: italic;", h4("Data Analysis with R/Shiny"))

# User interface
ui <- dashboardPage(

	customHeader,
	
	sidebar = dashboardSidebar(disable = TRUE),
	
	body = dashboardBody(
		useShinyjs(),
		#fluidRow(width = 12, div(style="text-align:center;", "Data Analysis with R/Shiny")),

		fluidRow(
			column(id = "selector_col", width = 2,
				box(id = "data_selector", title = "Select Data",
					width = NULL,
					collapsible = TRUE,
					wellPanel(dataSelect_mod_ui("dataSelect"))
				),

				box(id = "app_info", title = " App Info",
					width = NULL,
					collapsible = TRUE,
					wellPanel(p("Choose from a selection of demo data sets, or upload your own custom data as a .CSV file"),
						p("Provides summary statistics; scatter plots with linear regression; and histograms"),
						p("Save result plots and download saved results as ZIP or PDF"),
						p("Supports analysis of numerical data sets with up to one categorical variable")
					)
				)
			),

			column(
				width = 10,
				hidden(div(id = "errorMsgDiv", textOutput("errorMsg"))),
				hidden(wellPanel(id = "toolsPanel",
					tabsetPanel(
						tabPanel("Summary",
							summaryTools_mod_ui("summaryTools")
						),

						tabPanel("Plots",
							plotTools_mod_ui("plotTools")
						),

						tabPanel("Results",
							results_mod_ui("results")
						),

						id = "toolTab"
					),
				)) # End hidden "toolsPanel" wellPanel
			)
		)
	), # End dashboardBody
	title = "R Analysis"
)

# Server logic
server <- function(input, output, session) {
	dataSelect_res <- callModule(dataSelect_mod_server, "dataSelect")

	rv <- reactiveValues()

	observeEvent(dataSelect_res$toolsBtn(), {
		rv$user_data <- dataSelect_res$df()
		rv$error = NULL

		# Determine numeric variables
		rv$num_var <- colnames(select_if(rv$user_data, is.numeric))

		# Custom datasets: convert character columns to factors
		char_data <- colnames(select_if(rv$user_data, is.character))
		rv$user_data[char_data] <- lapply(rv$user_data[char_data], as.factor)

		# Get information about categorical variables
		factor_data <- select_if(rv$user_data, is.factor)
		rv$cat_var <- colnames(factor_data)
		rv$cat_val <- sapply(factor_data, levels)
		
		if(length(rv$cat_var) != 0) {
			rv$has_categorical = TRUE
			if(length(rv$cat_var) > 1) {
				rv$error <- "Invalid data selection"
			}
		} else {
			rv$has_categorical = FALSE
		}

		if(is.null(rv$error)){
			hideElement("errorMsgDiv")
			showElement("toolsPanel")
		} else {
			hideElement("toolsPanel")
			output$errorMsg <- renderText({
				rv$error
			})
			showElement("errorMsgDiv")
		}

		updateTabsetPanel(session, "toolTab", selected = "Summary")
	})

	callModule(summaryTools_mod_server, "summaryTools", data_rv = rv)
	saved_result <- callModule(plotTools_mod_server, "plotTools", data_rv = rv)
	callModule(results_mod_server, "results", new_result = saved_result)
}

# Run app
shinyApp(ui = ui, server = server)