#
# User interface definition for "Baseball Stadium Explorer" shiny application
#

library(shiny); require(leaflet); library(shinythemes)

stadium_list <- sort(rawData$Stadium.Name)

dimensions <- c("Food Safety" = "Food.Safety",
				 "Altitude" = "Altitude",
				 "Capacity" = "Capacity",
				 "Park Factor" = "Park.Factor",
				 "Stadium Age" = "Seasons")

# Define UI
shinyUI(navbarPage(
	
	title = "Baseball Stadium Explorer",
	position = "fixed-top",
	id = "nav",
	tabPanel(
		## CSS--------
		tags$head(
			tags$style(HTML("
			body {
				padding-top: 55px;
			}
			div.outer {
				position: fixed;
				top: 41px;
				left: 0;
				right: 0;
				bottom: 0;
				overflow: hidden;
				padding: 0
			}
			#controls {
				background-color: white;
				padding: 5px 15px 15px 15px;
				cursor: move;
				opacity: 0.70;
				zoom: 0.9;
				transition: opacity 1500ms 400ms;
			}
			#controls:hover {
				opacity: 0.95;
				transition-delay: 0;
			}
			.leaflet-popup-content-wrapper {
				background-color: white;
				opacity: 0.85;
			}
		"))
		),
		
		## continue shiny code-----
		title = "Recommendations",
		fluidRow(
			div(
				class = "outer",
				# leaflet map goes here
				leafletOutput(outputId = "map", width = "100%", height = "100%"),
				# Input controls
				absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
							  draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
							  width = 300, height = "90%",

							  # 1. single dropdown list to select home stadium (keep static)
							  h4("Stadiums"),
							  h5("Where does your", span("favorite team", style = "color:blue"), "play?"),
							  selectInput("home", label = NULL,
							  			choices = stadium_list,
							  			selected = "United States of America",
							  			selectize = T),
							  
							  # 2. multiple dropdown list to select stadiums visited
							  h5("Which other stadiums have you", span("been to", style = "color:#25a31a"), "?"),
							  selectInput("visited", label = NULL,
							  			choices = stadium_list, 
							  			multiple = T, selectize = T),
							  br(),
							  
							  h4("Dimensions"),
							  # 3. checkboxes for dimensions
							  h5("What dimensions do you care about?"),
							  checkboxGroupInput("dimensions",
							  				   label = NULL,
							  				   choices = dimensions,
							  				   selected = NULL
							  )
							  
							  # # 4. condition panel to choose economy dimension
							  # ,conditionalPanel(
							  # 	condition = "input.dimensions.includes('economy')",
							  # 	h5("Pick an economic dimension"),
							  # 	radioButtons("economy_choice", label = NULL,
							  # 				choices = econ_dimensions,
							  # 				selected = econ_dimensions[1])
							  # )
				)
			)
		)

	)
	
))
