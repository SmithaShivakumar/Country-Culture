#
# Server logic for "Stadium Explorer" shiny application
#

library(shiny); require(dplyr); require(leaflet); require(jsonlite)
require(geojsonio); require(scales);

## this code only runs once and is available across sessions ---------------
map_data <- geojson_read("data/countries.geojson", what = "sp")
culture_dims <- c("IDV", "IND", "LTO", "MAS", "PDI", "UAI")
culture_data <- rawData[, culture_dims]

# calculate the normalized cultural distance between a given country and home
edist <- function(X, home, country) {
	# calculate distances from home
	distE <- as.matrix(dist(X, method = "euclidean"))[,home]
	
	# calculate the max distance from home (normalization denomimator)
	home_df <- X[row.names(X) == home,]
	anti_home <- ifelse(home_df < 50, 100, 0)
	anti_matrix <- matrix(c(home_df, anti_home), nrow = 2, ncol = 6, byrow = T)
	anti_dist <- dist(anti_matrix)[1]
	
	# normalize
	CD_norm <- 100*(distE/anti_dist)
	
	# find the normalized distance for given country
	CD_norm[names(CD_norm) == country]
}

# ***Recommendation Engine***
# Generate recommendations based on min euclidean distance between
# new countries and each visited country, within the N-dimensional space defined
# by the user-selected dimensions. Returns back the input dataset with a
# recommendation score appended.
# First, check the inputs
inputsCheck <- function(X, visited, home, dims){
	char_dims <- unlist(dims) # coerce dims to vector
	if (length(char_dims) == 1) {
		rdata <- as.data.frame(X[ ,char_dims])
		row.names(rdata) <- X$Country
		names(rdata) <- eval(char_dims)
		incompletes <- X[!complete.cases(X[ ,char_dims]), "Country"]
	}
	else {
		rdata <- X[ ,char_dims] #subset X to only include user-selected dimensions
		incompletes <- row.names(rdata[!complete.cases(rdata), ])
	}
	
	# check that home country is valid
	if (home %in% incompletes) {
		message <- paste(home, "has missing data for one or more of the dimensions",
						 "you selected. Please select different dimensions or try",
						 "a different home country.")
		
		return(list(type = "error", message = message, incompletes = NULL, data = NULL,
					visited = NULL, valid_visited = NULL, invalid_visited = NULL,
					home = NULL, dims = NULL, rdata = NULL))
	}
	
	# check that visited countries are valid. If not, return only visited
	# countries that are valid
	valid_visited <- visited[!visited %in% incompletes]
	invalid_visited <- visited[visited %in% incompletes]
	
	if (length(invalid_visited)>0) {
		# TO DO: list out individual invalid countries
		message <- paste("The following countries that you've visited have",
						 "missing data for one or more of the dimensions",
						 "you selected. You can leave them in, but they will not",
						 "contribute to your travel recommendations.",
						 paste(invalid_visited, collapse = ", "))
		return(list(type = "warning", message = message, incompletes = incompletes, data = X,
					visited = visited, valid_visited = valid_visited, 
					invalid_visited = invalid_visited,
					home = home, dims = char_dims, rdata = rdata))
	}
	
	return(list(type = NULL, message = NULL, incompletes = incompletes, data = X,
				visited = visited, valid_visited = visited, invalid_visited = NULL,
				home = home, dims = char_dims, rdata = rdata))
	
} 

# Second, calculate the recommendation score
recommend <- function(inputs) {
		home <- inputs$home
		visited <- inputs$visited
		valid_visited <- inputs$valid_visited
		X <- inputs$data
		rdata <- inputs$rdata
		incompletes <- inputs$incompletes
		dims <- inputs$dims
		
		missing_and_not_visited <- incompletes[!(incompletes %in% visited)]
		current <- c(home, valid_visited) # vector for all visited countries
		
		
		# remove Countries (rows) that don't have complete data for
		# user-selected dimensions
		if (length(dims) == 1) {
			rdata2 <- as.data.frame(rdata[complete.cases(rdata),])
			labels <- X$Country
			rows <- labels[!labels %in% incompletes]
			row.names(rdata2) <- labels[!labels %in% incompletes]
			names(rdata2) <- eval(dims)
		}
		else rdata2 <- rdata[complete.cases(rdata),]
	
		# weight the 6 culture dims collectively equally to each other dim
		if (sum(culture_dims %in% dims) == 6) {
			for(i in culture_dims) {
				rdata2[, i] <- rdata2[, i]/sqrt(6)
			}
		}
		
		# 2. generate a distance matrix for all countries
		rdist <- as.data.frame(as.matrix(suppressWarnings(dist(rdata2))))
		
		# 3. subset the distance matrix for only the rows corresponding to visited
		# and home countries...
		r_df <- rdist %>% filter(row.names(rdist) %in% current)
		# ...and columns corresponding to all other countries
		r_df2 <- r_df[, -which(names(r_df) %in% current)]
		
		# 4. summarize by column according to min - POTENTIALLY CHANGE THIS
		r_means <- summarize_each(r_df2, funs(min))
		
		# 5. return the dataframe with recommendation score, type, and display color
		# variables appended
		new <- data.frame(Country = names(r_means), score = unlist(r_means),
						  type = "new", stringsAsFactors = F)
		
		h <- data.frame(Country = home, score = NA, type = "home", stringsAsFactors = F)
		
		if (length(visited) == 0) {v <- data.frame(Country = NA, score = NA,
												   type = NA)
		}
		else {v <- data.frame(Country = visited,
							  score = NA,
							  type = "visited", 
							  stringsAsFactors = F)
		}
		
		if (length(missing_and_not_visited ) == 0) {missing <- data.frame(Country = NA, score = NA,
															 type = "missing")
		
		}
		else {
			missing <- data.frame(Country = missing_and_not_visited,
								  score = NA,
								  type = "missing",
								  stringsAsFactors = F)
		}
		
		vars <- rbind(h, new, missing, v)
		result_df <- left_join(X, vars, by = "Country")
	}

## code inside this unnamed function runs each session
shinyServer(function(input, output, session) {
	
	# render the initial map
	output$map <- renderLeaflet({
		leaflet() %>%
			addProviderTiles("CartoDB.Positron") %>%
			setView(lng = -95, lat = 38, zoom = 5)
	})

	# assign country types according to user input
	home <- reactive({input$home})
	visited <- reactive({input$visited})
	# assign dimensions according to user input
	dims <- reactive({
		dims <- input$dimensions
		if ("culture" %in% dims) {
			dims <- c(dims[!dims %in% "culture"], culture_dims)
		}
		if ("economy" %in% dims) {
			dims <- c(dims[!dims %in% "economy"],input$economy_choice)
		}
		dims
	})
	
	
	# generate country recommendations based on user input
	countries <- reactive({
		if (!is.null(dims())) {
			# check inputs
			inputs <- inputsCheck(rawData, visited(), home(), dims())
			message <- inputs$message

			invalid_visited <- inputs$invalid_visited
			message_type <- inputs$type
			
			# throw a warning if visited countries have missing data
			# for given inputs
			if (is.null(message_type)) {
				removeNotification("visited_warning")
			}
			else if (message_type == "warning"){
				showNotification(
					ui = message,
					duration = 5,
					type = message_type,
					id = "visited_warning"
				)
			}
			
			# throw an error modal if the home country has missing data 
			# for given inputs
			else if (message_type == "error") {
				return(showModal(modalDialog(
					title = "Missing Data",
					message,
					easyClose = T,
					footer = NULL
				)))
			}
	
			# calculate recommendation scores
			countries <- recommend(inputs)
		}
	})
	
	
	# apply country formatting according to recommendations
	observe ({
		if(!is.null(countries())) {
		# update country input lists
		home_list <- sort(filter(countries(), type != "visited")$Country)
		updateSelectInput(session, "home", choices = home_list, selected = home())
		# TO DO: figure out how to exclude home country from visited list without
		# 		 re-rendering the visitor list after each selection
		# updateSelectInput(session, "visited", choices = visited_list, selected = visited)

		# set up country polygon colors
		top3 <- head(arrange(countries(), desc(score)), 3)$Country
		rec_score <- countries()$score
		score_alpha <- rec_score/max(rec_score, na.rm = T)
		
		colors <- data.frame(Country = countries()$Country, 
							 fill = ifelse(countries()$type == "missing", "grey",
							 			  ifelse(countries()$type == "home", "blue",
						 			   	      ifelse(countries()$type == "visited", "green",
						 		 	   	   	     "red"))),
							 line = ifelse(countries()$type == "missing", "#eaeaea", 
							 			  ifelse(countries()$Country %in% top3, "orange",
							 			  	   "#919191")),
							 weight = ifelse(countries()$Country %in% top3, 4, 1),
							 alpha = ifelse(countries()$type == "missing", 0,
							 			   ifelse(countries()$type == "home", .6,
							 			   	   ifelse(countries()$type == "visited", .6,
							 			   	   	   score_alpha)))
							 )
		
		# update polygons according to recommendation scores
		leafletProxy("map", data = map_data) %>%
			clearShapes() %>%
			addPolygons(layerId = map_data$subunit, smoothFactor = 0.5,
						# fill the polygon
						fillOpacity = colors$alpha, fillColor = colors$fill,
						# render the lines
						stroke = TRUE, opacity = 1, color = colors$line, weight = colors$weight,
						# highlight if hovered over
						highlightOptions = highlightOptions(color = "white", weight = 2,
															bringToFront = TRUE) 
						)
		
		##TEMPORARY: print the list of countries and recommend scores as a simple table
		# output$recs <- renderTable({
		# 	result <- countries() %>% select(Country, score) %>% arrange(desc(score))
		# 	})
		}
	})
	
	# Show a popup at the given country
	observe({
		showCountryPopup <- function(country, lat, lng) {
			selectedCountry <- countries()[countries()$Country == country,]
			
			# show home/visited status or rec score
			if (country %in% home()) {
					status <- sprintf("Status: home country")
				}
			else if (country %in% visited()) {
				status <- sprintf("Status: visited")
			}
			else status <- sprintf("Recommendation Score: %.0f", round(selectedCountry$score),1)
			
			# show cultural distance from home country, if not home country
			if(!country %in% home()) {
				dist <- edist(culture_data, home(), country)
				distance <- sprintf("Cultural distance from %s: %.0f", home(), dist)
			}
			else distance <- NULL

			content <- as.character(tagList(
				tags$h4(country),
				tags$strong(HTML(status)), tags$br(),
				distance, tags$br(),
				sprintf("Consumption Expenditure per capita: %s", dollar_format()(selectedCountry$CEX)), tags$br(),
				sprintf("GDP per capita: %s", dollar_format()(selectedCountry$GDP)), tags$br(),
				sprintf("Urbanization: %.1f%%", selectedCountry$urbanization), tags$br(),
				sprintf("Income Inequality (Gini coefficent): %.1f%%", selectedCountry$gini), tags$br(),
				sprintf("Population Density: %.0f people per km^2", selectedCountry$pop_density)
			))
			leafletProxy("map") %>% 
				addPopups(lng, lat, content, layerId = "popup",
						  options = popupOptions(closeOnClick = T))
		}
		
		# When map is clicked, show a popup with country info
		leafletProxy("map") %>% clearPopups()
		event <- input$map_shape_click
		if (is.null(event))
			return()
		isolate({
			showCountryPopup(event$id, event$lat, event$lng)
		})
	})
})
