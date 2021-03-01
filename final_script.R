setwd("~/Documents/School/GEOG0164 - Cartography/week 6")

# Load SCI flow data file 
data <- read.table(file='2020-12-16_country_country.tsv')

# Clean column names 
colnames(data) <- data[1,]
data <- data[-1,]
gb <- subset(data, user_loc == "GB")
gb[,3] <- as.numeric(gb[,3])
gb<-gb[,-150] # remove gb to gb 

# Convert ISO2 codes to country names 
library(countrycode)
gb$dest_country <- countrycode(gb$fr_loc, "iso2c", "country.name", warn = TRUE, custom_dict = NULL,
                               custom_match = NULL, origin_regex = FALSE)
gb$ori_country <- "Great Britain"
gb$continent <- countrycode(gb$fr_loc, "iso2c", "continent")

# Get country centroid coordinates 
wmap <- getMap(resolution="high")
centroids <- gCentroid(wmap, byid=TRUE)
latlong <- as.data.frame(centroids)
latlong$country <- row.names(latlong)
gb.latlong <- as.data.frame(as.matrix(nrow=2, ncol=1, NA))
gb.latlong$gb.x <- latlong$x[latlong$country=="United Kingdom"]
gb.latlong$gb.y <- latlong$y[latlong$country=="United Kingdom"]
gb.latlong <- gb.latlong[,-1] 

# Join coordinates to original dataframe 
gb <- merge(gb, latlong, by.x="dest_country", by.y="country") 
gb <- merge(gb, gb.latlong)

# Subset only largest flows outside of Europe 
gb <- subset(gb, scaled_sci>10000)
gb <- subset(gb, continent!="Europe")

# Change the scale of SCI such that they can be viewed as flow widths, in pixels
gb$scaled_sci_10000 <- gb$scaled_sci/10000*2

library(mapdeck)

# Set token (need to create your own unique individual token)
set_token("MYTOKEN")  ## set your mapbox token here

# Plot the map here
mapdeck(
  style = mapdeck_style('dark') # Set base map as dark 
  , zoom = 4
  , pitch = 20
) %>%
  add_arc(
    data = gb
    , origin = c("gb.x", "gb.y")
    , destination = c("x", "y")
    , layer_id = 'arcs' 
    # Adding the following two lines will make the colour of 
    # each flow vary from start to end, by a given colour scheme 
    # in the argument 'palette='):
    # , stroke_from = "ori_country"
    # , stroke_to = "dest_country" 
    , stroke_from_opacity = 200
    , stroke_to_opacity = 100
    , palette = "sequential_hcl"
    , stroke_width = "scaled_sci_10000" 
  )

# The map appears as an interactive one in Viewer. I am unaware of any way to save the map except for the 'Export' button in Viewer. 
# Saving as webpage gives you an interactive map, while saving as image gives you a static one. 
