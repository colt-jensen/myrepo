#building geographic map
library(maps)
library(igraph)
library(ggrepel)
library(ggsflabel)


df<-data.frame(from = c("at", "be", "ch", "cz"), to= c("be", "ch", "cz", "at"), 
               weight=c(0.02,0.145,0.257,.109))
meta <- data.frame("name"=c("at", "be", "ch", "cz"), 
                   "lon"=c(14.55,4.46,8.227,14.4738),  
                   "lat"=c(47.51,50.5,46.818,50.0755))

g <- graph.data.frame(df, directed=F, vertices=meta)
E(g)$color <- "brown"
E(g)$width <- E(g)$weight*10
lo <- as.matrix(meta[,2:3])
map("world",  xlim = c(-8, 30),
    ylim = c(35, 55), asp=1)
plot(g, layout=lo, add = TRUE, rescale = FALSE)


#starting over
#load edge list ------
city_edgelist = read.csv("Data/Book1.csv")

View(city_edgelist)

library(igraph)

#create matrix
city_edgelist <- as.matrix(city_edgelist)

#make edgelist in R
city_edgelist <- graph.edgelist(city_edgelist, directed=TRUE)

#explore
city_edgelist

#nodes
V(city_edgelist)$name

#initial plot
plot(city_edgelist)

#vertex = node size
plot(city_edgelist, vertex.size = 25)

#node color, name color and size
plot(city_edgelist, vertex.size = 10, vertex.color = "tomato", vertex.frame.color = NA, vertex.label.cex = .7, vertex.label.color = "black")

#w/o labels
plot(city_edgelist, vertex.size = 10, vertex.color = "tomato", vertex.frame.color = NA, vertex.label = NA)

#curved edges
plot(city_edgelist, vertex.size = 10, vertex.color = "tomato", vertex.frame.color = NA, vertex.label.cex = .7,  vertex.label = NA, edge.curved = .1, edge.arrow.size = .3, edge.width = .7)

# first we run the layout function on our graph
kamadaLayout <- layout.kamada.kawai(city_edgelist)

# and then we change the default layout setting to equal the layout we generated above
plot(city_edgelist, layout = kamadaLayout, vertex.size = 10, vertex.color = "tomato", vertex.frame.color = NA, vertex.label.cex = .7,  vertex.label = NA, edge.curved = .1, edge.arrow.size = .3, edge.width = .7)

# first we run the layout function on our graph
fruchtermanLayout <- layout.fruchterman.reingold(city_edgelist)

# and then we change the default layout setting to equal the layout we generated above
plot(city_edgelist, layout = fruchtermanLayout, vertex.size = 10, vertex.color = "tomato", vertex.frame.color = NA, vertex.label.cex = .7, vertex.label = NA, edge.curved = .1, edge.arrow.size = .3, edge.width = .7)

#attributes in networks -------
# Load in the edge list again
city_edgelist <- read.csv("data/Book1.csv", stringsAsFactors = F)

# Load in the attributes again
attributes <- read.csv("data/attributes.csv", stringsAsFactors = F)

# Put them both in the network.
city_network <- graph_from_data_frame(city_edgelist, directed = T, vertices = attributes)

city_network

nrow(rawedges) # how many edges are there?

#it worked!!!

#check attributes on network nodes
V(city_network)$per_white

#examples of network graphing based on attributes
#V(moneyNetwork)$color <- ifelse(V(moneyNetwork)$Gender == "Male", "dodgerblue3","seagreen")
#plot(moneyNetwork, vertex.size = 10, vertex.frame.color = "black", vertex.label.cex = .7, vertex.label = NA, edge.curved = .1, edge.arrow.size = .3)
#V(moneyNetwork)$size = V(moneyNetwork)$Age/5
#https://bookdown.org/markhoff/social_network_analysis/network-visualization-and-aesthetics.html#the-basics
#plot(moneyNetwork, vertex.label.cex = .7, vertex.label = NA, edge.curved = .1, vertex.frame.color = "black", edge.arrow.size = .3, edge.width = .7, edge.color = "grey30")
#V(moneyNetwork)$size = V(moneyNetwork)$Age/5
#plot(moneyNetwork, vertex.label.cex = .7, vertex.label = NA, edge.curved = .1, vertex.frame.color = "black", edge.arrow.size = .3, edge.width = .7, edge.color = "grey30")

#geographic plotting
library(tidyverse)  # For ggplot, dplyr, and friends
library(sf)         # For GIS magic

catawba_cities <- read_sf("data/CATCO_Cities/CATCO_Cities.shp")

catawba_county <-read_sf("data/CATCO_CntyBndy/CATCO_CntyBndy.shp")

catawba_county

catawba <- data.frame(catawba_county)

ggplot() + 
  geom_sf(data = catawba_county, color = "#EC8E55", size = 3) + 
  geom_sf(data = catawba_cities, fill = "#A5D46A", color = "white") + 
  theme_void()

V(city_network)$name

cat_cities <- tribble(
  ~city, ~lat, ~long,
  "Newton", 35.66965142781459, -81.2171693643138,
  "Hickory", 35.73489034142729, -81.34213884022306,
  "Conover", 35.70722608224313, -81.2203450996219,
  "Madien", 35.58201828025933, -81.21745426018545, 
  "Claremont", 35.71479753664958, -81.14557038112304, 
  "Catawba", 35.707990733659656, -81.0745247872627
)
cat_cities

#Google Maps uses EPSG:4326, or the GPS system, so we specify that
cat_cities_geometry <- cat_cities %>% 
  st_as_sf(coords = c("long", "lat"), crs = st_crs("EPSG:4326"))
cat_cities_geometry

catawba_county_geo <- catawba_county %>%
  st_as_sf(coords = c("long", "lat"), crs= st_crs("EPSG:4326"))

head(cat_cities)
st_geometry_type(catawba_county)

##loading in a shapefile in a way that works better with ggplot -----
library(rgdal)

catawba_county <-readOGR("data/CATCO_CntyBndy/CATCO_CntyBndy.shp")

cat_shapefile_df <- fortify(catawba_county)

view(cat_shapefile_df)

cat_shapefile_df$long <- cat_shapefile_df$long * -1 
cat_shapefile_df$lat <- cat_shapefile_df$lat/10

catawba_cities <- readOGR("data/CATCO_Cities/CATCO_Cities.shp")

catcit_shapefile_df <- fortify(catawba_cities)

view(catcit_shapefile_df)


ggplot() + 
  geom_path(data=cat_shapefile_df, 
            aes(x=long, y=lat, group=group),
            color='gray', fill='white', size =.2) +
  geom_path(data = catcit_shapefile_df, 
            aes(x=long, y=lat, group=group),
            color='gray', fill='white', size =.2)

#map county and cities
map <-ggplot() +
  geom_sf(data = catawba_county, color = "#EC8E55")  +
  geom_sf(data = cat_cities_geometry, size = 3) +
  theme_void()

#add labels - from: https://datavizs21.classes.andrewheiss.com/example/12-example/
ggplot() +
  geom_sf(data = catawba_county, color = "#EC8E55")  +
  geom_sf(data = cat_cities_geometry, size = 3) +
  geom_sf_label(data = cat_cities_geometry, aes(label = city),
                nudge_y = 0.2)
  theme_void()

plot(g, layout=lo, add = TRUE, rescale = FALSE)

#combining the two
# notes:
library(dplyr)   # tidyverse: dataframe manipulation
library(tidyr)   # tidyverse: data tidying
library(stringr) # tidyverse: improves base R's string manipulation
library(igraph)  # graph tools
library(purrr)   # improves base R's functional programming
library(ggmap)   # spatial tools to interact with ggplot2
library(ggplot2) # tidyverse: actually more basic than base R plotting


city_edgelist <- data.frame(city_edgelist)
city_edgelist

city_edgelist$lat <- cat_cities$lat
city_edgelist$long <- cat_cities$long * -1 

city_network1

city_edgelist

g <- graph_from_data_frame(city_edgelist, directed = TRUE)
catawba_county
ggplot() + 
  geom_polygon(data = catawba_county, 
               aes=shape_STAr, Shape_STLe, group = group, fill=region)

library(ggnetwork)
ggplot(ggnetwork(city_network1))

# https://cran.r-project.org/web/packages/ggnetwork/vignettes/ggnetwork.html
ggplot(city_network1, aes(x=from_long, y = from_lat, xend = to_long, yend = to_lat)) +
  geom_edges(aes(linetype = "x"), color="grey50") +
  geom_nodes(aes(color="black", size = 8)) + 
  theme_blank()



 

basegg +
  coord_cartesian()
  
cat_cities

basegg <- ggplot()+
  geom_path(data=cat_shapefile_df, 
               aes(x=long, y=lat, group=group),
               color='gray', fill='white', size =.2) +
  geom_point(data = cat_cities,
             aes(long, lat), color = "red") + 
  geom_segment(data = city_network1,
               aes(x = from_long, xend = to_long,
                   y = from_lat, yend = to_lat),
               arrow = arrow(length = unit(0.1, "inches"), # optional arrows
               type = "open"),
               size = 0.25,
               alpha = 0.5) +
  theme_bw() +
  labs(title = "Geospatial Network",
       subtitle = "Plot Example",
       caption = "Colt Jensen")+
  theme_bw() +
  theme(axis.line = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank()) +
  labs(title = "Geospatial Network",
       subtitle = "Plot Example",
       caption = "Colt Jensen")
  
#different approach
df<-data.frame(from = c("at", "be", "ch", "cz"), to= c("be", "ch", "cz", "at"), 
               weight=c(0.02,0.145,0.257,.109))


meta <- data.frame("name"=c("at", "be", "ch", "cz"), 
                   "lon"=c(14.55,4.46,8.227,14.4738),  
                   "lat"=c(47.51,50.5,46.818,50.0755))

g <- graph.data.frame(df, directed=F, vertices=meta) #df - edgelist # meta -coords
E(g)$color <- "brown"
E(g)$width <- E(g)$weight*10
lo <- as.matrix(meta[,2:3])
map("world",  xlim = c(-8, 30),
    ylim = c(35, 55), asp=1)
plot(g, layout=lo, add = TRUE, rescale = FALSE)

#trying again
city_edgelist = read.csv("Data/Book1.csv")
df1 <- data.frame(city_edgelist)

map_cat <- data.frame("name"=c("Newton", "Hickory", "Conover", "Madien", "Claremont", "Catawba"), 
                      "lon"=c(-81.2171693643138,-81.34213884022306,-81.2203450996219,-81.21745426018545, -81.14557038112304, -81.0745247872627),  
                      "lat"=c(35.66965142781459,35.73489034142729,35.70722608224313,35.58201828025933,35.71479753664958, 35.707990733659656))

g1 <- graph.data.frame(city_edgelist, directed=T, vertices=map_cat)


E(g)$color <- "brown"
lo <- as.matrix(map_cat[,2:3])
map(catawba_county)
plot(g, layout=lo, add = TRUE, rescale = FALSE)

city_network$gis <- cat_cities_geometry
city_network$lat <- cat_cities$lat
city_network$lon <- cat_cities$long

city_network

plot(city_network$gis)

#dataframe from igraph ------
city_network1 <- as_long_data_frame(city_network)
city_network1

#Transform from shape to google maps compatiable -----
cat_shapefile_df <- spTransform(catawba_county, CRS("+proj=longlat +datum=WGS84"))

ggplot()+
  geom_polygon(data=cat_shapefile_df, 
            aes(x=long, y=lat, group=group),
            color='gray', fill='white', size =.2) +
  geom_point(data = cat_cities,
             aes(long, lat), color = "red") +
  geom_sf_label_repel(data=cat_cities_geometry, 
                      aes(label = city),
                      force = 100, nudge_x = -0.75, nudge_y = 0.1, seed = 10) +
  geom_segment(data = city_network1,
               aes(x = from_long, xend = to_long,
                   y = from_lat, yend = to_lat),
               arrow = arrow(length = unit(0.1, "inches"), # optional arrows
                             type = "closed"), color = "blue",
               size = 0.5,
               alpha = 0.75) +        
  theme_bw() +
  labs(title = "Geospatial Network",
       subtitle = "Plot Example",
       caption = "Colt Jensen")

cat_cities

#ERGMs -----
library(statnet)
library(intergraph)

statnet59 <- asNetwork(city_network)
statnet59

#plotting the network
plot(statnet59, 
     vertex.col = "tomato", 
     vertex.cex = 1)

#documenation for ERGM
?ergm.terms

#ERGM model
random_graph <- ergm(statnet59 ~ edges, control = control.ergm(seed = 1234))

#Coefficients in ERGMs represent the change in the (log-odds) likelihood of a 
#tie for a unit change in a predictor. We can use a simple formula for converting 
#log-odds to probability to understand them better.
inv.logit <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

theta <- coef(random_graph)
inv.logit(theta)
#interpretation: So the probability of an edge being in the graph is roughly 0.02. 
#The probability of an edge being drawn should in theory be the same as density - let’s check
network.density(statnet59)

#model fit
summary(random_graph)

#We can also simulate graphs using our ERGM fit. We did something similar to 
#this when we simulated random graphs and set the probability of an edge being 
#drawn to be equal to our network’s density.
set.seed(1234)
hundred_simulations <- simulate(random_graph, 
                                coef = theta,
                                nsim = 100,
                                control = control.simulate.ergm(MCMC.burnin = 1000,
                                                                MCMC.interval = 1000))

#lets looks at the first nine simulations 
par(mfrow = c(3, 3))
sapply(hundred_simulations[1:9], plot, vertex.cex = 1, vertex.col = "tomato")


#We can compare the number of edges our observed graph has to the average of the simulated networks.
net_densities <- unlist(lapply(hundred_simulations, network.density))

hist(net_densities, xlab = "Density", main = "", col = "lightgray")
abline(v = network.density(statnet59), col = "red", lwd = 3, lty = 2)
abline(v = mean(net_densities), col = "blue", lwd = 3, lty = 1)

#Interpretation: seems pretty close

"Another way to evaluate our model is to use the built-in goodness of fit measures. 
Essentially, we will evaluate whether our network has similar structural features 
as the simulated graphs. ergm has a built-in function - gof() - which calculates 
the statistics for us. We just have to tell it how many simulations we want to 
use in the comparison set - the larger the number, the more accurate representation of the model."

gof_stats <- gof(random_graph)

par(mfrow = c(2, 3))
plot(gof_stats, main = '')

#we can improve model fit by adding more terms

"nodematch is the homophily term in ergm. We can specify the attribute we want 
to examine as well as the diff argument, which allows for differential levels 
of homophily for different groups."
model1 <- ergm(statnet59 ~ edges + 
                 nodematch("per_white") + 
                 nodematch("population") + 
                 nodematch("tax_rev"))

summary(model1)
#inter- they are all significant!

"Let’s try it with diff set to T. We will limit our examination to only 
grade/race/sex categories represented by a large number of vertices in our network. 
You can examine this using the table function"
table(V(city_network)$tax_rev) #need matching values for this

model4 <- ergm(statnet59 ~ edges + 
                 nodematch("per_white", diff=T, levels = c(1,2)) + 
                 nodematch("population", diff=T, levels = c(1,2,5)) + 
                 nodematch("tax_rev", diff=T, levels = as.character(c(7:12))))

summary(model4)

#mutal = reciprocity
model2 <- ergm(statnet59 ~ edges + 
                 nodematch("per_white") + 
                 nodematch("population") + 
                 nodematch("tax_rev")+ 
                 mutual) 

summary(model2)

"Now let’s add a term for triadic closure. There are a few terms for triads - 
one of them, triangles, tends to lead to degeneracy. The gwesp term behaves better, 
though convergence is far from guaranteed. It may be a good time to use the bathroom. 
This will take a while…"

model3 <- ergm(statnet59 ~ edges + 
                 nodematch("per_white") + 
                 nodematch("population") + 
                 nodematch("tax_rev")+ 
                 mutual + 
                 gwesp(0.25, fixed = T),
               control=control.ergm(MCMLE.maxit= 40))

# you can change a number of other things about the MCMC algorithm - from its burn-in to its step and sample size
# here we just up the maximum iterations we wait to see if it has converged) 

summary(model3)

#adding all the terms
model70 <- ergm(statnet59 ~ edges + 
                 nodematch("per_white") + 
                 nodematch("population") + 
                 nodematch("tax_rev")+ 
                 mutual + 
                 twopath +
                  density  
                  ) 

summary(model70)

#next how to create multiplex networks in R 
#Also how measure: Transitivity(this may just be gwesp), multiple two-path,
# multiplexity, popularity spread, activity spread, and sender/receiver effects from Siciliano 2015

#Start here: https://rdrr.io/bioc/RandomWalkRestartMH/man/create.multiplex.html
# and here: https://rdrr.io/cran/ergm/man/ergm-terms.html

