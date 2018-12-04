########################################################################
## Project: VA
## Script purpose: Divide Santiago´s shapefile into equal size hexagons.
##                 megrge census data + voters data with hexagons
##
## Date: 29 - 11 - 2018
## Author: Cristóbal Montt
########################################################################

## Create hexagons of equal sized from shp_rm --------------------------
set.seed(1)
size = 0.004 # original 0.0041, no se pueden hacer más pequeños que 0.000041
hex_points <- spsample(shp_rm, type = "hexagonal", cellsize = size)
hex_grid <- HexPoints2SpatialPolygons(hex_points, dx = size)
hex_grid$area_sqm <- area(hex_grid)/1000000

## Merge census blocks data with hexagons ------------------------------
# First we get the polygons ID
#library(spatialEco)
pid <- sapply(slot(hex_grid, "polygons"), function(x) slot(x, "ID"))

# Then we assign the polygon´s ID as the rownames of a new data frame
p.df <- data.frame(ID = 1:length(hex_grid), row.names = pid) 

# We coerce the hexagons object to a SpatialPolygonsDataFrame 
# were p.df corresponds to the attributes of each hexagon, in this 
# case we only have the ID.
hex_grid <- SpatialPolygonsDataFrame(hex_grid, p.df)

# points.in.poly() assigns each block in block_centers object to
# its corresponding hexagon.
blocks_in_hex <- point.in.poly(blocks_centers, hex_grid)

# Creates vector with the hexagons ID which at least have data from
# one census block. 
id_df <- unique(blocks_in_hex$ID)

# We subset hexagons for which at least we have data from one census
# block
hex_grid <- hex_grid[hex_grid$ID %in% id_df,]

# Remove unnecessary objects from workspace
rm(blocks_centers, hex_points, p.df, id_df, pid, size)


## Add census block data to hexagon spatial poolygons data frame----------

# First we add the variable total_blocks, which indicates how many census blocks 
# fell in the same hexagon
manzanas_x_hex <- blocks_in_hex@data %>% group_by(ID) %>% dplyr::summarise(total_blocks = n())
hist(manzanas_x_hex$total_blocks)
summary(manzanas_x_hex$total_blocks)

# Merge hex_grid with new variable
hexgrid_blocks <- merge(hex_grid, manzanas_x_hex, by = "ID")


# Subset blocks_in_hex, just keep variables of interest 
# Maybe is better to use var names instead of their index.
blocks_in_hex <- blocks_in_hex[, c(1,33:150,154,155)]

# Some variables are the mean for each block, others are the total of persons
# per block. Next we get the average of those variables, hencem for some
# the values is the average of an average acrross blocks, for others
# is the average of frequencies. Think about possible consequences!!!!
blocks_in_hex@data <-  blocks_in_hex@data %>% group_by(ID) %>% 
                                summarise_all(funs(median)) #just changed it to median

# The varaibles that are now the mean of the frequency we are going to convert 
# them to a percentage by dividing by the average total of people.
# makes sense?

# Variables that correspond to the total number of people
vars <- c("tot_per", "tot_muj", "tot_hom","d0a7", "d7a17", "d18a29", 
          "d30a39", "d40a49", "d50a59", "d60a69", "d70a79", "d80a89",
          "d90a_", "ID")

freq_to_mean <- blocks_in_hex@data[, vars]
prop_freqs <- freq_to_mean %>% mutate_at(.vars = vars(tot_muj:d90a_),
                                     .funs = funs(. / tot_per))
# got it from here https://stackoverflow.com/questions/45634566/using-mutate-at-from-dplyr

# Si sumo los porcentajes dan 1!! asumo todo esta bien, yo cacho
# borrar <- prop_freqs %>% mutate(sumVar = rowSums(.[2:3]))
# borrar <- prop_freqs %>% mutate(sumVar = rowSums(.[4:13]))
# summary((borrar$sumVar))

vars <- c("tot_per", "tot_muj", "tot_hom","d0a7", "d7a17", "d18a29", 
          "d30a39", "d40a49", "d50a59", "d60a69", "d70a79", "d80a89",
          "d90a_")

blocks_in_hex <- blocks_in_hex[,!(names(blocks_in_hex) %in% vars)] 

# ADD prop_freqs to blocks_in_hex
blocks_in_hex <- merge(blocks_in_hex, prop_freqs, by = "ID")

# from https://gis.stackexchange.com/questions/109652/removing-columns-in-a-spatialpolygonsdataframe-in-r

# Add to hex spdf the data from census blocks aggregated at hexagon level
hexgrid_blocks <- merge(hexgrid_blocks, blocks_in_hex@data, by = "ID")

## merge with voters data ---------------------------------------------------

# voters data (voter register + elections results)
# Contains the data from SERVEL´s voters register as well as data from 
# election results.

# the data is too big for Github, so I stored the data in Amazon Web Services's
# S3 bucket. Below is the credentials and code for accesing it.

padron <- s3readRDS("padron_resultados_elecciones_rm.rds", bucket = "voter.register",
                     key = "", secret = "", region = "eu-west-2")


# padron <- readRDS("./data/padron_resultados_elecciones_rm.rds") 

vars <- c("lat", "long", "CG", "JAK", "SP", 
          "AG", "BS", "MEO", "EA", "AN", "VVE", "VN",
          "VB", "VT", "tmesa", "part", "SP2", "AG2", 
          "VVE2", "VN2", "VB2", "VT2", "part2")

padron <- padron[, names(padron) %in% vars]

# Clean dataset
voters <- padron[!is.na(padron$long), ] 

# convert voters which is not a spatial dataframe to a spatialpointsdatframe
coordinates(voters) <- ~long + lat 
projection(voters) <- projection(shp_rm) 
voters <- spTransform(voters, projection(shp_rm))
# We just keep voters that fall within the hexagons that we already
# filtered by having at least one census block.
voters <- voters[hex_grid, ] 

voters_in_hex <- point.in.poly(voters, hex_grid)
rm(voters, padron)

# Create right_vote and left_vote for first round
voters_in_hex@data <- voters_in_hex@data %>% 
                                 group_by(ID) %>% 
                                 mutate(left_1 = CG + AG + BS + MEO + EA + AN,
                                                 right_1 = SP + JAK,
                                                 left_perc_1 = left_1/VT,
                                                 right_perc_1 = right_1/VT,
                                                 left_perc_2 = AG2/VT2,
                                                 right_perc_2 = SP2/VT2)

# Create variables by hexagon ID, just mean of voting?
voters_by_hex <- voters_in_hex@data
voters_by_hex <- voters_by_hex %>% group_by(ID) %>% 
                                summarise_all(funs(mean(., na.rm = TRUE)))

# leave only the meaningful variables? 

hex_voters <- merge(hex_grid, voters_by_hex, by = "ID")


## Final dataset ----------------------------------------------------------
hexGrid <- merge(hexgrid_blocks, hex_voters@data, by = "ID")
hexGrid <- hexGrid[!is.na(hexGrid$right_perc_2), ]

# Difference betweeen right and left
hexGrid$diff_1 <- hexGrid$right_perc_1 - hexGrid$left_perc_1
hexGrid$diff_2 <- hexGrid$right_perc_2 - hexGrid$left_perc_2

hexGrid$swing <- ((hexGrid$right_perc_2 - hexGrid$right_perc_1) - (hexGrid$left_perc_2 - hexGrid$left_perc_1))/2

rm(blocks_in_hex, freq_to_mean, hex_grid, hex_voters, 
   hexgrid_blocks, manzanas_x_hex, prop_freqs, shp_rm, 
   voters_by_hex, voters_in_hex, vars)


# Save file if it doesn´t exist already
#if(!(file.exists("./data/hexGrid.rds"))){
    saveRDS(hexGrid, "./data/hexGrid.rds")  
#    print("Saving file")
#}
