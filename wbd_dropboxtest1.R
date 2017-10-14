# Package/Dropbox Managment -----------------------------------------------


# Install all neccesary packages
# This will create LOTS of pop up windows the first time - it's okay.

if (!require("pacman")) install.packages("pacman")
pacman::p_load('tidyverse', 'raster', 'rdrop2', 'DT')

# Lat/Long Input ----------------------------------------------------------


# Input soil lat/long here

lat <- 38
long <- -90


# Below this point is the nitty gritty - beware and please don't touch. 

#       __ __  _____ ____  ______       ________  ____  ________  ______  ______
#   __/ // /_/ ___// __ \/  _/ /      / ____/ / / / / /_  __/ / / / __ \/ ____/
#  /_  _  __/\__ \/ / / // // /      / /   / / / / /   / / / / / / /_/ / __/   
# /_  _  __/___/ / /_/ // // /___   / /___/ /_/ / /___/ / / /_/ / _, _/ /___   
#  /_//_/  /____/\____/___/_____/   \____/\____/_____/_/  \____/_/ |_/_____/   


# The Rest of the Code ----------------------------------------------------



# Read NetCDF Files from public share links 

if(!file.exists("ET.nc")){
  
  drop_download(path = "https://www.dropbox.com/s/uyq9arqgnprxzv1/ET.nc?dl=0",
                local_path = "ET.nc",
                overwrite = FALSE)
}


if(!file.exists("precip.nc")){
  
  drop_download(path = "https://www.dropbox.com/s/sgmhq8cmth1jd8h/precip.nc?dl=0",
                local_path = "precip.nc",
                overwrite = FALSE)
}



# Name the NetCDF files and convert them to rasterStack format
# Change the layer names to Month names

precip_lowres <- stack("precip.nc")
names(precip_lowres) <- month.name

ET_stack <- stack("ET.nc")
names(ET_stack) <- month.name



# Use the input lat/long and create a spatial data frame

coords <- data.frame(cbind(long, lat))

coordinates(coords) <- ~ long + lat



# Gather the Precip/ET dataframes to 2 columns each

precip_vals <- data.frame(raster::extract(precip_lowres, coords)) %>% 
  gather(key = "Month", value = "Precip") %>% 
  mutate(Precip = round(Precip, 0))

ET_vals <- data.frame(raster::extract(ET_stack, coords))%>% 
  gather(key = "Month", value = "ET")



# Join the dataframes on the Month column

combo_vals <- precip_vals %>% 
  left_join(ET_vals) %>% 
  mutate(ET = ET*3)



# Factor the Month column for ggplots

combo_vals$Month <- factor(combo_vals$Month, levels = month.name)



# Print dataframe as interactive table

datatable(combo_vals)



# Print dataframe as plot
# This can be #'d out if graph is not desired

ggplot(combo_vals, aes(x = Month)) +
  geom_smooth(aes(y = ET, group = 1, color = 'ET'), se = FALSE)+
  geom_smooth(aes(y = Precip, group = 1, color = 'Precip'), se = FALSE)+
  ylab("Millimeters per Month")
