# create a map of the canton zurich with the municipalities marked 
install.packages("tidygeocoder")
library(tidygeocoder)

# convert excel to csv
# Load the readxl package
library(readxl)
# Read the Excel file
excel_data <- read_excel("data_raw/Adressen_politische_Gemeinden_ZH_2020.xlsx")
# Write the data to a CSV file
write.csv(excel_data, file = "data_raw/Adressen_politische_Gemeinden_ZH_2020.csv", row.names = FALSE)

# create table with column address and name
df <- read.csv("data_raw/Adressen_politische_Gemeinden_ZH_2020.csv", encoding = "UTF-8")
head(df)

df$address <- paste(df$Strasse...Nr., df$Ort, df$PLZ, sep=",")

geocode(tibble(df[,"address"]), method = 'osm', lat = latitude , long = longitude)
tibble::tribble(df)
# geocode the addresses
lat_longs <- df[,c("address", "Ort")] %>%
  geocode(address, method = 'osm', lat = latitude , long = longitude)

library(ggplot2)

# create a dataframe with addresses
some_addresses <- tibble::tribble(
~name,                  ~addr,
"White House",          "1600 Pennsylvania Ave NW, Washington, DC",
"Transamerica Pyramid", "600 Montgomery St, San Francisco, CA 94111",     
"Willis Tower",         "233 S Wacker Dr, Chicago, IL 60606"                                  
)

# geocode the addresses
lat_longs <- some_addresses %>%
  geocode(addr, method = 'osm', lat = latitude , long = longitude)


ggplot(lat_longs, aes(longitude, latitude), color = "grey99") +
  borders("state") + geom_point() +
  ggrepel::geom_label_repel(aes(label = Ort)) +
  theme_void()