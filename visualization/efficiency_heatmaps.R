# Effizienzanalyse 2022, basierend auf Personalaufwand pro Einwohner

#plot data here afterwards: https://www.zh.ch/de/planen-bauen/geoinformation/geodaten/geodatenbezug/vorlagen-fuer-administrativer-grenzen-und-karten.html#1791164494

#load some libraries

library(readxl)
library(openxlsx)


setwd("~/Dropbox/Ausbildung/Uni/Master/2. Semester/Stats Lab/Daten")
# data analysis

df_einwohner <- read.csv("./Einwohnerzahlen 2022/data_5225434.csv", sep = ";", fileEncoding = "UTF-8") 
df_aufwand <- read.csv("./Personalaufwand 2022/data_1259238.csv", sep = ";", fileEncoding = "ISO-8859-1") #encoding helps so that its read in correctly

df_einwohner <- df_einwohner[c("GEBIET_NAME", "INDIKATOR_VALUE")]
df_einwohner$GEBIET_NAME <- sub("(bis 2022)", "", df_einwohner$GEBIET_NAME)
df_einwohner$GEBIET_NAME <- sub(" ", "", df_einwohner$GEBIET_NAME)
df_einwohner$GEBIET_NAME <- sub("\\(.*?\\)", "", df_einwohner$GEBIET_NAME)

# correct some by hand for correct merge
df_einwohner[144, 1] <- c("Aesch")
df_einwohner[106, 1] <- c("Uetikonam See")
df_einwohner[104, 1] <- c("Oetwilam See")
df_einwohner[32, 1] <- c("Thalheima.d.Thur")



df_aufwand <- df_aufwand[c("GEBIET_NAME", "INDIKATOR_VALUE")]
df_aufwand

df_aufwand$GEBIET_NAME <- sub(",.*", "", df_aufwand$GEBIET_NAME)
df_aufwand$GEBIET_NAME <- sub(" ", "", df_aufwand$GEBIET_NAME)
df_aufwand



df_total <- merge(df_aufwand, df_einwohner, by = "GEBIET_NAME", all = TRUE)
colnames(df_total) <- c("Gemeinde", "Personalaufwand (CHF)", "Einwohnerzahl")
df_total[, "Aufwand pro Einwohner"] <- df_total$`Personalaufwand (CHF)` / df_total$Einwohnerzahl
df_total <- df_total[order(df_total$Gemeinde, decreasing = TRUE), ]
df_total



# in order to visualize load GEBIETS_ID

df_gebiets_id <- read_excel("./Beispielfile Visualisierung/GEBIETS_ID.xls", col_names = FALSE)
?read_excel
#change again some names so that merging is successfull


print(df_gebiets_id, n = 200)
df_gebiets_id[3,3] <- c("Aeugsta.A.")
df_gebiets_id[4,3] <- c("Affolterna.A.")
df_gebiets_id[75,3] <- c("Langnaua.A.")
df_gebiets_id[55,3] <- c("Hausena.A.")
df_gebiets_id[69,3] <- c("Kappela.A.")
df_gebiets_id[18,3] <- c("Bucha.I.")
df_gebiets_id[12,3] <- c("Berga.I.")
df_gebiets_id[136,3] <- c("Uetikonam See")
df_gebiets_id[98,3] <- c("Oetwilam See")
df_gebiets_id[99,3] <- c("Oetwila.d.L.")
df_gebiets_id[150,3] <- c("Wettswila.A.")
df_gebiets_id[131,3] <- c("Thalheima.d.Thur")
df_gebiets_id[37,3] <- c("Ellikona.d.Th.")



colnames(df_gebiets_id) <- c("Gebiets_ID", "Wert", "Gemeinde")
df_final <- merge(df_total, df_gebiets_id, by = "Gemeinde")
df_save <- df_final[, c("Gebiets_ID", "Aufwand pro Einwohner", "Gemeinde")]
write.xlsx(df_save, "heatmap.xlsx", sheetName = "MyData", rowNames = FALSE, colNames = FALSE)
df_save <- df_save[-c(162, 158),]
write.xlsx(df_save, "heatmap_ohne_zürich_winterthur.xlsx", sheetName = "MyData", rowNames = FALSE, colNames = FALSE)

print(df_gebiets_id, n = 200)

                    