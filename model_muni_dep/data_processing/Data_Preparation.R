##############################################
#  Stats Lab
#  Predicting Staff Costs per Department
#  Data Cleaning and Preparation 
#############################################

####### PACKAGES #######
library(tidyr)
library(dplyr)
library(zoo)
#######################

######################### PROCESSING RAW DATA ###################################

# Loading Raw Data
data.predictors <- read.csv("/Users/lea/Desktop/Complete_Predictors_Data.csv", header = TRUE, sep = ";")
data.financial <- read.csv("/Users/lea/Desktop/ethz_wiedemeier_gefis_data_2013_2022 copy.csv", header = TRUE, sep = ";")

# Rearrangement of Raw Dataframe
data.predictors <- data.predictors %>% select(-c(1,3,4,5,6,10,11,12))
data.predictors <- data.predictors %>% select(YEAR = INDIKATOR_JAHR, GEMEINDE = GEBIET_NAME, PREDICTOR = INDIKATOR_NAME, PRED_VALUE = INDIKATOR_VALUE)
data.predictors <- data.predictors %>% pivot_wider(names_from = PREDICTOR, values_from = PRED_VALUE)
data.predictors <- data.predictors %>% select(-matches(c("Nettoaufwand", "Ertrags- (+)")))

data.financial <- subset(data.financial, KONTO_BEZ_RLG %in% c(
  "Löhne des Verwaltungs- und Betriebspersonals", 
  "Löhne Verwaltungs-, Betriebspersonal",
  "Erstattung von Lohn des Verwaltungs- und Betriebspersonals", 
  "Entschädigungen für temporäre Arbeitskräfte", 
  "AG-Beiträge AHV, IV, EO, ALV, Verwaltungskosten", 
  "AG-Beiträge an Pensionskasse", 
  "Sozialleistungen", 
  "AG-Beiträge an Familienausgleichskasse", 
  "AG-Beiträge an Krankentaggeldversicherungen",
  "Übriger Personalaufwand")) # removed: Dienstleistungen Dritter, Aus- und Weiterbildung des Personals
data.financial <- data.financial[,-c(2, 3, 5, 6, 13)]
data.financial <- data.financial %>% select(YEAR = RGJAHR, GEMEINDE = EINHEIT_BEZ, COSTS_CATEG = FUNKTION_BEZ_RLG, STAFF_COSTS = SALDO)

# Renaming Gemeinden so that they align between the two datasets
patterns <- c("\\(bis 201[2-9]\\)", "\\(bis 2022\\)", "ZH") # Removing redundant years in names
for(pattern in patterns) {
  data.predictors$GEMEINDE <- gsub(pattern, "", data.predictors$GEMEINDE)
}
data.predictors$GEMEINDE <- trimws(data.predictors$GEMEINDE)

patterns.name <- c("Politische Gemeinde", "Stadt", "\\(bis 202[0-2]\\)", "\\(bis 2019\\)", "ZH")
for(pattern in patterns.name) {
  data.financial$GEMEINDE <- gsub(pattern, "", data.financial$GEMEINDE)
}
data.financial$GEMEINDE <- trimws(data.financial$GEMEINDE)

data.financial$GEMEINDE <- gsub("Thalheim a.d.Thur", "Thalheim a.d.Th.", data.financial$GEMEINDE)
data.financial$GEMEINDE <- gsub("Uetikon am See", "Uetikon a.S.", data.financial$GEMEINDE)
data.financial$GEMEINDE <- gsub("Oetwil am See", "Oetwil a.S.", data.financial$GEMEINDE)

# Remove Zurich and Winterthur from dataset
data.predictors <- subset(data.predictors, !(GEMEINDE %in% c("Zürich", "Winterthur")))
data.financial <- subset(data.financial, !(GEMEINDE %in% c("Zürich", "Winterthur")))

# Checking for Alignment between Gemeinden in the two datasets
unique.predictors <- unique(data.predictors$GEMEINDE) # 169
unique.financial <- unique(data.financial$GEMEINDE) # 170

common.municipalities <- intersect(unique.predictors, unique.financial)
non.intersecting.predictors <- setdiff(unique.predictors, unique.financial) # All intersect
non.intersecting.financial <- setdiff(unique.financial, unique.predictors) # All except Bertschikon - only have data for Bertschikon in 4 categories in 2013


################# CHECKING PREDICTORS DATA FOR INCONSISTENSIES #################################################
# Checking for NAs in Predictors Data

# Number of NAs per predictor
Nas.by.predictor <- colSums(is.na(data.predictors[, 3:length(data.predictors)]))
Nas.by.predictor <- as.data.frame(Nas.by.predictor, stringsAsFactors = FALSE)
names(Nas.by.predictor) <- c("Number_of_NAs")
Nas.by.predictor$Predictor <- rownames(Nas.by.predictor)
rownames(Nas.by.predictor) <- NULL
Nas.by.predictor <- Nas.by.predictor %>% arrange(desc(Number_of_NAs))
sum(Nas.by.predictor$Number_of_NAs)
# Total Number of NAs: 77704
# Predictor with most NAs related to: Kantonsratswahlen, Fläche, Erschliessung


# Number of NAs per Gemeinde
Nas.by.Gemeinde <- data.predictors %>% group_by(GEMEINDE) %>% summarise(across(everything(), ~sum(is.na(.))))
Nas.by.Gemeinde$NAs <- rowSums(Nas.by.Gemeinde[3:length(data.predictors)])
Nas.by.Gemeinde <- Nas.by.Gemeinde %>% select(GEMEINDE, NAs, everything())
sum(Nas.by.Gemeinde$NAs)
# Municipalities have between 123-1928 NAs
# Total NAs 77704

# Number of NAs per year
NAs.by.year <- data.predictors %>% group_by(YEAR) %>% summarise(across(everything(), ~sum(is.na(.))))
NAs.by.year$NAs <- rowSums(NAs.by.year[3:length(data.predictors)])
NAs.by.year <- NAs.by.year %>% select(YEAR, NAs, everything())
sum(NAs.by.year$NAs)
# Years with NAs (between 2864-14084): 2013-2021, with 2020 and 2021 with the most NAs
# Total NAs 77704

# Checking if there is data for every year for each Gemeinde
data.year.gemeinde.count <- data.predictors %>% group_by(GEMEINDE, YEAR) %>% summarise(RecordCount = n(), .groups = 'drop') 
data.year.gemeinde.matrix <- data.year.gemeinde.count %>% pivot_wider(names_from = YEAR, values_from = RecordCount, values_fill = list(RecordCount = 0))
data.presence.matrix <- data.year.gemeinde.matrix %>% select(-GEMEINDE) %>% mutate(across(everything(), ~ .x > 0)) 
data.presence.matrix <- bind_cols(data.year.gemeinde.matrix %>% select(GEMEINDE), data.presence.matrix)
data.presence.matrix$TOTAL <- rowSums(data.presence.matrix[,c(2:11)])
# For all except for 9 Gemeinden there is at least some data every year
# For Gemeinden Sternenberg and Kyburg there is only data until 2014/2015

# Checking if there is data for every predictor of every year
# Count of non-NA entries for each predictor
NAs.yearVSpredictor <- data.predictors %>% group_by(YEAR) %>% summarise(across(3:length(data.predictors)-1, ~sum(!is.na(.)), .names = "non_na_{.col}")) %>% ungroup()
entries.zero <- NAs.yearVSpredictor %>% pivot_longer(cols = -YEAR, names_to = "Predictor", values_to = "Non_NA_Count") %>% filter(Non_NA_Count == 0)
# entries.zero identifies the year and predictor where there is no data
# Some predictors have no data at all for the years 2013-2022, should definitely remove these


# CONCLUSION TO BE DRAWN:
# Gemeinden Sternenberg and Kyburg were discontinued, no longer eigene politische Gemeinden,
# this explains why there is only data till 2014/2015. Kyburg is part of Gemeinde Illnau-Effretikon, 
# and Sternenberg part of Gemeinde Bauma. I suggest combining the data from 2013-2015

####################### NA CHECKS AND FILL NA'S #############################################

data.predictors[data.predictors == "null"] <- NA

NAs_rows <- rowSums(is.na(data.predictors))
cor(data.predictors$YEAR, NAs_rows) # 0.3783507

NAs_columns <- data.frame(column=colnames(data.predictors), NA_Cols=colSums(is.na(data.predictors)), ratio=colSums(is.na(data.predictors)) / dim(data.predictors)[1])
rownames(NAs_columns[NAs_columns$ratio > 0.4, ]) # mostly Wahlen predictors, 43 total

# forward fill
data.predictors <- data.predictors[order(data.predictors$GEMEINDE, data.predictors$YEAR),]
data.predictors.frwd <- do.call("rbind", by(data.predictors, data.predictors$GEMEINDE, na.locf, na.rm = FALSE))

# look at columns with extreme missings
NAs_rows <- rowSums(is.na(data.predictors.frwd))
cor(data.predictors.frwd$YEAR, NAs_rows) # -0.7865

# backwards fill
data.predictors.filled <- do.call("rbind", by(data.predictors.frwd, data.predictors.frwd$GEMEINDE, na.locf, fromLast = TRUE, na.rm = FALSE))

data.predictors.filled.col.missings <- data.frame(column = colnames(data.predictors.filled), NA_Cols = colSums(is.na(data.predictors.filled)), ratio = colSums(is.na(data.predictors.filled)) / nrow(data.predictors.filled))
data.predictors.filled.col.missings[data.predictors.filled.col.missings$ratio > 0,]
missing.cols <- rownames(data.predictors.filled.col.missings[data.predictors.filled.col.missings$ratio > 0,]) # 57
filled.cols <- rownames(data.predictors.filled.col.missings[data.predictors.filled.col.missings$ratio == 0,])

data.predictors.filled <- data.predictors.filled[, filled.cols]
stopifnot(sum(is.na(data.predictors.filled)) == 0)
#stopifnot(dim(data.predictors.filled)[2] == 295) # 259
#dim(data.predictors.filled) # 250
#dim(data.predictors) # 307


# Converting predictor columns to type numeric
for(col in names(data.predictors.filled)[-c(1, 2)]) {
  data.predictors.filled[[col]] <- as.numeric(data.predictors.filled[[col]])
}

############################ SCALING PREDICTORS USING METHOD OF STANDARDIZATION ##############################
# Method: Standardization (Z-Score Normalization)

data.predictors.scaled <- data.predictors.filled
data.predictors.scaled[ , 3:ncol(data.predictors.scaled)] <- scale(data.predictors.scaled[ , 3:ncol(data.predictors.scaled)])

############################ `Bevölkerung [Pers.]`############################ CHECKING FINANCIAL DATA FOR INCONSISTENSIES ############################ 
# Checking for NAs in financial data
sum(is.na(data.financial$STAFF_COSTS)) # Sum is 0, so we do not have any NAs

all.possible.combinations <- expand.grid(YEAR = unique(data.financial$YEAR), GEMEINDE = unique(data.financial$GEMEINDE), COSTS_CATEG = unique(data.financial$COSTS_CATEG))
existing.combinations <- data.financial %>% select(YEAR, GEMEINDE, COSTS_CATEG) %>% distinct()
missing.combinations <- anti_join(all.possible.combinations, existing.combinations, by = c("YEAR", "GEMEINDE", "COSTS_CATEG"))

# There are not all categories in every Gemeinde for every year, but I think that is normal. We should not be worried about that 

############### NARROWING DOWN DATA RELEVANT FOR DEPARTMENT: ÖFFENTLICHE ORDNUNG UND SICHERHEIT ###################################

# Terms from subcategories in Department Öffentliche Ordnung und Sicherheit
terms.OrdnungSicherheit <- c("Polizei", "Verkehrssicherheit", "Rechtsprechung", "Allgemeines Rechtswesen", 
           "Feuerwehr", "Militärische Verteidigung", "Zivile Verteidigung",
           "Regionale Zivilschutzorganisation", "Militär", "Feuerwehr und Feuerpolizei",
           "Rechtspflege", "Zivilschutz/Gemeindeführungsstab", "Zivilschutz",
           "Ziviler Gemeindeführungsstab")

data.financial.OrdnungSicherheit <- data.financial[sapply(data.financial$COSTS_CATEG, function(x) {
  any(sapply(terms.OrdnungSicherheit, function(term) grepl(term, x)))
}), ]

# Summing the Konten that are from the same year, municipality, category
data.financial.OrdnungSicherheit <- aggregate(STAFF_COSTS ~ YEAR + GEMEINDE + COSTS_CATEG, 
                                           data = data.financial.OrdnungSicherheit, FUN = sum)
data.financial.OrdnungSicherheit <- aggregate(STAFF_COSTS ~ YEAR + GEMEINDE, data = data.financial.OrdnungSicherheit, FUN = sum)

# Merging Dataframes
data.combined.OrdnungSicherheit <- merge(data.predictors.scaled, data.financial.OrdnungSicherheit, by = c("YEAR", "GEMEINDE"))



############### NARROWING DOWN DATA RELEVANT FOR DEPARTMENT: KULTUR, SPORT, UND FREIZEIT ###################################
terms.KultSportFrei <- c("Museen und bildende Kunst", "Denkmalpflege und Heimatschutz", "Bibliotheken", 
                         "Musik und Theater", "Kultur, Übriges", "Film und Kino", "Massenmedien", "Sport", "Freizeit", 
                         "Kirchen und religiöse Angelegen- heiten", "Kulturförderung", "Uebrige Freizeitgestaltung",
                         "Kirche", "Denkmalpflege, Heimatschutz", "Bibliotheken und Literatur")

data.financial.KultSportFrei <- data.financial[sapply(data.financial$COSTS_CATEG, function(x) {
  any(sapply(terms.KultSportFrei, function(term) grepl(term, x)))
}), ]

# Summing the Konten that are from the same year, municipality, category
data.financial.KultSportFrei<- aggregate(STAFF_COSTS ~ YEAR + GEMEINDE + COSTS_CATEG, 
                                              data = data.financial.KultSportFrei, FUN = sum)
data.financial.KultSportFrei <- aggregate(STAFF_COSTS ~ YEAR + GEMEINDE, data = data.financial.KultSportFrei, FUN = sum)

# Merging Dataframes
data.combined.KultSportFrei <- merge(data.predictors.scaled, data.financial.KultSportFrei, by = c("YEAR", "GEMEINDE"))


############### NARROWING DOWN DATA RELEVANT FOR DEPARTMENT: GESUNDHEIT ###################################
terms.Gesundheit <- c("Spitäler", "Kranken-, Alters- und Pflegeheime", "Psychiatrische Kliniken", 
                      "Ambulante Krankenpflege", "Rettungsdienste", "Alkohol- und Drogenprävention", 
                      "Krankheitsbekämpfung übrige", "Schulgesundheitsdienst", "Lebensmittelkontrolle", 
                      "Altersheime", "Gesundheitswesen übriges", "Krankheitsbekämpfung",
                     "Kranken- und Pflegeheime", "Betreuung Suchtabhängiger", "Krankheitsbekämpfung, übrige",
                     "Freiwillige wirtschaftliche Hilfe", "Fürsorge, Übriges")

data.financial.Gesundheit <- data.financial[sapply(data.financial$COSTS_CATEG, function(x) {
  any(sapply(terms.Gesundheit, function(term) grepl(term, x)))
}), ]

# Summing the Konten that are from the same year, municipality, category
data.financial.Gesundheit <- aggregate(STAFF_COSTS ~ YEAR + GEMEINDE + COSTS_CATEG, 
                                         data = data.financial.Gesundheit, FUN = sum)
data.financial.Gesundheit <- aggregate(STAFF_COSTS ~ YEAR + GEMEINDE, data = data.financial.Gesundheit, FUN = sum)

# Merging Dataframes
data.combined.Gesundheit <- merge(data.predictors.scaled, data.financial.Gesundheit, by = c("YEAR", "GEMEINDE"))


############### NARROWING DOWN DATA RELEVANT FOR DEPARTMENT: SOZIALE SICHERHEIT ###################################
terms.SozialeSicherheit <- c("Krankenversicherung", "Prämienverbilligungen", "Unfallversicherungen", 
                             "Ergänzungsleistungen IV", "Invalidenheime", "Leistungen an Invalide", 
                             "Alters- und Hinterlassenenversicherung AHV","Ergänzungsleistungen AHV", 
                             "Leistungen an Pensionierte", "Wohnen im Alter (ohne Pflege)", 
                             "Leistungen an das Alter", "Alimentenbevorschussung und -inkasso", "Jugendschutz", 
                             "Leistungen an Familien", "Leistungen an Arbeitslose", "Arbeitslosigkeit Übriges", 
                             "Sozialer Wohnungsbau", "Beihilfen/Zuschüsse", "Wirtschaftliche Hilfe", "Asylwesen", 
                             "Fürsorge Übriges", "Hilfsaktionen im Inland", "Hilfsaktionen im Ausland",
                             "Zusatzleistungen zur AHV/IV", "Soziale Wohlfahrt übriges", "Altersheime",
                             "Sozialversicherung allgemein", "Asylbewerberbetreuung", "Pflegefinanzierung Alters- und Pflegeheime (ab 01.01.2011)",
                             "Gesetzliche wirtschaftliche Hilfe", "Arbeitslosenhilfe", "Fürsorge, Übriges",
                             "Pflegefinanzierung ambulante Krankenpflege (Spitex)", "Arbeitslosigkeit, Übriges",
                             "Pflegefinanzierung Kranken-, Alters- und Pflegeheime", "Ergänzungsleistungen IV")

data.financial.SozialeSicherheit <- data.financial[sapply(data.financial$COSTS_CATEG, function(x) {
  any(sapply(terms.SozialeSicherheit, function(term) grepl(term, x)))
}), ]

# Summing the Konten that are from the same year, municipality, category
data.financial.SozialeSicherheit <- aggregate(STAFF_COSTS ~ YEAR + GEMEINDE + COSTS_CATEG, 
                                       data = data.financial.SozialeSicherheit, FUN = sum)
data.financial.SozialeSicherheit <- aggregate(STAFF_COSTS ~ YEAR + GEMEINDE, data = data.financial.SozialeSicherheit, FUN = sum)

# Merging Dataframes
data.combined.SozialeSicherheit <- merge(data.predictors.scaled, data.financial.SozialeSicherheit, by = c("YEAR", "GEMEINDE"))


############### NARROWING DOWN DATA RELEVANT FOR DEPARTMENT: VERKEHR UND NACHRICHTENÜBERMITTLUNG ###################################
terms.VerkehrNachrichten <- c("Nationalstrassen", "Hauptstrassen nach Bundesrecht", "Kantonsstrassen, übrige", 
                              "Gemeindestrassen", "Privatstrassen", "Strassen, Übriges", 
                              "Öffentliche Verkehrsinfrastruktur","Regional- und Agglomerationsverkehr", 
                              "Öffentlicher Verkehr Übriges", "Schifffahrt", "Luft- und Raumfahrt", 
                              "Sonstige Transportsysteme", "Verkehrsplanung allgemein", "Nachrichtenübermittlung",
                              "Regionalverkehr", "Bundesbahnen", "Staatsstrassen")

data.financial.VerkehrNachrichten <- data.financial[sapply(data.financial$COSTS_CATEG, function(x) {
  any(sapply(terms.VerkehrNachrichten, function(term) grepl(term, x)))
}), ]

# Summing the Konten that are from the same year, municipality, category
data.financial.VerkehrNachrichten <- aggregate(STAFF_COSTS ~ YEAR + GEMEINDE + COSTS_CATEG, 
                                              data = data.financial.VerkehrNachrichten, FUN = sum)
data.financial.VerkehrNachrichten <- aggregate(STAFF_COSTS ~ YEAR + GEMEINDE, data = data.financial.VerkehrNachrichten, FUN = sum)

# Merging Dataframes
data.combined.VerkehrNachrichten <- merge(data.predictors.scaled, data.financial.VerkehrNachrichten, by = c("YEAR", "GEMEINDE"))


############### NARROWING DOWN DATA RELEVANT FOR DEPARTMENT: UMWELTSCHUTZ UND RAUMORDNUNG ###################################
terms.UmweltRaumordnung <- c("Wasserversorgung", "Abwasserbeseitigung", "Abfallwirtschaft", "Gewässerverbauungen", 
                             "Schutzverbauungen übrige", "Arten- und Landschaftsschutz", "Luftreinhaltung und Klimaschutz", 
                             "Übrige Bekämpfung von Umweltverschmutzung", "Friedhof und Bestattung", "Umweltschutz, Übriges", 
                             "Raumordnung", "Gewässerunterhalt und -verbauungen", "Abfallbeseitigung", "Uebriger Umweltschutz", 
                             "Naturschutz", "Wasserversorgung", "Wasserwerk", "Parkanlagen, Wanderwege", "Raumplanung",
                             "Abfallwirtschaft (Gemeindebetrieb)", "Wasserwerk (Gemeindebetrieb)", "Abfallwirtschaft (allgemein)",
                             "Gewässerverbauungen", "Abwasserbeseitigung (allgemein)", "Abwasserbeseitigung (Gemeindebetrieb)",
                             "Raumordnung", "Übrige Bekämpfung von Umweltverschmutzung", "Forstwesen", "Regionale Friedhoforganisation",
                             "Kläranlagen (Gemeindebetrieb)")

data.financial.UmweltRaumordnung <- data.financial[sapply(data.financial$COSTS_CATEG, function(x) {
  any(sapply(terms.UmweltRaumordnung, function(term) grepl(term, x)))
}), ]

# Summing the Konten that are from the same year, municipality, category
data.financial.UmweltRaumordnung <- aggregate(STAFF_COSTS ~ YEAR + GEMEINDE + COSTS_CATEG, 
                                               data = data.financial.UmweltRaumordnung, FUN = sum)
data.financial.UmweltRaumordnung <- aggregate(STAFF_COSTS ~ YEAR + GEMEINDE, data = data.financial.UmweltRaumordnung, FUN = sum)

# Merging Dataframes
data.combined.UmweltRaumordnung <- merge(data.predictors.scaled, data.financial.UmweltRaumordnung, by = c("YEAR", "GEMEINDE"))


############### NARROWING DOWN DATA RELEVANT FOR DEPARTMENT: VOLKSWIRTSCHAFT ###################################
terms.Volkswirtschaft <- c("Landwirtschaftliche Strukturverbesserungen", "Landwirtschaftliche Produktionsverbesserungen Vieh", 
                           "Landwirtschaftliche Produktionsverbesserungen Pflanzen", "Alpwirtschaft", "Forstwirtschaft", 
                           "Jagd und Fischerei", "Tourismus", "Industrie", "Gewerbe", "Handel", "Banken und Versicherungen", 
                           "Elektrizität", "Erdöl und Gas", "Nichtelektrische Energie", "Energie Übriges", "Sonstige gewerbliche Betriebe",
                           "Landwirtschaft", "Tourismus, kommunale Werbung", "Uebrige Gemeindebetriebe", "Gasversorgung", "Elektrizitätswerk - Stromhandel übriges",
                           "Energieversorgung", "Energie - übriges", "Landwirtschaftliche Gutsbetriebe", "Jagd und Fischerei",
                           "Industrie, Gewerbe, Handel", "Landwirtschaftliche Produktionsverbesserungen Pflanzen", "Landwirtschaftliche Strukturverbesserungen",
                           "Forstliche Nebenbetriebe", "Fernwärmebetrieb Energie, Übriges (Gemeindebetrieb)", "Hauptbetrieb",
                           "Sonstige gewerbliche Betriebe", "Gemeinwirtschaftliche Forstleistungen", "Elektrizitätswerk - Elektrizitätsnetz (Gemeindebetrieb)",
                           "Elektrizitätswerk - Stromhandel und Übriges (ohne Elektrizitätsnetz) (Gemeindebetrieb)", "Nichtelektrische Energie (allgemein)",
                           "Gasversorgung (Gemeindebetrieb)", "Fernwärmebetrieb nichtelektrische Energie (Gemeindebetrieb)", "Forstwirtschaft, Hauptbetrieb")

data.financial.Volkswirtschaft <- data.financial[sapply(data.financial$COSTS_CATEG, function(x) {
  any(sapply(terms.Volkswirtschaft, function(term) grepl(term, x)))
}), ]

# Summing the Konten that are from the same year, municipality, category
data.financial.Volkswirtschaft <- aggregate(STAFF_COSTS ~ YEAR + GEMEINDE + COSTS_CATEG, 
                                              data = data.financial.Volkswirtschaft, FUN = sum)
data.financial.Volkswirtschaft <- aggregate(STAFF_COSTS ~ YEAR + GEMEINDE, data = data.financial.Volkswirtschaft, FUN = sum)

# Merging Dataframes
data.combined.Volkswirtschaft <- merge(data.predictors.scaled, data.financial.Volkswirtschaft, by = c("YEAR", "GEMEINDE"))


############### NARROWING DOWN DATA RELEVANT FOR DEPARTMENT: FINANZEN UND STEUERN ###################################

terms.FinanzenSteuern <- c("Steuern, Finanz- und Lastenausgleich", "Ertragsanteile übrige ohne Zweckbindung", 
                           "Zinsen", "Emissionskosten", "Liegenschaften des Finanzvermögens", "Finanzvermögen Übriges", 
                           "Rückverteilungen aus CO2-Abgabe", "Nicht aufgeteilte Posten", "Neutrale Aufwendungen und Erträge", 
                           "Abschluss", "Zweckgebundene Zuwendungen", "Grundeigentum Finanzvermögen", "Gemeindesteuern", 
                           "Liegenschaften des Finanzvermögens", "Zweckgebundene Zuwendungen", "Allgemeine Gemeindesteuern",
                           "Sondersteuern")

data.financial.FinanzenSteuern <- data.financial[sapply(data.financial$COSTS_CATEG, function(x) {
  any(sapply(terms.FinanzenSteuern, function(term) grepl(term, x)))
}), ]

# Summing the Konten that are from the same year, municipality, category
data.financial.FinanzenSteuern <- aggregate(STAFF_COSTS ~ YEAR + GEMEINDE + COSTS_CATEG, 
                                            data = data.financial.FinanzenSteuern, FUN = sum)
data.financial.FinanzenSteuern <- aggregate(STAFF_COSTS ~ YEAR + GEMEINDE, data = data.financial.FinanzenSteuern, FUN = sum)

# Merging Dataframes
data.combined.FinanzenSteuern <- merge(data.predictors.scaled, data.financial.FinanzenSteuern, by = c("YEAR", "GEMEINDE"))


############### NARROWING DOWN DATA RELEVANT FOR DEPARTMENT: ALLGEMEINE VERWALTUNG ###################################
terms.AllgemeineVerwaltung <- c("Legislative", "Exekutive", "Finanz- und Steuerverwaltung", "Allgemeine Dienste, übrige", 
                                "Verwaltungsliegenschaften, übrige", "Gemeinde-/Stadtverwaltung", "Verwaltungsliegenschaften")

data.financial.AllgemeineVerwaltung <- data.financial[sapply(data.financial$COSTS_CATEG, function(x) {
  any(sapply(terms.AllgemeineVerwaltung, function(term) grepl(term, x)))
}), ]

# Summing the Konten that are from the same year, municipality, category
data.financial.AllgemeineVerwaltung <- aggregate(STAFF_COSTS ~ YEAR + GEMEINDE + COSTS_CATEG, 
                                            data = data.financial.AllgemeineVerwaltung, FUN = sum)
data.financial.AllgemeineVerwaltung <- aggregate(STAFF_COSTS ~ YEAR + GEMEINDE, data = data.financial.AllgemeineVerwaltung, FUN = sum)

# Merging Dataframes
data.combined.AllgemeineVerwaltung <- merge(data.predictors.scaled, data.financial.AllgemeineVerwaltung, by = c("YEAR", "GEMEINDE"))

######################## COMBINING DEPARTMENT SPECIFIC FINANCIAL DATA INTO 1 DATAFRAME ###################################

all.departments.combined.data <- list(
  Gesundheit = data.combined.Gesundheit,
  KultSportFrei = data.combined.KultSportFrei,
  OrdnungSicherheit = data.combined.OrdnungSicherheit,
  SozialeSicherheit = data.combined.SozialeSicherheit,
  AllgemeineVerwaltung = data.combined.AllgemeineVerwaltung,
  FinanzenSteuern = data.combined.FinanzenSteuern,
  UmweltRaumordnung = data.combined.UmweltRaumordnung,
  VerkehrNachrichten = data.combined.VerkehrNachrichten,
  Volkswirtschaft = data.combined.Volkswirtschaft
)