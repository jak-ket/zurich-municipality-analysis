df <- read.csv("data_raw/gemeinde_finanzen/gefis_data_2013_2022.csv", sep=";")
dim(df)
head(df)

# identify the relevant KONTO numbers: 
# include: general staff costs, Dienstleistungen Dritter 
# exclude: education costs (sometimes spread across munis)

# personal: naive KONTO filtering
# KONTO_BEZ_RLG_personal <- unique(df[(df$KONTO >= 3000) & (df$KONTO <= 3099), "KONTO_BEZ_RLG"])
# filter_personal <- (df$KONTO >= 3000) & (df$KONTO <= 3099) 

# personal: filtering by hand-crafted strings 
KONTO_BEZ_RLG_personal <- c("Behörden und Kommissionen", "Allgemeiner Personalaufwand", "Renten, Ruhegehälter", "Besoldungszulagen", "Aushilfsentschädigungen", "Arbeitgeberbeiträge Pensionsversicherung", "Arbeitgeberbeiträge übrige Pers.-Vers.", "Löhne des Verwaltungs- und Betriebspersonals",  "Löhne Verwaltungs-, Betriebspersonal",  "Erstattung von Lohn des Verwaltungs- und Betriebspersonals",  "Entschädigungen für temporäre Arbeitskräfte",  "AG-Beiträge AHV, IV, EO, ALV, Verwaltungskosten",  "AG-Beiträge an Pensionskasse",  "Sozialleistungen",  "AG-Beiträge an Familienausgleichskasse",  "AG-Beiträge an Krankentaggeldversicherungen",  "Aus- und Weiterbildung des Personals",  "Übriger Personalaufwand")
filter_personal <- df$KONTO_BEZ_RLG %in% KONTO_BEZ_RLG_personal

# more filters
filter_dritte <- df["KONTO_BEZ_RLG"] == "Dienstleistungen Dritter"
filter_bildung <- grepl("Bildung|bildung", df$FUNKTION_BEZ_RLG)

# filter for relevant KONTO numbers
filters <- (filter_personal | filter_dritte) & !filter_bildung
# summary(filter_personal|filter_dritte)
df <- df[filters,]
dim(df)

# unify column names for aggregation
dfagg <- df[,c("EINHEIT_BEZ", "RGJAHR", "SALDO")]
colnames(dfagg) <- c("municipality", "year", "total_staff_costs") 
head(dfagg)

# remove pre- and postfixes in municipality names
dfagg$municipality <- gsub("Politische Gemeinde ", "", dfagg$municipality)
dfagg$municipality <- gsub("Stadt ", "", dfagg$municipality)
dfagg$municipality <- gsub(" \\(bis\\s\\d{4}\\)", "", dfagg$municipality) # reduces number of unique municipalities as some appear with (bis 2022) and without this bracket
unique(dfagg$municipality)

# group all staff costs per year and municipality
dfagg <- aggregate(dfagg, total_staff_costs ~ year + municipality, sum)
head(dfagg,30)

# sanity checks
stopifnot(length(unique(dfagg$municipality))==172) 

write.csv(dfagg, file="data/gefis_data_2013_2022_agg.csv", row.names=F)

