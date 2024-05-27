# Hinweise	
# 
# Spalten	
# EINHEIT	Identifikator Körperschaft im GEFIS-System Kanton ZH
# GDEART	Typ der Körperschaft
# KONTOBEZEICHNUNG_RLG	Kontobezeichnung gemäss Standardbezeichnung HRM2-Kontenplan, keine Originale
# 
# Codes	
# GDEART	Abbildung struktureller Unterschiede
# PS	Einheitsgemeinde (Politische Gemeinde, Primarschule, Sekundarschule)
# PP	Einheitsgemeinde (Politische Gemeinde, Primarschule)
# P	Einheitsgemeinde (Politische Gemeinde)
# 
# Rechnungslegung HRM1 / HRM2
# HRM2 und HRM1 haben eine vergleichbare Systematik.
# HRM2 ist genauer als HRM2 (HRM2: Funktion 4-stellig, Konto 6-stellig, HRM1: Funktion 3-stellig, Konto 4-stellig)
# Anwendungsregel für Vergleiche über die HRM1/HRM2: Je aggregierter desto vergleichbarer. Aggregation auf 1-Stufe ist vollständig gegeben.
# 
# Strukturen Körperschaften
# Wie in Codes ersichtlich sind die rechnungslegenden Körperschaften strukturell nicht identisch. Dies hat überwiegend Auswirkungen auf Funktion 2 (Bildung).
# In der Gemeindefinanzstatistik (GEFIS) wird bei jeder strukturellen Anpassung eine neue ID für EINHEIT vergeben. Die Zeitreihe kann über BFS-Nummer erfolgen.
# Fusion: Die GEFIS-Daten entsprechen der tatsächlichen Lieferung zum Lieferzeitpunkt. Der Stand vor Fusion einer fusionierten Gemeinde muss gegebenenfalls aus den vorangehenden Gemeinden berechnet werden.

df <- read.csv("data_raw/gemeinde_finanzen/gefis_data_2013_2022.csv", sep=";")

dim(df)

head(df)

unique(df$RGJAHR)

# column names
# Hier handelt es sich bei 
#   SOLL um Auszahlungen, bei 
#   HABEN um Einnahmen
# Soll: Stellt in der Buchhaltung Vermögenswerte dar, 
#   zum Beispiel verfügbares Vermögen aus Bank- und Barvermögen. 
# Haben: Stellt in der Buchhaltung Verbindlichkeiten dar. 
#   zum Beispiel hierfür lang- und kurzfristige Kredite und Schulden.

table(df$GDEART)

length(unique(df$EINHEIT_BEZ))

# Gemeinden mit den meisten Ausgaben
df_gde_soll <- aggregate(SOLL ~ EINHEIT_BEZ, FUN=sum, data=df)
df_gde_soll[order(df_gde_soll$SOLL, decreasing = T),]

# saldo ist innerhalb und über alle gemeinden hinweg 0 
df_gde_soll <- aggregate(SALDO)
sum(df_gde_soll[,df_gde_soll$SALDO])