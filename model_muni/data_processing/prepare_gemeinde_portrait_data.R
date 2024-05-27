# create wide dataframe with muni(cipality) portrait data
# dimension: year x municipalities x 1 (feature value)
# https://wwlenw.web.statistik.zh.ch/gpv2/ 

# imports
library(zoo)

# read raw data
df <- read.csv("data_raw/gemeinde_portrait/data_5636189.csv", sep=";")
dim(df)
head(df)

unique(df$GEBIET_NAME)


# are there munis with 100 % missings for some features?
df[df=="null"] <- NA
df$value_missing <- is.na(df$INDIKATOR_VALUE)
sum(df$value_missing)
missings_per_muni_and_feature <- aggregate(df, value_missing ~ GEBIET_NAME + INDIKATOR_NAME, sum)
missings_per_muni_and_feature$ratio <- missings_per_muni_and_feature[,"value_missing"] / (length(unique(df$INDIKATOR_JAHR)))
head(missings_per_muni_and_feature[missings_per_muni_and_feature$ratio>0.5,], 30)
max(missings_per_muni_and_feature$ratio) # 77 %


relevant_cols <- c(
    "GEBIET_NAME", 
    "INDIKATOR_JAHR", 
    "INDIKATOR_NAME", 
    "INDIKATOR_VALUE"
)

# put dataframe from long to wide (one column per feature)
dfw <- reshape(
  df[,relevant_cols],
  idvar = c("GEBIET_NAME", "INDIKATOR_JAHR"),
  timevar = "INDIKATOR_NAME",
  direction = "wide",
)
dim(dfw)

# we want number of added columns be equal to number of unique features
stopifnot(length(unique(df$INDIKATOR_NAME)) == dim(dfw)[2]-2)

head(colnames(dfw))

# do we have all years for each gemeinde?
length(unique(dfw$GEBIET_NAME)) * length(unique(dfw$INDIKATOR_JAHR)) # 11151
# 11066 / 11151

# analzye missings over years and munis
dfw_row_missings <- rowSums(is.na(dfw))

dfw[,"n_row_missings"] <- dfw_row_missings

# missings vary greatly over years; U-shape: many missings for very old and recent years
missings_per_year <- aggregate(dfw, n_row_missings ~ INDIKATOR_JAHR, sum)
missings_per_year$ratio <- missings_per_year[,"n_row_missings"] / (length(unique(dfw$GEBIET_NAME))*(dim(dfw)[2]-2))
plot(missings_per_year$INDIKATOR_JAHR, missings_per_year$ratio)
missings_per_year

# missings ratio over munis is rather stable => many munis have same missings over years
missings_per_muni <- aggregate(dfw, n_row_missings ~ GEBIET_NAME, sum)
missings_per_muni$ratio <- missings_per_muni[,"n_row_missings"] / (length(unique(dfw$INDIKATOR_JAHR))*(dim(dfw)[2]-2))
summary(missings_per_muni)
boxplot(missings_per_muni$ratio)

# analyze missings over features
# there are many features with
dfw_col_missings <- data.frame(column=colnames(dfw), n_col_missings=colSums(is.na(dfw)), ratio=colSums(is.na(dfw)) / dim(dfw)[1])
dfw_col_missings[order(dfw_col_missings$ratio, decreasing = T), c("column", "ratio")]
boxplot(dfw_col_missings$ratio)

# for which years are the features missing?
time_filter <- dfw[,"INDIKATOR_JAHR"]>=2013
dfw_col_missings_filtered <- data.frame(column=colnames(dfw[time_filter,]), n_col_missings=colSums(is.na(dfw[time_filter,])), ratio=colSums(is.na(dfw[time_filter,])) / dim(dfw[time_filter,])[1])
dfw_col_missings_filtered[order(dfw_col_missings_filtered$ratio, decreasing = T), c("column", "ratio")]
boxplot(dfw_col_missings_filtered$ratio)

# make colnames consistent
colnames(dfw) <- gsub("INDIKATOR_VALUE.", "", colnames(dfw))
colnames(dfw)[1:2] <- c("municipality", "year") 

# remove missings information
dfw$n_row_missings <- NULL

# remove suffices in municipality names
dfw$municipality <- gsub(" \\(bis\\s\\d{4}\\)", "", dfw$municipality)
unique(dfw$municipality)

# remove features related to gemeinde-finances
colnames_aufwand <- as.list(colnames(dfw)[grepl("aufwand|Aufwand", colnames(dfw))])
stopifnot(length(dfw) == length(colnames_aufwand) + length(colnames(dfw)[!colnames(dfw) %in% colnames_aufwand]))
dfw <- dfw[,!colnames(dfw) %in% colnames_aufwand]
dim(dfw)

# checkpoint wide
write.csv(dfw, file="data/data_5636189_wide.csv", row.names=F)
