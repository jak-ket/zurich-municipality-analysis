library(zoo)
options("width"=200)

# read data
dfw <- read.csv("data/data_5636189_wide.csv", check.names=FALSE)
stopifnot(dim(dfw)==c(11066, 312))
stopifnot(sum(is.na(dfw[,c("municipality", "year")])) == 0)
head(colnames(dfw))

# look at missings per row
dfw_row_missings <- rowSums(is.na(dfw))
cor(dfw$year, dfw_row_missings) # -0.8553744 => less missings for more recent years

# look at missings per feature
dfw_col_missings <- data.frame(column=colnames(dfw), n_col_missings=colSums(is.na(dfw)), ratio=colSums(is.na(dfw)) / dim(dfw)[1])
rownames(dfw_col_missings[dfw_col_missings$ratio > 0.7,]) # mostly wahl features

# look at missings per feature for relevant time frame
time_filter <- dfw[,"year"]>=2013
dfw_col_missings_recent <- data.frame(column=colnames(dfw[time_filter,]), n_col_missings=colSums(is.na(dfw[time_filter,])), ratio=colSums(is.na(dfw[time_filter,])) / dim(dfw[time_filter,])[1])
dfw_col_missings_recent[order(dfw_col_missings_recent$ratio, decreasing = T), c("column", "ratio")]
rownames(dfw_col_missings_recent[dfw_col_missings_recent$ratio > 0.4,]) # mostly wahl features

# remove features with extreme missings
# ISSUE: missings also include many very relevant characteristics such as area of muni

# forward missings per municipality with years in order
dfw <- dfw[order(c(dfw$municipality, df$year)),]
dfw_locf <- do.call("rbind", by(dfw, dfw$municipality, na.locf, na.rm = FALSE)) # forward fill
head(dfw_locf[,1:4], 20)

# again look at columns with extreme missings
dfw_col_missings_recent <- data.frame(column=colnames(dfw_locf[time_filter,]), n_col_missings=colSums(is.na(dfw_locf[time_filter,])), ratio=colSums(is.na(dfw_locf[time_filter,])) / dim(dfw_locf[time_filter,])[1])
dfw_col_missings_recent[order(dfw_col_missings_recent$ratio, decreasing = T), c("column", "ratio")]
rownames(dfw_col_missings_recent[dfw_col_missings_recent$ratio > 0.4,]) # mostly wahl features

# backward fill
dfw_filled <- do.call("rbind", by(dfw_locf, dfw_locf$municipality, na.locf, fromLast = TRUE, na.rm = FALSE)) # backward fill
head(dfw_filled[,1:4], 20)

# which columns still have missings? remove!
dfw_filled_col_missings <- data.frame(column=colnames(dfw_filled), n_col_missings=colSums(is.na(dfw_filled)), ratio=colSums(is.na(dfw_filled)) / dim(dfw_filled)[1])
#dfw_filled_col_missings <- dfw_filled_col_missings[order(dfw_filled_col_missings$ratio),]
dfw_filled_col_missings[dfw_filled_col_missings$ratio > 0,]
missing_cols <- rownames(dfw_filled_col_missings[dfw_filled_col_missings$ratio > 0,])
dfw_filled[,missing_cols] <- NULL
stopifnot(sum(is.na(dfw_filled)) == 0)
stopifnot(dim(dfw_filled)[2] == 295)

# testing the fill 
# t <- dfw[,c("municipality", "year", "Wahlbeteiligung Nationalrat [%]", "Wahlbeteiligung Kantonsrat [%]")]
# tf <- do.call("rbind", by(t, t$municipality, na.locf, na.rm = F))
# tf <- do.call("rbind", by(tf, tf$municipality, na.locf, fromLast = TRUE, na.rm = F))
# head(merge(t, tf, by=c("municipality", "year")), 100)

# still have data for all years and municipalities
stopifnot(length(unique(dfw_filled$year)) == length(unique(dfw$year)))
stopifnot(length(unique(dfw_filled$municipality)) == length(unique(dfw$municipality))) # we loose munis!

# check types
head(str(dfw_filled), 20)
tail(str(dfw_filled), 20)

# checkpoint wide missings filled
write.csv(dfw_filled, file="data/data_5636189_wide_filled.csv", row.names=F)

# graveyard
#for(col in colnames(dfw)[3:dim(dfw)[2]]){
    # dfw[,col] <- ave(df)
# }
# dfw$wahl_filled <- ave(dfw[,c("Wahlbeteiligung Nationalrat [%]")], dfw$municipality, FUN=na.locf)
# dfw[dfw$municipality=="Aeugst a.A.","Wahlbeteiligung Nationalrat [%]"]
# head(t[,c("year", "municipality", "Wahlbeteiligung Nationalrat [%]", "wahl_filled")], 100)