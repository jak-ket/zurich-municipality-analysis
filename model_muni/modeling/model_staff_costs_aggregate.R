# create model to estimate staff costs by gemeinde features
# output gemeinden that deviate most from their estimated staff costs

PKEY <- c("municipality", "year")

# read in municipality portrait and finance data
dfp <- read.csv("data/data_5636189_wide_filled.csv", check.names=FALSE)
stopifnot(dim(dfp)==c(11066,284))
stopifnot(sum(is.na(dfp)) == 0)
stopifnot(sum(PKEY %in% colnames(dfp)) == 2)

dff <- read.csv("data/gefis_data_2013_2022_agg.csv")
stopifnot(dim(dff)==c(1336,3))
stopifnot(sum(is.na(dff)) == 0)
stopifnot(sum(PKEY %in% colnames(dff)) == 2)

# check types
# head(str(dff), 20)
# tail(str(dff), 20)

dfm <- merge(dff, dfp, by=PKEY)
head(dfm[,1:5])
dim(dfm)

length(unique(dff$municipality))
length(unique(dfp$municipality))
length(unique(dfm$municipality))

length(unique(dff$year))
length(unique(dfp$year))
length(unique(dfm$year))
colnames(dfm)

# remove very big munis
# dfm <- dfm[dfm["Bevölkerung [Pers.]"] < 1e5,]

# train-test-split along municipalities
sample_size <- floor(0.75 * length(unique(dfm$municipality))) ## 75% of the sample size
set.seed(123) ## set the seed to make your partition reproducible
train_muni <- sample(unique(dfm$municipality), size = sample_size)
train_ind <- dfm$municipality %in% train_muni

# remove municipality
non_predictor_cols <- c("municipality")
df <- dfm[,!colnames(dfm) %in% non_predictor_cols]

# transform predictors
# predictor_cols = colnames(df)!="total_staff_costs"
# df[,predictor_cols] <- log(df[,predictor_cols] + 1)
# dim(df)
# dim(na.omit(df))
# df <- df[, colSums(is.na(df)) == 0]

# transform target
# df$log_total_staff_costs <- log(df$total_staff_costs)
# df$total_staff_costs <- NULL

# train and test
df_train <- df[train_ind,]
df_test <- df[!train_ind,]
stopifnot(dim(df_train)[1] + dim(df_test)[1] == dim(df)[1])

# short-hands
predictor_cols = colnames(df)!="total_staff_costs"
X_train <- (df_train[,predictor_cols])
y_train <- log(df_train$total_staff_costs)
X_test <- (df_test[,predictor_cols])
y_test <- log(df_test$total_staff_costs)

# LM
linreg <- lm(y_train ~ ., data=cbind(y_train, X_train)) # model on log scale
# linreg$coefficients[abs(linreg$coefficients) > 0.5]
summary(linreg)$adj.r.squared # 0.9848241
y_train_pred_lm <- linreg$fitted.values # predictions on log scale, same as 
y_test_pred_lm <- predict(linreg, newdata = X_test)
head(y_test_pred_lm)
plot(linreg, which=1)
plot(linreg, which=2)

# mse on log scale
mse.train <- (mean((y_train_pred_lm - y_train)^2)) # equal to sqrt(mean(linreg$residuals^2))
mse.train
mse.test <- (mean((y_test_pred_lm - y_test)^2))
mse.test

plot(y_train, y_train_pred_lm, main="Train set: True vs Predicted on log scale")
abline(a=0, b=1)
plot(y_test, y_test_pred_lm, main="Test set: True vs Predicted on log scale")
abline(a=0, b=1)

plot(exp(y_train), exp(y_train_pred_lm), main="Train set: True vs Predicted on original scale")
abline(a=0, b=1)
plot(exp(y_test), exp(y_test_pred_lm), main="Test set: True vs Predicted on original scale")
abline(a=0, b=1)

# look at efficieny
y <- c(y_train, y_test)
X <- rbind(X_train, X_test)
linreg_eff <- lm(y ~ ., data=cbind(y, X)) # model on log scale
summary(linreg_eff)$adj.r.squared # 0.9848241
y_train_pred_lm_eff <- linreg_eff$fitted.values # predictions on log scale, same as 
# y_true - y_pred > 0 => muni has higher costs than predicted => bad, should get consulting
# y_true - y_pred < 0 => muni has less costs than predicted => great
df_eff <- data.frame(municipality = dfm[,"municipality"], year = dfm$year, population = dfm[,"Bevölkerung [Pers.]"], y_relative_deviation=(y - y_train_pred_lm_eff)/y)
plot(df_eff$population, df_eff$y_relative_deviation) # there are also big cities that deviate heavily
df_eff_agg <- aggregate(y_relative_deviation ~ municipality, FUN=mean, data=df_eff) 
df_eff_agg[order(df_eff_agg$y_relative_deviation, decreasing=T),]
View(df_eff_agg)
hist(df_eff_agg$y_relative_deviation, breaks=3)
write.csv(df_eff_agg, file="efficiency.csv", row.names=F)

# stuff
# rmse.train <- sqrt(mean((y_train_pred_lm - y_train)^2)) # 7795913 <- 1.04484 <- 1.062937
# rmse.train/1e6
# rmse.test <- sqrt(mean((y_test_pred_lm - y_test)^2)) # 13976612 <- 1.953457e+14 <- 8.378263e+16
# rmse.test/1e6
# mse.train  <- sum(exp(linreg$residuals)^2)/length(y_train) 
# mse.test  <- sum((y_test_pred_lm - y_test)^2)/length(y_test) 

###############################
# LASSO
###############################
# train and test
df_train <- df[train_ind,]
df_test <- df[!train_ind,]
stopifnot(dim(df_train)[1] + dim(df_test)[1] == dim(df)[1])

# scale features
# means_train <- colMeans(df_train)
# View(means_train)
# sds_train <- apply(df_train, 2, sd)
# View(sds_train)
df_train[,predictor_cols] = scale(df_train[,predictor_cols]) # (df_train[,predictor_cols] - means_train) / sds_train
df_test[,predictor_cols] = scale(df_test[,predictor_cols]) # (df_test[,predictor_cols] - means_train) / sds_train

# short-hands
X_train <- as.matrix(df_train[,predictor_cols])
y_train <- log(df_train$total_staff_costs)
X_test <- as.matrix(df_test[,predictor_cols])
y_test <- log(df_test$total_staff_costs)

library(glmnet)
lasso <- cv.glmnet(x=X_train, y=y_train, alpha=1, nfolds = 10,)
# lasso <- cv.glmnet(x=rbind(X_train, X_test), y=c(y_train, y_test), alpha=1, nfolds = 10,)
lasso$lambda.min
plot(lasso)
plot(lasso$glmnet.fit)

y_train_pred_lasso <- predict(lasso, newx=X_train, s=lasso$lambda.min)
y_test_pred_lasso <- predict(lasso, newx=X_test, s=lasso$lambda.min)

plot(y_train, y_train_pred_lasso, main="True vs Predicted")
abline(a=0, b=1)
plot(y_test, y_test_pred_lasso, main="True vs Predicted")
abline(a=0, b=1)

mse.train <- (mean((y_train_pred_lasso - y_train)^2))
mse.train
mse.test <- (mean((y_test_pred_lasso - y_test)^2))
mse.test

# evaluate performance on test set in CHF
# mean(abs(exp(y_test_pred_lasso) - exp(y_test)))/1e6
mean(exp(abs(y_test_pred_lasso - y_test)))
# mean(exp(y_test))/1e6

# Extract coefficients
lasso_coefficients <- coef(lasso)
# Identify non-zero coefficients and corresponding feature names
non_zero_indices <- which(lasso_coefficients != 0, arr.ind = TRUE)
non_zero_features <- rownames(lasso_coefficients)[non_zero_indices[,1]]
non_zero_coeffs <- lasso_coefficients[non_zero_indices]
non_zero_coeffs_df <- data.frame(Feature = non_zero_features, Coefficient = non_zero_coeffs)
non_zero_coeffs_df <- non_zero_coeffs_df[order(non_zero_coeffs_df$Coefficient, decreasing = T),]
print(non_zero_coeffs_df)
View(non_zero_coeffs_df)

length(non_zero_coeffs)


###############################
# GROUP LASSO
###############################

# install.packages("grplasso")
library(grplasso)

# define the groups
group_lasso_features <- read.csv("column_names_group_lasso.txt", check.names=FALSE, sep="\n")$colname
stopifnot(length(setdiff(group_lasso_features, colnames(X_train))) == 0)

group_assignments <- c(48,
                       1, 1, 1, 1, 1, 1, 1, 1, 1,
                       2, 2, 2, 2,
                       3, 3, 3, 3, 3, 3, 3, 3, 3,
                       4, 4, 4,
                       5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
                       6, 6, 6, 6, 6, 6,
                       7, 7, 7, 7, 7, 7, 7,
                       8, 8, 8, 8,
                       9, 9,
                       10, 10, 10, 10, 10, 10,
                       11, 11, 11, 11,
                       12, 12, 12,
                       13, 13, 13, 13,
                       14, 14, 14, 14,
                       15, 15, 15, 15,
                       16, 16, 16, 16, 16, 16, 16, 16,
                       17, 17, 17, 17, 17, 17, 17,
                       18, 18,
                       19, 19, 19,
                       20, 20, 20, 20,
                       21, 21, 21, 21, 21, 21, 21,
                       22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22,
                       23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 
                       24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24,
                       25, 25, 25,
                       26, 26, 26, 26, 26, 26,
                       27, 27,
                       28, 28, 28,
                       29,
                       30, 30, 30, 30,
                       31, 31, 31, 31,
                       32, 32, 32, 32, 32,
                       33, 33, 33, 33, 33,
                       34, 34, 34, 34, 34, 34,
                       35, 35,
                       36, 36,
                       37,
                       38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38,
                       39, 39, 39, 39, 39, 39, 39, 39,
                       40, 40, 40, 40, 40, 40,
                       41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41,
                       42, 42, 42,
                       43,
                       44, 44,
                       45,
                       46, 46, 46,
                       47, 47, 47)
stopifnot(length(group_assignments) == length(group_lasso_features))

groups.predictors <- data.frame(
  Predictor = group_lasso_features,
  Group = group_assignments)
View(groups.predictors)

# 1 = "Bevölkerungsbestand"
# 2 = "Bevölkerungsentwicklung"
# 3 = "Altersstruktur"
# 4 = "Konfession"
# 5 = "Haushalte"
# 6 = "Zuzüge und Wegzüge"
# 7 = "Geburten und Sterbefälle"
# 8 = "Heiraten und Scheidungen"
# 9 = "Bürgerrechtswechsel"
# 10 = "Schüler Volksschule"
# 11 = "Schüler Mittelschule und Berufsbildung"
# 12 = "Schüler nach ISCED"
# 13 = "Steuerbares Einkommen"
# 14 = "Steuerbares Vermögen"
# 15 = "Aerzte und Apotheken"
# 16 = "Arbeitsstätten/Betriebe"
# 17 = "Beschäftigung/Vollzeitäquivalente"
# 18 = "Betriebe Landwirtschaft"
# 19 = ""Beschäftigte Landwirtschaft""
# 20 = "Vieh"
# 21 = "Hotels/Restaurants"
# 22 = "Gebäudevolumen"
# 23 = "Bauinvestitionen"
# 24 = "Wohnungsbestand"
# 25 = "Wohnbautätigkeit"
# 26 = "Bauzonen"
# 27 = "Bauzonenverbrauch"
# 28 = "Abfall"
# 29 = "Verbrauch"
# 30 = "Motorfahrzeugbestand"
# 31 = "Motorfahrzeug-Neuzulassungen"
# 32 = "PW-Bestand nach Antriebsart"
# 33 = "PW-Neuzulassungen nach Antriebsart"
# 34 = "Erschliessung"
# 35 = "Verkehrsmittelwahl"
# 36 = "Verkehrsaufkommen"
# 37 = "Unfälle"
# 38 = "Gemeindekennziffern"
# 39 = "Steuergrundlagen"
# 40 = "Steuerkraft"
# 41 = "Steuererträge"
# 42 = "ordentlich Besteuerte"
# 43 = "Personalsteuerpflichtige"
# 44 = "Quellensteuerpflichtige"
# 45 = "juristische Personen"
# 46 = "Kirchensteuerpflichtige"
# 47 = "Steuerfüsse"
# 48 = "Year"

# design matrix
X_train_design <- X_train[,group_lasso_features]
intercept <- cbind(intercept=rep(1, dim(X_train)[1]))
X_train_design <- cbind(intercept, X_train_design)

# add intercept 
group_assignments = c(NA, group_assignments)
stopifnot(length(group_assignments) == dim(X_train_design)[2])

# find the lambda that leads to all groups = zero => upper bound 
lambdamax <- lambdamax(
    x=X_train_design, y=y_train, index=group_assignments, penscale = sqrt, model = LinReg(), 
    center = FALSE, standardize = FALSE
) 
lambdas <- lambdamax * 0.5^(0:10) # create grid of lambdas
lambdas <- 1000

# fit Group Lasso for every lambda
group_lasso <- grplasso(
    x=X_train_design, y=y_train, index=group_assignments, penscale = sqrt, model = LinReg(),
    control = grpl.control(update.hess = "lambda", trace = 0), 
    lambda = lambdas, center=F, standardize=F
)

y_pred = predict(group_lasso, newdata=X_train_design) 
residuals <- y_train - y_pred

# TA plot
plot(y_pred, residuals, main="Tukey-Anscombe Plot")
abline(a=0, b=0, col="black")

# QQ plot
qqnorm(residuals, main="test")
qqline(residuals)





group_lasso$lambda

View(group_lasso$coefficients)
plot(group_lasso)

length(predict(group_lasso, newdata=X_train_design)[,1])


################################################
################################################

# library(grplasso)

# # find the lambda that leads to all groups = zero => upper bound for lambda 
# lambdamax <- lambdamax(
#     x=X, y=y, index=group_assignments, model = LinReg()
# ) 

# # create array of 20 candidate lambdas
# lambdas <- lambdamax * 0.5^(0:19) 

# # fit Group Lasso for every lambda
# group_lasso <- grplasso(
#     x=X, y=Y, index=group_assignments, model = LinReg(), lambda = lambdas 
# )

# # plot lambdas vs. coefficients
# plot(group_lasso)


