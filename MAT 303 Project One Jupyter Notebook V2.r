
housing <- read.csv(file="housing_v2.csv", header=TRUE, sep=",")

# converting appropriate variables to factors  
housing <- within(housing, {
   view <- factor(view)
   backyard <- factor(backyard)
})

# number of columns
ncol(housing)

# number of rows
nrow(housing)

# Print the first 10 rows
# print("head")
# head(housing, 10)

# We then create a couple of scatterplots, price vs. living area and price vs. age
plot(housing$sqft_living, housing$price, 
     main = "Scatterplot of Price against Living Area",
     xlab = "Living Area", ylab = "Price",
     col="blue", 
     pch = 19, frame = FALSE)

plot(housing$age, housing$price, 
     main = "Scatterplot of Price against Age",
     xlab = "Age", ylab = "Price",
     col="blue", 
     pch = 19, frame = FALSE)

# subsetting the data to only the required variables for the correlation matrices
# myvars <- c("price","sqft_living","age")
# housing_subset <- housing[myvars]

# Print the correlation matrix
# print("Pearson Correlation Coefficient Matrix")
# corr_matrix <- cor(housing_subset, method = "pearson")
# round(corr_matrix, 4)



housing <- read.csv(file="housing_v2.csv", header=TRUE, sep=",")

# converting appropriate variables to factors  
housing <- within(housing, {
   view <- factor(view)
   backyard <- factor(backyard)
})

# Create the regression model and print summary statistics. 
model_1 <- lm(price ~ sqft_living + sqft_above + age + bathrooms + view, data=housing)
# summary(model_1)


# We now find fitted values and residuals
# fitted_values <- fitted.values(model_1) 
# fitted_values

#residuals <- residuals(model_1)
# residuals

# We then plot the fitted values against the residuals
#plot(fitted_values, residuals, 
#     main = "Residuals against Fitted Values",
#     xlab = "Fitted Values", ylab = "Residuals",
#     col="blue", 
#     pch = 19)

# We then produce a Normal Q-Q plot
#qqnorm(residuals, pch = 19, col="blue", frame = FALSE)
#qqline(residuals, col = "red", lwd = 2)

# We now make prediction and confidence intervals with the information provided from the problem statement
# newdata <- data.frame(sqft_living=2150, sqft_above=1050, age=15, bathrooms=3, view='0')

#print("Prediction interval")
#prediction_pred_int <- predict(model_1, newdata, interval="predict", level=0.90) 
#round(prediction_pred_int, 4)

#print("Confidence interval")
#prediction_conf_int <- predict(model_1, newdata, interval="confidence", level=0.90) 
#round(prediction_conf_int, 4)


# We now make prediction and confidence intervals with the information provided from the problem statement
newdata <- data.frame(sqft_living=4250, sqft_above=2100, age=5, bathrooms=5, view='2')

print("Prediction interval")
prediction_pred_int <- predict(model_1, newdata, interval="predict", level=0.90) 
round(prediction_pred_int, 4)

print("Confidence interval")
prediction_conf_int <- predict(model_1, newdata, interval="confidence", level=0.90) 
round(prediction_conf_int, 4)


housing <- read.csv(file="housing_v2.csv", header=TRUE, sep=",")

# converting appropriate variables to factors  
housing <- within(housing, {
   view <- factor(view)
   backyard <- factor(backyard)
})

# Create a scatterplot of Price and School Rating
   plot(housing$school_rating, housing$price, 
       main = "Scatterplot of Price and School Rating",
        xlab = "School Rating", ylab = "Price",
        col="blue", 
        pch = 19, frame = FALSE)

# Create a scatterplot of Price and Crime rate per 100,000
   plot(housing$crime, housing$price, 
       main = "Scatterplot of Price and Crime",
        xlab = "Crime (per 100,000 residents)", ylab = "Price",
        col="blue", 
        pch = 19, frame = FALSE)





housing <- read.csv(file="housing_v2.csv", header=TRUE, sep=",")

# Create the second order regression model and print the statistics
model_2 <- lm(price ~ school_rating + crime + school_rating:crime + I(school_rating^2) + I(crime^2), data=housing)
# summary(model_2)

# We now find fitted values and residuals
# fitted_values <- fitted.values(model_2) 
# fitted_values

# residuals <- residuals(model_2)
# residuals

# We then plot the fitted values against the residuals
#plot(fitted_values, residuals, 
#     main = "Residuals against Fitted Values",
#     xlab = "Fitted Values", ylab = "Residuals",
#     col="blue", 
#     pch = 19)

# We then produce a Normal Q-Q plot
#qqnorm(residuals, pch = 19, col="blue", frame = FALSE)
#qqline(residuals, col = "red", lwd = 2)

# We now make prediction and confidence intervals with the information provided from the problem statement
#newdata <- data.frame(school_rating=9.80, crime=81.02)

#print("Prediction interval")
#prediction_pred_int <- predict(model_2, newdata, interval="predict", level=0.90) 
#round(prediction_pred_int, 4)

#print("Confidence interval")
#prediction_conf_int <- predict(model_2, newdata, interval="confidence", level=0.90) 
#round(prediction_conf_int, 4)

# We now make prediction and confidence intervals with the information provided from the problem statement
newdata <- data.frame(school_rating=4.28, crime=215.50)

print("Prediction interval")
prediction_pred_int <- predict(model_2, newdata, interval="predict", level=0.90) 
round(prediction_pred_int, 4)

print("Confidence interval")
prediction_conf_int <- predict(model_2, newdata, interval="confidence", level=0.90) 
round(prediction_conf_int, 4)




housing <- read.csv(file="housing_v2.csv", header=TRUE, sep=",")

# Create the model, first order plus interaction term
model_3 <- lm(price ~ school_rating + crime + school_rating:crime, data=housing)
#summary(model_3)

# Bring forward the complete, second order model generated in Section 2 above
model_3_complete <- lm(price ~ school_rating + crime + school_rating:crime + I(school_rating^2) + I(crime^2), data=housing)
#summary(model_3_complete)

# Create the reduced model, without the quadratic terms
model_3_reduced <- lm(price ~ school_rating + crime + school_rating:crime, data=housing)
#summary(model_3_reduced)

# Perform the nested F-test
anova(model_3_complete, model_3_reduced)


