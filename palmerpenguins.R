# To install the dataset
#install.packages("palmerpenguins")
#
# see more details and a picture at 
# https://allisonhorst.github.io/palmerpenguins/

library(palmerpenguins)
library(dplyr)

head(penguins)

#select a small subset of numerical features.
df <- select(penguins, bill_length_mm, flipper_length_mm, body_mass_g)
summary(df)

#remove all the cases with any NA
df <- df[complete.cases(df),]
summary(df)

plot(df$bill_length_mm, df$body_mass_g, 
     xlab="Bill Length (in mm)", ylab="Body mass (in g)", 
     main="Predict penguin body mass.")

plot(df$flipper_length_mm, df$body_mass_g, 
     xlab="Flipper Length (in mm)", ylab="Body mass (in g)", 
     main="Predict penguin body mass.")


# By doing a joint regression we can predict the mass better.
regMonLs <- lm(body_mass_g ~ flipper_length_mm + bill_length_mm, df)

Xmat <- cbind(1, df$flipper_length_mm, df$bill_length_mm)


y <- df$body_mass_g

XtX <- t(Xmat) %*% Xmat
invXtX <- solve(XtX)

# First we find BetaHat:
BetaHat <- invXtX %*% t(Xmat) %*% y

# Next, we find residuals:
p  <- ncol(Xmat) - 1
n <- nrow(Xmat)
resid <- y - Xmat %*% BetaHat

RSE2 <- sum(resid^2)/(n - (p+1))

VarBetaHat <- RSE2 * invXtX

#The standard errors for the coefficients:
sqrt(diag(VarBetaHat))

# matches with
summary(regMonLs)
