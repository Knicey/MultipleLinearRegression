# To install the dataset
#install.packages("palmerpenguins")
#
# see more details and a picture at 
# https://allisonhorst.github.io/palmerpenguins/

library(palmerpenguins)
library(dplyr)

head(penguins)

#select a small subset of features.
df <- select(penguins, bill_length_mm, flipper_length_mm, body_mass_g, sex)
summary(df)

df

#remove all the cases with any NA
df <- df[complete.cases(df),]
summary(df)

df <- df |> mutate(
  sexmale = case_when(
   sex == "male" ~ 1,
   sex == "female" ~ 0,
  )
)

# Design Matrix
Xmat <- cbind(1, df$flipper_length_mm, df$bill_length_mm, df$sexmale)
head(Xmat)

# The first column is our bias term
# The second column is the flipper length in mm
# The third column is the bill length in mm
# The fourth column is if that penguin is male

regMonLs <- lm(body_mass_g ~ flipper_length_mm + bill_length_mm + sexmale, df)
summary(regMonLs)

XtX <- t(Xmat) %*% Xmat
invXtX <- solve(XtX)

y <- df$body_mass_g
BetaHat <- invXtX %*% t(Xmat) %*% y

p  <- ncol(Xmat) - 1
n <- nrow(Xmat)
resid <- y - Xmat %*% BetaHat

RSE2 <- sum(resid^2)/(n - (p+1))

VarBetaHat <- RSE2 * invXtX

# Coefficients
coefficients = t(BetaHat)
coefficients

# Standard errors for the coefficients:
std_errors = sqrt(diag(VarBetaHat))
std_errors

# T-Values
t_values = coefficients/std_errors
t_values

# Based on the t-value threshold of 2, only Flipper Length and Sexmale
# are statistically significant predictors