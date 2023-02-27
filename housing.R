# libraries
library(olsrr)
library(caret)
library(car)



# import csv file
df = read.csv(file=
                "C:/Users/Aryan/OneDrive/Documents/Rutgers Fall2022-23/Regression Methods/Project/Housing.csv",header = TRUE)

# data set is regression of the price of NY housing
head(df)

# file directory to save plots
# saved location of pdf file
pdf(file = "C:/Users/Aryan/OneDrive/Documents/Rutgers Fall2022-23/Regression Methods/Project/Plots.pdf")

# cleaning data
# label encoding
df$mainroad = as.numeric(factor(df$mainroad))
df$guestroom = as.numeric(factor(df$guestroom))
df$basement  = as.numeric(factor(df$basement ))
df$hotwaterheating = as.numeric(factor(df$hotwaterheating))
df$airconditioning  = as.numeric(factor(df$airconditioning ))
df$prefarea  = as.numeric(factor(df$prefarea ))
df$furnishingstatus = as.numeric(factor(df$furnishingstatus))


# scatter plot 
# visualization of correlation between variables
pairs( price ~., data=df, pch=19, lower.panel = NULL)

# there seems to exists multicolinearity 
vif(lmod)
# no value greater than 2 all less than 10
# we can ignore multicolinearity issue


# 1. creating full model
# 12 predictors
lmod = lm(price~., data=df)

# anova table
anova(lmod)
# seems like all predictors are significant

# summary
summary(lmod)

# best model for n parameters
ols_step_best_subset(lmod, print_plot=True )
# closest Cp 
# highest R^2 adj
# 11 - area bathrooms stories mainroad guestroom basement hotwaterheating airconditioning parking prefarea furnishingstatus 
# 12 - all

# backward selection parameters
ols_step_backward_p(lmod)
# did not negelct any predictors

# lets see if this method agrees
ols_step_both_p(lmod)

# create partial models

# 11 paras
lmod1 = lm(price~ area + bathrooms + stories + mainroad + guestroom + 
           basement + hotwaterheating + airconditioning +
             parking + prefarea + furnishingstatus, data=df)



summary(lmod1)


# compare to full model
anova(lmod, lmod1)
# singinificant diff
# lmod 1 RMSE lower than full model 




# so model of 10 parameters has most significance

#2. K folds
# to get average rmse
set.seed(13245)
train.control <- trainControl(method = "cv", number = 10)
# Train the model
model <- train(price~.  , data=df, method = "lm", trControl = train.control)

model2 <- train(price~ area + bathrooms + stories + mainroad + guestroom + 
                  basement + hotwaterheating + airconditioning +
                  parking + prefarea + furnishingstatus , data=df, method = "lm", trControl = train.control)

print(model)
print(model2)

ols_plot_resid_qq(lmod1)
ols_test_correlation(lmod1)
# majority of points follow slope
# same distribution

#g.Residual vs fitted plot
ols_plot_resid_fit(lmod1)

#h.Studentized residuals vs leverage plot
ols_plot_resid_lev(lmod1)

#d.Observed vs fitted values plot
ols_plot_obs_fit(lmod1)

##----------------------------------------------------------------------------#
# to create the file
dev.off()

