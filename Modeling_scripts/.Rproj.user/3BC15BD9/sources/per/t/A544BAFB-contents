library(MASS)
library(dplyr)
library(tidyr)
library(stringr)
library(AER)
library(car)
library(ggplot2)
library(ggeffects)
library(gridExtra)

##THIS code is all for ONE MODEL the first one run without Narcan added into it
##changing column name so it doesnt break my code
names(clean_complete)[names(clean_complete) == "Non-violent"] <- "Non_violent"
clean_complete


##Creating a predictive model off the regression one we just made
clean_complete$predicted_ems <- predict(step_model, type="response")

mae <- mean(abs(clean_complete$Total_ems - clean_complete$predicted_ems))
rmse <- sqrt(mean((clean_complete$Total_ems - clean_complete$predicted_ems)^2))
r_squared <- cor(clean_complete$Total_ems, clean_complete$predicted_ems)^2

cat("MAE:", mae, "\nRMSE:", rmse, "\nR-squared:", r_squared, "\n")

##creating a randomized dataset that is using the current one as a reference to test predictions
random_data <- function(n_samples=100, reference_data) {
  library(MASS)
 
  ##Calculate the correlation matrix from actual data
  corr_matrix <- cor(reference_data[, c("Total_ME", "Non_violent", "median_age", "median_income")])
  
  ##Calculate mean and SD
  means <- colMeans(reference_data[,c("Total_ME", "Non_violent", "median_age", "median_income")])
  sds <- apply(reference_data[,c("Total_ME", "Non_violent", "median_age", "median_income")], 2, sd)
  
  ##Generate multivariate normal data with the same correlation structure
  r_data <- mvrnorm(n_samples, mu=means, Sigma = corr_matrix*(sds%*%t(sds)))
  
  ##convert data into new data frame
  new_data <- as.data.frame(r_data)
  colnames(new_data) <- c("Total_ME", "Non_violent", "median_age", "median_income")
  
  ##ENsure values are reasonable
  new_data$Total_ME <- pmax(0, new_data$Total_ME)
  new_data$'Non_violent' <- pmax(0, new_data$'Non_violent')
  new_data$median_age <- pmax(1, new_data$median_age)
  new_data$median_income <- pmax(0, new_data$median_income)
  
  return(new_data)
}


new_data <- random_data(100, clean_complete)

##Using this new generated data to see what it predicts
new_data
new_data$predicted_EMS <- predict(step_model, newdata = new_data, type="response")


##creating a test data set where i can change one variable at a time to see what happens
sysm_data <- function(reference_data, variable_to_vary, n_steps=10) {
  
  #Get the means for all the values
  base_values <- colMeans(reference_data[, c("Total_ME", "Non_violent", "median_age", "median_income")])
  
  ##Create sequence for the variable we want to change
  var_range <- range(reference_data[[variable_to_vary]])
  var_sequence <- seq(var_range[1], var_range[2], length.out = n_steps)
  
  ##Create data fram with base values
  new_data <- as.data.frame(matrix(rep(base_values, n_steps),
                                   nrow=n_steps,
                                   byrow=TRUE))
  
  #set column names
  colnames(new_data) <- c("Total_ME", "Non_violent", "median_age", "median_income")
  
  #replace column to vary with sequence 
  new_data[[variable_to_vary]] <- var_sequence
  
  return(new_data)

}

test <- sysm_data(clean_complete, "Non_violent", 20)

test$predicted_EMS <- predict(step_model, newdata = test, type="response")

test

###graphing results
##this can be used to graph one variable at a time while holding all others at a constant mean 
visualize_systematic_predictions <- function(model, reference_data, variable_name, n_steps = 20) {
  # Generate data with systematic variations
  variation_data <- sysm_data(reference_data, variable_name, n_steps)
  
  # Generate predictions
  variation_data$predicted_ems <- predict(model, newdata = variation_data, type = "response")
  
  # Try to calculate confidence intervals if possible
  tryCatch({
    pred_with_se <- predict(model, newdata = variation_data, type = "link", se.fit = TRUE)
    critical_value <- qnorm(0.975)  # For 95% CI
    
    # Transform CI to response scale
    variation_data$lower_ci <- exp(pred_with_se$fit - critical_value * pred_with_se$se.fit)
    variation_data$upper_ci <- exp(pred_with_se$fit + critical_value * pred_with_se$se.fit)
    
    # Create plot with confidence intervals
    p <- ggplot(variation_data, aes_string(x = variable_name, y = "predicted_ems")) +
      geom_line(color = "blue", size = 1) +
      geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.2, fill = "blue")
  }, error = function(e) {
    # If CI calculation fails, create plot without confidence intervals
    p <- ggplot(variation_data, aes_string(x = variable_name, y = "predicted_ems")) +
      geom_line(color = "blue", size = 1)
  })
  
  # Create nice labels for the plot
  nice_names <- list(
    "Total_ME" = "Total Fatal OD Deaths",
    "Non_violent" = "Non-violent Incidents",
    "median_age" = "Median Age", 
    "median_income" = "Median Income"
  )
  
  x_label <- nice_names[[variable_name]]
  if(is.null(x_label)) x_label <- variable_name
  
  # Add labels and theme to the plot
  p <- p +
    labs(title = paste("Effect of", x_label, "on Predicted EMS Calls"),
         x = x_label,
         y = "Predicted EMS Calls",
         subtitle = "All other variables held at their mean values") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"),
          axis.title = element_text(face = "bold"))
  
  return(p)
}



##this will plot all four predictors with help from the function above. 
visualize_all_predictors <- function(model, reference_data, n_steps = 20) {
  # List of predictors
  names(reference_data)[names(reference_data) == "Non-violent"] <- "Non_violent"
  predictors <- c("Total_ME", "Non_violent", "median_age", "median_income")
  
  # Generate plots for each predictor
  plot_list <- lapply(predictors, function(pred) {
    visualize_systematic_predictions(model, reference_data, pred, n_steps)
  })
  
  # Arrange in a grid
  grid.arrange(grobs = plot_list, ncol = 2)
}

visualize_all_predictors(step_model, clean_complete, n_steps=20)







