#model evaluation

model_types = c("pruned tree","random forest","xgboost","linear regression")
final_models = lst(tree_final_fit, rf_final_fit, boost_final_fit, lm_fit)
rmses = c(0.09, 0.36, 0.02, 0.56)


# put the model types and their ROC AUC's into one table
combined <- as_tibble(cbind(model_types,rmses))
combined

# using the table of rocaucs, find the model type with the best roc auc,
# and set "best_model" equal to that model's final workflow
best_model <- final_models[which.min(combined$rmses)]
best_model

#re-fit on test data
test_mod <- augment(best_model[[1]], new_data = df_test)
test_mod

#RMSE
test_mod %>%
  rmse(truth = rating_average, estimate=.pred)

#scatterplot
augment(best_model[[1]], new_data = df_test) %>%
  ggplot(aes(rating_average, .pred)) +
  geom_abline() +
  geom_point(alpha = 0.5)

# Fitting the random forest model to the testing data since that performed best in-sample

#finalizing workflow
model_final <- finalize_workflow(boost_wf, best_boost)
model_final_fit <- fit(model_final, data = df_train)
model_final_fit

#rmse for training data
augment(model_final_fit, new_data = df_train) %>%
  rmse(truth = rating_average, estimate = .pred)

#rmse for testing data
augment(model_final_fit, new_data = df_test) %>%
  rmse(truth = rating_average, estimate = .pred)
