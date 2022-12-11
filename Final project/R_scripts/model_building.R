#linear regression

simple_recipe <- recipe(form, data = df_train)

#dummy coding categorical predictors
final_recipe <- simple_recipe %>%
  step_dummy(all_nominal_predictors()) %>%
  step_center(all_numeric_predictors()) %>%
  step_scale(all_numeric_predictors())

lm_model <- linear_reg() %>%
  set_engine("lm")

lm_wflow <- workflow() %>%
  add_model(lm_model) %>%
  add_recipe(final_recipe)

summary(lm_wflow)

#fitting to model and recipe
lm_fit <- fit(lm_wflow, df_train)
lm_fit %>%
  extract_fit_parsnip() %>%
  tidy()

#training RMSE
lm_train_res <- predict(lm_fit, new_data = df_train %>% select(-rating_average))
lm_train_res %>%
  head()

#reattaching column of actual observed age
lm_train_res <- bind_cols(lm_train_res, df_train %>% select(rating_average))
lm_train_res %>%
  head()

#metrics
lm_metrics <- metric_set(yardstick::rmse, yardstick::rsq, yardstick::mae)
lm_metrics(lm_train_res, truth = rating_average, estimate = .pred)

#plotting predicted vs. observed
lm_train_res %>%
  ggplot(aes(x=.pred, y=jitter(rating_average))) +
  geom_point(alpha = 0.2) +
  geom_abline(lty=2) +
  theme_classic() +
  coord_obs_pred()



#Decision tree
#model
tree_spec <- decision_tree() %>%
  set_engine("rpart") %>%
  set_mode("regression")

#workflow
tree_wf <- workflow() %>%
  add_model(tree_spec %>% set_args(cost_complexity = tune())) %>%
  add_formula(form)

#tuning cost_complexity
param_grid <- grid_regular(cost_complexity(range = c(-3, -1)), levels = 10)

tune_res <- tune_grid(
  tree_wf,
  resamples = df_fold,
  grid = param_grid,
  metrics = metric_set(yardstick::rmse),
  control = control_grid(verbose = T)
)

autoplot(tune_res)


tune_res %>%
  collect_metrics() %>%
  arrange(mean)

best_penalty <- select_best(tune_res, metric = "rmse")
best_penalty

tree_final <- finalize_workflow(tree_wf, best_penalty)
tree_final_fit <- fit(tree_final, data = df_train)
tree_final_fit

augment(tree_final_fit, new_data = df_train) %>%
  rmse(truth = rating_average, estimate=.pred)

collect_metrics(tune_res) %>% arrange(mean)



#XGBoost

# training XGB boost for regression
#model
xgb_spec <- boost_tree(
  trees = tune(),
  tree_depth = tune(),
  min_n = tune(),
  loss_reduction = tune(),                     ## first three: model complexity
  sample_size = tune(),
  mtry = tune(),         ## randomness
  learn_rate = tune(),                         ## step size
) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

xgb_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  trees(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), df_train),
  learn_rate(),
  size = 25
)


#workflow
boost_wf <- workflow() %>%
  add_formula(form) %>%
  add_model(xgb_spec)


xgb_res <- tune_grid(
  boost_wf,
  resamples = df_fold,
  grid = xgb_grid,
  metrics = metric_set(yardstick::rmse),
  control = control_grid(verbose=T)
)


xgb_res
autoplot(xgb_res)

xgb_res %>%
  collect_metrics() %>%
  arrange(mean)

best_boost <- select_best(xgb_res, metric = "rmse")
best_boost

boost_final <- finalize_workflow(boost_wf, best_boost)
boost_final_fit <- fit(boost_final, data = df_train)
boost_final_fit

augment(boost_final_fit, new_data = df_train) %>%
  rmse(truth = rating_average, estimate=.pred)

collect_metrics(xgb_res) %>% arrange(mean)



#Random forest
# training Random forest for predicting rating_average
#model
rf_spec <- rand_forest(mtry = tune(), trees = tune(), min_n = tune()) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("regression")

p_grid <- grid_regular(mtry(range = c(1,8)),trees(range = c(100,500)),
                       min_n(range = c(1,10)), levels = 8)

#workflow
rf_tree_wf <- workflow() %>%
  add_model(rf_spec) %>%
  add_formula(form)

tune_param <- tune_grid(
  rf_tree_wf,
  resamples = df_fold,
  grid = p_grid,
  metrics = metric_set(yardstick::rmse),
  control = control_grid(verbose = T)
)

autoplot(tune_param)

tune_param
autoplot(tune_param)

tune_param %>%
  collect_metrics() %>%
  arrange(mean)

best_rf <- select_best(tune_param, metric = "rmse")
best_rf

rf_final <- finalize_workflow(rf_tree_wf, best_rf)
rf_final_fit <- fit(rf_final, data = df_train)
rf_final_fit

augment(rf_final_fit, new_data = df_train) %>%
  rmse(truth = rating_average, estimate=.pred)

collect_metrics(tune_param) %>% arrange(mean)



#K-mean cluster Model
# training k-means clustering for recommendation engine (unsupervised learning)
# first, we are going to apply PCA

df <- read.csv("C:\\Users\\kashyap\\Desktop\\PSTAT-231\\Final project\\data\\processed\\cleaned_final.csv")

df <- df[,c("year_published", "min_players", "max_players", "play_time", "min_age", "users_rated", "rating_average", "bgg_rank", "complexity_average", "game_age")]

# First, perform PCA to reduce dimensionality and expedite k-means clustering & plotting
res.pca <- prcomp(df, scale = TRUE)
# plot the pca results
fviz_eig(res.pca)

df_transform = as.data.frame(-res.pca$x[,1:2])

fviz_nbclust(df_transform, kmeans, method = "silhouette")

# set k based on the above determination of optimal clusters
k = 2
kmeans_post_pca = kmeans(df_transform, centers = 2, nstart = 50)
# it appears there are two relatively distinct clusters
fviz_cluster(kmeans_post_pca, data = df_transform)


# Calculate a distance matrix to be used to hierachial clustering
distance_mat <- dist(df, method = 'euclidean')

# try hierchial clustering for comparison
Hierar_cl <- hclust(distance_mat, method = "average")
plot(Hierar_cl)

abline(h = 110, col = "green")

# Cutting tree by no. of clusters
fit <- cutree(Hierar_cl, k = 3 )

table(fit)
rect.hclust(Hierar_cl, k = 3, border = "green")
# it appears that the hierachial clustering is not a good fit for our dataset



#Hierarchical cluster model
# alternative hierchial clustering
# Calculate a distance matrix to be used to hierachial clustering
distance_mat <- dist(df_transform, method = 'euclidean')

# try hierchial clustering for comparison
Hierar_cl <- hclust(distance_mat, method = "average")
plot(Hierar_cl)

abline(h = 110, col = "green")

# Cutting tree by no. of clusters
fit <- cutree(Hierar_cl, k = 3 )

table(fit)
rect.hclust(Hierar_cl, k = 3, border = "green")
# it appears that the hierachial clustering is not a good fit for our dataset
