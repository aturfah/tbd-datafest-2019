source('helpers.R')
library(randomForest)
library(tree)

# Fill in Load the Datasets
proc_data = load_true_data()

# Train/Test Split
set.seed(10)
train_rows <- sample(1:nrow(proc_data), size=0.7*nrow(proc_data))
train_data <- proc_data[train_rows, ]
test_data <- proc_data[-train_rows, ]

## Decision Tree
# Fit Tree
trump_tree <- tree(trump_binary ~ . - pct_trump_rel_clint - county - state - fips,
                  data = train_data)
# cv_trump_tree <- cv.tree(trump_tree, FUN = prune.misclass) # best = 6

prune_trump_tree <- prune.misclass(trump_tree, best = 6)
plot(prune_trump_tree, main='Pruned Decision Tree')
text(prune_trump_tree, pretty = 0, cex = 0.9)

tree_train_err <- tree_err_func(prune_trump_tree, train_data, 'trump_binary')
tree_test_err <- tree_err_func(prune_trump_tree, test_data, 'trump_binary')


## Random Forest
# Fit RF on Training Data
trump_rf = randomForest(trump_binary ~ . - pct_trump_rel_clint - county - state - fips,
                       data = train_data,
                       importance = TRUE)
importance(trump_rf)
varImpPlot(trump_rf, main='')

# Train/Test Error
rf_train_err <- tree_err_func(trump_rf, train_data, 'trump_binary')
rf_test_err <- tree_err_func(trump_rf, test_data, 'trump_binary')


## Put in Dataframe
result_df = data.frame("Train Error" = c(tree_train_err$val, rf_train_err$val),
                       "Test Error" = c(tree_test_err$val, rf_test_err$val))
rownames(result_df) = c("Pruned Tree", "Random Forest")
knitr::kable(result_df)
