# load_mnist <- function(train_prop=0.7, val_prop=0.2) {
#   if (train_prop + val_prop > 1 || train_prop + val_prop < 0) {
#     return("Error: Training and Validate proportions must be between 0 and 1")
#   }
#   mnist_raw <- read_csv("https://pjreddie.com/media/files/mnist_train.csv", col_names = FALSE)
#
#   train_index <- train_prop * nrow(mnist_raw)
#   val_index <- val_prop * nrow(mnist_raw) + train_index
#
#   train_data <- mnist_raw[1:train_index, -1]
#   train_target <- data.frame(matrix(rep(0, train_index * 10), ncol=10))
#   for (i in 1:train_index) {
#     train_target[i, mnist_raw$X1[[i]] + 1] = 1
#   }
#
#   return(list(train_data, train_target))
# }
