#' Load Training Data
#'
#' Loads MNIST training and validation data and generates one hot encodings
#' for the targets.
#'
#' @param train_prop the proportion of the data used for training data; the
#' remaining will be used for validation
#'
#' @return list of training and validation data and targets
#' @export
#'
#' @examples
#' \donttest{
#' mnist_train <- load_mnist_train()
#' train_data <- mnist_train[1]
#' train_target <- mnist_train[2]
#' validate_data <- mnist_train[3]
#' validate_target <- mnist_train[4]
#' }
load_mnist_train <- function(train_prop=0.7) {
  if (train_prop < 0 || train_prop > 1) {
    return("Error: Train data proportion must be between 0 and 1")
  }
  mnist_train <- readr::read_csv("https://pjreddie.com/media/files/mnist_train.csv", col_names = FALSE)
  mnist_train <- sample(mnist_train)  # shuffle raw data

  # index to split training and validation
  train_index <- train_prop * nrow(mnist_train)

  # split raw data into train and validate sets
  train_raw <- mnist_train[1:train_index,]
  validate_raw <- mnist_train[train_index:nrow(mnist_train),]

  # training data and targets
  train_data <- train_raw[,-1]
  train_target <- one_hot_encoding(train_raw)

  # validation data and targets
  validate_data <- validate_raw[,-1]
  validate_target <- one_hot_encoding(validate_raw)

  return(list(train_data, train_target, validate_data, validate_target))
}

#' Load Testing Data
#'
#' Loads MNIST testing data and generates one hot encodings for the targets.
#'
#' @return list of testing data and targets
#' @export
#'
#' @examples
#' \donttest{
#' mnist_test <- load_mnist_test()
#' test_data <- mnist_test[1]
#' test_target <- mnist_test[2]
#' }
load_mnist_test <- function() {
  mnist_test <- readr::read_csv("https://pjreddie.com/media/files/mnist_test.csv", col_names = FALSE)

  test_data <- mnist_test[,-1]
  test_target <- one_hot_encoding(mnist_test)

  return(list(test_data, test_target))
}

#' One Hot Encoding
#'
#' Creates a one hot encoding matrix with the specified number of categories
#' for the targets. Target must be the first column of the data_raw input.
#'
#' @param data_raw data input to create encoding; target must be first column
#' @param ncat number of categories to use for the encoding
#'
#' @return targets in a one hot encoding matrix
one_hot_encoding <- function(data_raw, ncat=10) {
  n = nrow(data_raw)
  target <- data.frame(matrix(rep(0, n * ncat), ncol=ncat))

  for (i in 1:n) {
    target[i, data_raw$X1[[i]] + 1] = 1
  }

  return(target)
}
