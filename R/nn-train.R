#' Train Network
#'
#' Train the network with specified hyperparameters.
#'
#' @param train_data set of training data
#' @param train_target set of targets in one-hot encoded form
#' @param model list of weights and biases
#' @param alpha learning rate
#' @param epochs number of epochs
#' @param batch_size mini-batch size
#'
#' @return list of weights and biases after training
#' @export
#'
#' @examples
#' \dontrun{
#' model <- init_nn(784, 100, 50, 10)
#' mnist_train <- load_mnist_train()
#' train_data <- mnist_train[1]
#' train_target <- mnist_train[2]
#' train_nn(train_data, train_target, model, 0.01, 1, 10)
#' }
train_nn <- function(train_data, train_target, model, alpha, epochs, batch_size=nrow(train_data)) {
  train_data <- as.matrix(as.data.frame(train_data))
  train_target <- as.matrix(as.data.frame(train_target))

  n <- nrow(train_data)
  num_iter <- n %/% batch_size

  for (epoch in 1:epochs) {
    boots <- sample(1:n, size = n, replace = FALSE)
    train_data <- train_data[boots,]
    train_target <- train_target[boots,]
    for (iter in 1:num_iter) {
      start <- (iter - 1) * batch_size + 1
      end <- min(n, (iter) * batch_size)
      x <- train_data[start:end,]
      t <- train_target[start:end,]

      forward_pass <- forwardprop(model, x)
      prediction <- softmax(forward_pass["y"])

      ce <- -sum(t * log(prediction)) / nrow(x)
      accuracy <- sum(max.col(prediction) == max.col(t)) / nrow(x)
      cat(epoch, iter, "Cross Entropy: ", ce, "Accuracy: ", accuracy, "\n")

      error <- (prediction - t) / nrow(x)
      back_pass <- backprop(model, error, forward_pass)
      model <- update(model, back_pass, alpha)
    }
  }
  return(model)
}
