#' Train Network
#'
#' Train the network with specified hyperparameters.
#'
#' @param train_set set of training data
#' @param target_set set of targets in one-hot encoded form
#' @param model list of weights and biases
#' @param alpha learning rate
#' @param epochs number of epochs
#' @param batch_size mini-batch size
#'
#' @return list of weights and biases after training
#' @export
#'
#' @examples
#' train_nn(train, target, model, 0.01, 100, 25)
train_nn <- function(train_set, target_set, model, alpha, epochs, batch_size=nrow(train_set)) {
  n <- nrow(train_set)
  num_iter <- n %/% batch_size

  for (epoch in 1:epochs) {
    boots <- sample(1:n, size = n, replace = FALSE)
    train_set <- train_set[boots,]
    target_set <- target_set[boots,]
    for (iter in 1:num_iter) {
      start <- (iter - 1) * batch_size + 1
      end <- min(n, (iter) * batch_size)
      x <- train_set[start:end,]
      t <- target_set[start:end,]

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
