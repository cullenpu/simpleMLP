#' Initialize network
#'
#' Initialize 3 layer fully connected neural network, also known as multilayer perceptron,
#' setting biases to 0 and using the Xavier initialization method for weights.
#'
#' @param num_inputs dimension of inputs
#' @param num_hidden_1 dimension of first hidden layer
#' @param num_hidden_2 dimension of second hidden layer
#' @param num_outputs dimension of output
#'
#' @return dictionary containing weight and biase matrices in each layer of the network
#' @export
#'
#' @examples
init_nn <- function(num_inputs, num_hidden_1, num_hidden_2, num_outputs) {
  w1 <- matrix(rnorm(num_inputs * num_hidden_1, 0, 1 / num_inputs), nrow=num_inputs)
  w2 <- matrix(rnorm(num_hidden_1 * num_hidden_2, 0, 1 / num_hidden_1), nrow=num_hidden_1)
  w3 <- matrix(rnorm(num_hidden_2 * num_outputs, 0, 1 / num_hidden_2), nrow=num_hidden_2)
  b1 <- rep(0, num_hidden_1)
  b2 <- rep(0, num_hidden_2)
  b3 <- rep(0, num_outputs)
  model <- dict(list('w1' = w1, 'w2' = w2, 'w3' = w3, 'b1' = b1, 'b2' = b2, 'b3' = b3))
  return(model)
}


affine <- function(x, w, b) {
  return(x %>% w + b)
}


affine_back <- function(grad_y, x, w) {
  grad_x <- grad_y %*% t(w)
  grad_w <- t(x) %*% grad_y
  grad_b <- grad_y
  return(list(grad_x, grad_w, grad_b))
}

relu <- function(x) {
  return(pmax(x, rep(0, length(x))))
}

relu_back <- function(grad_y, x) {
  grad_x <- grad_y
  grad_x[x <= 0] <- 0
  return(grad_x)
}
