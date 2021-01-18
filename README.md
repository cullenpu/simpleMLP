
<!-- README.md is generated from README.Rmd. Please edit that file -->

# simpleMLP

<!-- badges: start -->

<!-- badges: end -->

simpleMLP is an implementation of a multilayer perceptron, a type of
feedforward, fully connected neural network. It features 2 ReLU hidden
layers and supports hyperparamter tuning for learning rate, batch size,
epochs, and hidden units for both layers.

## Installation

You can install the released version of simpleMLP from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("simpleMLP")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Cullenpu/simpleMLP")
```

## Example

The training data must be a design matrix (i.e. each row is an
observation) and the targets must be a matrix of one-hot encodings.

``` r
library(simpleMLP)

train_data    # Assume the train data is of dimension 1000
train_target  # Assume there are 10 classes

# First initialize the network model. The data has dimension 1000, there are 10 classes, and we choose 100 and 50 hidden units for the first and second hidden layers.
mlp_model <- init_nn(1000, 100, 50, 10)

# Now we train the model. We decide on the following hyperparameters. Overall, the hyperparameters should be tuned as to avoid overfitting.
alpha <- 0.01
epochs <- 10
batch_size <- 32
train_nn(train_data, train_target, mlp_model, alpha, epochs, batch_size)
```
