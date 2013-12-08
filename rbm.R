#### http://alandgraf.blogspot.co.at/2013/01/restricted-boltzmann-machines-in-r.html

# rbm_w is a matrix of size <number of hidden units> by <number of visible units>
# visible_state is matrix of size <number of visible units> by <number of data cases>
# hidden_state is a binary matrix of size <number of hidden units> by <number of data cases>

visible_state_to_hidden_probabilities <- function(rbm_w, visible_state) {
  1/(1+exp(-rbm_w %*% visible_state))
}

hidden_state_to_visible_probabilities <- function(rbm_w, hidden_state) {
  1/(1+exp(-t(rbm_w) %*% hidden_state))
}

configuration_goodness <- function(rbm_w, visible_state, hidden_state) {
  out=0
  for (i in 1:dim(visible_state)[2]) {
    out=out+t(hidden_state[,i]) %*% rbm_w %*% visible_state[,i]
  }
  out/dim(visible_state)[2]
}

configuration_goodness_gradient <- function(visible_state, hidden_state) {
  hidden_state %*% t(visible_state)/dim(visible_state)[2]
}

sample_bernoulli <- function(mat) {
  dims=dim(mat)
  matrix(rbinom(prod(dims),size=1,prob=c(mat)),dims[1],dims[2])
}

cd1 <- function(rbm_w, visible_data) {
  visible_data = sample_bernoulli(visible_data)
  H0=sample_bernoulli(visible_state_to_hidden_probabilities(rbm_w, visible_data))
  vh0=configuration_goodness_gradient(visible_data, H0)
  V1=sample_bernoulli(hidden_state_to_visible_probabilities(rbm_w, H0))
  H1=visible_state_to_hidden_probabilities(rbm_w, V1)
  vh1=configuration_goodness_gradient(V1, H1)
  vh0-vh1
}

rbm <- function(num_hidden, training_data, learning_rate, n_iterations, mini_batch_size=100, momentum=0.9, quiet=FALSE) {
  #   This trains a model that's defined by a single matrix of weights.
  #   <num_hidden> is the number of hidden units
  #   cd1 is a function that takes parameters <model> and <data> and returns the gradient (or approximate gradient in the case of CD-1) of the function that we're maximizing. Note the contrast with the loss function that we saw in PA3, which we were minimizing. The returned gradient is an array of the same shape as the provided <model> parameter.
  #   This uses mini-batches no weight decay and no early stopping.
  #   This returns the matrix of weights of the trained model.
  n=dim(training_data)[2]
  p=dim(training_data)[1]
  if (n %% mini_batch_size != 0) {
    stop("the number of test cases must be divisable by the mini_batch_size")
  }
  model = (matrix(runif(num_hidden*p),num_hidden,p) * 2 - 1) * 0.1
  momentum_speed = matrix(0,num_hidden,p)
  
  start_of_next_mini_batch = 1;
  for (iteration_number in 1:n_iterations) {
    if (!quiet) {cat("Iter",iteration_number,"\n")}
    mini_batch = training_data[, start_of_next_mini_batch:(start_of_next_mini_batch + mini_batch_size - 1)]
    start_of_next_mini_batch = (start_of_next_mini_batch + mini_batch_size) %% n
    gradient = cd1(model, mini_batch)
    momentum_speed = momentum * momentum_speed + gradient
    model = model + momentum_speed * learning_rate
  }
  return(model)
}