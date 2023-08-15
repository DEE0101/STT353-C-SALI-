library(tensorflow)
library(keras)
library(readr)


# Load and preprocess the MNIST dataset
mnist <- dataset_mnist()
x_train <- mnist$train$x
y_train <- mnist$train$y
x_test <- mnist$test$x
y_test <- mnist$test$y

# Reshape and normalize input data
x_train <- array_reshape(x_train, c(nrow(x_train), 28, 28, 1))
x_test <- array_reshape(x_test, c(nrow(x_test), 28, 28, 1))
x_train <- x_train / 255
x_test <- x_test / 255

# Convert labels to categorical format
y_train <- to_categorical(y_train, 10)
y_test <- to_categorical(y_test, 10)

# Build a simple CNN model
model <- keras_model_sequential()
model %>%
  layer_conv_2d(filter = 32, kernel_size = c(3, 3), activation = 'relu', input_shape = c(28, 28, 1)) %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_flatten() %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dense(units = 10, activation = 'softmax')

# Compile the model
model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_adam(),
  metrics = c('accuracy')
)

# Train the model
history <- model %>% fit(
  x_train, y_train,
  epochs = 10, batch_size = 128,
  validation_split = 0.2
)

# Generate sample data
num_samples <- 1000
sequence_length <- 10
input_dim <- 1

set.seed(42)
x <- array(runif(num_samples * sequence_length), dim = c(num_samples, sequence_length, input_dim))
y <- apply(x, c(1, 2), function(seq) sum(seq))

# Build an LSTM model
model <- keras_model_sequential()
model %>%
  layer_lstm(units = 50, input_shape = c(sequence_length, input_dim)) %>%
  layer_dense(units = 1)

# Compile the model
model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam()
)

# Train the model
history <- model %>% fit(x, y, epochs = 10, batch_size = 32)

# Generate predictions
predictions <- model %>% predict(x)

# Print sample predictions
cat("Sample predictions:\n")
for (i in 1:10) {
  cat("Actual:", y[i], "Predicted:", predictions[i], "\n")
}

# Load required library
library(e1071)

# Generate sample data
num_samples <- 100
set.seed(42)
X <- matrix(runif(num_samples * 2), ncol = 2)
Y <- factor(ifelse(X[, 1] + X[, 2] > 1, "+", "-"))

# Split data into training and testing sets
split_ratio <- 0.8
split_index <- floor(num_samples * split_ratio)
X_train <- X[1:split_index, ]
Y_train <- Y[1:split_index]
X_test <- X[(split_index + 1):num_samples, ]
Y_test <- Y[(split_index + 1):num_samples]

# Train a Support Vector Machine (SVM)
svm_model <- svm(Y_train ~ ., data = data.frame(Y_train, X_train), kernel = "linear")

# Predict using the SVM model
predictions <- predict(svm_model, data.frame(X_test))

# Evaluate the SVM model
accuracy <- sum(predictions == Y_test) / length(Y_test)
cat("Accuracy:", accuracy, "\n")

# Load required library
library(e1071)

# Generate sample data
num_samples <- 100
set.seed(42)
X <- matrix(runif(num_samples * 2), ncol = 2)
Y <- factor(ifelse(X[, 1] + X[, 2] > 1, "+", "-"))

# Split data into training and testing sets
split_ratio <- 0.8
split_index <- floor(num_samples * split_ratio)
X_train <- X[1:split_index, ]
Y_train <- Y[1:split_index]
X_test <- X[(split_index + 1):num_samples, ]
Y_test <- Y[(split_index + 1):num_samples]

# Train a Support Vector Machine (SVM)
svm_model <- svm(Y_train ~ ., data = data.frame(Y_train, X_train), kernel = "linear")

# Predict using the SVM model
predictions <- predict(svm_model, data.frame(X_test))

# Evaluate the SVM model
accuracy <- sum(predictions == Y_test) / length(Y_test)
cat("Accuracy:", accuracy, "\n")

# Load required libraries
library(keras)

# Generate sample data
num_samples <- 1000
input_dim <- 10

set.seed(42)
x_real <- array(runif(num_samples * input_dim), dim = c(num_samples, input_dim))

# Define the Generator model
generator <- keras_model_sequential()
generator %>%
  layer_dense(units = 128, input_shape = c(input_dim)) %>%
  layer_leaky_relu(alpha = 0.2) %>%
  layer_batch_normalization() %>%
  layer_dense(units = 256) %>%
  layer_leaky_relu(alpha = 0.2) %>%
  layer_batch_normalization() %>%
  layer_dense(units = input_dim, activation = 'tanh')

# Define the Discriminator model
discriminator <- keras_model_sequential()
discriminator %>%
  layer_dense(units = 256, input_shape = c(input_dim)) %>%
  layer_leaky_relu(alpha = 0.2) %>%
  layer_dense(units = 128) %>%
  layer_leaky_relu(alpha = 0.2) %>%
  layer_dense(units = 1, activation = 'sigmoid')

# Compile the Discriminator
discriminator %>% compile(
  loss = 'binary_crossentropy',
  optimizer = optimizer_adam()
)

# Define the GAN model
discriminator$trainable <- FALSE
gan_input <- layer_input(shape = c(input_dim))
gan_output <- discriminator(generator(gan_input))
gan <- keras_model(inputs = gan_input, outputs = gan_output)

# Compile the GAN
gan %>% compile(
  loss = 'binary_crossentropy',
  optimizer = optimizer_adam()
)

# Train the GAN
for (epoch in 1:1000) {
  # Generate fake data
  noise <- array(runif(num_samples * input_dim), dim = c(num_samples, input_dim))
  x_fake <- predict(generator, noise)
  
  # Train discriminator
  batch_size <- 32
  idx_real <- sample.int(num_samples, batch_size)
  idx_fake <- sample.int(num_samples, batch_size)
  x_batch_real <- x_real[idx_real, ]
  x_batch_fake <- x_fake[idx_fake, ]
  x_batch <- rbind(x_batch_real, x_batch_fake)
  y_batch <- c(rep(1, batch_size), rep(0, batch_size))
  discriminator_loss <- discriminator %>% train_on_batch(x_batch, y_batch)
  
  # Train generator
  noise <- array(runif(num_samples * input_dim), dim = c(num_samples, input_dim))
  y_fake <- rep(1, num_samples)
  generator_loss <- gan %>% train_on_batch(noise, y_fake)
  
  cat("Epoch:", epoch, "Discriminator Loss:", discriminator_loss, "Generator Loss:", generator_loss, "\n")
}

# Generate sample data
num_samples <- 1000
num_features <- 5

set.seed(42)
X <- matrix(runif(num_samples * num_features), ncol = num_features)
Y_relevance <- runif(num_samples) > 0.5

# Build a relevance estimation model
model_relevance <- keras_model_sequential()
model_relevance %>%
  layer_dense(units = 64, activation = 'relu', input_shape = c(num_features)) %>%
  layer_dropout(0.5) %>%
  layer_dense(units = 1, activation = 'sigmoid')

# Compile the model
model_relevance %>% compile(
  loss = 'binary_crossentropy',
  optimizer = optimizer_adam(),
  metrics = c('accuracy')
)

# Train the model
history <- model_relevance %>% fit(X, Y_relevance, epochs = 10, batch_size = 32)

# Load required libraries
library(keras)

# Generate sample data
num_samples <- 1000
num_features <- 5

set.seed(42)
X <- matrix(runif(num_samples * num_features), ncol = num_features)
Y_value <- rowSums(X)

# Build a value calibration model
model_value <- keras_model_sequential()
model_value %>%
  layer_dense(units = 64, activation = 'relu', input_shape = c(num_features)) %>%
  layer_dropout(0.5) %>%
  layer_dense(units = 1)

# Compile the model
model_value %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam()
)

# Train the model
history <- model_value %>% fit(X, Y_value, epochs = 10, batch_size = 32)



# Load required library
library(sensitivity)

# Define a function (e.g., model, simulation, etc.)
my_function <- function(x) {
  return(x[1]^2 + x[2]*10)
}

# Define input parameters and ranges
parameters <- c("param1", "param2")
ranges <- list(param1 = c(-10, 10), param2 = c(-5, 5))

# Perform sensitivity analysis using Morris method
sensitivity_results <- morris(model = my_function, factors = parameters, ranges = ranges, n = 100)

# Print sensitivity indices
print(sensitivity_results$mu)


# Generate sample predicted and actual vectors
predicted <- c(2.5, 3.0, 4.0, 4.5, 5.0)
actual <- c(2.0, 2.5, 3.5, 4.0, 4.5)

# Calculate RMSE
rmse <- sqrt(mean((predicted - actual)^2))

# Print the RMSE
cat("Root Mean Squared Error:", rmse, "\n")
