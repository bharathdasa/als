rm(list=ls())

#loading keras library
library(keras)

#loading the keras inbuilt mnist dataset
data<-dataset_mnist()
?dataset_mnist #MNIST database of handwritten digits

#Training Data
train_x<-data$train$x
train_y<-data$train$y

dim(train_x)# checking the dimensions of the training data
dim(train_y)# checking the dimensions of the training targets data

#Test Set
test_x<-data$test$x
test_y<-data$test$y

dim(test_x)# checking the dimensions of the test data
dim(test_y)# checking the dimensions of the test targets data


#converting a 2D array into a 1D array for feeding 
#into the MLP and normalising the matrix
train_x <- array(as.numeric(train_x), dim = c(dim(train_x)[[1]], 784))
test_x <- array(as.numeric(test_x), dim = c(dim(test_x)[[1]], 784))

train_x <- train_x / 255
test_x <- test_x / 255

dim(train_x)#to display the dimensions of the training data after flattening
dim(test_x)# to display the dimensions of the test data after flattening

#convert class vectors to binary class matrices, or one-hot encoding
head(train_y)

train_y<-to_categorical(train_y,10)
test_y<-to_categorical(test_y,10)

head(train_y)

dim(train_y)
dim(test_y)

# Defining the neural network model with input layer [784 neurons], 1 hidden layer [784 neurons] and 1 output layer [10 neurons]

model_SNN <- keras_model_sequential()

model_SNN %>%
  layer_dense(units = 784, input_shape = 784, activation = "relu") %>% 
  layer_dense(units = 10, activation = "softmax")

summary(model_SNN)

# Compiling the model

model_SNN %>%
  compile(loss ="categorical_crossentropy",
          optimizer = "adam",
          metrics= c("accuracy"))


# Train the model on the training dataset  
# epochs = No of iterations on a dataset.
# batchsize = Number of samples per gradient update.
# validation_split = how much of the training data to hold out while training to evaluate how well the model is performing with every iteration

history <- model_SNN %>% fit(
  train_x, 
  train_y, 
  epochs = 10,
  batch_size = 200, 
  validation_split = 0.2
)

# Visualizing the loss 
# Plot the model loss of the training data
plot(history$metrics$loss, main="Model Loss", xlab = "epoch", ylab="loss", col="blue", type="l")
 
# Plot the model loss of the test data
lines(history$metrics$val_loss, col="green")
 
# Add legend
legend("topright", c("train","test"), col=c("blue", "green"), lty=c(1,1))


# Visualizing the accuracy
# Plot the accuracy of the training data 
plot(history$metrics$acc, main="Model Accuracy", xlab = "epoch", ylab="accuracy", col="blue", type="l")
 
# Plot the accuracy of the validation data
lines(history$metrics$val_acc, col="green")
 
# Add Legend
legend("bottomright", c("train","test"), col=c("blue", "green"), lty=c(1,1))

# Evaluating the model on the test data

scores<-model_SNN%>%evaluate(test_x,test_y,batch_size=200)
scores

# Predicting with the model
#pred_model<-model_SNN%>%predict_classes(test_x)
#pred_model
