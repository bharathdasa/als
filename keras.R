# To setup keras with tensorflow in R 

install.packages("devtools")
library("devtools")

install.packages("keras")
library("keras") 

install.packages("tensorflow")
library("tensorflow")

install_tensorflow() # if needed with cpu support 
install_tensorflow(gpu=TRUE) # if needed with gpu support and gpu is available on the machin

#To check if the installtion is successful. Please execute the below code

sess = tf$Session()
hello <- tf$constant('Hello, TensorFlow!')
sess$run(hello)
