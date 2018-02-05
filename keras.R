# To setup keras with tensorflow in R 

install.packages("devtools")
devtools::install_github("rstudio/keras")
library(keras) 

# install only one of the below two lines 

install_tensorflow() # if needed with cpu support 
install_tensorflow(gpu=TRUE) # if needed with gpu support and gpu is available on the machine
