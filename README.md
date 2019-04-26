# ODiff
Open source automatic differentiation API that can be used as a machine learinng framework for OCaml. 

## demo ##
Currently, ODiff supports the features required to implement a fully connected neural network. You can try training an FCNN on MNIST with the mnist_demo example. First, download the MNIST train and test sets as CSV files from [here](https://www.pjreddie.com/projects/mnist-in-csv/ "Named link title"), then change the `training_data_dir` and `test_data_dir` to be the path to the training and testing data. Then run 
```make demo```
which will install the CSV library if you don't have it (to load the csv files), and then compiler and run the demo. It is possible to change the number of iterations to train and the learning rate by changing the parameters in the demo file.

## How it works ##

ODiff works by wrapping a float matrix in Diff variable wrapper and then perform operations that computes the partial derivatives after using them.

It is straightforward to implement custom operators in ODiff. All what you need is to define how the operator should be evaluated in create a function to calculate the partial derivatives with respect to that gradient.

## TODO ##

- [x] support matrices
- [ ] parallelize matrix operations in Math submodule
- [ ] add better optimizers (ex. SGD, Adam)
- [ ] add GPU support
- [ ] add more layers (ex. convolution layer, recurrent layer)
