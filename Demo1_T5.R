#Here seq-function creates a sequence from -1 to 1 with step 0.01.
#rep-function repeats each value 10 times.
x <- rep(seq(-1, 1, by=0.01), each=10)

#Here normally distributed error terms are created
#with mean=0 and variance=0.8 (standard deviation is square root of this)
error <- rnorm(length(x), mean=0, sd=sqrt(0.8))

#Here values for variable y are created 
y <- 0.2+3*x+error

#Here values of x and y are added to a dataframe.
t5_data <- data.frame(x,y)
t5_data
