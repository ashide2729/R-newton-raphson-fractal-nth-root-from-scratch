#Installing required packages
pkgs <- c("dplyr","numDeriv")
install.packages(pkgs)

#Loading the required packages 
library(dplyr)
library(numDeriv)

#Declaring the function to find roots
func <- function(x) {
    x^5-1
}

#Declaring the function for newton raphson method
newton.raphson <- function(a) {
    
    b=2
    tol = 1e-5
    n = 1000
    
    x0 <- a # Set start value to supplied lower bound
    k <- n # Initialize for iteration results
    
    # Check the upper and lower bounds if approximation is 0
    if (func(a) == 0.0) {
        return(a)
    }

    #Iterating over the vertex to find the roots by closest approximate
    for (i in 1:n) {
        dx <- genD(func = func, x = x0)$D[1] # First-order derivative f'(x0)
        x1 <- x0 - (func(x0) / dx) # Calculate next value x1
        k[i] <- x1 # Store x1
        # In case of the point tending to zero the output is not comparable and hence result to a root
        if (is.infinite(x1)){
            return(1-0i)
        }
        # Output the results, once the difference between x0 and x1 becomes sufficiently small
        if (abs(x1 - x0) < tol) {
            root.approx <- tail(k, n=1)
            #res <- list('root approximation' = root.approx, 'iterations' = k)
            return(root.approx)
        }
        # If no convergence set x1 as x0 and continue
        x0 <- x1
    }
    print('Too many iterations in method')
}

#Declaring a vector to store the vertex coordinates
vertices <- c()

#Starting from the vertex -1-1i 
x <- -1
y <- -1
z <- complex( real = x, imaginary = y)
vertices <- append(vertices, z)
n <- 2^11
k <- 2^-10
#Iterating over the vertex to find coordinates along one of the edges of grid
for (i in 1:n) {
    x <- x + k
    z <- complex( real = x, imaginary = y)
    vertices <- append(vertices, z)
}

rm(x,y,z,k)

temp <- vertices

k <- complex(real = 0,imaginary = 2^-10)

#Iterating over the calculated vertices to find vertices along other edges at once
for (i in 1:n){
    temp <- temp + k
    vertices <- append(vertices,temp)
}

rm(temp, n, k)

root <- c()

# Finding the value of roots the vertices are converging to using 
# newton raphson

# Using sapply as the number of iterations are large and sapply is 
# faster than looping
root <- sapply(vertices, newton.raphson)

# Creating a dataframe for ease of grouping and plotting
dataf <- data.frame(vertices,root)

# Rounding the values of roots for ease
dataf$root <- round(dataf$root,digits = 3)

#Writing the outputed dataframe to csv for future use
write.csv(dataf,file = "C:\\Users\\ashish.deshwal\\Desktop\\fractal_data.csv",row.names = FALSE, col.names = TRUE)

data <- read.csv("C:\\Users\\ashish.deshwal\\Desktop\\fractal_data.csv")

plot(data$vertices[data$root==1.000+0.000i], main="Plot of convergence of vertices", xlab="Real(z)", ylab = "Imaginary(z)", type="p", col="#800000", lty=1 )
points(data$vertices[data$root==-0.809-0.588i], col="#006666")
points(data$vertices[data$root==-0.809+0.588i], col="#E2E1C0")
points(data$vertices[data$root==0.309+0.951i], col="#666666")
points(data$vertices[data$root==0.309-0.951i], col="#D8CBCB")
