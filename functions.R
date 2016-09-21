## Assignment 1 ####
## solve by elimination

elim_solve <- function(A, b) {
  #find pivot
  pivot_1 <- A[1, 1]
  
  #swap if pivot == 0
  if (pivot_1 == 0) {
    A <- A[c(2, 1, 3), ]
    pivot_1 <- A[1, 1]
    if (pivot_1 == 0) {
      A <- A[c(3, 2, 1), ]
      pivot_1 <- A[1, 1]
    }
  }
  #multiplier & eliminate row 2
  mult_2 <- A[2, 1] / pivot_1
  A[2, ] <- A[2, ] - mult_2 * A[1, ]
  b[2] <- b[2] - mult_2 * b[1]
  #multiplier & eliminate row 3
  mult_3 <- A[3, 1] / pivot_1
  A[3, ] <- A[3, ] - mult_3 * A[1, ]
  b[3] <- b[3] - mult_3 * b[1]
  
  #find pivot
  pivot_2 <- A[2, 2]
  
  #swap if pivot == 0
  if (pivot_2 == 0) {
    A <- a[c(1, 3, 2), ]
    pivot_2 <- A[2, 2]
  }
  
  #multiplier & eliminate row 3
  mult_3 <- A[3, 2] / pivot_2
  A[3, ] <- A[3, ] - mult_3 * A[2, ]
  b[3] <- b[3] - mult_3 * b[2]
  
  #solve for x3
  x3 <- b[3] / A[3, 3]
  
  #substitution
  x2 <- (b[2] - A[2, 3] * x3) / A[2, 2]
  x1 <- (b[1] - A[1, 3] * x3 - A[1, 2] * x2) / A[1, 1]
  
  #results
  x <- matrix(c(x1, x2, x3), nrow = 3)
  x
}



## Assignment 2 ####
## matrix factorization

factorize <- function(A, LDU = FALSE) {
  if (nrow(A) == ncol(A)) {
    square <- TRUE
  } else {square <- FALSE}
  
  if (square == TRUE) {
    size <- nrow(A)
    L <- diag(size)
    U <- A
    
    for (i in 1:(size - 1)) {
      for (j in (i + 1):size) {
        # get multipliers
        L[j, i] <- U[j, i] / U[i, i]
        # pivots and multiplication
        U[j, ]  <- U[j, ] - L[j, i] * U[i, ]
      }
    }
    
    #results
    if (LDU == TRUE) {
      D <- diag(size)
      for (k in 1:size) {
        D[k, k] <- U[k, k]
        U[k, ] <- U[k, ] / D[k, k]
      }
      LU <- list("L" = L, "D" = D, "U'" = U)
    } else {
      LU <- list("L" = L, "U" = U)
    }
  } else {LU <- "Error: matrix is not square"}
  LU
}



## Assignment 4 ####
## inverse with co-factors

myinverse <- function(A) {
  if (nrow(A) == ncol(A) & Matrix::rankMatrix(A)[1] == nrow(A)) {
    fr_square <- TRUE
  } else {fr_square <- FALSE}
  
  if (fr_square == TRUE) {
    size <- nrow(A)
    C <- matrix(nrow = size, ncol = size)
    for (i in 1:size) {
      for (j in 1:size) {
        M <- A[-i, -j]
        C[i, j] <- (-1)^(i + j) * det(M)
      }
    }
    B <- t(C) / det(A)
  } else {B <- "Matrix is not invertible"}
  B
}
