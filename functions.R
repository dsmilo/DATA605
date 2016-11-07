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



## Assignment 6 ####
## Birthday Problem

same_bday <- function(n) {
  unique <- prod(364:(364 - n + 2)) / (365 ^ (n - 1))
  ifelse(n > 364, 1, 1 - unique)
}

## probability of words in text file
word_prob <- function(flnm, ranked = FALSE, printAllWords = FALSE, returnTable = FALSE) {
  library(stringr)
  # read in file
  body <- read.delim(flnm, header = FALSE, stringsAsFactors = FALSE)
  # collapse and convert to lowercase
  body <- str_to_lower(paste(body, collapse = ' '))
  # remove anything that isnt a space or lowercase letter
  body <- str_replace_all(body, '[^[:lower:]^ ]', '')
  # convert to vector
  word_vec <- str_split(body, ' ')[[1]]
  # get count of words and convert to probability
  word_table <- as.data.frame(table(word_vec)[-1], stringsAsFactors = FALSE) #remove count of spaces
  suppressPackageStartupMessages(library(dplyr))
  word_table <- word_table %>% transmute(Word = word_vec, Probability = Freq / sum(Freq))
  if(ranked) {
    word_table <- arrange(word_table, desc(Probability))
  }
  print(head(word_table, ifelse(printAllWords, nrow(word_table), 10)))
  if(returnTable){return(word_table)}
}

## probability of a phrase in text file
phrase_prob <- function(flnm, word1, word2) {
  # adapt previous function
  library(stringr)
  body <- read.delim(flnm, header = FALSE, stringsAsFactors = FALSE)
  body <- str_to_lower(paste(body, collapse = ' '))
  body <- str_replace_all(body, '[^[:lower:]^ ]', '')
  word_vec <- str_split(body, ' ')[[1]]
  word_table <- as.data.frame(table(word_vec)[-1], stringsAsFactors = FALSE)
  suppressPackageStartupMessages(library(dplyr))
  word_table <- word_table %>% transmute(Word = word_vec, Probability = Freq / sum(Freq))
  # get individual word probabilities
  prob_1 <- word_table[word_table$Word == word1, 2]
  prob_2 <- word_table[word_table$Word == word2, 2]
  # find count of words appearing together
  phrase_count <- str_count(body, paste(word1, word2)) + str_count(body, paste(word2, word1))
  # divide by the number of possible phrases -- 1 fewer than the number of words
  prob_phrase <- phrase_count[1] / (length(word_vec) - 1)
  probs <- data.frame(Term = c(word1, word2, paste0(paste(word1, word2), ";", paste(word2, word1))), 
                      Probability = c(prob_1, prob_2, prob_phrase), stringsAsFactors = FALSE)
  probs
}



## Assignment 7 ####
## Expectation & Variance
sum_stats <- function(input) {
  # get unique values in vector and count, convert to number
  input_tbl <- as.data.frame(table(input))
  names(input_tbl)[1] <- "x"
  input_tbl$x <- as.numeric(as.character(input_tbl$x))
  # get probability for each value of x
  input_tbl$p <- input_tbl$Freq / sum(input_tbl$Freq)
  #return E(X) & sd(X)
  E_x <- sum(input_tbl$x * input_tbl$p)
  var_x <- sum(input_tbl$x * input_tbl$x * input_tbl$p) - E_x^2
  c("E(x)" = E_x, "sd(x)" = sqrt(var_x), "N" = length(input))
}

## running statistics
running_stats <- function(newinput) {
  #read in existing stats; 0 if not defined
  E_X <- ifelse(exists('E'), E, 0)
  var_X <- ifelse(exists('SD'), SD^2, 0)
  N_X <- ifelse(exists('N'), N, 0)
  
  #get figures for new array
  E_Y <- sum_stats(newinput)[[1]]
  var_Y <- sum_stats(newinput)[[2]]^2
  N_Y  <- sum_stats(newinput)[[3]]
  
  #calculate & store new stats
  E_tot <- (N_X * E_X + N_Y * E_Y) / (N_X + N_Y)
  var_tot <- (N_X * var_X + N_Y * var_Y + N_X * (E_X - E_tot)^2 + N_Y * (E_Y - E_tot)^2) / (N_X + N_Y)
  E <<- E_tot
  SD <<- sqrt(var_tot)
  N <<- N_X + N_Y
  
  #return values
  c("E" = E, "SD" = SD, "N" = N)
}



## Assignment 9 ####
## Central Limit Theorem
central_limit <- function(n, PDF) {
  xbar <- NULL
  for (i in 1:n) {
    samp <- do.call(PDF, list(1000))
    xbar <- c(xbar, mean(samp))
  }
  plot_title <- paste(n, 'samples:', as.character(substitute(PDF)))
  hist(xbar, xlab = NULL, ylab = NULL, main = plot_title)
}



## Assignment 10 ####
## PageRank Power Iteration
power_iterate <- function(mat, vec) {
  converged <- FALSE
  n <- 0
  while(!converged) {
    vec <- crossprod(mat, vec)
    n <- n + 1
    if(identical(crossprod(mat, vec), vec)) {
      converged <- TRUE
    }
  }
  print(paste('Converged in', n, 'iterations'))
  return(vec)
}



## Assignment 14 ####
## Taylor Series Approximation of 1/(1-x)
taylor1 <- function(x, n) {
  sum <- 0
  for(i in 0:n) {
    sum <- sum + x^i
  }
  sum
}

## Taylor Series Approximation of e^x
taylor2 <- function(x, n) {
  sum <- 0
  for(i in 0:n) {
    sum <- sum + x^i / factorial(i)
  }
  sum
}

## Taylor Series Approximation of ln(1+x)
taylor3 <- function(x, n) {
  sum <- 0
  for(i in 1:n) { # starting at n=1 since n=0 causes Inf
    sum <- sum + (-1)^(i + 1) * x^i / i
  }
  sum
}
