  fcm.infer <- function (activation_vec, weight_mat, iter = 20, infer = 'k', transform = 's', lambda = 1, e = 0.001) {
    # ------------------------------------------ checks on function input ------------------------------------------------------------------------------------ #
    # Check the values of the activation vector
    if (length(which(activation_vec > 1)) & length(which(activation_vec > -1))) {
      stop ("Please check the concepts' values of the activation vector. They must be in the range -1 and 1.")
    }
    # Check the weights of the matrix
    if (length(which(weight_mat > 1)) & length(which(weight_mat > -1)) ) {
      stop ("Please check the weights of the matrix. They must be in the range -1 and 1.")
    }
    # Check for missing values
    if (sum(is.na(activation_vec)) > 0) {
      stop ("Please check the activation vector for missing values.")
    }
    if (sum(is.na(weight_mat)) > 0) {
      stop ("Please check the weight matrix for missing values.")
    }
    # Check the variable of the transformation function
    if(iter <= 0 ) stop ("The iterations must be higher than zero.")
    # Check the variable of the Inference Rule
    if(sum(!(infer %in% c('k', 'mk', 'r', 'kc', 'mkc', 'rc'))) > 0)
      stop ("For the Inference Rule only Kosko 'k', modified Kosko 'mk',  Rescale 'r', Kosko-clamped 'kc', modified Kosko-clamped 'mkc' or Rescale-clamped 'rc' variables are allowed.")
    # Check the variable of the transformation function
    if(sum(!(transform %in% c('b', 'tr', 's', 't'))) > 0)
      stop ("For the transformation functions only Bivalent 'b', Trivalent 'tr', Sigmoid 's' or
            Hyperbolic tangent 't' variables are allowed.")
    # Check the variable of the lambda value
    if((lambda <= 0) || (lambda >= 10)) stop ("Lambda value should be in the range 1 to 10.")
    # Check the variable of e parameter
    if((e < 0.000001) || (e > 0.01)) stop ("Epsillon (e) value should be in the range 0.01 to 0.000001.")
    # ------------------------------------------ Input values ------------------------------------------------------------------------------------ #
    m <- ncol(weight_mat)
    # ------------------------------------------ Inference Rules  ------------------------------------------------------------------------------------ #
    mylist <- list()
    for(i in 1:(iter-1)) {
      if(i == 1) {
        if (infer == "k" || infer == "kc"){
          initial_vec <- colSums(t(activation_vec) * weight_mat)
        } else if  (infer == "mk" || infer == "mkc"){
          initial_vec <- activation_vec + colSums(t(activation_vec) * weight_mat)
        } else if (infer == "r" || infer == "rc"){
          initial_vec <- (2 * activation_vec - 1) + colSums(t((2 * activation_vec) - 1) * weight_mat)
        }

        if (transform == "s") {
          initial_vec <- 1/(1+exp(- lambda * initial_vec)) }
        if (transform == "t") {
          initial_vec <- tanh(lambda * initial_vec)
        }
      } else {
        # calculates the new vector (for the second until the last iteration or time step)
        if (infer == "k" || infer == "kc"){
          initial_vec <- colSums(t(initial_vec) * weight_mat)
        } else if  (infer == "mk" || infer == "mkc"){
          initial_vec <- initial_vec + colSums(t(initial_vec) * weight_mat)
        } else if (infer == "r" || infer == "rc"){
          initial_vec <- (2 * initial_vec - 1) + colSums(t((2 * initial_vec) - 1) * weight_mat)
        }

        if (transform == "s") {
          initial_vec <- 1/(1+exp(- lambda * initial_vec)) }
        if (transform == "t") {
          initial_vec <- tanh(lambda * initial_vec)
        }
      }
      if (transform == "b") {
        for(j in 1:m) {
          if (initial_vec[j] > 0){
            initial_vec[j] <- 1
          } else if (initial_vec[j] <= 0){
            initial_vec[j] <- 0
          }
        }
      }
      if (transform == "tr") {
        for(j in 1:m) {
          if (initial_vec[j] > 0){
            initial_vec[j] <- 1
          } else if (initial_vec[j] < 0){
            initial_vec[j] <- - 1
          } else initial_vec[j] <- 0
        }
      }
      if (infer == "kc" || infer == "mkc" || infer == "rc"){
        for(k in 1:m) {
          if(activation_vec[k] == 1) {
            initial_vec[k] <- (initial_vec[k] = 1)
          }
        }
      }
      mylist[[i]] <- initial_vec     # insert each produced stabilized vector in the list
   }
    # transform the produced stabilized vectors into a data frame
    steps_t <- (as.data.frame (do.call("rbind",mylist)))
    step_1 <- as.numeric(unlist(activation_vec))
    # Insert the activation vector in the first row of the dataframe that contains the stabilized vectors of all time steps
    A <- (rbind(step_1, steps_t))
    last_conv <- as.double(A[iter,] - A[(iter-1),])   # check if the steady state has been reached of the last two iterations
    Res_e <- (length(last_conv[last_conv <= e]))    # Set the residual value (epsillon "e") equal to 0.001
    if ( Res_e < m)  {
      cat("\n WARNING: More iterations are required to reach the convergence.\n \n")
    } else {
      mylist1 <- list()
      for(i in 2:(iter)){
        subst <- abs(apply(A, 2, function(x) x[i] - x[i-1]))   # subtraction of "ith" - "(i-1)th" state
        mylist1[[i]] <- subst     # Save all subtraction vectors in a list
      }
      subst.mat <- do.call("rbind",mylist1)
      w <- as.data.frame(matrix(e, (iter - 1), m))    # Create a dataframe [(iterations - 1), m)] of values = epsillon
      mylist3 <- list()
      for(i in 1:(iter-1)){
        if(all(subst.mat[i,] < w[i,]))    # Check for the converged state
        {
          cv <- 1      # The concepts' value (cv) is converged
        }
        else {
          cv <- 2      # The concepts' value is NOT converged
        }
        mylist3[[i]] <- cv
      }
      cv.mat<-do.call("rbind",mylist3)
      if (cv.mat[[i]] == 2) {
        cat("\n WARNING: More iterations are required to reach the convergence.\n \n")
      } else {
      conv_state <- min(which(cv.mat == 1))
      cat(sprintf("\n The concepts' values are converged in the %ith state (e <= %f) \n", conv_state + 1, e))
      cat("\n")
      print(A[(conv_state + 1),], row.names = FALSE)
      cat("\n")
       }
     }
     outlist <- list('values'= A)     # the concepts values in each state
    return (outlist)
  }
