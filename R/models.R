#' @name LogisticRegression
#' @title Multinomial Logistic Regression
#' @description
#' The `LogisticRegression` class provides an implementation of multinomial logistic regression
#' that includes data preprocessing for mixed data types (numeric, categorical, and mixed).
#' The model is trained using gradient descent, and predictions can be made for class labels or
#' class probabilities.
#'
#' @section Methods:
#' - `new(learning_rate = 0.01, num_iterations = 1000)`: Create a new instance of the `LogisticRegression` class.
#' - `fit(X, y)`: Train the logistic regression model on features `X` and labels `y`.
#' - `predict(X)`: Predict class labels for the input data `X`.
#' - `predict_proba(X)`: Compute the class probabilities for the input data `X`.
#' - `preprocess_data(X)`: Preprocess the input data `X` based on the type (numeric, categorical, or mixed).
#' - `var_importance()`: Return a ranked list of feature importances.
#' - `summary()`: Display a detailed summary of the model.
#' - `print()`: Print a quick overview of the model.
#'
#' @section Usage:
#' ```
#' # Create a new LogisticRegression instance
#' model <- LogisticRegression$new(learning_rate = 0.01, num_iterations = 1000)
#'
#' # Fit the model
#' model$fit(X, y)
#'
#' # Predict
#' predictions <- model$predict(X_new)
#'
#' # Feature importance
#' importance <- model$var_importance()
#' ```
#'
#' @section Preprocessing:
#' The model preprocesses the data based on its type:
#' - Numeric: Scaled using standard normalization.
#' - Categorical: Transformed using Multiple Correspondence Analysis (MCA).
#' - Mixed: Transformed using Factorial Analysis of Mixed Data (FAMD).
#'
#' @section Attributes:
#' - `learning_rate`: Learning rate for gradient descent.
#' - `num_iterations`: Number of iterations for gradient descent.
#' - `weights`: Weight matrix for the model.
#' - `bias`: Bias terms for the model.
#' - `transformation_type`: The type of preprocessing applied (`"scale"`, `"MCA"`, or `"FAMD"`).
#' - `transformation_model`: The fitted transformation model (`FAMD` or `MCA`).
#' - `original_classes`: The original class labels from the training data.
#'
#' @examples
#' # Example dataset
#' X <- data.frame(a = rnorm(100), b = rnorm(100), c = rnorm(100))
#' y <- sample(c("A", "B", "C"), 100, replace = TRUE)
#'
#' # Initialize the model
#' model <- LogisticRegression$new(learning_rate = 0.05, num_iterations = 500)
#'
#' # Fit the model
#' model$fit(X, y)
#'
#' # Predict on new data
#' predictions <- model$predict(X)
#'
#' # Feature importance
#' importance <- model$var_importance()
#'
#' @import R6
#' @import FactoMineR
#' @import dplyr
#' @export

#library(R6)
#library(dplyr)
LogisticRegression  <- R6Class("LogisticRegression",
  public = list(
    #' @field learning_rate Learning rate for gradient descent.
    learning_rate = NULL,
    
    #' @field num_iterations Number of iterations for training.
    num_iterations = NULL,
    
    #' @field weights Weight matrix of the model.
    weights = NULL,
    
    #' @field bias Bias terms of the model.
    bias = NULL,
    
    #' @field num_classes Number of classes in the training data.
    num_classes = NULL,
    
    #' @field transformation_model Transformation model for preprocessing.
    transformation_model = NULL,
    
    #' @field transformation_type Type of preprocessing applied.
    transformation_type = NULL,
    
    #' @field original_classes Original class labels from the training data.
    original_classes = NULL,
    
    # Constructeur de classe
    #' Initialize the LogisticRegression model
    #'
    #' @param learning_rate Numeric. Learning rate for gradient descent.
    #' @param num_iterations Integer. Number of iterations for training.
    initialize = function(learning_rate = 0.01, num_iterations = 1000) {
      self$learning_rate <- learning_rate
      self$num_iterations <- num_iterations
      self$weights <- NULL
      self$bias <- NULL
      print(paste("Valeur actuelle de self$transformation_type :", self$transformation_type))
    },
    #' @description Compute the softmax transformation for the input matrix.
    #' @param z A numeric matrix. Input for the softmax function.
    #' @return A numeric matrix with softmax-applied values.
    softmax = function(z) { 
      z = z - max(z)  # On soustrait la valeur maximale pour eviter des valeurs trop grandes
      exp_z = exp(z)
      return(exp_z / rowSums(exp_z))  # Normalisation par la somme des exponentielles
    },
    
    #' @description Compute the cost for the logistic regression model.
    #' @param y A matrix of true labels (one-hot encoded).
    #' @param y_pred A matrix of predicted probabilities.
    #' @return Numeric. The cost value.
    compute_cost = function(y, y_pred) {
      m <- nrow(y)
      cost <- -(1 / m) * sum(y * log(y_pred))
      return(cost)
    },
    # Pretraitement des donnees (conversion en matrices)
    #' @param X DataFrame
    preprocess_data = function(X) {
      # Verifier la structure des colonnes
      is_numeric <- sapply(X, is.numeric)
      is_categorical <- sapply(X, is.factor) | sapply(X, is.character)
      print(paste("Valeur actuelle de self$transformation_type :", self$transformation_type))
      
      #if (any(is_numeric) && any(is_categorical)) {
        #print("Donnees mixtes detectees : application de l'AFDM.")
        #self$transformation_type <- "FAMD"
        #self$transformation_model <- FAMD(X, graph = FALSE)
        #processed_data <- as.data.frame(self$transformation_model$ind$coord)
      #} else if (all(is_categorical)) {
        #print("Donnees categorielle detectees : application de l'ACM.")
        #self$transformation_type <- "MCA"
        #self$transformation_model <- MCA(X, graph = FALSE)
        #processed_data <- as.data.frame(self$transformation_model$ind$coord)
      #} else 
      if (all(is_numeric)) {
        print("Donnees quantitatives detectees : normalisation.")
        self$transformation_type <- "scale"
        self$transformation_model <- scale(X, center = TRUE, scale = TRUE)
        processed_data <- as.data.frame(scale(X, center = attr(self$transformation_model, "scaled:center"), scale = attr(self$transformation_model, "scaled:scale")))
      } else {
        stop("Impossible de determiner le type de donnees.")
      }
      return(processed_data)
    },
    #' Fit the model
    #'
    #' @param X Data frame or matrix. Features for training.
    #' @param y Factor or vector. Target labels.
    fit = function(X, y) {
      X <- self$preprocess_data(X)
      print(paste("Transformation utilisee dans fit apres preprocess_data :", self$transformation_type))  # Verification
      X <- as.matrix(X)
      m <- nrow(X)
      n <- ncol(X)
      # Sauvegarde des classes d'origine
      self$original_classes <- levels(as.factor(y))
      print(paste("Classes d'origine :", paste(self$original_classes, collapse = ", ")))
      self$num_classes <- length(self$original_classes)
      y <- as.integer(as.factor(y))
      print(paste("Classes encodees :", paste(unique(y), collapse = ", ")))
      y_one_hot <- matrix(0, nrow = m, ncol = self$num_classes)
      for (i in 1:m) {
        y_one_hot[i, y[i]] <- 1
      }
      self$weights <- matrix(rnorm(n * self$num_classes, mean = 0, sd = 0.01), nrow = n, ncol = self$num_classes)
      self$bias <- rep(0, self$num_classes)
      
      for (i in 1:self$num_iterations) {
        linear_model <- X %*% self$weights + matrix(rep(self$bias, each = m), nrow = m)
        y_pred <- self$softmax(linear_model)
        dw <- (1 / m) * t(X) %*% (y_pred - y_one_hot)
        db <- (1 / m) * colSums(y_pred - y_one_hot)
        self$weights <- self$weights - self$learning_rate * dw
        self$bias <- self$bias - self$learning_rate * db
      }
    },
    
    #' @description
    #' This method preprocesses new data based on the transformation type
    #' learned during the training phase.
    #'
    #' @param X_new A data frame or matrix. The new input data to preprocess.
    #' @return A processed data frame or matrix ready for prediction.
    preprocess_new_data = function(X_new) {
      if (is.null(self$transformation_type)) {
        stop("La transformation n'a pas ete definie. Veuillez appeler la methode `fit` avant `predict`.")
      }
      if (self$transformation_type == "FAMD") {
        print("Application de l'AFDM sur les nouvelles donnees.")
        processed_data <- predict(self$transformation_model, newdata = X_new)$coord
      } else if (self$transformation_type == "MCA") {
        print("Application de l'ACM sur les nouvelles donnees.")
        processed_data <- predict(self$transformation_model, newdata = X_new)$coord
      } else if (self$transformation_type == "scale") {
        print("Application de la normalisation sur les nouvelles donnees.")
        processed_data <- as.data.frame(scale(X_new, center = attr(self$transformation_model, "scaled:center"), scale = attr(self$transformation_model, "scaled:scale")))
      } else {
        stop("Aucune transformation enregistree pour ces donnees.")
      }
      return(processed_data)
    },
    # Calcul des probas d'appartenance
    #' Predict class probabilities
    #'
    #' @param X Data frame or matrix. Features for prediction.
    #' @return A matrix of probabilities.
    predict_proba = function(X) {
      if (!is.matrix(X)) {
        X <- as.data.frame(lapply(X, as.numeric))
        X <- as.matrix(X)
      }
      linear_model <- X %*% self$weights + matrix(rep(self$bias, each = nrow(X)), nrow = nrow(X))
      return(self$softmax(linear_model))
    },
    
    #' Predict class labels
    #'
    #' @param X Data frame or matrix. Features for prediction.
    #' @return A vector of predicted labels.
    predict = function(X) {
      X <- self$preprocess_new_data(X)
      probas <- self$predict_proba(X)
      print("Distribution des probabilites pour chaque classe :")
      predicted_indices <- max.col(probas)
      # Mapper les indices aux classes d'origine
      predicted_classes <- self$original_classes[predicted_indices]
      return(predicted_classes)
    },
    #' @description
    #' This method prints a brief summary of the logistic regression model,
    #' including hyperparameters and basic information.
    #'
    #' @return None. Prints information to the console.
    print = function() {
      cat("MLR Model\n")
      cat("============================\n")
      cat("Learning Rate:", self$learning_rate, "\n")
      cat("Number of Iterations:", self$num_iterations, "\n")
      cat("Transformation Type:", ifelse(is.null(self$transformation_type), "Not fitted yet", self$transformation_type), "\n")
      cat("Number of Classes:", ifelse(is.null(self$num_classes), "Not fitted yet", self$num_classes), "\n")
      cat("Weights:", ifelse(is.null(self$weights), "Not initialized", "Initialized"), "\n")
      cat("Bias:", ifelse(is.null(self$bias), "Not initialized", "Initialized"), "\n")
    }, 
    #' @description
    #' This method provides a detailed summary of the logistic regression model,
    #' including the transformation type, weight dimensions, and number of classes.
    #'
    #' @return None. Prints information to the console.
    summary = function() {
      cat("MLR Model Summary\n")
      cat("============================\n")
      cat("Hyperparameters:\n")
      cat("  - Learning Rate:", self$learning_rate, "\n")
      cat("  - Number of Iterations:", self$num_iterations, "\n\n")
      cat("Model Details:\n")
      cat("  - Transformation Type:", ifelse(is.null(self$transformation_type), "Not fitted yet", self$transformation_type), "\n")
      if (!is.null(self$transformation_model)) {
        cat("  - Transformation Model Summary:\n")
        print(summary(self$transformation_model))
      }
      cat("  - Number of Classes:", ifelse(is.null(self$num_classes), "Not fitted yet", self$num_classes), "\n\n")
      cat("Weights and Bias:\n")
      cat("  - Weights:", ifelse(is.null(self$weights), "Not initialized", paste(dim(self$weights), collapse = " x ")), "\n")
      cat("  - Bias:", ifelse(is.null(self$bias), "Not initialized", length(self$bias)), "\n")
      if (!is.null(self$original_classes)) {
        cat("Classes:\n")
        cat("  -", paste(self$original_classes, collapse = ", "), "\n")
      }
    },
    
    #' Calculate feature importance
    #'
    #' @return A data frame of feature importance.
    var_importance = function() {
      # Calcul de l'importance : moyenne absolue des poids pour chaque variable
      importance_scores <- rowMeans(abs(self$weights))
      
      # Noms des variables : utiliser les noms des colonnes si disponibles
      variable_names <- if (!is.null(colnames(self$weights))) {
        colnames(self$weights)
      } else {
        paste0("Var", seq_along(importance_scores))
      }
      
      # Construction du tableau d'importance
      importance_df <- data.frame(
        Variable = variable_names,
        Importance = importance_scores
      )
      
      # Tri par importance decroissante
      importance_df <- importance_df[order(-importance_df$Importance), ]
      
      # Retour du tableau pour un usage ulterieur
      return(importance_df)
    }
  )
)
