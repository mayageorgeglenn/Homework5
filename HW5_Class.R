## HW5_Class.R

setClass(
  Class = "sparse_numeric",
  slots = c(
    value = "numeric",
    pos = "integer",
    length = "integer"
  )
)

## validity check
setValidity("sparse_numeric", function(object) {
  
  if (length(object@value) != length(object@pos))
    return("Length of 'value' and 'pos' must match.")
  
  if (any(is.na(object@pos)))
    return("'pos' contains NA values.")
  
  if (any(object@pos < 1 | object@pos > object@length))
    return("'pos' values must be within vector length range.")
  
  TRUE
})

## coercion methods
setAs("numeric", "sparse_numeric", function(from) {
  new("sparse_numeric",
      value = from[from != 0],
      pos = as.integer(which(from != 0)),
      length = as.integer(length(from)))
})

setAs("sparse_numeric", "numeric", function(from) {
  x <- numeric(from@length)
  x[from@pos] <- from@value
  x
})

## generics

setGeneric("sparse_add", function(x, y, ...) standardGeneric("sparse_add"))
setGeneric("sparse_sub", function(x, y, ...) standardGeneric("sparse_sub"))
setGeneric("sparse_mult", function(x, y, ...) standardGeneric("sparse_mult"))
setGeneric("sparse_crossprod", function(x, y, ...) standardGeneric("sparse_crossprod"))

# helper function
get_val <- function(named_vec, p) {
  val <- named_vec[as.character(p)]
  ifelse(!is.na(val), val, 0)
}

## sparse_add
setMethod("sparse_add", c("sparse_numeric", "sparse_numeric"), function(x, y) {
  if (x@length != y@length)
    stop("Vectors must be of same length")
  
  all_pos <- sort(union(x@pos, y@pos))
  x_vals <- setNames(x@value, x@pos)
  y_vals <- setNames(y@value, y@pos)
  
  summed <- vapply(all_pos, function(p) get_val(x_vals, p) + get_val(y_vals, p), numeric(1))
  non_zero <- summed[summed != 0]
  
  new("sparse_numeric",
      value = as.numeric(non_zero),
      pos = as.integer(all_pos[summed != 0]),
      length = x@length)
})

## sparse_sub
setMethod("sparse_sub", c("sparse_numeric", "sparse_numeric"), function(x, y) {
  if (x@length != y@length)
    stop("Vectors must be of same length")
  
  all_pos <- union(x@pos, y@pos)
  x_vals <- setNames(x@value, x@pos)
  y_vals <- setNames(y@value, y@pos)
  
  diff <- sapply(all_pos, function(p) get_val(x_vals, p) - get_val(y_vals, p))
  non_zero <- diff[diff != 0]
  valid_idx <- !is.na(non_zero) & !is.na(as.integer(names(non_zero)))
  
  new("sparse_numeric",
      value = as.numeric(non_zero[valid_idx]),
      pos = as.integer(names(non_zero)[valid_idx]),
      length = x@length)
})

## sparse_mult
setMethod("sparse_mult", c("sparse_numeric", "sparse_numeric"), function(x, y) {
  if (x@length != y@length)
    stop("Vectors must be of same length")
  
  common_pos <- intersect(x@pos, y@pos)
  prod_vals <- x@value[match(common_pos, x@pos)] * y@value[match(common_pos, y@pos)]
  
  new("sparse_numeric",
      value = prod_vals,
      pos = as.integer(common_pos),
      length = x@length)
})

## sparse_crossprod
setMethod("sparse_crossprod", c("sparse_numeric", "sparse_numeric"), function(x, y) {
  if (x@length != y@length)
    stop("Vectors must be same length")
  
  common_pos <- intersect(x@pos, y@pos)
  sum(x@value[match(common_pos, x@pos)] * y@value[match(common_pos, y@pos)])
})

## operator overloading
setMethod("+", c("sparse_numeric", "sparse_numeric"), function(e1, e2) sparse_add(e1, e2))
setMethod("-", c("sparse_numeric", "sparse_numeric"), function(e1, e2) sparse_sub(e1, e2))
setMethod("*", c("sparse_numeric", "sparse_numeric"), function(e1, e2) sparse_mult(e1, e2))

## display and utility methods

setMethod("show", "sparse_numeric", function(object) {
  cat("Sparse numeric vector of length", object@length, "\n")
  cat("Non-zero elements:\n")
  print(data.frame(pos = object@pos, value = object@value))
})

setMethod("plot", c("sparse_numeric", "sparse_numeric"), function(x, y) {
  plot(x@pos, x@value, col = "blue", pch = 16,
       xlab = "Position", ylab = "Value",
       main = "Sparse Vector Comparison")
  points(y@pos, y@value, col = "red", pch = 17)
  legend("topright", legend = c("x", "y"),
         col = c("blue", "red"), pch = c(16,17))
})

## choice method find L2 norm
setGeneric("sparse_norm", function(x) standardGeneric("sparse_norm"))
setMethod("sparse_norm", "sparse_numeric", function(x) sqrt(sum(x@value^2)))