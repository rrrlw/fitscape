# file defines the FitLandDF class to store fitness landscape data as a
#   data frame
# definition includes the constructor, validator, and helper

#####CLASS DEFINITION#####
# constructor for FitLandDF class
new_FitLandDF <- function(scape_df, dims) {
  # ensure that scape_df is a data frame with at least 2 values and 3 columns
  if (!is.data.frame(scape_df)) {
    stop("data not provided in data frame format")
  } else if (nrow(scape_df) < 2) {
    stop("must have at least 2 values for a FitLandDF instance")
  } else if (ncol(scape_df) < 3) {
    stop(paste("must have at least 3 columns for a FitLandDF instance",
               "(2 dimensions, 1 column for values)"))
  }

  # ensure that dims is a vector of integers with at least 2 members
  if (!is.integer(dims)) {
    stop("dimensions provided are not integer values")
  } else if (length(dims) < 2) {
    stop("must have at least 2 dimensions for a FitLandDF instance")
  }

  # name columns: last column contains values, others are coordinates
  names(scape_df)[ncol(scape_df)] <- "Value"
  col_seq <- seq_len(ncol(scape_df) - 1)
  names(scape_df)[col_seq] <- paste0("Var", col_seq)

  # create FitLandDF object
  structure(scape_df,
            dims = dims,
            class = c("FitLandDF", "data.frame"))
}

# validator for FitLandDF class
validate_FitLandDF <- function(x) {
  # make sure all coordinates in data frame are within range
  for (curr_col in seq_len(ncol(x) - 1)) {
    curr_vals <- x[[curr_col]]
    min_val <- 1
    max_val <- attr(x, "dims")[curr_col]

    outside_vals <- curr_vals[curr_vals < min_val | curr_vals > max_val]
    if (length(outside_vals) > 0) {
      stop(paste("in dimension", curr_col, "fitness landscape has coordinates",
                 "that are outside of specified dimensions"))
    }
  }

  # return unmodified object if all checks passed
  x
}

#' Create New FitLandDF Instance
FitLandDF <- function(scape_data, dims = dim(scape_data)) {
  scape_df <- NULL

  # if scape_data is a data frame, all good
  if (is.data.frame(scape_data)) {
    scape_df <- scape_data

  # if scape_data is an array, convert to data frame, then all good
  } else if (is.array(scape_data)) {
    # confirm that dims match
    stopifnot(dim(scape_data) == dims)

    # create data frame version of array
    dim_list <- list()
    for (i in seq_len(length(dims))) {
      dim_list[[i]] <- seq_len(dims[i])
    }
    scape_df <- expand.grid(dim_list,
                            KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)

    vals <- vector(mode = "numeric", length = nrow(scape_df))
    for (i in seq_len(nrow(scape_df))) {
      vals[i] <- scape_data[t(as.integer(scape_df[i, ]))]
    }
    scape_df$Value <- vals

    # remove NAs
    scape_df <- subset(scape_df, !is.na(Value))

  # if scape_data is neither array nor data frame, then there's an issue
  } else {
    stop("scape_data parameter is neither data frame nor array, cannot process")
  }

  # return instance
  validate_FitLandDF(new_FitLandDF(scape_df, dims))
}

#####GENERICS#####
# print
print.FitLandDF <- function(x, ...) {
  print(paste("A fitness landscape with dimensions",
              paste(dims(x), collapse = "x"),
              "and for which", nrow(x), "values are known."))
}

# mean
mean.FitLandDF <- function(x, ...) {
  mean(x$Value, ...)
}

# median
median.FitLandDF <- function(x, ...) {
  median(x$Value, ...)
}

#####METHODS#####
# confirm that an object is a valid instance of FitLandDF
is.FitLandDF <- function(x) {
  "FitLandDF" %in% class(validate_FitLandDF(x))
}
is_FitLandDF <- function(x) is.FitLandDF(x)

# dimensions of FitLandDF object
dims <- function(x) {
  stopifnot(is.FitLandDF(x))

  attr(x, "dims")
}

# standard deviation and variance of values in fitness landscape
variance <- function(x, ...) {
  stopifnot(is.FitLandDF(x))

  var(x$Value, ...)
}
sdev <- function(x, ...) {
  stopifnot(is.FitLandDF(x))

  sd(x$Value, ...)
}

# least and highest fitness values
min_fit <- function(x, ...) {
  min(x$Value)
}
max_fit <- function(x, ...) {
  max(x$Value)
}

# get the underlying data frame from the FitLandDF object
extract_df <- function(x) {
  stopifnot(is.FitLandDF(x))

  attr(x, "dims") <- NULL
  class(x) <- "data.frame"
  x
}

# some of the ones below might be moved to a different package someday
# range (max - min)
# extract_values to get matrix/array out
# is_complete to check if all values of the fitness landscape are known
# ?epistasis
