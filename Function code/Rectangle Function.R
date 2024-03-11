#' This function determines the volume and surface area of a rectangular prism
#' @param height pyramid height default=1
#' @param width pyramid width default=1
#' @param depth pyramid depth default=1
#' @return volume (W)

RectangleProperties <- function(height = 1, width = 1, depth = 1) {
  # Error checking for non-positive values
  if(height <= 0) {
    stop("Height must be greater than 0")
  }
  if(width <= 0) {
    stop("Width must be greater than 0")
  }
  if(depth <= 0) {
    stop("Depth must be greater than 0")
  }
  
  # Calculate volume
  volume = height * width * depth
  
  # Calculate surface area
  surfaceArea = 2 * (width * depth + width * height + height * depth)
  
  # Return both volume and surface area in a list
  return(list(volume = volume, surfaceArea = surfaceArea))
}

# Example usage
# This will work
properties <- RectangleProperties(height = 4, width = 2, depth = 1)
cat("Volume:", properties$volume, "Surface Area:", properties$surfaceArea, "\n")

# This will stop the function and give an error message
# properties <- RectangleProperties(height = -1, width = 2, depth = 1)

properties <- RectangleProperties(height = -1, width = 2, depth = 1)
#returns error message

properties <- RectangleProperties(height = 0, width = 0, depth = 0)
#returns error message

library(testthat)

# Example test file content
test_that("Function returns expected volume", {
  expect_equal(RectangleProperties(1, 1, 1)$volume, 1)
})

test_that("Function errors on negative dimensions", {
  expect_error(RectangleProperties(-1, 1, 1))
})
