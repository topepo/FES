# ------------------------------------------------------------------------------
# Feature Engineering and Selection: A Practical Approach for Predictive Models
# by Max Kuhn and Kjell Johnson
#
# ------------------------------------------------------------------------------
# Code to make more readable variable names for Ames data

options(width = 120)


clean_value <- function(x) {
  x <- str_replace(x, "Neighborhood_", "Neighborhood: ")
  x <- str_replace(x, "MS_SubClass_", "MS SubClass: ")
  x <- str_replace(x, "Land_Contour_", "Land Contour: ")
  x <- str_replace(x, "Roof_Style_", "Roof Style: ")
  x <- str_replace(x, "Foundation_", "Foundation: ")
  x <- str_replace(x, "Garage_Finish_", "Garage Finish: ")
  x <- str_replace(x, "Central_Air", "Central Air:")
  x <- str_replace(x, "Bldg_Type", "Building Type")
  x <- str_replace(x, "Alley", "Alley:")
  x <- str_replace(x, "Gr_Liv_Area", "Living Area")
  x <- str_replace(x, "_bs_1", " (spline)")   
  x <- str_replace(x, "_bs_2", " (spline)")
  x <- str_replace(x, "_bs_3", " (spline)")
  x <- str_replace_all(x, "_", " ")
  x
}

