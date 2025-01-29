my_colors <- c("darkorchid4", "deeppink4", "aquamarine3", "darkolivegreen1")

test_that("Error with no color input", {
  expect_error(create_cell_colors(cell_type = c("A", "B", "C", "D"),
                                  pallet_name = NULL))
  
})

test_that("Error with bad pallet_name put", {
  expect_error(create_cell_colors(cell_type = c("A", "B", "C", "D"),
                                  pallet_name = "NOT A PALLET"))
  
})

test_that("Error with less color than ct", {
  expect_error(create_cell_colors(cell_type = c("A", "B", "C", "D"),
                                  pallet = my_colors[1:3]))
})

create_cell_colors(cell_type = c("A"),
                   pallet = my_colors[1:3])
