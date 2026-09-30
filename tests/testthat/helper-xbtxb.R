# x_b'x_b for equation jx, computed as in draw_parameters_j(): x_matrix
# restricted to the columns not restricted to zero in equation jx.
xbtxb_for <- function(x_matrix, character_beta_matrix, jx) {
  indices_to_remove <- grep("\\b0\\b", character_beta_matrix[, jx])
  if (length(indices_to_remove) > 0) {
    x_matrix <- x_matrix[, -indices_to_remove, drop = FALSE]
  }
  crossprod(x_matrix)
}
