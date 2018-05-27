#' @export
normalise_pc_set_transition <- function(context_pc_set_id,
                                        continuation_pc_set_id) {
  context_pc_set_id <- as.integer(context_pc_set_id)
  continuation_pc_set_id <- as.integer(continuation_pc_set_id)
  stopifnot(length(context_pc_set_id) == 1L)
  stopifnot(length(continuation_pc_set_id) == 1L)
  stopifnot(!is.na(continuation_pc_set_id))
  context_pc_set <- if (!is.na(context_pc_set_id)) {
    decode_pc_set(context_pc_set_id)
  }
  continuation_pc_set <- decode_pc_set(continuation_pc_set_id)
  anchor_pc_set <-
    if (is.null(context_pc_set)) continuation_pc_set else context_pc_set
  transposition <- get_transposition(
    get_pc_set_normal_form(anchor_pc_set))
  trans_context_pc_set_id <- if (is.na(context_pc_set_id)) {
    as.integer(NA)
  } else {
    encode_pc_set(transpose(context_pc_set,
                            transposition,
                            safe = FALSE))
  }
  trans_continuation_pc_set_id <- encode_pc_set(
    transpose(continuation_pc_set, transposition, safe = FALSE)
  )
  c(trans_context_pc_set_id, trans_continuation_pc_set_id)
}
