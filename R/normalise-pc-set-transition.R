#' @export
normalise_pc_set_transition <- function(context_pc_set_id,
                                        continuation_pc_set_id) {
  checkmate::qassert(context_pc_set_id, "x1")
  checkmate::qassert(continuation_pc_set_id, "X1")
  context_pc_set <- if (!is.na(context_pc_set_id))
    decode(context_pc_set_id, "pc_set")[[1]]
  continuation_pc_set <- decode(continuation_pc_set_id, "pc_set")[[1]]
  anchor_pc_set <-
    if (is.null(context_pc_set)) continuation_pc_set else context_pc_set
  transposition <- get_transposition(pc_set_norm_form(anchor_pc_set))
  trans_context_pc_set_id <- if (is.na(context_pc_set_id))
    as.integer(NA) else encode(transpose(context_pc_set, transposition))
  trans_continuation_pc_set_id <- encode(transpose(continuation_pc_set,
                                                   transposition))
  c(trans_context_pc_set_id, trans_continuation_pc_set_id)
}
