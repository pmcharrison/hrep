#' This function is very inefficient when max_notes is much less than 12, but at least it's cached.
#' @export
get_alphabet <- function(min_notes = 1, max_notes = 12, cache = TRUE) {
  cacheR::cache(
    fun_name = "get_alphabet",
    cache = cache,
    cache_root = "cache",
    cache_dir = "HarmonyUtils/get_alphabet",
    expr = {
      lapply(
        0:11,
        function(bass_pc) {
          sets::set_power(
            x = setdiff(0:11, bass_pc)
          ) %>% as.list %>%
            (function(x) {
              lapply(x, function(y) {
                c(bass_pc + 48,
                  as.numeric(y) + 60)
              })
            })
        }
      ) %>% (function(x) do.call(c, x)) %>%
        (function(x) Filter(
          f = function(y) {
            length(y) >= min_notes && length(y) <= max_notes
          }, x = x
        ))
    })
}
