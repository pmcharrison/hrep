#' @export
get_alphabet <- function(cache = TRUE) {
  cacheR::cache(
    fun_name = "get_alphabet",
    cache = cache,
    cache_root = "cache",
    cache_dir = "HarmonyStats/get_alphabet",
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
      ) %>% (function(x) do.call(c, x))
    })
}
