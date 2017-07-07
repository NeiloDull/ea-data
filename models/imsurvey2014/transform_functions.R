list(
  'make_p_inc_donate' = function(donate2013, income2013) {
    p_inc_donate <- mapply(function(d, i) {
      d <- as.numeric(d)
      i <- as.numeric(i)
      if (is.na(i)) return("")
      if (is.na(d)) return("")
      (d / i) * 100
    }, donate2013, income2013)
    is.annoying <- function(x) {
      is.na_like(x) | is.nan(x) |
        is.infinite(x) | x == "NaN" | x == "Inf"
    }
    sapply(p_inc_donate, function(p) {
      if (is.annoying(p)) return(NA)
      p
    })
  }
)
