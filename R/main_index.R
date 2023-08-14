#' Data concurrence index
#'
#' @param rs multiple bands `rast`, with each band of `sign(slope) * pvalue <= alpha`
#'
#' @return
#' ### `cal_DCI`
#' - `n`: number of models
#' - `ND`: number of valid models for each pixel
#' - `NS`: number of significant trend for each pixel
#' - `NSO`: offset significant trend for each pixel
#' - `DCI1`: Data concurrence index, with the denominator of `ND`
#' - `DCI2`: Data concurrence index, with the denominator of `ND`
#' - `ICI1`: Inconsistent index, with the denominator of `ND`
#' - `ICI2`: Inconsistent index, with the denominator of `NS`
#'
#' ### `cal_CSI`
#' - `CSI`: Critical success index, `CSI = a / (a + b + c)`
#' - `bias`: bias = `(a + b) / (a + c)`.
#'    `bias < 1`: A has more significant trend
#'    `bias > 1`: B has more significant trend
#' 
#' @references
#' 1. Kim, S., Anabalón, A., & Sharma, A. (2021). An assessment of concurrency
#'    in evapotranspiration trends across multiple global datasets. Journal of
#'    Hydrometeorology, 22(1), 231–244. doi:10.1175/JHM-D-20-0059.1
#' @export
cal_DCI <- function(rs) {
  if (is.character(rs[[1]])) {
    rs <- read_dci(rs)
  }
  ND <- sum(!is.na(rs)) # number of models
  NS <- sum(abs(rs), na.rm = TRUE) # number of significant model

  NSO <- sum(rs, na.rm = TRUE) # significant trend offset
  DCI1 <- NSO / ND
  DCI2 <- NSO / NS
  # 再添加一个绝对值的

  perc_NS <- NS / ND
  ICI1 <- perc_NS - abs(DCI1) # (NS - NSO) / ND, 不一致性
  ICI2 <- 1 - abs(DCI2) # (NS - NSO) / NS, 分母不同
  listk(n = nlyr(rs), NS, ND, NSO, DCI1, DCI2, ICI1, ICI2)
}

## 写一个站点尺度的
cal_DCI_dt <- function(d, alpha = 0.05) {
  ND <- nrow(d)
  NS <- sum(d$pval <= alpha)
  NSO <- sum(sign(d$slp) * (d$pval <= alpha)) 

  DCI1 <- NSO / ND
  DCI2 <- NSO / NS
  perc_NS <- NS / ND
  ICI1 <- perc_NS - abs(DCI1) # (NS - NSO) / ND, 不一致性
  ICI2 <- 1 - abs(DCI2) # (NS - NSO) / NS, 分母不同
  listk(NS, ND, NSO, DCI1, DCI2, ICI1, ICI2)
}
