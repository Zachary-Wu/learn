
#' @title Alpha_diversity_index
#' @description The source code from the url:
#' \url{http://blog.sciencenet.cn/blog-3406804-1179983.html}
#' @param x Community data matrix
#' @param tree A phylo tree object
#' @param base The logarithmic base of the formula
#'
#' @return Calculate Alpha Diversity Index by OTUs:
#' Richness, Chao1, ACE, Shannon, Simpson, Pielou,
#' goods_coverage, Pedigree diversity
#'
#' @importFrom picante pd
#' @importFrom vegan diversity estimateR
#' @export
#'
#' @examples Alpha_diversity_index(phylocom$sample, tree = phylocom$phylo)
#'
Alpha_diversity_index <- function(x, tree = NULL, base = exp(1)) {
  est <- estimateR(x)
  Obs <-  est[1, ]
  Shannon <- diversity(x, index = 'shannon', base = base)
  Simpson <- diversity(x, index = 'simpson')    #Gini-Simpson 指数
  Pielou <- Shannon / log(Obs, base)
  goods_coverage <- 1 - rowSums(x == 1) / rowSums(x)

  result <- rbind(est, Shannon, Simpson,
                  Pielou, goods_coverage)
  # 添加Pd指数计算
  if (!is.null(tree)) {
    # Pd 同时计算谱系多样性(PD)和物种丰富度(SR)
    Pd <- pd(x, tree, include.root = FALSE)[1]
    Pd <- t(Pd)
    result <- rbind(result, Pd)
  }
  result
}


