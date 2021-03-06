% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Alpha_diversity_index.R
\name{Alpha_diversity_index}
\alias{Alpha_diversity_index}
\title{Alpha_diversity_index}
\usage{
Alpha_diversity_index(x, tree = NULL, base = exp(1))
}
\arguments{
\item{x}{Community data matrix}

\item{tree}{A phylo tree object}

\item{base}{The logarithmic base of the formula}
}
\value{
Calculate Alpha Diversity Index by OTUs:
Richness, Chao1, ACE, Shannon, Simpson, Pielou,
goods_coverage, Pedigree diversity
}
\description{
The source code from the url:
\url{http://blog.sciencenet.cn/blog-3406804-1179983.html}
}
\examples{
Alpha_diversity_index(phylocom$sample, tree = phylocom$phylo)

}
