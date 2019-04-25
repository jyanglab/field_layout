#' \code{Assign genotypes to random rows.}
#'
#'
#'
#' @param rows A vector of row numbers. [vector, =c(1001, 1002)]
#' @param genos A vector of genotypes with the same length with rows. [vector, =c("G1", "G2")]
#'
#' @return return a data.frame with rows and genos
#'
#' @examples
#' assign_random_rows()
#'
#' @export
assign_random_rows <- function(rows=c(1001, 1002), genos=c("Genotype1", "Genotype2")){
  #startrow: the starting row of a split plot
  #genotypes: The genotypes to be randomized

  if(length(rows) != length(genos)) stop("[ERROR!] length of rows not eq to the length of genos!")

  a <- sample(x=rows, size=length(genos))
  df <- data.frame(row=a, geno=genos)

  return(df[order(df$row),])
}
