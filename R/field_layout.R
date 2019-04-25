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
#'
field_layout <- function(randnum=1234567, mygrp, addrow=0, ){
  #randnum: seed value for the random number generator

  set.seed(randnum)
  ## layout the blocks
  idx <- sample(1:4, 4)

  out1 <- mygrp[[idx[1]]]
  out1$PI <- as.character(out1$PI) #57 genotypes
  subp11 <- sample_rows(startrow=1001, genotypes=c(out1$PI[1:19], "check", "check"))
  subp12 <- sample_rows(startrow=1017, genotypes=c(out1$PI[20:38], "check", "check"))
  subp13 <- sample_rows(startrow=1033, genotypes=c(out1$PI[39:58], "check", "check"))

  out2 <- mygrp[[idx[2]]]
  out2$PI <- as.character(out2$PI) #57 genotypes
  subp21 <- plotsample(startrow=1049, genotypes=c(out2$PI[1:19], "check", "check"))
  subp22 <- plotsample(startrow=1065, genotypes=c(out2$PI[20:38], "check", "check"))
  subp23 <- plotsample(startrow=1081, genotypes=c(out2$PI[39:58], "check", "check"))

  out3 <- mygrp[[idx[3]]]
  out3$PI <- as.character(out3$PI) #57 genotypes
  subp31 <- plotsample(startrow=1301, genotypes=c(out3$PI[1:19], "check", "check"))
  subp32 <- plotsample(startrow=1317, genotypes=c(out3$PI[20:38], "check", "check"))
  subp33 <- plotsample(startrow=1333, genotypes=c(out3$PI[39:58], "check", "check"))

  out4 <- mygrp[[idx[4]]]
  out4$PI <- as.character(out4$PI) #57 genotypes
  subp41 <- plotsample(startrow=1349, genotypes=c(out4$PI[1:19], "check", "check"))
  subp42 <- plotsample(startrow=1365, genotypes=c(out4$PI[20:38], "check", "check"))
  subp43 <- plotsample(startrow=1381, genotypes=c(out4$PI[39:58], "check", "check"))

  out <- rbind(subp11, subp12, subp13, subp21, subp22, subp23,
               subp31, subp32, subp33, subp41, subp42, subp43)

  out <- subset(out, !is.na(geno))
  out$row <- out$row + addrow
  return(out[order(out$row),])

}


assign_random_rows <- function(rows=c(1001, 1002), genos=c("Genotype1", "Genotype2")){
  #startrow: the starting row of a split plot
  #genotypes: The genotypes to be randomized

  if(length(rows) != length(genos)) stop("[ERROR!] length of rows not eq to the length of genos!")

  a <- sample(x=rows, size=length(genos))
  df <- data.frame(row=a, geno=genos)

  return(df[order(df$row),])
}
