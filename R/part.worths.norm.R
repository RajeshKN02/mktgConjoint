part.worths.norm <- function(ratings, bundles, standard = 0) {
  #  Copyright 2016 Jordi L. Sintas
  #  This program is free software; you can redistribute it and/or
  #  modify it under the terms of the GNU General Public License
  #  as published by the Free Software Foundation; either version 2
  #  of the License, or (at your option) any later version.
  #
  #  This program is distributed in the hope that it will be useful,
  #  but WITHOUT ANY WARRANTY; without even the implied warranty of
  #  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  #  GNU General Public License for more details.
  #
  #  You should have received a copy of the GNU General Public License
  #  along with this program; if not, write to the Free Software
  #  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
  #
  #' @title part.worths.norm
  #' @description
  #'
  #'   This function computes the partworth of attribute levels for each
  #'   respondent. It uses treatment effects; ratings is a matrix of clients
  #'   (rows) by bundles rated (colums); bundles is a matrix of
  #'   bundles (rows) by attributes (colums).
  #'   This function computes the partworth of attribute levels for each
  #'   respondent But transform coefficient so that the sum of part worths
  #'   for each attribute is zero Uses treatment effects ratings is a matrix
  #'   of clients (rows) by bundles rated (colums) bundles is a
  #'   matrix of bundles (rows) by attributes (colums) If standard is 1,
  #'   then parth worths are normalized read in conjoint survey profiles
  #'   with respondent ranks
  #' @param ratings                 a data frame with all clients' ratings
  #' @param bundles                 a data frame with all product profiles rated by clients
  #' @param standard                whether we should standaridized clientes ratings. Default equal to FALSE (0)
  #' @return conjoint.fit.sum.zero     the object returned if standard=0
  #' @return conjoint.fit.sum.zero.st  the object returned if standard=1
  #' @export
  #' @examples
  #'   data(osc)
  #'   part.worths(osc$ratings, osc$bundles)
  #' @author Jordi L. Sintas

  ############################# other variables in the function #########
  #' conjoint.fit            a data frame with the coefficients of many objects returned by lm()
  # clients                 number of clients that have rated the set of bundles
  # y                       the ratings of client number i
  # df                      a compposed file of client's ratings plus the bundles description according to bundles matrix
  # n                       number of variables in df data table
  # main.effects.model.fit  an object return by lm() function
  # clients.names           a vector with clients' names
  # conjoint.results        a list wiht contrasts, xlevels, attributes, and coeficients taken from main.effects.model.fit
  # part.worths             a vector with conjoint.results$xlevels
  # end.index.for.coefficient and index for coefficients
  # part.worths.vector      a vetor for collecting part worths
  # nlevels                 an index with attribute's number of levels
  # begin.index.for.coefficient an index for attribute's level coefficient
  # end.index.for.coefficient   an index for attribute's level coefficient
  # last.part.worth
  # clients.names           a vector with clients' names
  # level.names             a matrix with attributes' level names
  # row.names               a vector with levels' names
  ###################################################

  conjoint.fit.sum.zero <- data.frame()
  conjoint.fit.sum.zero.st <- data.frame()
  clients <- nrow(ratings)
  y <- as.data.frame(t(ratings[1, ]))
  names(y) <- "y"
  dim(y)
  dim(bundles)
  df <- cbind(y, bundles)
  dim(df)
  class(df)
  head(df)
  n <- length(df)
  # set up sum contrasts for effects coding as needed for conjoint
  # analysis
  options(contrasts = c("contr.treatment", "contr.poly"))

  # main effects model specification fit linear regression model using
  # main effects only (no interaction terms)

  main.effects.model.fit <- lm(y ~ ., data = df)
  main.effects.model.fit
  # save key list elements of the fitted model as needed for conjoint
  # measures
  conjoint.results <- main.effects.model.fit[c("contrasts", "xlevels",
                                               "coefficients")]
  conjoint.results
  conjoint.results$attributes <- names(conjoint.results$contrasts)

  # compute and store part-worths in the conjoint.results list structure
  part.worths <- conjoint.results$xlevels
  part.worths

  # list of same structure as xlevels
  end.index.for.coefficient <- 1  # intitialize skipping the intercept
  part.worth.vector <- NULL  # used for accumulation of part worths
  for (index.for.attribute in seq(along = conjoint.results$contrasts)) {
    nlevels <- length(unlist(conjoint.results$xlevels[index.for.attribute]))
    nlevels
    begin.index.for.coefficient <- end.index.for.coefficient + 1
    end.index.for.coefficient <- begin.index.for.coefficient + nlevels -
      2
    last.part.worth <- -sum(conjoint.results$coefficients[begin.index.for.coefficient:end.index.for.coefficient])
    part.worths[index.for.attribute] <- list(as.numeric(c(conjoint.results$coefficients[begin.index.for.coefficient:end.index.for.coefficient],
                                                          last.part.worth)))
    part.worth.vector <- c(part.worth.vector, unlist(part.worths[index.for.attribute]))
  }
  part.worths
  conjoint.results$part.worths <- part.worths
  class(conjoint.results$part.worths)
  conjoint.fit.sum.zero <- data.frame()
  conjoint.results$part.worths
  conjoint.fit.sum.zero <- data.frame(matrix(unlist(part.worths), byrow = T))
  conjoint.fit.sum.zero
  # compute standardized part-worths
  standardize <- function(x) {
    (x - mean(x))/sd(x)
  }
  conjoint.results$standardized.part.worths <- lapply(conjoint.results$part.worths,
                                                      standardize)
  conjoint.results$standardized.part.worths
  conjoint.fit.sum.zero.st <- data.frame(matrix(unlist(conjoint.results$standardized.part.worths),
                                               byrow = T))
  conjoint.fit.sum.zero.st
  if (clients > 1) {
    # loop for computing all parthWoths
    for (client in 2:clients) {
      # select client ratings
      y <- as.data.frame(t(ratings[client, ]))
      names(y) <- "y"  #change client name to y
      df <- cbind(y, bundles)  #column bind client's ratings with bundles design
      n <- length(df)
      # main effects model specification fit linear regression model using
      # main effects only (no interaction terms)
      main.effects.model.fit <- lm(y ~ ., data = df)
      main.effects.model.fit
      # save key list elements of the fitted model as needed for conjoint
      # measures
      conjoint.results <- main.effects.model.fit[c("contrasts", "xlevels",
                                                   "coefficients")]
      conjoint.results
      conjoint.results$attributes <- names(conjoint.results$contrasts)

      # compute and store part-worths in the conjoint.results list structure
      part.worths <- conjoint.results$xlevels
      part.worths
      # list of same structure as xlevels
      end.index.for.coefficient <- 1  # intitialize skipping the intercept
      part.worth.vector <- NULL  # used for accumulation of part worths
      for (index.for.attribute in seq(along = conjoint.results$contrasts)) {
        nlevels <- length(unlist(conjoint.results$xlevels[index.for.attribute]))
        nlevels
        begin.index.for.coefficient <- end.index.for.coefficient +
          1
        end.index.for.coefficient <- begin.index.for.coefficient +
          nlevels - 2
        last.part.worth <- -sum(conjoint.results$coefficients[begin.index.for.coefficient:end.index.for.coefficient])
        part.worths[index.for.attribute] <- list(as.numeric(c(conjoint.results$coefficients[begin.index.for.coefficient:end.index.for.coefficient],
                                                              last.part.worth)))
        part.worth.vector <- c(part.worth.vector, unlist(part.worths[index.for.attribute]))
      }
      conjoint.results$part.worths <- part.worths
      conjoint.results$part.worths
      conjoint.fit.sum.zero <- cbind(conjoint.fit.sum.zero, data.frame(matrix(unlist(part.worths),
                                                                        byrow = T)))
      conjoint.fit.sum.zero
      # compute standardized part-worths standardize <- function(x) {(x -
      # mean(x)) / sd(x)}
      conjoint.results$standardized.part.worths <- lapply(conjoint.results$part.worths,
                                                          standardize)
      conjoint.results$standardized.part.worths
      conjoint.fit.sum.zero.st <- cbind(conjoint.fit.sum.zero.st, data.frame(matrix(unlist(conjoint.results$standardized.part.worths),
                                                                                  byrow = T)))
      conjoint.fit.sum.zero.st
    }

  }
  ######################################################## End of normalized parthworths
  clients.names <- row.names(ratings)
  names(conjoint.fit.sum.zero.st) <- clients.names
  names(conjoint.fit.sum.zero) <- clients.names
  level.names <- matrix(unlist(conjoint.results$xlevels))
  level.names
  row.names(conjoint.fit.sum.zero.st) <- level.names[, 1]
  row.names(conjoint.fit.sum.zero) <- level.names[, 1]
  conjoint.fit.sum.zero.st <- as.data.frame(t(conjoint.fit.sum.zero.st))
  conjoint.fit.sum.zero <- as.data.frame(t(conjoint.fit.sum.zero))
  ifelse(standard == 1, return(conjoint.fit.sum.zero.st), return(conjoint.fit.sum.zero))

}
