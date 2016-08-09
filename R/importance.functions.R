############################## importance of attributes

importance.of.attributes <- function(ratings, bundles, standardize=0, print.digits=2)
  {
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
  #' @description
  #'
  #' This function computes attributes' importance. First I prepare the
  #' list conjoint.results, after that I compute the sample's mean. It uses
  #' treatment effects to estimate part worths; ratings is a matrix of clients (rows) by bundles
  #' rated (colums); bundles is a matrix of bundles (rows) by attributes
  #' (colums). I adapt  code from Marketing Data Science book, chapter 1.
  #' Frist we prepare data
  #' @title importance.of.attributes
  #' @param ratings                 a data frame with all clients' ratings
  #' @param bundles                 a data frame with all product profiles rated by clients
  #' @param standardize              0, not standardize, 1 o other value, standardize ratings before computing importance
  #' @param print.digits            number of digits to be used when printing results
  #' @return conjoint.results       the object returned by this function
  #' @export
  #' @examples
  #' data(osc)
  #' names(osc)
  #' conjoint.results<-importance.of.attributes(osc$ratings, osc$bundles, 0)
  #' conjoint.results.norm<-importance.of.attributes(osc$ratings, osc$bundles, 1)

  ################# other variables in the function ##################
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
  # pw                      a data frame with all part worths, resulting from calling part.worths() function
  # pwMean                  a vector with all part worths means
  #########################################
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
  #conjoint.results
  conjoint.results$attributes <- names(conjoint.results$contrasts)
  ifelse(standardize==0,
          pw <- part.worths(ratings, bundles),
          pw<-part.worths.norm(ratings, bundles)
  )
  pwMeans <- apply(pw, 2, mean)
  pwMeans
  conjoint.results$coefficients <- pwMeans
  #conjoint.results$coefficients
  # compute and store part-worths in the conjoint.results list structure
  part.worths <- conjoint.results$xlevels
  #part.worths

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
  conjoint.results$part.worths <- part.worths
  conjoint.results$part.worths
  class(conjoint.results)
  ###### Now we compute the importance and attach it to conjoint.results list
  #conjoint.results <- importance(conjoint.results)
  ############# This is the code of the old function importance(conjoint.results)
  ############################## other variables in the function #########
  # standardize             a function to standardize data
  # clients                 number of clients that have rated the set of bundles
  # attribute.importance    a vector with attributes' importance
  # index.for.attribute     an index for controlling the attribute
  # attribute.name          a vector with attribute name
  # attribute.importance    a variable with attribute importance
  # k                       an index
  # ratings                 a data frame with all clients' ratings
  # bundles                 a data frame with all product profiles rated by clients
  #
  standardize <- function(x) {
    (x - mean(x))/sd(x)
  }
  conjoint.results$standardized.part.worths <- lapply(conjoint.results$part.worths,
                                                      standardize)
  conjoint.results$standardized.part.worths
  # compute and store part-worth ranges for each attribute
  part.worth.ranges <- conjoint.results$contrasts
  for (index.for.attribute in seq(along = conjoint.results$contrasts)) part.worth.ranges[index.for.attribute] <- dist(range(conjoint.results$part.worths[index.for.attribute]))
  conjoint.results$part.worth.ranges <- part.worth.ranges

  sum.part.worth.ranges <- sum(as.numeric(conjoint.results$part.worth.ranges))
  # compute and store importance values for each attribute
  attribute.importance <- conjoint.results$contrasts
  for (index.for.attribute in seq(along = conjoint.results$contrasts)) attribute.importance[index.for.attribute] <- (dist(range(conjoint.results$part.worths[index.for.attribute]))/sum.part.worth.ranges) *
    100
  conjoint.results$attribute.importance <- attribute.importance
  # c<-contrasts()

  # data frame for ordering attribute names
  attribute.name <- names(conjoint.results$contrasts)
  attribute.importance <- as.numeric(attribute.importance)
  temp.frame <- data.frame(attribute.name, attribute.importance)
  conjoint.results$ordered.attributes <- as.character(temp.frame[sort.list(temp.frame$attribute.importance,
                                                                           decreasing = TRUE), "attribute.name"])
  # respondent internal consistency added to list structure
  # conjoint.results$internal.consistency <-
  # summary(main.effects.model.fit)$r.squared

  # user-defined function for printing conjoint measures
  if (print.digits == 2)
    pretty.print <- function(x) {
      sprintf("%1.2f", round(x, digits = 2))
    }
  if (print.digits == 3)
    pretty.print <- function(x) {
      sprintf("%1.3f", round(x, digits = 3))
    }
  # report conjoint measures to console use pretty.print to provide
  # nicely formated output
  for (k in seq(along = conjoint.results$ordered.attributes))
    {
    cat("\n", "\n")
    cat(conjoint.results$ordered.attributes[k], "Levels: ", unlist(conjoint.results$xlevels[conjoint.results$ordered.attributes[k]]))

    cat("\n", " Part-Worths:  ")
    cat(pretty.print(unlist(conjoint.results$part.worths[conjoint.results$ordered.attributes[k]])))

    cat("\n", " Standardized Part-Worths:  ")
    cat(pretty.print(unlist(conjoint.results$standardized.part.worths[conjoint.results$ordered.attributes[k]])))

    cat("\n", " Attribute Importance:  ")
    cat(pretty.print(unlist(conjoint.results$attribute.importance[conjoint.results$ordered.attributes[k]])))
    }
  return(conjoint.results)

  ##############This is the end od the old function importance(conjoint.results)
  #return(conjoint.results)

}

##############################################





########### plot importances #################
visualize.importance <- function(conjoint.results) {
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
  #' @description
  #' This functions print attributes' importance using R standard plotting system
  #' This functions print attributes' importance using R standard plotting system
  #' @title print.attributes.importance
  #' @param conjoint.results       a  list with the results
  #' @export
  #' @examples
  #' data(osc)
<<<<<<< HEAD
<<<<<<< HEAD
  #' conjoint.results<-importance.of.attributes(osc$ratings, osc$bundles)
  #' visualize.importance(conjoint.results)
=======
  #' conjoint.results<-importance(osc$ratings, osc$bundles)
  #' print.importance(conjoint.results)
>>>>>>> parent of 2e4b20a... Merge branch 'master' of https://github.com/jlopezsi/mktgConjoint
=======
  #' conjoint.results<-importance.of.attributes(osc$ratings, osc$bundles)
  #' visualize.importance(conjoint.results)
>>>>>>> origin/master


  #################other variables in the function ###############
  # n.attributes           a variable with the number of attributes
  # r                      a variable that defines the number of lines of a composed plot
  # importance             a data frame with attributes' importance
  # slices                 defines the data percentages for the pie plot
  # lbls                   labels for attributes
  ########################
  n.attributes <- length(conjoint.results$part.worths)  #get the number of attributes
  r <- abs(1 + (n.attributes%/%2))  #define the number of lines of a composed plot. Only works with R gui
  r  # check the number of rows in the composed plot
  # mfrow=c(r, 2) # define the grid of the composed plot grid(2,r)
  mfrow <- c(r, 2)  # define the grid of the composed plot

  importance <- as.data.frame(conjoint.results$attribute.importance)  # get attributes' importance
  slices <- as.numeric(importance)  #define the data percentages for the pie plot
  class(slices)
  lbls <- names(importance)  # get the labels of data
  slices
  lbls <- paste(lbls, round(slices), "%")  #Attach % t the labels
  pie(slices, lbls, main = "Importance of attributes")  #plot the pie chart
  for (j in 1:n.attributes) {
    ln <- as.character(eval(parse(text = paste("conjoint.results$xlevels$",
                                               names(conjoint.results$part.worths[j]), sep = ""))))  #compose the the levels' labels
    data <- eval(parse(text = paste("conjoint.results$part.worths$",
                                    names(conjoint.results$part.worths[j]), sep = "")))  #compose the content
    plot(data, type = "b", xaxt = "n", main = names(conjoint.results$part.worths[j]),
         xlab = "Levels", ylab = "Utility")
    axis(1, at = 1:length(eval(parse(text = paste("conjoint.results$xlevels$",
                                                  names(conjoint.results$part.worths[j]), sep = "")))), labels = ln,
         las = 1)  #plot the axis with the levels' names
  }
  mfrow <- c(1, 1)  #return to the standard plot
}
