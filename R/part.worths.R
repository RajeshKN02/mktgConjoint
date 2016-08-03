part.worths <- function(ratings, bundles) {
  #  part.worths.R
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
  ######
  #' @title part.worths
  #' @description
  #' This function computes the partworth of attribute levels for each
  #' respondent. Uses treatment effects; ratings is a data frame of clients
  #' (rows) by bundle rated (colums); bundles is a data frame of
  #' bundles (rows) by attributes (colums)
  #' @param ratings                 a data frame with all clients' ratings
  #' @param bundles                 a data frame with all product profiles rated by clients
  #' @return conjont.fit            the object returned by the function
  #' @export
  #' @examples
  #'
  #'   data(osc)
  #'   part.worths(osc$ratings, osc$bundles)
  #
  ################### other variables in the function ###########
  # conjoint.fit            a data frame with the coefficients of many objects returned by lm()
  # clients                 number of clients that have rated the set of bundles
  # y                       the ratings of client number i
  # df                      a compposed file of client's ratings plus the bundles description according to bundles matrix
  # n                       number of variables in df data table
  # main.effects.model.fit  an object return by lm() function
  # clients.names           a vector with clients' names

  options(contrasts = c("contr.treatment", "contr.poly"))
  conjoint.fit <- data.frame()  #inicialize data frame
  clients <- nrow(ratings)  #counting the number of informants
  y <- as.data.frame(t(ratings[1, ]))  #selecting first informant data
  names(y) <- "y"  #changing the variable name to y
  dim(y)
  dim(bundles)
  df <- cbind(y, bundles)  #combining first informant ratings with profiles
  dim(df)
  class(df)
  head(df)
  n <- length(df)  #the number of variables in df
  main.effects.model.fit <- lm(y ~ ., data = df)  #estimating the main effects conjoint model
  # names(main.effects.model.fit)
  conjoint.fit <- as.data.frame(main.effects.model.fit[c("coefficients")])  #storing coefficients in data frame
  conjoint.fit
  ############### computing conjoint for the rest of the data set.
  if (clients > 2) {

    for (client in 2:clients) {
      # select client ratings
      y <- as.data.frame(t(ratings[client, ]))
      names(y) <- "y"  #change client name to y
      df <- cbind(y, bundles)  #column bind client's ratings with bundles design
      n <- length(df)
      main.effects.model.fit <- lm(y ~ ., data = df)  #fit model for client
      conjoint.fit <- cbind(conjoint.fit, main.effects.model.fit[c("coefficients")])
      # conjoint.fit
    }
  }
  # preparing the data file for returning
  clients.names <- row.names(ratings)  #selecting clients' names from ratings data frame
  names(conjoint.fit) <- clients.names  #attaching clients' names to results
  conjoint.fit <- as.data.frame(t(conjoint.fit))  #transposing data before returning
  return(conjoint.fit)
}
