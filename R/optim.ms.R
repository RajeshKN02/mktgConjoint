############ Optim market share

optim.ms.first.choice <- function(ratings, bundles, exis.profiles,
                                  experiment) {
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
  #' Computes market share for actual profiles as well as for all possible profiles
  #' in order to indentify the bundle that maximazes market share given actual profiles
  #' We use first choice rule
  #' If we combine all three functions into one function, we can use rule=1 for first choice, rule=2 for utility share, and
  #' the rest for BTL. With a if sentence we hace combine all 3 functions
  #' into one.
  #' @param ratings                 a data frame with all clients' ratings
  #' @param bundles                 a data frame with all product profiles rated by clients
  #' @param exis.profiles           a data frame with competitiors' profiles
  #' @param experiment              a data frame with the description of all possible profiles
  #' @return optim.list             a list with with the optim profile as well as optim market share
  #' @examples
  #' library(mktgConjoint)
  #' data(osc)
  #' osc.ms.op.1choice<-optim.ms.first.choice(osc$ratings, osc$bundles, osc$market.profiles, osc$full)
  #' osc.ms.op.1choice
  #'
  #######################other variables in the function################
  # rivals                  the number of competitors
  # n.bundles               the number of possible combinations
  #  ms.full.profile        a matri to store computations
  # profiles                a data frame that combines existing profiles with the first possible combination
  # ms.full.profile         the data matrix where we store computations
  # ms.max                  identifies the optim profile

  rivals <- nrow(exis.profiles)
  n.bundles <- nrow(experiment)  #gets the namber of possible combinations of attributes and levels
  ms.full.profile <- matrix()  #clean data matrix to store computations
  profiles <- rbind(exis.profiles, Optim = experiment[1, ])  #combines existing profiles with the first possible combination
  ######## rule first choice or election
  ms.full.profile <- t(ms.fe.conjoint(profiles, ratings, bundles))  #initilizes the data matrix where we store computations

  for (i in 2:n.bundles) {
    profiles <- rbind(exis.profiles, Optim = experiment[i, ])  #combines existing profiles with the i-essim possible combination
    ms.full.profile <- rbind(ms.full.profile, t(ms.fe.conjoint(profiles,
                                                             ratings, bundles)))  #combines the computations
  }
  ms.max <- which.max(ms.full.profile[, rivals + 1])  #identify the optimum combination
  optim.list <- list()  #inizilizes the data to be retorned.
  optim.list$OptimProfile <- experiment[ms.max, ]  # returns the optimum combination
  optim.list$OptimMS <- ms.full.profile[ms.max, ]  # returns the optimum market share
  return(optim.list)
}

optim.ms.utility.share <- function(ratings, bundles, exis.profiles,
                                   experiment) {
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
  #' Computes market share for actual profiles as well as for all possible profiles
  #' in order to indentify the bundle that maximazes market share given actual profiles
  #' We use utility share rule
  #' If we combine all three functions into one function, we can use rule=1 for first choice, rule=2 for utility share, and
  #' the rest for BTL. With a if sentence we hace combine all 3 functions
  #' into one.
  #' @param ratings                 a data frame with all clients' ratings
  #' @param bundles                 a data frame with all product profiles rated by clients
  #' @param exis.profiles           a data frame with competitiors' profiles
  #' @param experiment              a data frame with the description of all possible profiles
  #' @return optim.list             a list with with the optim profile as well as optim market share
  #' @examples
  #' library(mktgConjoint)
  #' data(osc)
  #' osc.ms.op.ut.share<-optim.ms.ut.share(osc$ratings, osc$bundles, osc$market.profiles, osc$full)
  #' osc.ms.op.ut.share
  #'
  ####################### other variabels in the function ##############
  # rivals                  the number of competitors
  # n.bundles               the number of possible combinations
  #  ms.full.profile        a matri to store computations
  # profiles                a data frame that combines existing profiles with the first possible combination
  # ms.full.profile         the data matrix where we store computations
  # ms.max                  identifies the optim profile
  # optim.list              a list with with the optim profile as well as optim market share
  # experiment              a data frame with the description of all possible profiles
  #'
  rivals <- nrow(exis.profiles)
  n.bundles <- nrow(experiment)  #gets the namber of possible combinations of attributes and levels
  ms.full.profile <- matrix()  #clean data matrix to store computations

  profiles <- rbind(exis.profiles, Optim = experiment[1, ])  #combines existing profiles with the first possible combination
  ######## rule share of utilities
  ms.full.profile <- t(ms.us.conjoint(profiles, ratings, bundles))  #initilizes the data matrix where we store computations

  for (i in 2:n.bundles) {
    profiles <- rbind(exis.profiles, Optim = experiment[i, ])  #combines existing profiles with the i-essim possible combination
    profiles
    ms.full.profile <- rbind(ms.full.profile, t(ms.us.conjoint(profiles,
                                                             ratings, bundles)))  #combines the computations
  }

  ms.max <- which.max(ms.full.profile[, rivals + 1])  #identify the optimum combination
  optim.list <- list()  #inizilizes the data to be retorned.
  optim.list$OptimProfile <- experiment[ms.max, ]  # returns the optimum combination
  optim.list$OptimMS <- ms.full.profile[ms.max, ]  # returns the optimum market share
  return(optim.list)
}

optim.ms.btl <- function(ratings, bundles, exis.profiles, experiment) {
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
  #' Computes market share for actual profiles as well as for all possible profiles
  #' in order to indentify the bundle that maximazes market share given actual profiles
  #' We use first choice rule
  #' If we combine all three functions into one function, we can use rule=1 for first choice, rule=2 for utility share, and
  #' the rest for BTL. With a if sentence we hace combine all 3 functions
  #' into one.
  #' @param ratings                 a data frame with all clients' ratings
  #' @param bundles                 a data frame with all product profiles rated by clients
  #' @param exis.profiles           a data frame with competitiors' profiles
  #' @param experiment              a data frame with the description of all possible profiles
  #' @return optim.list             a list with with the optim profile as well as optim market share
  #' @examples
  #' library(mktgConjoint)
  #' data(osc)
  #' osc.ms.op.btl<-optim.ms.btl(osc$ratings, osc$bundles, osc$market.profiles, osc$full)
  #' osc.ms.op.btl
  #'
  ####################### other variabels in the function ##############
  # rivals                  the number of competitors
  # n.bundles               the number of possible combinations
  #  ms.full.profile        a matri to store computations
  # profiles                a data frame that combines existing profiles with the first possible combination
  # ms.full.profile         the data matrix where we store computations
  # ms.max                  identifies the optim profile
  # optim.list              a list with with the optim profile as well as optim market share
  # experiment              a data frame with the description of all possible profiles
  #########################
  rivals <- nrow(exis.profiles)
  n.bundles <- nrow(experiment)  #gets the namber of possible combinations of attributes and levels
  ms.full.profile <- matrix()  #clean data matrix to store computations

  profiles <- rbind(exis.profiles, Optim = experiment[1, ])  #combines existing profiles with the first possible combination
  ######## rule Bradley, Terry and Luce
  ms.full.profile <- t(ms.btl.conjoint(profiles, ratings, bundles))  #initilizes the data matrix where we store computations

  for (i in 2:n.bundles) {
    profiles <- rbind(exis.profiles, Optim = experiment[i, ])  #combines existing profiles with the i-essim possible combination
    ms.full.profile <- rbind(ms.full.profile, t(ms.btl.conjoint(profiles,
                                                              ratings, bundles)))  #combines the computations
  }
  ms.max <- which.max(ms.full.profile[, rivals + 1])  #identify the optimum combination
  optim.list <- list()  #inizilizes the data to be retorned.
  optim.list$OptimProfile <- experiment[ms.max, ]  # returns the optimum combination
  optim.list$OptimMS <- ms.full.profile[ms.max, ]  # returns the optimum market share
  return(optim.list)
}
