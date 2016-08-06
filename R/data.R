#' @description
#' Data a conjoint analysis study.
#' It is fictional data used to illustrate the use of conjoint analysis
#' to decide the range of products to be offered by an office department store.
#' The data file osc is a list contening these files:
#'
#' design:     a data frame with as many rows as attributes used in the conjoint analysis and as many varaibles as levels in each attribute
#' bundles:    a data frame with as many rows as bundles of profiles indiviuals have rated and as many variables as attributes used in teh analysis
#' ratings:    a data frame with as many rows as individuals have rated the bundles displayed in the columns
#' full:       a data frame with a full conjoint design
#' market.profiles:  a data frame with as many rows as products are in the market (competitors particularly) by attributes (in columns)
#' constrains: a data frame with some constrains to be used in the estimation of optimal products
#' reveneu:    a data frame with attributes' variation in cost
#' @title Office System Conjoint data set. It is a list with all data sets needed to test the package
#' @name osc
#' @aliases osc
#' @docType data
#' @examples
#' data(osc)
#' names(osc)
#' head(names(osc))
#' @format list
#' @keywords datasets file
#' @format osc$full, a data frame with a full conjoint design, with as many rows as profiles, and as many columns as attributes used in the analysis:
#' \describe{
#'   \item{Location}{factor with Location's levels: Less2Miles, W2-5Miles, and W5-10Miles}
#'   \item{officeSupplies}{factor with OfficeSupplies' levels: VLAssortment, LAssortment, LimAssortment}
#'  \item{Forniture}{factor with Forniture's levels: Yes, No}
#'  \item{Computers}{factor with Computers' levels: NoComputers, Software, and SoftwareAndComputers}
#' }
#'
#' @format osc$ratings, a data frame with as many rows as individuals have rated the bundles displayed in the columns:
#' \describe{
#'   \item{Bundle1}{numeric}
#'   ...
#'   \item{Bundle16}{numeric}
#' }
#' @format osc$design, a data frame with as many rows as attributes used in the conjoint analysis and as many varaibles as levels in each attribute:
#' \describe{
#'   \item{Level1}{character}
#'   \item{Level2}{character}
#'   \item{Level3}{character}
#' }
#' @format osc$bundles, a data frame with as many rows as bundles of profiles indiviuals have rated and as many variables as attributes used in teh analysis:
#' \describe{
#'   \item{Location}{factor with Location's levels: Less2Miles, W2-5Miles, and W5-10Miles}
#'   \item{officeSupplies}{factor with OfficeSupplies' levels: VLAssortment, LAssortment, LimAssortment}
#'  \item{Forniture}{factor with Forniture's levels: Yes, No}
#'  \item{Computers}{factor with Computers' levels: NoComputers, Software, and SoftwareAndComputers}
#' }
#' @format osc$market.profiles, a data frame with as many rows as products are in the market (competitors particularly) by attributes (in columns):
#' \describe{
#'   \item{Location}{factor with Location's levels: Less2Miles, W2-5Miles, and W5-10Miles}
#'   \item{officeSupplies}{factor with OfficeSupplies' levels: VLAssortment, LAssortment, LimAssortment}
#'  \item{Forniture}{factor with Forniture's levels: Yes, No}
#'  \item{Computers}{factor with Computers' levels: NoComputers, Software, and SoftwareAndComputers}
#' }
#' @format osc$constrains, a data frame with some constrains to be used in the estimation of optimal products, in rows we have attributs and in columns their levels:
#' \describe{
#'   \item{Level1}{character}
#'   \item{Level2}{character}
#'   \item{Level3}{character}
#' }
#' @format osc$revenues, a data frame with some variations in cost to be used in the estimation of optimal products, in rows we have attributs and in columns their levels, in cells, variation in cost:
#' \describe{
#'   \item{Level1}{character}
#'   \item{Level2}{character}
#'   \item{Level3}{character}
#' }
#'
#' @export osc
#' @source \url{http://www.decisionpro.biz/}
"osc"
