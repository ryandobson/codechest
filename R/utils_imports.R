#' @importFrom stats setNames cor
#' @importFrom tidyselect everything all_of
NULL  #structural placeholder so R doesn't throw an error that the imports are
#not attached to any object (e.g., they're not attached to an exported function)

utils::globalVariables(c(".data", "variables", "item_name", "fac_num", "value"))
