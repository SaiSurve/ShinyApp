################################################################################
# Name    : FA_Code_v3.R
# Purpose : Create the back-end code for the Retail Analytics Interface (RAI)
#           This is a prototype of the back-end code to the Shiny based GUI
#           used to generate the front-end
################################################################################

#-------------------------------------------------------------------------------
# Clean Up
#-------------------------------------------------------------------------------

# Clean up
cat("\014")
rm(list = ls())
gc()

#-------------------------------------------------------------------------------
# Define
# Load
# Install all of the required libararies
#-------------------------------------------------------------------------------

# UPDATE: Required packages
# reqpkgs    <- c("stringr", "RCommon")
reqpkgs    <- c("stringr","shiny")

# Install packages only if they are not currently installed
checkPkgs <- function() {
    pkg.inst <- installed.packages()
    pkgs <- reqpkgs
    have.pkg <- pkgs %in% rownames(pkg.inst)

    if(any(!have.pkg)) {
        message("\nSome packages need to be installed.\n")
        r <- readline("Install necessary packages [y/n]? ")
        if(tolower(r) == "y") {
            need <- pkgs[!have.pkg]
            message("\nInstalling packages ",
                    paste(need, collapse = ", "))
            install.packages(need)
        }
    }
}

# Run function to install the required packages if they are not yet installed
checkPkgs

# Load the required packages
all(sapply(reqpkgs, require, character.only = TRUE))

#-------------------------------------------------------------------------------
# Define OPERATOR FUNCTIONS
#-------------------------------------------------------------------------------

# The SQL SELECT operator
fun_operator_sel    <- function(inp_str){
    fun_operator_sel    <- str_c("SELECT ", str_c(inp_str, collapse = "\n , "), sep = "\n", collapse = NULL)
    fun_operator_sel
}

# The SQL FROM operator
fun_operator_frm    <- function(inp_str){
    fun_operator_frm    <- str_c("FROM ( ", str_c(inp_str, collapse = ""), paste0(" )"," A", collapse = " "), sep = "\n", collapse = NULL)
    fun_operator_frm
}


# The SQL INNER JOIN operator
fun_operator_inj    <- function(inp_str){
    fun_operator_inj    <- str_c("INNER JOIN ", str_c(inp_str, collapse = ""), sep = "\n", collapse = NULL)
    fun_operator_inj
}

# The SQL JOIN ON operator
fun_operator_jon    <- function(inp_str){
    fun_operator_jon    <- str_c("ON ", str_c(inp_str, collapse = ""), sep = "\n", collapse = NULL)
    fun_operator_jon
}


# The SQL SELECT operator
fun_operator_whr    <- function(inp_str){
    fun_operator_whr    <- str_c("WHERE ", str_c(inp_str, collapse = "\n AND "), sep = "\n", collapse = NULL)
    fun_operator_whr
}

# GROUP BY operator
fun_operator_gby = function(inp_str){
  fun_operator_grp <- str_c("GROUP BY ",inp_str, sep = " ", collapse = NULL)
  return(fun_operator_grp)
}

# HAVING operator
fun_operator_hav = function(inp_str){
  fun_operator_hav <- str_c("HAVING ",inp_str, sep = " ", collapse = NULL)
  return(fun_operator_hav)
}


# string slicing function
str_slice_fun = function(s, n) {
    sapply(s, FUN = function(x) {x[[n]]})
}

# The SQL CASE operator (assumes that there are only three conditions) 
# Have chosen the function name such that it does not get picked while making the final query
#fun_operator_case = function(AS,condition1,condition2,condition3){
#  fun_operator_case1 <- str_c("CASE ", condition1, condition2, condition3, str_c("END AS", AS, sep = " "), sep = "\n", collapse = NULL)
#  return(fun_operator_case1)
#}


fun_Vec_create = function(CommaDlimitedVar){
  Vecotr_form                      <- c(as.vector(unlist(strsplit(CommaDlimitedVar, ","))))
  return(Vecotr_form)
}
fun_operator_case = function(AS,condition1,condition2,condition3){
  fun_operator_case1 <- str_c("CASE ", condition1, condition2, condition3, str_c("END AS", AS, sep = " "), sep = "\n", collapse = NULL)
  return(fun_operator_case1)
}

funnames    <- unlist(ls())
fun_names = function(){
                      
					   return(funnames)
                      }
