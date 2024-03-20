# # [SETUP] -----------------------------------------------------------------
# # - Packages ----------------------------------------------------------------
# # CRAN packages
# chr_pkg <- c(
#   'devtools'
# )
# 
# # Git packages
# chr_git <- c(
#   'CaoBittencourt' = 'atlas.gene',
#   'CaoBittencourt' = 'atlas.aeq'
# )
# 
# # genevate / install CRAN packages
# lapply(
#   chr_pkg
#   , function(pkg){
# 
#     if(!require(pkg, character.only = T)){
# 
#       install.packages(pkg)
# 
#     }
# 
#     require(pkg, character.only = T)
# 
#   }
# )
# 
# # genevate / install Git packages
# Map(
#   function(git, profile){
# 
#     if(!require(git, character.only = T)){
# 
#       install_github(
#         paste0(profile, '/', git)
#         , upgrade = F
#         , force = T
#       )
# 
#     }
# 
#     require(git, character.only = T)
# 
#   }
#   , git = chr_git
#   , profile = names(chr_git)
# )
# 
# rm(chr_pkg, chr_git)
# 

# [FUNCTIONS] ---------------------------
# - Competence function ---------------------------------------------------
fun_comp_competence <- function(
    dbl_profile
    , dbl_scale_lb = 0
    , dbl_scale_ub = 100
    , dbl_generality = NULL
){
  
  # Arguments validation
  stopifnot(
    "'dbl_profile' must be numeric." =
      is.numeric(dbl_profile)
  )
  
  stopifnot(
    "'dbl_scale_lb' must be numeric." =
      is.numeric(dbl_scale_lb)
  )
  
  stopifnot(
    "'dbl_scale_ub' must be numeric and greater than 'dbl_scale_lb'." =
      all(
        is.numeric(dbl_scale_ub)
        , dbl_scale_ub > 
          dbl_scale_lb
      )
  )
  
  stopifnot(
    "'dbl_generality' must be either NULL or numeric." =
      any(
        is.numeric(dbl_generality)
        , is.null(dbl_generality)
      )
  )
  
  # Data wrangling
  dbl_scale_lb[[1]] -> dbl_scale_lb
  
  dbl_scale_ub[[1]] -> dbl_scale_ub
  
  dbl_profile[!is.na(
    dbl_profile
  )] -> dbl_profile
  
  if(is.null(dbl_generality)){
    
    fun_gene_generality(
      dbl_profile
      , dbl_scale_lb =
        dbl_scale_lb
      , dbl_scale_ub = 
        dbl_scale_ub
    ) -> dbl_generality
    
  }
  
  dbl_generality[[1]] -> dbl_generality
  
  # Weighted mean of normalized item scores
  # adjusted by item importance and generality
  weighted.mean(
    x =
      dbl_profile / (
        dbl_scale_ub -
          dbl_scale_lb
      ) -
      dbl_scale_lb / (
        dbl_scale_ub -
          dbl_scale_lb
      )
    , w =
      fun_aeq_aequivalence(
        dbl_profile = 
          dbl_profile
        , dbl_scale_lb = 
          dbl_scale_lb
        , dbl_generality =
          dbl_generality
      )
  ) -> dbl_competence
  
  # Output
  return(dbl_competence)
  
}

# # [TEST] ------------------------------------------------------------------
# # - Competence test -------------------------------------------------------
# fun_comp_competence(
#   dbl_profile =
#     rnorm(50, 100, 25) |>
#     pmax(0) |>
#     pmin(100)
#   , dbl_scale_lb = 0
#   , dbl_scale_ub = 100
# )
# 
# fun_comp_competence(
#   dbl_profile =
#     rnorm(50, 50, 25) |>
#     pmax(0) |>
#     pmin(100)
#   , dbl_scale_lb = 0
#   , dbl_scale_ub = 100
# )
# 
# fun_comp_competence(
#   dbl_profile =
#     rnorm(50, 50, 5) |>
#     pmax(0) |>
#     pmin(100)
#   , dbl_scale_lb = 0
#   , dbl_scale_ub = 100
# )
# 
# fun_comp_competence(
#   dbl_profile =
#     rnorm(50, 50, 0) |>
#     pmax(0) |>
#     pmin(100)
#   , dbl_scale_lb = 0
#   , dbl_scale_ub = 100
# )
