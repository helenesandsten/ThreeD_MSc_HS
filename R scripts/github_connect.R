##### GITHUB RSTUDIO CONNECT #####
#################################################


## install if needed (do this exactly once):
## install.packages("usethis")

## STEP 1
library(usethis)
use_git_config(
  user.name = "helenesandsten", 
  user.email = "sandstenhelene@gmail.com"
)

## STEP 2
## to connect Rstudio and github to new project
create_github_token() # create token and copy the PAT
gitcreds::gitcreds_set()
git_vaccinate() # always vaccinate

## STEP 3
usethis::use_git() 
usethis::use_github()  
usethis::git_sitrep() 

#################################################
#################################################

## IF CERTAIN ERROR MESSAGE
## https://community.rstudio.com/t/troubleshooting-github-tokens/115489

## STEP 4
# delete the line GITHUB_PAT=xxx in R-environment
#usethis::edit_r_environ() 

## STEP 5
# restart R 

## STEP 6
# go back to STEP 2

