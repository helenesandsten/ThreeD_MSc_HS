##### GIT AND GITHUB #####

## install if needed (do this exactly once):
## install.packages("usethis")

library(usethis)
use_git_config(
  user.name = "helenesandsten", 
  user.email = "sandstenhelene@gmail.com"
)


## connect Rstudio and github
create_github_token()
gitcreds::gitcreds_set()
git_vaccinate()

## 
use_git()
use_github()
usethis::git_sitrep()

# trying to commit
# trying to commit after lunch

usethis::create_github_token()
gitcreds::gitcreds_set()
usethis::edit_r_environ()



