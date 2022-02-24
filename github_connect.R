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





