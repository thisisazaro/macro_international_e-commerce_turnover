sync_git <- function() {
      system("git add .")
      system('git commit -m "Auto-commit from RStudio"')
      system("git push origin main")
  }
