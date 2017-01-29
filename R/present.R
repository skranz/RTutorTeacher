get.presenter.ps.file = function(slides.dir) {
  pres = rev(file.path.split(slides.dir))[1]

  files = list.files(slides.dir,pattern = glob2rx("*.rps"))
  if (length(files)==0) return(NULL)
  if (length(files)==1) return(files)

  base = tools::file_path_sans_ext(files)
  ind = match(pres,base)
  if (!is.na(ind)) return(files[ind])

  ind = match(str.left.of(pres,"-"),base)
  if (!is.na(ind)) return(files[ind])

  return(files[1])
}

# We have a single presenter app for each tgroup on the server
# It can be used to show various presentations.

presenterApp = function(slides.dir, token.dir,clicker.dir=NULL, ps.file=get.presenter.ps.file(slides.dir), teacher="Teacher") {
  restore.point("presenterApp")

  if (is.null(ps.file)) {
    stop("Could not find an .rps file for your presentation.")
  }

  ps = read.rps(file.path(slides.dir,ps.file))

  app = slidesApp(ps = ps,user.name = teacher,dir = slides.dir,clicker.dir = clicker.dir)
  app
}

makePresenterAppDir = function(app.base.dir, hash=random.string(1,127), slides.dir,token.dir = "",clicker.dir="", teacher="JohnDoe", del.old.app.dirs = TRUE) {

  restore.point("makePresenterAppDir")
  #stop()

  app.dir = file.path(app.base.dir, hash[1])
  if (del.old.app.dirs) {
    dirs = setdiff(list.dirs(app.base.dir, recursive=FALSE),app.dir)
    for (dir in dirs) {
      try(unlink(dir))
    }
  }

  if (!dir.exists(app.dir)) {
    dir.create(app.dir,recursive = TRUE)
  }

  code = paste0('
# Automatically generated presentation app

library("RTutor3")
slides.dir = "',slides.dir,'"
clicker.dir = "',clicker.dir,'"
teacher = "', teacher,'"
token.dir = "',token.dir,'"

app = presenterApp(slides.dir=slides.dir, token.dir=token.dir, clicker.dir=clicker.dir, teacher=teacher)

shinyApp(ui = app$ui, server = app$server)
')
  app.file = file.path(app.dir, "app.R")
  writeLines(code, app.file)

  app.dir
}