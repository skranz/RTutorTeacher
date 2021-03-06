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

presenterApp = function(courseid="", slides.dir, token.dir,clicker.dir=NULL, ps.file=get.presenter.ps.file(slides.dir), teacher="Teacher") {
  restore.point("presenterApp")

  if (is.null(ps.file)) {
    stop("Could not find an .rps file for your presentation.")
  }

  ps = read.rps(file.path(slides.dir,ps.file))

  app = slidesApp(ps = ps,user.name = teacher,dir = slides.dir, opts=list(courseid=courseid, clicker.dir=clicker.dir, use.clicker=TRUE))
  app
}

makePresenterAppDir = function(courseid,slides,teacher, opts, hash=random.string(1,127),token.dir = "", del.old.app.dirs = TRUE) {
  restore.point("makePresenterAppDir")
  #stop()

  app.base.dir = file.path(opts$present.shiny.dir,"teachers",teacher,"courses",courseid,"slides",slides)
  app.dir = file.path(app.base.dir,hash[1])

  if (del.old.app.dirs) {
    dirs = setdiff(list.dirs(app.base.dir, recursive=FALSE),app.dir)
    for (dir in dirs) {
      try(unlink(dir))
    }
  }

  if (!dir.exists(app.dir)) {
    dir.create(app.dir,recursive = TRUE)
  }

  slides.dir = file.path(opts$teachers.dir,teacher,"courses",courseid,"slides",slides)

  code = paste0('
# Automatically generated presentation app

library("RTutorTeacher")
slides.dir = "',slides.dir,'"
clicker.dir = "',opts$clicker.dir,'"
teacher = "', teacher,'"
token.dir = "',token.dir,'"
courseid = "',courseid,'"

app = presenterApp(courseid=courseid, slides.dir=slides.dir, token.dir=token.dir, clicker.dir=clicker.dir, teacher=teacher)

appReadyToRun(app)

shinyApp(ui = app$ui, server = app$server)
')
  app.file = file.path(app.dir, "app.R")
  writeLines(code, app.file)

  app.dir
}
