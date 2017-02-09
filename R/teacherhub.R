
examples.teacherhub = function() {
  restore.point.options(display.restore.point = TRUE)

  tgroup.dir = "D:/libraries/RTutorTeacher/teacherhub/tgroups/kranz"
  app = TeacherHubApp(tgroup.dir=tgroup.dir,init.userid="kranz", need.password=FALSE, need.user=TRUE, fixed.password="ompo", use.signup=FALSE)
  res = viewApp(app)

  #res = "D:/libraries/RTutorTeacher/teacherhub/tgroups/kranz/shiny-server/present/teachers/kranz/courses/vwl/slides/2-chapter2b/app"

  if (dir.exists(res)) {
    restore.point.options(display.restore.point = TRUE)
    shiny::runApp(res,launch.browser = rstudioapi::viewer)
  }
}

# RTutor Teacher-Hub

# A teaching group shares common docker containers and directories
#
# a container for teachers
# a quiz container
# a problem set container

# directory structure:
#
# tgroup
#   running
#   teacher
#     courses
#       slides
#       problem_sets
#       quiz server

# tgroups must be added by an admin
# teachers can be added by an admin or teacher in the tgroup
#
# login database can be shared among different tgroups


TeacherHubApp = function(tgroup.dir, login.db.dir=NULL, app.title="RTutor TeacherHub", ...) {
  restore.point("TeacherHubApp")
  app = eventsApp()

  app$ui = teacher.hub.main.ui()

  glob = app$glob

  glob$opts = yaml.load_file(file.path(tgroup.dir,"settings/settings.yaml"))



  glob$tgroup.dir = tgroup.dir


  db.arg = list(dbname=paste0(login.db.dir,"/userDB.sqlite"),drv=SQLite())

  lop = loginModule(db.arg = db.arg, login.fun=teacher.hub.login, app.title=app.title,container.id = "centerUI",...)

  restore.point("TeacherHubApp.with.lop")

  appInitHandler(function(...,app=getApp()) {
    restore.point("TeachHubApp.initHandler")
    hide.jquery.pane("mainPanes","west")
    initLoginDispatch(lop)
  })
  app
}

teacher.hub.login = function(userid,app=getApp(),...) {
  restore.point("teacher.hub.login")

  tgroup.dir = app$glob$tgroup.dir
  user.dir = file.path(tgroup.dir,"teachers",userid)
  courses.dir = file.path(user.dir, "courses")
  courseids = list.dirs(courses.dir, full.names=FALSE, recursive=FALSE)

  courses = vector("list", length(courseids))
  names(courses) = courseids

  th = as.environment(nlist(
    userid,
    tgroup.dir,
    user.dir,
    courses.dir,
    courseids,
    courses
  ))


  app$th = th

  show.teacher.hub.ui(th, app)
}

teacher.hub.main.ui = function(app=getApp()) {
  restore.point("show.teacher.hub.ui")

  json.opts ="
  defaults: {
    resizable: true,
    closable: false,
    slideable: true,
    spacing_open: 5
  },
  north: {
    size: 'auto',
    resizable: false,
    closable: false,
    slideable: false,
    spacing_open: 0
  },
  east: {
    resizable: true,
    spacing_open: 0,
    spacing_closed: 0,
    size: 0
  },
  west: {
    resizable: true,
    size: 0.5
  },

  "

  panes = jqueryLayoutPanes(id="mainPanes",json.opts=json.opts,
    north = div(p("TeacherHub"),thinHR()),
    west = div(uiOutput("westUI")),
    center = div(uiOutput("centerUI"))
  )

  ui = bootstrapPage(
    contextMenuHeader(),
    fancytreeHeader(extensions=c("table","gridnav","dnd")),
    aceEditorHeader(),
    jqueryLayoutHeader(),
    panes
  )

}

show.teacher.hub.ui = function(th=app$th,app=getApp()) {
  restore.point("show.teacher.hub.ui")

  treeId = "thTree"
  ns = NS(treeId)
  tree = fancy.file.tree(treeId,root.dir = th$courses.dir,modify.nodes.fun = teacher.hub.modify.file.tree.nodes)

  setUI("westUI",tagList(
    filetreeButtons(treeId,c("Rename", "Duplicate","MakeDir","Delete")),
    tree
    ,filetreeButtons(treeId,c("Upload"))
  ))

  setUI("centerUI",HTML(""))
  show.jquery.pane("mainPanes",c("west","north"))

  classEventHandler("thShowSlidesBtn", event = "click",stop.propagation = TRUE, fun=th.show.slides.click)
  #setUI("westUI",tree)
  #setUI("centerUI",HTML(""))
}

# change filetree nodes
teacher.hub.modify.file.tree.nodes = function(cur.dir, label.nodes, head.nodes, file.nodes,..., app=getApp(), th=app$th) {
  restore.point("teacher.hub.modify.file.tree.nodes")


  below = file.path.length(cur.dir) - file.path.length(th$courses.dir)

  folders = rev(file.path.split(cur.dir))


  if (NROW(head.nodes)>0) {
    head.nodes$title = ".."
  }

  # coursesdir
  if (below==0) {
    label.nodes$title = "Course"
    label.nodes$col2 = label.nodes$col3 = ""
    file.nodes$col2 = file.nodes$col3 = ""
  # main folder of a course
  } else if (below==1) {
    courseid = folders[1]
    app$courseid = courseid

    label.nodes$title = folders[1]
    label.nodes$col2 = label.nodes$col3 = ""

    file.nodes = file.nodes %>% arrange(desc(itemId))
    file.nodes$col2 = file.nodes$col3 = ""

    try({
      num.ps = length(list.dirs(recursive=FALSE,file.path(cur.dir,"ps")))
      num.slides = length(list.dirs(recursive=FALSE,file.path(cur.dir,"slides")))
      file.nodes$col2 <- c(num.slides, num.ps)
    })

  # slides
  } else if (below==2 & folders[1]=="slides") {
    label.nodes$title = paste0("Slides ", folders[2])
    label.nodes$col2 = label.nodes$col3 = ""


    file.nodes = file.nodes %>% filter(itemType=="folder")
    file.nodes$col2 = file.nodes$col3 = ""
    file.nodes$col2 <- sapply(file.nodes$itemId, function(slide) {
      as.character(smallButton(id=paste0("thShowSlides_",slide),class.add = "thShowSlidesBtn",label = "Show", `data-slide`=slide,`data-dir`=file.path(cur.dir,slide)))
    })
  }

  nlist(label.nodes, head.nodes, file.nodes)
}

th.show.slides.click = function(data,..., courseid = app$courseid, app=getApp(), th=app$th) {
  restore.point("th.show.slides.click")
  slides = data$slide
  slides.dir = data$dir

  opts = app$glob$opts

  shiny.dir = first.non.null(opts$present$shiny.dir,"/srv/shiny-server/present")


  app.base.dir = paste0(shiny.dir, file.path.diff(slides.dir, app$glob$tgroup.dir))

  clicker.dir = opts$clicker$clicker.dir


  app.dir = makePresenterAppDir(courseid=courseid,hash="app",app.base.dir = app.base.dir,slides.dir = slides.dir,clicker.dir = clicker.dir)

  # add course to clicker
   write.clicker.running(courseid = courseid,clicker.dir = clicker.dir)


  if (isTRUE(opts$local)) {
    stopApp(app.dir)
  } else {
    # open app website
  }

}

shiny.to.js.html = function(txt,quotes='"') {
  txt = paste0(as.character(txt),collapse="")
  txt = gsub("\n","",txt, fixed=TRUE)
  if (isTRUE(quotes=="'")){
    txt = gsub('"',quotes,txt, fixed=TRUE)
  } else if (isTRUE(quotes=='"')) {
    txt = gsub("'",quotes,txt, fixed=TRUE)
  }
  txt
}

file.path.common = function(path1, path2) {
  restore.point("file.path.common")
  v1 = strsplit(path1,"",fixed = TRUE)[[1]]
  v2 = strsplit(path2,"",fixed = TRUE)[[1]]

  len = min(length(v1), length(v2))
  if (len==0) return("")
  differ = which(v1[1:len] != v2[1:len])
  if (length(differ)==0) {
    differ = len+1
  } else {
    differ = min(differ)
  }
  if (differ<=1) return("")
  substring(path1,1,differ-1)


}

file.path.diff = function(path1, path2) {
  restore.point("file.path.diff")

  common = file.path.common(path1, path2)

  if (nchar(common)==0) return(path1)
  str.right.of(path1,common)

}
