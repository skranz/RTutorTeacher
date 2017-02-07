examples.run.docker.container = function() {

  tgroup.dir = "D:/libraries/RTutorTeacher/teacherhub/tgroups/kranz"
  th.run.docker.container(tgroup.dir)
}

th.run.docker.container = function(tgroup.dir, tag="latest") {
  restore.point("run.docker.container")

  opts = yaml.load_file(file.path(tgroup.dir,"settings","settings.yaml"))

  opts = make.th.container.settings(opts)


  shiny.dir = file.path(tgroup.dir,"shiny-server")
  teachers.dir = file.path(tgroup.dir,"teachers")
  clicker.dir = file.path(tgroup.dir,"clicker")
  log.dir = file.path(tgroup.dir,"log")



  # teacherhub
  field = "teacherhub"
  name = opts[[field]]$container
  port = opts[[field]]$port

  run.com = paste0('docker run -entrypoint="/usr/bin/with-contenv bash" --name ',name,' -d -p ',port,':3838 -e ROOT=FALSE -e RUN_RSTUDIO=no -v ',file.path(shiny.dir,field),':/srv/shiny-server/',field,' -v ',tgroup.dir,':/home/srv/tgroup -v ', log.dir,':/var/log/ skranz/RTutorTeacher:',tag)

  rerun.container(name, run.com)

  # present
  field = "present"
  name = opts[[field]]$container
  port = opts[[field]]$port

  run.com = paste0('docker run -entrypoint="/usr/bin/with-contenv bash" --name ',name,' -d -p ',port,':3838 -e ROOT=FALSE -e RUN_RSTUDIO=no -v ',file.path(shiny.dir,field),':/srv/shiny-server/',field,' -v ',teachers.dir,':/srv/teachers -v ', log.dir,':/var/log/ -v ', clicker.dir,':/srv/clicker/ skranz/RTutorTeacher:',tag)

  rerun.container(name, run.com)


  # clicker
  field = "clicker"
  name = opts[[field]]$container
  port = opts[[field]]$port

  run.com = paste0('docker run -entrypoint="/usr/bin/with-contenv bash" --name ',name,' -d -p ',port,':3838 -e ROOT=FALSE -e RUN_RSTUDIO=no -v ',file.path(shiny.dir,field),':/srv/shiny-server/',field,'  -v ', log.dir,':/var/log/ -v ', clicker.dir,':/srv/clicker/ skranz/RTutorTeacher:',tag)

  rerun.container(name, run.com)

}

rerun.container = function(name, run.com) {
  system(paste0("docker stop ", name))
  system(paste0("docker rm ", name))
  system(run.com)
}

restart.container = function(name) {
  system(paste0("docker stop ", name))
  system(paste0("docker start ", name))
}

make.th.container.settings = function(opts) {
  fields = c("teacherhub","present","clicker")

  for (field in fields) {
    opts[[field]]$container = first.non.null(opts[[field]]$container,paste0(opts$container_prefix,"_",field))
  }
  opts
}
