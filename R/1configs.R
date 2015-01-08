if (.Platform$OS.type == "windows") {
  Sys.setlocale("LC_TIME", "English")
} else if (.Platform$OS.type == "unix") {
  Sys.setlocale("LC_TIME", "en_US.UTF-8")
}
