(declare-project 
  :name "janitor"
  :description "A tool for scanning janet projects"
  :dependencies [
                 "https://github.com/pyrmont/testament" 
                 "https://github.com/yumaikas/janet-tempfiles"
                 "path"])


(declare-executable :name "janitor" :entry "janitor.janet")

(phony "inst" ["build"] 
       (print "Installing")
       (os/cd "build")
       (os/shell "inst janitor.exe")
       (os/cd ".."))



