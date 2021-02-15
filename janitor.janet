(import tempfiles :as temp)
(import path :as p)
(import stringx :as str)

(math/seedrandom (os/time))

(defn git-scan? [obj]
  (match obj
    {:path (p (string? p)) :error err} true
    {
     :path (p (string? p))
     # a
     :unpushed-commits (uc (number? uc))
     # b
     :behind-commmits (bc (number? bc))
     # 1 2 u
     :changed-files (c (number? c))
     # ?
     :untracked-files (u (number? u))
     } true
    _ false))

(defn find-on-path 
  "Returns the absolute path of the given executable"
  [exename] 
  (def path-elems (match (os/which)
                    :windows (string/split ";" (os/getenv "PATH"))
                    _ (string/split ":" (os/getenv "PATH"))))
  (prompt :donezo
  (loop [pe :in path-elems
         :before (def pat (p/join pe exename))
         :when (when-let [f (os/stat pat) 
                          mode (f :mode)
                          ]
                 (= mode :file)) ]
      (return :donezo pat))))

# Ordinary changed entries of the following format
# 1 <XY> <sub> <mH> <mI> <mW> <hH> <hI> <path>

# Renamed entries have this format.
# 2 <XY> <sub> <mH> <mI> <mW> <hE> <hH> <hI> <X><score> <path><sep><origPath>

# Unmerged entries
# u <xy> <sub> <m1> <m2> <mW> <h1> <h2> <h3> <path>

# Untracked files
# ? <path>

(defn git-scan [path] 
  (os/cd path)
  (def gitproc (os/spawn ["git" "status" "--porcelain=v2" "--branch"] :p { :out :pipe :err :pipe }))
  (def status-output (:read (gitproc :out) :all))
  (def status-error (:read (gitproc :err) :all))
  (def retval (:wait gitproc))
  (when (not= retval 0) (break {:path path :error status-error}))

  (def lines (map string/trim (string/split "\n" status-output)))
  (def [unpushed-commits behind-commits] 
    (prompt :found 
            (each l lines 
              (match (peg/match ~(* "# branch.ab +" (<- :d+) " -" (<- :d+)) l)
                [a$ b$] (return :found [(scan-number a$) (scan-number b$)])))))

  (defn any-prefix? [prefixes str] (any? (map |(string/has-prefix? $ str) prefixes)))
  (def changed-files (length (filter |(any-prefix? ["1" "2" "u"] $) lines)))
  (def untracked-files (length (filter |(string/has-prefix? "?" $) lines)))

  {:path path
   :unpushed-commits unpushed-commits
   :behind-commits behind-commits
   :changed-files changed-files
   :untracked-files untracked-files})

(def jpm (match (os/which) 
           :windows (find-on-path "jpm.bat")
           _ (find-on-path "jpm")))

(defn make-deps-proc [target-dir mod-dir] 
  (def env (os/environ))
  (put env "JANET_MODPATH" mod-dir)
  (put env :out :pipe)
  (put env :err :pipe)
  (os/spawn [jpm "deps"] :ep env))

(defn deps-and-test-scan [path] 
  (def mod-dir (p/join (temp/dir) (temp/random-name 15)))
  (os/mkdir mod-dir)
  (os/cd path)
  (def deps-proc (make-deps-proc path mod-dir))
  (:read (deps-proc :out) :all)
  (def err-buf (:read (deps-proc :err) :all))
  (def deps-ret (:wait deps-proc))

  (when (not= 0 deps-ret) 
    (break [[false err-buf] [false @""]]))

  # From here on, we consider jpm-deps to have passed
  (os/cd path)
  (def env (os/environ))
  (put env "JANET_MODPATH" mod-dir)
  (put env :out :pipe)
  (def test-proc (os/spawn [jpm "test"] :ep env))
  # Drain the buffers
  # (:read (test-proc :out) :all)
  # (def err-buf (:read (test-proc :err) :all))
  (ev/deadline 60)
  (def test-ret (:wait test-proc))
  (if (not= 0 test-ret)
    [true [false test-ret]]
    [true true]))

(defn scan-dir [path chan]
  (ev/spawn 
    (ev/give chan [:git path (git-scan path)])
    (ev/give chan [:dep-test path (deps-and-test-scan path)])
    (ev/give chan [:done path])))

(defn list-dirs [path] 
  (defn dir-to-scan? [path] 
    (and 
      (= (get (os/stat path) :mode) :directory)
      (as?-> 
        (os/stat (p/join path "project.janet")) it
        (= (get it :mode) :file))
      (not (as?-> 
        (os/stat (p/join path ".janitor-ignore")) it
        (do (eprint "Ingnoring " it) it)
        (= (get it :mode) :file)))))
  (filter dir-to-scan? (os/dir path)))

(defn scan-dirs [paths] 
  (def ch (ev/chan))
  (def done (ev/chan))
  # Create a set of  
  (ev/spawn 
    (def res @{})
    (while (> (length paths) 0)
      (def chres (ev/take ch))
      (match chres 
        [:done path] (do 
                       (eprint "Done with all checks on " (p/basename path))
                       (put paths path nil)
                       (if (> (length paths) 3)
                         (eprint (length paths) " projects remain.")
                         (eprint (string/join (map p/basename (keys paths)) ", ") " remain")))
        [:git path scan] (do 
                           (eprint "Finished git scan of " (p/basename path))
                           (put-in res [path :git-scan] scan))
        [:dep-test path [deps test]] 
        (do 
          (eprint "Completed deps and test checks of " (p/basename path))
          (put-in res [path :deps-scan] deps)
          (put-in res [path :test-scan] test))
        other (error other)))
    (ev/give done res))
  (comment ```
           Info to show:
           git a/b
           git untracked files
           git changed files
           jpm deps (pass/fail)
           jpm test (pass/fail)

           @task[TODO: Get all of the information output in a table like this]

           | Path || Git: unpushed | behind | untracked | changed || jpm: deps | test |

           ```)
  (each p (keys paths) 
    (eprint "Starting scan of " (p/basename p))
    (try
    (scan-dir p ch)
    ([err] (do
             (eprint "Had to abort " p )
             (ev/give ch [:done p])))))
  (def res (ev/take done))

  (def max-path (max ;(map |(as-> $ it (p/basename it) (length it)) (keys res))))

  (print "")
  (prin (str/pad-right "|| Path " (+ 4 max-path)))
  (print "|| Git: changed | unpushed | behind | untracked || jpm: deps | test    |")
  (eachp [path report] res
    (prin (str/pad-right (string "|| " (p/basename path) " ")  (+ 4 max-path)))

    (def gitinfo (report :git-scan))
    (def deps-pass (match (report :deps-scan)
                     true "clean"
                     [false] "broken"))
    (def test-pass (match (report :test-scan)
                     true "pass"
                     [false] "failing"))
    (print 
      (str/pad-right (string "|| "(gitinfo :changed-files)) (length "|| Git: changed "))
      (str/pad-right (string "| " (gitinfo :unpushed-commits)) (length "| unpushed "))
      (str/pad-right (string "| " (gitinfo :behind-commits)) (length "| behind "))
      (str/pad-right (string "| "(gitinfo :untracked-files)) (length "| untracked "))
      (str/pad-right (string "|| " deps-pass) (length "|| jpm: deps "))
      (str/pad-right (string "| "test-pass " |") (length "| passing |")))))

(defn usage [] 
  (print ```
         Jantior usage:
         janitor scan-dirs
         janitor scan <dir>
         ```))

(defn main [& clargs]
  (def clargs (array ;clargs))
  (when (> (length clargs) 0) (array/remove clargs 0))
  (def [subcommand] clargs)
  (def args (array/slice clargs 1))

  (case subcommand 
    "scan-dirs" (do 
                  (def paths (table ;(flatten (map |[(p/abspath $) true] (list-dirs ".")))))
                  (scan-dirs paths)
                  (os/exit 0))
    "scan" (do 
             (match args 
               [dir] (scan-dirs @{(p/abspath dir) true})
               _ (do (print "Bad args to scan " args)(usage))))
    true (do
           (print "Unknown subcommand " subcommand)
           (usage))))
