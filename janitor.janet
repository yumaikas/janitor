(import tempfiles :as temp)
(import path)
(defn git-scan? [obj]
  (match obj
    {:path (p (string? p)) :error err} true
    {:path (p (string? p))
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
         :before (def pat (path/join pe exename))
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
    (reduce (fn [[a b] l] 
              (if-let [[a$ b$] (peg/match ~(* "branch.ab +" (<- :d+) " -" (<- :d+)) l)]
                [(scan-number a$) (scan-number b$)]
                [a b])) 
            [0 0] lines))

  (defn any-prefix? [prefixes str] (any? (map |(string/has-prefix? $ str) prefixes)))
  (def changed-files (length (filter |(any-prefix? ["1" "2" "u"] $) lines)))
  (def untracked-files (length (filter |(string/has-prefix? "?" $) lines)))

  {:path path
   :unpushed-commits unpushed-commits
   :behind-commmits behind-commits
   :changed-files changed-files
   :untracked-files untracked-files})

(def jpm (match (os/which) 
           :windows (find-on-path "jpm.bat")
           _ (find-on-path "jpm")))

(defn make-deps-proc [mod-dir] 
    (os/spawn [jpm "deps"] :e @{
                            "JANET_MODPATH" (buffer mod-dir)
                            "JANET_PATH" (buffer (os/getenv "JANET_PATH"))
                            "XDX" @"123"
                            "PATH" (buffer (os/getenv "PATH"))
                            :out :pipe
                            :err :pipe
                            }))

(defn deps-and-test-scan [path] 
  (def mod-dir (path/join (temp/dir) (temp/random-name 15)))
  (os/mkdir mod-dir)
  (os/cd path)
  (def deps-proc (make-deps-proc mod-dir))
  (:read (deps-proc :out) :all)
  (def err-buf (:read (deps-proc :err) :all))
  (def deps-ret (:wait deps-proc))
  (when (not= 0 deps-ret) 
    (break [[false err-buf] [false @""]]))
  # From here on, we consider jpm-deps to have paased
  (os/cd path)
  (def test-proc (os/spawn [jpm "test"] :ep
                           {"JANET_PATH" (os/getenv "JANET_PATH")
                            "PATH" (os/getenv "PATH")
                            "JANET_MODPATH" mod-dir
                            :out :pipe
                            :err :pipe}))
  # Drain the buffers
  (:read (test-proc :out) :all)
  (def err-buf (:read (test-proc :err) :all))
  (def test-ret (:wait test-proc))
  (if (not= 0 test-ret)
    [true [false test-ret]]
    [true true]))

(defn scan-dir [path chan]
  (ev/spawn 
    (ev/give chan [:git path (git-scan path)])
    (ev/give chan [:dep-test path (deps-and-test-scan path)])
    (ev/give chan [:done path])))

(defn scan-dirs [] 
  (def ch (ev/chan))
  (def done (ev/chan))
  # Create a set of  
  (def paths (table ;(flatten (map |[(path/abspath $) true] (os/dir ".")))))
  (ev/spawn 
    (def res @{})
    (while (> (length paths) 0)
      (def chres (ev/take ch))
      (match chres 
        [:done path] (put paths path nil)
        [:git path scan] (put-in res [path :git-scan] scan)
        [:dep-test path [deps test]] 
        (do 
          (put-in res [path :deps-scan] deps)
          (put-in res [path :test-scan] test))
        other (error other)))
    (ev/give done res))
  (each p (keys paths) (scan-dir p ch))
  (def res (ev/take done))
  (eachp [k v] res
    (prin k ":")
    (pp v)
    ))

(defn main [& clargs]
  (def clargs (array ;clargs))
  (when (> (length clargs) 0) (array/remove clargs 0))
  (def [subcommand] clargs)
  (def args clargs)

  (case subcommand 
    "scan" (scan-dirs)
    true ()))
