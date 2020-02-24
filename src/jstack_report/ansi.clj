(ns jstack-report.ansi)

(def ESC \u001b)

(def ANSI-CODES
  {:reset              "[0m"
   :bright             "[1m"
   :blink-slow         "[5m"
   :underline          "[4m"
   :underline-off      "[24m"
   :inverse            "[7m"
   :inverse-off        "[27m"
   :strikethrough      "[9m"
   :strikethrough-off  "[29m"

   :default "[39m"
   :white   "[37m"
   :black   "[30m"
   :red     "[31m"
   :green   "[32m"
   :blue    "[34m"
   :yellow  "[33m"
   :magenta "[35m"
   :cyan    "[36m"

   :bg-default "[49m"
   :bg-white   "[47m"
   :bg-black   "[40m"
   :bg-red     "[41m"
   :bg-green   "[42m"
   :bg-blue    "[44m"
   :bg-yellow  "[43m"
   :bg-magenta "[45m"
   :bg-cyan    "[46m"
   })

(def ^:dynamic *use-ansi* "Rebind this to false if you don't want to see ANSI codes in some part of your code." true)

(defn- cursor-fn [& args]
  (print (str ESC (apply str args))))

(defn cursor-pos [x y]
  (cursor-fn "[" y "," x "H"))

(defn cursor-up [n]
  (cursor-fn "[" n "A"))

(defn cursor-down [n]
  (cursor-fn "[" n "B"))

(defn cursor-right [n]
  (cursor-fn "[" n "C"))

(defn cursor-left [n]
  (cursor-fn "[" n "D"))

(defn cursor-clear-display
	"clear screen, move to 0, 0"
  []
  (cursor-fn "[2j"))

(defn cursor-clear-line []
  (cursor-fn "[2j"))

(defn cursor-save [] 
  (cursor-fn "[s"))

(defn cursor-restore [] 
  (cursor-fn "[s"))

(defn ansi
  "Output an ANSI escape code using a style key.
   (ansi :blue)
   (ansi :underline)
  Note, try (style-test-page) to see all available styles.
  If *use-ansi* is bound to false, outputs an empty string instead of an
  ANSI code. You can use this to temporarily or permanently turn off
  ANSI color in some part of your program, while maintaining only 1
  version of your marked-up text.
  "
  [code]
  (if *use-ansi*
    (str ESC (get ANSI-CODES code (:reset ANSI-CODES)))
    ""))

(defmacro without-ansi
  "Runs the given code with the use-ansi variable temporarily bound to
  false, to suppress the production of any ANSI color codes specified
  in the code."
  [& code]
  `(binding [*use-ansi* false]
     ~@code))

(defmacro with-ansi
  "Runs the given code with the use-ansi variable temporarily bound to
  true, to enable the production of any ANSI color codes specified in
  the code."
  [& code]
  `(binding [*use-ansi* true]
     ~@code))


(defn style
  "Applies ANSI color and style to a text string.
   (style \"foo\" :red)
   (style \"foo\" :red :underline)
   (style \"foo\" :red :bg-blue :underline)
 "
  [s & codes]
  (str (apply str (map ansi codes)) s (ansi :reset)))

(defn wrap-style
  "Wraps a base string with a stylized wrapper.
  If the wrapper is a string it will be placed on both sides of the base,
  and if it is a seq the first and second items will wrap the base.
  To wrap debug with red brackets => [debug]:
  (wrap-style \"debug\" [\"[\" \"]\"] :red)
  "
  [base wrapper & styles]
  (str (apply style wrapper styles)
       base
       (apply style wrapper styles)))

(defn style-test-page
  "Print the list of supported ANSI styles, each style name shown
  with its own style."
  []
  (doall
    (map (fn [c]
           (println (style (name c) c))
           (println (style (str "bright " (name c)) :bright c)))
         (sort-by name (keys ANSI-CODES))))
  nil)

