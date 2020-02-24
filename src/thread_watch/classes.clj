(ns thread-watch.classes
  {:no-doc true}
  (:require
   [cheshire.core :as json]
   #_[clojure.string :as str]))

;; (def os-name (str/lower-case (System/getProperty "os.name")))
;; (def os (cond (str/includes? os-name "mac") :mac
;;               (or (str/includes? os-name "nix")
;;                   (str/includes? os-name "nux")) :linux
;;               (str/includes? os-name "win") :windows))
;; (def unix-like? (or (identical? os :linux)
;;                     (identical? os :mac)))

(def classes
  '{:all [java.io.BufferedReader
          java.io.File
          java.io.Reader
          java.time.Duration
          java.time.LocalDate
          java.time.LocalDateTime
          java.time.LocalTime
          java.time.ZoneOffset
          java.time.format.DateTimeFormatter]
    :constructors []
    :methods []
    :fields []
    :instance-checks []
    :custom {clojure.lang.LineNumberingPushbackReader {:allPublicConstructors true
                                                       :allPublicMethods true}
             java.lang.Thread
             {:allPublicConstructors true
              ;; generated with `public-declared-method-names`, see in
              ;; `comment` below
              :methods [{:name "activeCount"}
                        {:name "checkAccess"}
                        {:name "currentThread"}
                        {:name "dumpStack"}
                        {:name "enumerate"}
                        {:name "getAllStackTraces"}
                        {:name "getContextClassLoader"}
                        {:name "getDefaultUncaughtExceptionHandler"}
                        {:name "getId"}
                        {:name "getName"}
                        {:name "getPriority"}
                        {:name "getStackTrace"}
                        {:name "getState"}
                        {:name "getThreadGroup"}
                        {:name "getUncaughtExceptionHandler"}
                        {:name "holdsLock"}
                        {:name "interrupt"}
                        {:name "interrupted"}
                        {:name "isAlive"}
                        {:name "isDaemon"}
                        {:name "isInterrupted"}
                        {:name "join"}
                        {:name "run"}
                        {:name "setContextClassLoader"}
                        {:name "setDaemon"}
                        {:name "setDefaultUncaughtExceptionHandler"}
                        {:name "setName"}
                        {:name "setPriority"}
                        {:name "setUncaughtExceptionHandler"}
                        {:name "sleep"}
                        {:name "start"}
                        {:name "toString"}
                        {:name "yield"}]}
             java.net.URL
             {:allPublicConstructors true
              :allPublicFields true
              ;; generated with `public-declared-method-names`, see in
              ;; `comment` below
              :methods [{:name "equals"}
                        {:name "getAuthority"}
                        {:name "getContent"}
                        {:name "getDefaultPort"}
                        {:name "getFile"}
                        {:name "getHost"}
                        {:name "getPath"}
                        {:name "getPort"}
                        {:name "getProtocol"}
                        {:name "getQuery"}
                        {:name "getRef"}
                        {:name "getUserInfo"}
                        {:name "hashCode"}
                        {:name "openConnection"}
                        {:name "openStream"}
                        {:name "sameFile"}
                        ;; not supported: {:name "setURLStreamHandlerFactory"}
                        {:name "toExternalForm"}
                        {:name "toString"}
                        {:name "toURI"}]}}})

(defmacro gen-class-map []
  (let [classes (concat (:all classes)
                        (keys (:custom classes))
                        (:constructors classes)
                        (:methods classes)
                        (:fields classes)
                        (:instance-checks classes))
        m (apply hash-map
                 (for [c classes
                       c [(list 'quote c) c]]
                   c))]
    (assoc m :public-class
           (fn [v]
             (cond (instance? java.nio.file.Path v)
                   java.nio.file.Path
                   (instance? java.lang.Process v)
                   java.lang.Process
                   ;; added for issue #239 regarding clj-http-lite
                   (instance? java.io.ByteArrayOutputStream v)
                   java.io.ByteArrayOutputStream)))))

(def class-map (gen-class-map))

(defn generate-reflection-file
  "Generate reflection.json file"
  [& args]
  (let [entries (vec (for [c (sort (:all classes))
                           :let [class-name (str c)]]
                       {:name class-name
                        :allPublicMethods true
                        :allPublicFields true
                        :allPublicConstructors true}))
        constructors (vec (for [c (sort (:constructors classes))
                                :let [class-name (str c)]]
                            {:name class-name
                             :allPublicConstructors true}))
        methods (vec (for [c (sort (:methods classes))
                           :let [class-name (str c)]]
                       {:name class-name
                        :allPublicMethods true}))
        fields (vec (for [c (sort (:fields classes))
                          :let [class-name (str c)]]
                      {:name class-name
                       :allPublicFields true}))
        custom-entries (for [[c v] (:custom classes)
                             :let [class-name (str c)]]
                         (assoc v :name class-name))
        all-entries (concat entries constructors methods fields custom-entries)]
    (spit (or
           (first args)
           "reflection.json") (json/generate-string all-entries {:pretty true}))))

(comment

  (defn public-declared-method? [c m]
    (and (= c (.getDeclaringClass m))
         (not (.getAnnotation m Deprecated))))

  (defn public-declared-method-names [c]
    (->> (.getMethods c)
         (keep (fn [m]
                 (when (public-declared-method? c m)
                   {:name (.getName m)})) )
         (distinct)
         (sort-by :name)
         (vec)))

  (public-declared-method-names java.lang.UNIXProcess)
  (public-declared-method-names java.net.URL)
  )
