(ns repl-enrich.core
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [compliment.core :as compl]))

(defn eval-with-ns [ns forms]
  (if ns
    (do (when-not (find-ns ns) (require ns))
        (binding [*ns* (or (find-ns ns) *ns*)]
          (eval forms)))
    (eval forms)))

(defn var-loc [sym]
  (-> (meta sym)
      (select-keys [:file :line :column])
      seq
      sort
      ((partial map second))))

(defn completions [prefix]
  (compl/completions prefix {:plain-candidates true}))

(defn file-name-for-ns [n]
  (some-> n
          ns-publics
          first
          second
          meta
          :file))

(defn function-alias [n {:keys [name ns]}]
  (let [namespace->alias (set/map-invert (.getAliases n))
        alias            (namespace->alias ns)]
    {:full-name (if alias (str alias "/" name) (str name))
     :alias     alias}))

(defn source-for-file [file]
  (or (try {:source (slurp (str "src/" file))
            :path   (str "src/" file)}
           (catch Exception e))
      (try {:source (slurp (str "test/" file))
            :path   (str "test/" file)}
           (catch Exception e))))

(defn find-replace-in-source [{:keys [full-name alias new-name] :as m}]
  (update m :source str/replace (re-pattern (str "([\\s\\[{(]+)" full-name "([\\s)}\\]]+)"))
          (if alias
            (str "$1" alias "/" new-name "$2")
            (str "$1" new-name "$2"))))

(defn sync-source-to-file! [{:keys [source path ns]}]
  (spit path source)
  (require (.getName ns) :reload))

(defn rename-function-in-project [old-name new-name]
  (let [function    (meta (resolve (symbol old-name)))
        function-ns (:ns function)]
    (->> (all-ns)
         (filter #((set (vals (.getAliases %))) function-ns))
         (concat [function-ns])
         (map (fn [n]
                (let [file (file-name-for-ns n)]
                  (merge {:ns       n
                          :file     file
                          :new-name new-name}
                         (source-for-file file)
                         (function-alias n function)))))
         (map find-replace-in-source)
         (run! sync-source-to-file!))))
