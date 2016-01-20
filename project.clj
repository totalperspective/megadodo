(defproject totalperspective/megadodo "0.3.0-SNAPSHOT"
  :description "Mustache and macros for hiccup style markup"
  :url "https://github.com/totalperspective/megadodo"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [org.clojure/core.logic "0.8.10"]]
  :profiles {:dev {:dependencies [[midje "1.8.3"]
                                  [goat "0.1.0-SNAPSHOT"]]
                   :plugins [[lein-midje "3.1.3"]
                             [michaelblume/lein-marginalia "0.9.0"]
                             [lein-auto "0.1.2"]
                             [cc.artifice/lein-gossip "0.2.1"]]
                   :auto {:default {:file-pattern #"\.(clj|cljs|cljc|edn)$"}}}})
