(defproject totalperspective/megadodo "0.1.0"
  :description "Mustache and macros for hiccup style markup"
  :url "https://github.com/totalperspective/megadodo"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/core.match "0.3.0-alpha4"]]
  :profiles {:dev {:dependencies [[midje "1.8.3"]]
                   :plugins [[lein-midje "3.1.3"]]}})
