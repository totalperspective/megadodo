(ns megadodo.core-test
  (:require [megadodo.core :refer :all]
            [midje.sweet :refer :all]
            [clojure.core.match :refer [match]]))

(defn tag-map [tag attrs body]
  {:tag tag
   :attrs attrs
   :body body})

(defn foo-macro [tag attrs body]
  (tag-map :bar attrs body))

(defn baz-macro [tag attrs body]
  (tag-map :foo attrs body))

(def todos [{:subject "Get milk" :complete true}
            {:subject "Finish library" :complete false}])

(def macros {:foo foo-macro
             :baz baz-macro})

(facts "About tag parsing"
       (tabular
        (fact "We can parse a tag"
              (parse ?unparsed) => ?parsed)
        ?unparsed                   ?parsed
        "foo"                       "foo"
        [:foo]                      {:tag :foo}
        [:foo {:a :b}]              {:tag :foo :attrs {:a :b}}
        [:foo {:a :b}]              {:tag :foo :attrs {:a :b}}
        [:foo [:bar]]               {:tag :foo :body [{:tag :bar}]}
        [:foo [:bar] "baz"]         {:tag :foo :body [{:tag :bar} "baz"]}
        [:foo {:a :b} [:bar] "baz"] {:tag :foo :attrs {:a :b} :body [{:tag :bar} "baz"]}
        '(a [:foo])                 {:ctx 'a :body {:tag :foo}})
       (tabular
        (fact "We can unparse a tag"
              (unparse ?parsed) => ?unparsed)
        ?unparsed                   ?parsed
        "foo"                       "foo"
        [:foo]                      {:tag :foo}
        [:foo {:a :b}]              {:tag :foo :attrs {:a :b}}
        [:foo {:a :b}]              {:tag :foo :attrs {:a :b}}
        [:foo [:bar] "baz"]         {:tag :foo :body [{:tag :bar} "baz"]}
        [:foo {:a :b} [:bar] "baz"] {:tag :foo :attrs {:a :b} :body [{:tag :bar} "baz"]}
        [:foo "bar" "baz"]          {:tag :foo :body [["bar" "baz"]]}))

(facts "About macro expansion"
       (tabular
        (fact "We can expand a single tag"
              (->> ?in parse (expand-tag macros) unparse) => ?out)
        ?in ?out
        nil nil
        "foo" "foo"
        [:bar] [:bar]
        [:foo] [:bar]
        [:foo [:foo]] [:bar [:foo]])
       (tabular
        (fact "We can expand a single tag"
              (->> ?in parse (expand-tag-all macros) unparse) => ?out)
        ?in ?out
        nil nil
        "foo" "foo"
        [:baz] [:bar]
        [:foo [:foo]] [:bar [:bar]]
        [:foo [:baz]] [:bar [:bar]]))

(facts "About data binding"
       (tabular
        (fact "We can lookup single values"
              (->> ?in
                   parse
                   (bind-data {:a "b"
                               :c [1 2 3]
                               :d {:e "f"}
                               :g {:h {:i "j"}}
                               :k/j "m"
                               :n/o {:p "q"}
                               :r {:s/t "u"}})
                   unparse)
              => ?out)
        ?in       ?out
        '[:tag a]       [:tag "b"]
        '[:tag d.e]     [:tag "f"]
        '[:tag g.h.i]   [:tag "j"]
        '[:ul (c [:li .])] [:ul
                            [:li 1]
                            [:li 2]
                            [:li 3]]
        '(. [:tag a])   [:tag "b"]
        '(d [:tag e])   [:tag "f"]
        '(g [:tag h.i]) [:tag "j"]
        '(g.h [:tag i]) [:tag "j"]
        '[:tag k/j]     [:tag "m"]
        '[:tag n/o.p]   [:tag "q"]
        '[:tag r.s/t]   [:tag "u"]
        '[:tag (d [:tag e])] [:tag [:tag "f"]]))

(facts "About rendering"
       (fact "rendering nil returns nil"
             (render nil nil nil) => nil)
       (fact "Specifying no macros does nothing"
             (render macros nil nil) => nil)
       (fact "Specifying only a context does nothing"
             (render nil nil todos) => nil)
       (fact "Specifying only macros and ctx does nothing"
             (render macros nil todos) => nil)
       (tabular
        (fact "Only supplying a the form returns the form"
              (render nil ?form nil) => ?form)
        ?form
        [:h1 "Test"]
        [:div [:h1 "Bar"]])
       (tabular
        (fact "Passing a macro causes expansion"
              (render macros ?in nil) => ?out)
        ?in    ?out
        [:foo] [:bar]
        [:foo {:a :b}] [:bar {:a :b}]
        [:foo [:foo {:a :b}]] [:bar [:bar {:a :b}]]))
