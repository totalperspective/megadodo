(ns megadodo.core-test
  (:require [megadodo.core :refer :all]
            [midje.sweet :refer :all]))

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
        ?in ?out
        [:foo] [:bar]
        [:foo {:a :b}] [:bar {:a :b}]
        [:foo [:foo {:a :b}]] [:bar [:bar {:a :b}]]))

(facts "About tag parsing"
       (tabular
        (fact "We can parse a tag"
              (parse ?unparsed) => ?parsed)
        ?unparsed                   ?parsed
        "foo"                       "foo"
        [:foo]                      {:tag :foo}
        [:foo {:a :b}]              {:tag :foo :attrs {:a :b}}
        [:foo {:a :b}]              {:tag :foo :attrs {:a :b}}
        [:foo [:bar]]         {:tag :foo :body [{:tag :bar}]}
        [:foo [:bar] "baz"]         {:tag :foo :body [{:tag :bar} "baz"]}
        [:foo {:a :b} [:bar] "baz"] {:tag :foo :attrs {:a :b} :body [{:tag :bar} "baz"]})
       (tabular
        (fact "We can unparse a tag"
              (unparse ?parsed) => ?unparsed)
        ?unparsed                   ?parsed
        "foo"                       "foo"
        [:foo]                      {:tag :foo}
        [:foo {:a :b}]              {:tag :foo :attrs {:a :b}}
        [:foo {:a :b}]              {:tag :foo :attrs {:a :b}}
        [:foo [:bar] "baz"]         {:tag :foo :body [{:tag :bar} "baz"]}
        [:foo {:a :b} [:bar] "baz"] {:tag :foo :attrs {:a :b} :body [{:tag :bar} "baz"]}))

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
