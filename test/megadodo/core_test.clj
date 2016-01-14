(ns megadodo.core-test
  (:require [megadodo.core :refer :all]
            [midje.sweet :refer :all]
            [clojure.core.match :refer [match]]))

(defn tag-map
  ([tag]
   (tag-map tag nil))
  ([tag attrs]
   (tag-map tag attrs nil))
  ([tag attrs body]
   {:tag tag
    :attrs attrs
    :body body}))

(defn foo-macro [tag attrs body]
  (tag-map :bar attrs body))

(defn baz-macro [tag attrs body]
  (tag-map :foo attrs body))

(def macros {:foo foo-macro
             :baz baz-macro})

(def todos [{:subject "Get milk" :complete true :img "avatar.jpg"}
            {:subject "Finish library" :complete false}])

(def todo-macros {:todo-list '[:ul ($0 [:todo-item])]
                  :todo-item '[:li {:class (complete "complete" nil)}
                               (img [:profile .]
                                    [:profile "none.jpg"])
                               subject]
                  :profile '[:img {:class "profile" :src $0}]})

(facts "About tag parsing"
       (tabular
        (fact "We can parse a tag"
              (parse ?unparsed) => ?parsed)
        ?unparsed                   ?parsed
        "foo"                       "foo"
        [:foo]                      {:tag :foo :attrs nil :body nil}
        [:foo {:a :b}]              {:tag :foo :attrs {:a :b} :body nil}
        [:foo {:a :b}]              {:tag :foo :attrs {:a :b} :body nil}
        [:foo [:bar]]               {:tag :foo :attrs nil :body [(tag-map :bar)]}
        [:foo [:bar] "baz"]         {:tag :foo :attrs nil :body [(tag-map :bar) "baz"]}
        [:foo {:a :b} [:bar] "baz"] {:tag :foo :attrs {:a :b} :body [(tag-map :bar) "baz"]}
        '(a [:foo])                 {:ctx 'a :body (tag-map :foo) :else false}
        '(a [:foo] [:bar])          {:ctx 'a :body (tag-map :foo) :else (tag-map :bar)})
       (tabular
        (fact "We can unparse a tag"
              (unparse ?parsed) => ?unparsed)
        ?unparsed                   ?parsed
        "foo"                       "foo"
        [:foo]                      {:tag :foo :attrs nil :body nil}
        [:foo {:a :b}]              {:tag :foo :attrs {:a :b} :body nil}
        [:foo {:a :b}]              {:tag :foo :attrs {:a :b} :body nil}
        [:foo [:bar] "baz"]         {:tag :foo :attrs nil :body [(tag-map :bar) "baz"]}
        [:foo {:a :b} [:bar] "baz"] {:tag :foo :attrs {:a :b} :body [(tag-map :bar) "baz"]}
        [:foo "bar" "baz"]          {:tag :foo :attrs nil :body [["bar" "baz"]]}
        '(a [:foo])                 {:ctx 'a :body (tag-map :foo) :else nil}
        '(a [:foo] [:bar])          {:ctx 'a :body (tag-map :foo) :else (tag-map :bar)}))

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
        (fact "We can expand a recursively"
              (->> ?in parse (expand-tag-all macros) unparse) => ?out)
        ?in ?out
        nil nil
        "foo" "foo"
        [:baz] [:bar]
        [:foo [:foo]] [:bar [:bar]]
        [:foo [:baz]] [:bar [:bar]]
        '(. [:foo])   '(. [:bar])))

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
        '(z [:foo] [:bar]) [:bar]
        '[:tag k/j]     [:tag "m"]
        '[:tag n/o.p]   [:tag "q"]
        '[:tag r.s/t]   [:tag "u"]
        '[:tag (d [:tag e])] [:tag [:tag "f"]]
        [:tag {:a 'a}]  [:tag {:a "b"}]))

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
        [:foo [:foo {:a :b}]] [:bar [:bar {:a :b}]])
       (facts "We can put it all together"
              (render todo-macros
                      '[:todo-list {:class "top"} todos]
                      {:todos todos})
              =>
              [:ul {:class "top"}
               [:li {:class "complete"} [:img {:class "profile", :src "avatar.jpg"}] "Get milk"]
               [:li {:class nil} [:img {:class "profile" :src "none.jpg"}] "Finish library"]]))
