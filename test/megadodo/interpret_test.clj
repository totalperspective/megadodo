(ns megadodo.interpret-test
  (:refer-clojure :exclude [eval])
  (:require [megadodo.interpret :refer :all]
            [midje.sweet :refer :all]))

(def todos [{:subject "Get milk" :complete true :img "avatar.jpg"}
            {:subject "Finish library" :complete false}])

(def todo-macros {:v '[:div.vertical ($$ [:div.row $.])]
                  :todo '[:v [:header] [:todo-list $0] [:footer]]
                  :todo-list '[:ul {:class "todos"} ($0 [:todo-item .])]
                  :todo-item '[:li {:class (complete "complete" nil)}
                               (img [:profile .]
                                    [:profile "none.jpg"])
                               subject]
                  :profile '[:img {:class "profile" :src $0}]})

(facts "About evaluation"
       (fact "We can eval layout"
             (eval todo-macros {} '[:v [:header] [:main] [:footer]])
             =>
             [:div.vertical
              [:div.row [:header]]
              [:div.row [:main]]
              [:div.row [:footer]]])
       (fact "We can eval a profile"
             (eval todo-macros {:src "image.jpg"} '[:profile src])
             =>
             '[:img {:class "profile" :src "image.jpg"}])
       (fact "We can eval a todo item"
             (eval todo-macros (first todos) '[:todo-item .])
             =>
             '[:li {:class "complete"}
               [:img {:class "profile" :src "avatar.jpg"}]
               "Get milk"])
       (fact "We can eval a todo list"
             (eval todo-macros {:todos todos} '[:todo-list todos])
             =>
             [:ul {:class "todos"}
              [:li {:class "complete"} [:img {:class "profile", :src "avatar.jpg"}] "Get milk"]
              [:li {:class nil} [:img {:class "profile" :src "none.jpg"}] "Finish library"]])
       (fact "We can eval the todo list header"
             (eval (dissoc todo-macros :todo-item) {:todos todos} '[:todo todos])
             =>
             [:div.vertical
              [:div.row [:header]]
              [:div.row
               (apply vector :ul {:class "todos"}
                      (map (fn [item] [:todo-item item]) todos))]
              [:div.row [:footer]]])
       (fact "We build a todo list in a layout"
             (eval todo-macros {:todos todos} '[:todo todos])
             =>
             [:div.vertical
              [:div.row [:header]]
              [:div.row
               [:ul {:class "todos"}
                [:li {:class "complete"} [:img {:class "profile", :src "avatar.jpg"}] "Get milk"]
                [:li {:class nil} [:img {:class "profile" :src "none.jpg"}] "Finish library"]]]
              [:div.row [:footer]]]))
