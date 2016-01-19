(ns megadodo.compile-test
  (:require [megadodo.compile :refer :all]
            [midje.sweet :refer :all]))

(facts "About macro compilation"
       (fact "A form with no macro definition yeilds no macros"
             (macros [:div
                      [:ul
                       [:li "Hello"]]])
             =>
             {})
       (fact "A form with a data-macro-... attribute yeilds a macro of that name"
             (macros [:div {:data-md-macro-list ""}
                      [:ul
                       [:li "Hello"]]])
             =>
             {:list [:div {:data-md-macro-list ""}
                     [:ul
                      [:li "Hello"]]]})
       (fact "A form with a nested macros will separate them"
             (macros [:div {:data-md-macro-list ""}
                      [:ul
                       [:li {:data-md-macro-list-item ""} "Hello"]]])
             =>
             {:list [:div {:data-md-macro-list ""}
                     [:ul
                      [:list-item]]]
              :list-item [:li {:data-md-macro-list-item ""} "Hello"]})
       (fact "A tag in a macro can refer to a name for it's body"
             (macros [:li {:data-md-macro-list-item ""
                           :data-md-body "subject"} "Foo"])
             =>
             '{:list-item [:li {:data-md-macro-list-item ""
                                :data-md-body "subject"} subject]})
       (fact "Attributes can refer to names for their value"
             (macros [:div {:data-md-macro-list ""}
                      [:ul {:display "block"
                            :data-md-attr-display "(visible \"block\" \"none\")"}
                       [:li "Hello"]]])
             =>
             '{:list [:div {:data-md-macro-list ""}
                      [:ul {:display (visible "block" "none")
                            :data-md-attr-display "(visible \"block\" \"none\")"}
                       [:li "Hello"]]]})
       (fact "A tag in a macro can refer to a name for it's context"
             (macros [:div {:data-md-macro-list ""}
                      [:ul
                       [:li {:data-md-ctx "."
                             :data-md-body "subject"}
                        "Hello"]]])
             =>
             {:list '[:div {:data-md-macro-list ""}
                      [:ul
                       (. [:li {:data-md-ctx "."
                                :data-md-body "subject"}
                           subject])]]})
       (fact "A context at the macro level is represented in the parent macro"
             (macros [:div {:data-md-macro-list ""}
                      [:ul
                       [:li {:data-md-macro-list-item ""
                             :data-md-ctx "."
                             :data-md-body "subject"}
                        "Hello"]]])
             =>
             {:list '[:div {:data-md-macro-list ""}
                      [:ul
                       (. [:list-item])]]
              :list-item '[:li {:data-md-macro-list-item ""
                                :data-md-ctx "."
                                :data-md-body "subject"}
                           subject]})
       (fact "Macros can have an argument"
             (macros [:img {:class "profile"
                            :src "avatar.jpg"
                            :data-md-args "[$src]"
                            :data-md-macro-profile "profile"
                            :data-md-attr-src "$src"}])
             =>
             '{:profile [:img {:class "profile"
                               :src $0
                               :data-md-args "[$src]"
                               :data-md-macro-profile "profile"
                               :data-md-attr-src "$src"}]})
       (fact "Macros can have more than one argument"
             (macros '[:div.modal {:data-md-macro-modal ""
                                   :data-md-args "[$buttons $main]"}
                       [:div.main {:data-md-body "$main"} [...]]
                       [:div.buttons {:data-md-body "$buttons"} [...]]])
             =>
             '{:modal [:div.modal {:data-md-macro-modal ""
                                   :data-md-args "[$buttons $main]"}
                       [:div.main {:data-md-body "$main"} $1]
                       [:div.buttons {:data-md-body "$buttons"} $0]]}))
