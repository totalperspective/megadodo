;; # Megadodo syntax
;;
;; The syntax in megadodo hiccup is almost identical to that of
;; traditional hiccup with a couple of differences to add some mustache
;; like properties.
(ns megadodo.syntax
  (:require [clojure.core.logic :as m]))

;; ## Primatives

;; Hiccup uses keywords for tag names
(defn keywordo [x]
  (m/predc x keyword? (fn [c v r a]
                        `(~'keyword ~(m/walk* r (m/walk* a x))))))

;; Maps for attributes
(defn mapo [x]
  (m/predc x map? (fn [c v r a]
                    `(~'map ~(m/walk* r (m/walk* a x))))))

;; And obviously strings for text
(defn stringo [x]
  (m/predc x string? (fn [c v r a]
                       `(~'string ~(m/walk* r (m/walk* a x))))))

;; Megadodo additionally has symbols to represent context
(defn symbolo [x]
  (m/predc x symbol? (fn [c v r a]
                       `(~'symbol ~(m/walk* r (m/walk* a x))))))

;; ## Forms

(declare tago contexto)

(defn hiccup? [form]
  (pos? (count (m/run 1 [q]
                 (hiccupo form)))))

;; Valid megadodo hiccup form is made up of either:
;;
;; - a string
;; - a symbol
;; - a tag
;; - a context
(defn hiccupo [f]
  (m/conde
   [(stringo f)]
   [(symbolo f)]
   [(tago f)]
   [(contexto f)]))

(declare bodyo attro)

;; A tag is normally a list/vector of tag name, attribute map and/or body and can take
;; any of the forms:
;;
;; - `[tag-name]`
;; - `[tag-name attrs]`
;; - `[tag-name & body]`
;; - `[tag-name attrs & body]`
(defn tago [f]
  (m/fresh [t r]
    (m/conso t r f)
    (keywordo t)
    (m/conde
     [(bodyo r)]
     [(m/fresh [a b]
        (m/conso a b r)
        (attro a)
        (bodyo b))])))

;; Attributes are repesented as maps
(defn attro [a]
  (mapo a))

;; And the body can be empty or any number of valid forms.
(defn bodyo [b]
  (m/conde
   [(m/emptyo b)]
   [(m/fresh [h t]
      (m/conso h t b)
      (hiccupo h)
      (bodyo t))]))

;; In megadodo hiccup we ad an additional concept of a context.
;; This represents some data passed to the form to be rendered.
(defn contexto [f]
  (m/fresh [sym cons-ante cons ante* ante]
    (m/conso sym cons-ante f)
    (symbolo sym)
    (m/conso cons ante* cons-ante)
    (hiccupo cons)
    (m/conde
     [(m/emptyo ante*)]
     [(m/fresh [r]
        (m/conso ante r ante*)
        (m/emptyo r)
        (hiccupo ante))])))
