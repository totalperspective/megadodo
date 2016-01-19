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

(declare hiccupo)

(defn hiccup [form]
  (first (m/run 1 [q]
           (m/fresh [parse]
             (hiccupo form parse)))))

(defn hiccup? [form]
  (not (nil? (hiccup form))))

(declare tago contexto)

;; Valid megadodo hiccup form is made up of either:
;;
;; - a string
;; - a symbol
;; - a tag
;; - a context
(defn hiccupo [f parse]
  (println :hiccupo)
  (m/fresh [type args]
    (m/== [type args] parse)
    (m/conde
     [(stringo f) (m/== type :string) (m/== args f)]
     [(symbolo f) (m/== type :symbol) (m/== args f)]
     [(m/fresh [t a b]
        (m/== type :tag)
        (m/== args [t a b])
        (tago f t a b))]
     #_[(m/fresh [s c a]
        (contexto f s c a)
        (m/== type :context)
        (m/== args [s c a]))])))

(declare bodyo attro)

;; A tag is normally a list/vector of tag name, attribute map and/or body and can take
;; any of the forms:
;;
;; - `[tag-name]`
;; - `[tag-name attrs]`
;; - `[tag-name & body]`
;; - `[tag-name attrs & body]`
(defn tago [f t a b]
  (println :tago)
  (m/fresh [r]
    (m/conso t r f)
    (keywordo t)
    (m/conde
     [(m/== a nil) (bodyo r b)]
     [(m/fresh [rr]
        (m/conso a rr r)
        (attro a)
        (bodyo rr b))])))

;; Attributes are repesented as maps
(defn attro [a]
  (println :attro)
  (mapo a))

;; And the body can be empty or any number of valid forms.
(defn bodyo [f b]
  (println :bodyo)
  (m/conde
   [(m/emptyo f) (m/emptyo b)]
   [(m/fresh [f-h f-t b-h b-t]
      (m/conso f-h f-t f)
      (m/conso b-h b-t b)
      (hiccupo f-h b-h)
      (bodyo f-t b-t))]))

;; In megadodo hiccup we ad an additional concept of a context.
;; This represents some data passed to the form to be rendered.
(defn contexto [f sym cons ante]
  (println :contexto)
  (m/fresh [f-t]
    (m/conso sym f-t f)
    (symbolo sym)
    (m/fresh [f-t-h f-t-t]
      (m/conso f-t-h f-t-t f-t)
      (hiccupo f-t-h cons)
      (m/conde
       [(m/emptyo f-t-t) (m/== ante nil)]
       [(m/fresh [f-t-t-h f-t-t-t]
          (m/conso f-t-t-h f-t-t-t f-t-t)
          (m/emptyo f-t-t-t)
          (hiccupo f-t-t-h ante))]))))
