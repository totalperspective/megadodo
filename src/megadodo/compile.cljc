(ns megadodo.compile
  (:require [clojure.core.match :refer [match]]))

(defn suffix [prefix n]
  (-> (str "^" prefix "-(.*)$")
      re-pattern
      (re-matches n)
      second))

(defn md-data-name [[k v]]
  (when-let [n (->> k
                    name
                    (suffix "data-md"))]
    (if-let [macro (suffix "macro" n)]
      [:macro (keyword macro)]
      [(keyword n) (read-string v)])))

(defn md-data [attrs]
  (->> attrs
       (keep md-data-name)
       (into {})))

(defn clean-tag [t a b]
  (let [b (if (seq? b) b [b])]
    (if (empty? a)
      (into [t] b)
      (if b
        (into [t a] b)
        [t a]))))

(defn walk-form
  ([fn form]
   (walk-form fn form form))
  ([fn default form]
   (match
    [form]
    [([?t ?a :guard map? & ?b] :seq) :guard vector?] (fn form ?t ?a ?b)
    [([?t & ?b] :seq) :guard vector?] (walk-form fn default (into [?t {}] ?b))
    [([?c ?f] :seq) :guard list?] (if (= form default)
                                    (list ?c (walk-form fn default ?f))
                                    (walk-form fn default ?f))
    :else (do
            #_(println "D" default)
            default))))

(defn apply-md-body [form]
  (walk-form
   (fn [form ?t ?a ?b]
     (if-let [body (:body (md-data ?a))]
       (clean-tag ?t ?a body)
       (let [body (map apply-md-body ?b)]
         (clean-tag ?t ?a body))))
   form))

(defn apply-md-attrs [form]
  (walk-form
   (fn [form ?t ?a ?b]
     (let [body (map apply-md-attrs ?b)
           attrs (->> ?a
                      md-data
                      (map (fn [[k v]]
                             (when-let [attr (suffix "attr" (name k))]
                               [(keyword attr) v])))
                      (into {})
                      (merge ?a))]
       (clean-tag ?t attrs body)))
   form))

(defn apply-md-ctx [form]
  (walk-form
   (fn [form ?t ?a ?b]
     (let [body (map apply-md-ctx ?b)
           ctx (:ctx (md-data ?a))]
       (if ctx
         (list ctx (clean-tag ?t ?a body))
         (clean-tag ?t ?a body))))
   form))

(defn extract-macros [form]
  (walk-form
   (fn [form ?t ?a ?b]
     #_(println "EM" ?b)
     (let [body-macros (map extract-macros ?b)
           macros (reduce merge {} body-macros)]
       #_(println "MS" macros)
       (if-let [macro (:macro (md-data ?a))]
         (assoc macros macro form)
         macros)))
   {}
   form))

(declare inline-macro)

(defn call-macro [form]
  (walk-form
   (fn [form ?t ?a ?b]
     (let [{:keys [macro]} (md-data ?a)]
       (if macro
         [macro]
         (inline-macro form))))
   form))

(defn inline-macro [form]
  (walk-form
   (fn [form ?t ?a ?b]
     (let [calls (map call-macro ?b)]
       (clean-tag ?t ?a calls)))
   form))

(defn inline-macros [macros]
  (->> macros
       (map (fn [[m form]]
              [m (inline-macro form)]))
       (into {})))

(defn macros [form]
  (-> form
      apply-md-body
      apply-md-attrs
      apply-md-ctx
      extract-macros
      inline-macros))
