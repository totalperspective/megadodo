(ns megadodo.interpret
  (:refer-clojure :exclude [eval])
  (:require [clojure.core.match :refer [match]]))

(defn has-prefix? [sym prefix]
  (or (nil? prefix)
      (not (nil? (re-find (re-pattern (str "^[" prefix "]")) (str sym))))))

(defn resolve-sym [ctx macro? sym]
  (let [has-prefix? (has-prefix? sym '$)]
    #_(println "Resolve sym" [sym macro? has-prefix?] "in :" ctx)
    (if (= has-prefix? macro?)
      (cond
        (= sym '.) ctx
        (= sym '$.) ctx
        (map? ctx) (let [sym-path (map keyword (clojure.string/split (str sym) #"\."))
                         val (get-in ctx sym-path)]
                     #_(println sym-path val)
                     val)
        :else nil)
      sym)))

(declare eval)

(defn expand-form [macros tag args]
  (if-let [macro (get macros tag)]
    (let [ctx (into {} (map-indexed (fn [i a]
                                      (let [k (keyword (str "$" i))]
                                        [k a]))
                                    args))
          ctx (assoc ctx :$$ args)]
      #_(println "Expand form " tag args)
      (eval macros ctx true macro))
    (into [tag] args)))

(defn boolean? [x]
  (instance? Boolean x))

(defn eval
  ([macros ctx form]
   (let [expanded-form (eval macros ctx true form)
         new-form (eval macros ctx false expanded-form)]
     (comment
       (prn "***********************************")
       (prn expanded-form)
       (prn new-form)
       (prn "***********************************"))
     new-form))
  ([macros ctx macro? form]
   #_(println "eval" (if macro? "macro" "form") (pr-str form) "in" ctx)
   (let [eval* (partial eval macros ctx macro?)]
     (match
      [form]
      [?n :guard nil?] ?n
      [?b :guard boolean?] ?b
      [?i :guard integer?] ?i
      [?s :guard string?] ?s
      [?k :guard keyword?] ?k
      [?s :guard symbol?] (resolve-sym ctx macro? ?s)
      [([?t & ?args] :seq) :guard vector?] (let [args (map eval* ?args)
                                                 new-form (expand-form macros ?t args)
                                                 new-form (if (and (seq? (last new-form))
                                                                   (vector? (first (last new-form))))
                                                            (let [head (vec (butlast new-form))
                                                                  tail (last new-form)]
                                                              (into head tail))
                                                            new-form)]
                                             (if (= new-form form)
                                               new-form
                                               (eval* new-form)))
      [?m :guard map?] (->> ?m
                            (map (fn [[k v]]
                                   [(eval* k) (eval* v)]))
                            (into {}))
      [([?ctx ?cons] :seq) :guard seq?] (eval* (list ?ctx ?cons nil))
      [([?ctx ?cons ?ante] :seq) :guard seq?] (let [new-ctx (eval* ?ctx)]
                                                (if (symbol? new-ctx)
                                                  (list new-ctx (eval* ?cons) (eval* ?ante))
                                                  (if new-ctx
                                                    (let [eval-fn #(eval macros % macro? ?cons)]
                                                      (if (sequential? new-ctx)
                                                        (map eval-fn new-ctx)
                                                        (eval-fn new-ctx)))
                                                    (eval* ?ante))))
      :else (assert false (str "can't eval " (type form)))))))
