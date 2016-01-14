(ns megadodo.core
  (:require [clojure.core.match :refer [match]]))


(defn parse [unparsed]
  (match
   [unparsed]
   [([?c :guard symbol? ?b] :seq)] {:ctx ?c :body (parse ?b) :else false}
   [([?c :guard symbol? ?b ?e] :seq)] {:ctx ?c :body (parse ?b) :else (parse ?e)}
   [[?k]] {:tag ?k :attrs nil :body nil}
   [[?k ?a :guard map?]] {:tag ?k :attrs ?a :body nil}
   [([?k ?a :guard map? & ?b] :seq)] {:tag ?k :attrs ?a :body (map parse ?b)}
   [([?k & ?b] :seq)] {:tag ?k :attrs nil :body (map parse ?b)}
   :else unparsed))

(defn make-tag [tag attrs body]
  {:pre [(keyword? tag)]}
  (->> body
       (into [tag attrs])
       (remove nil?)
       (into [])))

(defn unparse [parsed]
  (match
   [parsed]
   [?s :guard string?] ?s
   [{:tag ?t :attrs ?a :body ?b}] (make-tag ?t ?a (unparse ?b))
   [{:ctx ?c :body ?b :else ?e}] (if ?e
                                   (list ?c (unparse ?b) (unparse ?e))
                                   (list ?c (unparse ?b)))
   [?s :guard sequential?] (if (and (= 1 (count ?s))
                                    (sequential? (first ?s)))
                             (map unparse (first ?s))
                             (map unparse ?s))
   :else parsed))

(defn expand-tag [macros tag-map]
  (match
   [macros tag-map]
   [m :guard map? {:tag tag :attrs attrs :body body}]
   (let [tag-name (keyword (name tag))]
     (if-let [macro (macros tag-name)]
       (let [new-tag (macro tag attrs body)]
         new-tag)
       tag-map))
   [m :guard map? {:ctx _ :body body :else else}]
   (-> tag-map
       (update :body (partial expand-tag macros))
       (update :else (partial expand-tag macros)))
   :else
   tag-map))

(defn expand-tag-all [macros tag-map]
  (if (map? tag-map)
    (let [new-tag (expand-tag macros tag-map)]
      (if (identical? new-tag tag-map)
        (let [body (:body new-tag)
              new-body (if (sequential? body)
                         (map (partial expand-tag-all macros) body)
                         (expand-tag-all macros body))]
          (assoc new-tag :body new-body))
        (expand-tag-all macros new-tag)))
    tag-map))

(defn sym->path [sym]
  (map keyword (clojure.string/split (str sym) #"\.")) )

(defn lookup [ctx data]
  (if (symbol? ctx)
    (if (= '. ctx)
      (if-let [d (:data (meta data))]
        d
        data)
      (get-in data (sym->path ctx) ctx))
    ctx))

(defn bind-data [data tag]
  (match
   [tag]
   [{:ctx ?c :body ?b :else ?e}] (let [ctx (lookup ?c data)
                                       bind-fn (if (and (not (false? ?e))
                                                        (or (not ctx)
                                                            (= ?c ctx)))
                                                 #(bind-data % ?e)
                                                 #(bind-data % ?b))]
                                   (if (sequential? ctx)
                                     (map bind-fn ctx)
                                     (bind-fn ctx)))
   [?s :guard sequential?] (map (partial bind-data data) ?s)
   [{:body ?b :attrs ?a}] (let [data (if (map? data)
                                       data
                                       (with-meta {} {:data data}))
                                data (if-let [args (:args tag)]
                                       (->> args
                                            (map-indexed (fn [i a]
                                                           (let [k (keyword (str "$" i))
                                                                 v (lookup a data)]
                                                             [k v])))
                                            (into {})
                                            (merge data))
                                       data)]
                            (-> tag
                                (assoc :body (bind-data data ?b))
                                (assoc :attrs (clojure.walk/prewalk (fn [s]
                                                                      (cond
                                                                        (symbol? s) (lookup s data)
                                                                        (list? s) (let [p (parse s)]
                                                                                    (bind-data data p))
                                                                        :else s))
                                                                    ?a))))
   [?s :guard symbol?] (lookup ?s data)
   :else tag))

(defn template [form]
  (let [parsed (parse form)]
    (fn [tag attrs body]
      (-> parsed
          (update :attrs merge attrs)
          (assoc :args body)))))

(defn render [macros form ctx]
  (let [macros (->> macros
                    (map (fn [[k m]]
                           (if (fn? m)
                             [k m]
                             [k (template m)])))
                    (into {}))]
    (->> form
         parse
         (expand-tag-all macros)
         (bind-data ctx)
         unparse)))
