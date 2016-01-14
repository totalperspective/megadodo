(ns megadodo.core
  (:require [clojure.core.match :refer [match]]))


(defn parse [unparsed]
  (match [unparsed]
         [([?c :guard symbol? ?b] :seq)] {:ctx ?c :body (parse ?b)}
         [[?k]] {:tag ?k}
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
  (cond
    (string? parsed) parsed
    (map? parsed) (let [{:keys [tag attrs body]} parsed
                        body (unparse body)]
                    (make-tag tag attrs body))
    (sequential? parsed) (if (and (= 1 (count parsed))
                                  (sequential? (first parsed)))
                           (map unparse (first parsed))
                           (map unparse parsed))
    :else parsed))

(defn expand-tag [macros tag-map]
  (if (and (map? macros) (map? tag-map))
    (let [{:keys [tag attrs body]} tag-map
          tag-name (keyword (name tag))]
      (if-let [macro (macros tag-name)]
        (let [new-tag (macro tag attrs body)]
          new-tag)
        tag-map))
    tag-map))

(defn expand-tag-all [macros tag-map]
  (if (map? tag-map)
    (let [new-tag (expand-tag macros tag-map)]
      (if (identical? new-tag tag-map)
        (let [new-body (map (partial expand-tag-all macros)
                            (:body new-tag))]
          (assoc new-tag :body new-body))
        (expand-tag-all macros new-tag)))
    tag-map))

(defn sym->path [sym]
  (map keyword (clojure.string/split (str sym) #"\.")) )

(defn lookup [ctx data]
  (if (symbol? ctx)
    (if (= '. ctx)
      data
      (get-in data (sym->path ctx)))
    ctx))

(defn bind-data [data tag]
  (match [tag]
         [{:ctx ?c :body ?b}] (let [ctx (lookup ?c data)
                                    bind-fn #(bind-data % ?b)]
                                (if (sequential? ctx)
                                  (map bind-fn ctx)
                                  (bind-fn ctx)))
         [?s :guard sequential?] (map (partial bind-data data) ?s)
         [{:body ?b :attrs ?a}] (-> tag
                                    (assoc :body (bind-data data ?b))
                                    (assoc :attrs (clojure.walk/postwalk (fn [s]
                                                                           (if (symbol? s)
                                                                             (lookup s data)
                                                                             s)) ?a)))
         [?s :guard symbol?] (lookup ?s data)
         :else tag))

(defn render [macros form ctx]
  (->> form
       parse
       (expand-tag-all macros)
       (bind-data ctx)
       unparse))
