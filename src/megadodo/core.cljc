(ns megadodo.core
  (:require [clojure.core.match :refer [match]]))


(defn parse [unparsed]
  (match [unparsed]
         [[?k]] {:tag ?k}
         [[?k ?a :guard map?]] {:tag ?k :attrs ?a}
         [([?k ?a :guard map? & ?b] :seq)] {:tag ?k :attrs ?a :body (map parse ?b)}
         [([?k & ?b] :seq)] {:tag ?k :body (map parse ?b)}
         :else unparsed))

(defn make-tag [tag attrs body]
  {:pre [(keyword? tag)]}
  (->> body
       (into [tag attrs])
       (remove nil?)
       (into [])))

(defn unparse [parsed]
  (if (map? parsed)
    (let [{:keys [tag attrs body]} parsed
          body (map unparse body)]
      (make-tag tag attrs body))
    parsed))

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

(defn render [macros form ctx]
  (->> form
       parse
       (expand-tag-all macros)
       unparse))
