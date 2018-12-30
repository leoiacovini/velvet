(ns clj-graphql.render
  (:require [clojure.string :as str])
  (:import (clj_graphql.core Field On)))

(defn render-args [args]
  (str "("
       (str/join (drop-last 2
                            (reduce (fn [acc [arg-name arg-var]]
                                      (str acc (name arg-name) ": $" (name arg-var) ", ")) "" args)))
       ")"))

(defn render-field [field]
  (cond
    (instance? Field field)
    (if-let [args (:args field)]
      (str (name (:field-name field)) (render-args args))
      (str (name (:field-name field))))

    (instance? On field)
    (str "... on " (name (:type-name field)))))

(defn render-value [value]
  )

(defn render-query [query-map]
  (reduce
    (fn [acc [field-k field-v]]
      (if (and (not (record? field-v)) (map? field-v))
        (str acc " " (render-field field-k) " {" (render-query field-v) " }")
        (str acc " " (render-field field-k))))
    ""
    query-map))

(defn render-type [graphql-type]
  (let [make-required (fn [a] (str a "!"))
        make-list (fn [a] (str "[" a "]"))] (cond-> (name (:type-name graphql-type))
           (-> graphql-type meta :graphql.type/list) make-list
           (-> graphql-type meta :graphql.type/maybe not) make-required)))

(defn render-variables [vars]
  (str "("
       (str/join (drop-last 2
                            (reduce (fn [acc [var-name var-type]]
                                      (str acc "$" (name var-name) ": " (render-type var-type) ", ")) "" vars)))
       ")"))

(defn render [graphql-definition]
  (str (name (:type graphql-definition))
       " "
       (name (:operation-name graphql-definition))
       (render-variables (:variables graphql-definition))
       " {"
       (render-query (:query graphql-definition))
       " } "))