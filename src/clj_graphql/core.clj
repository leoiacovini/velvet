(ns clj-graphql.core
  (:require [clojure.string :as str]
            [clj-graphql.types :as types]))

(defrecord Field [field-name args])
(defrecord On [type-name])

(defn field
  ([name] (field name nil))
  ([name args] (->Field name args)))

(defn on [type-name]
  (->On type-name))

;; Renderer

(defn prepare-query [query-map]
  (into {} (map (fn [[k v]]

                  [(cond
                     (instance? Field k) k
                     (keyword? k) (field k)
                     :default k)

                   (if (and (not (record? v)) (map? v))
                     (prepare-query v)
                     v)]) query-map)))


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

(defn render-variables [vars]
  (str "("
       (str/join (drop-last 2
                            (reduce (fn [acc [var-name var-type]]
                                      (str acc "$" (name var-name) ": " (name var-type) ", ")) "" vars)))
       ")"))

(defn to-graphql [graphql-definition]
  (str (name (:type graphql-definition))
       " "
       (name (:operation-name graphql-definition))
       (render-variables (:variables graphql-definition))
       " {"
       (render-query (:query graphql-definition))
       " } "))

;; DSL

(defn make-graphql-operation [op-type op-name body variables]
  {:operation-name op-name
   :variables      variables
   :type           op-type
   :query          (prepare-query body)})

(defmacro defoperation [op-type query-name query-args query-body]
  (let [prepared-args (reduce #(assoc %1 (keyword (first %2)) (second %2)) {} (partition 2 (remove #(= % :-) query-args)))]
    `(let ~(vec (mapcat #(do [(symbol (name (key %))) (key %)]) prepared-args))
       (def ~query-name (make-graphql-operation
                          ~op-type
                          ~(keyword (name query-name))
                          ~query-body
                          ~prepared-args)))))

(defmacro defquery [query-name query-args query-body]
  `(defoperation :query ~query-name ~query-args ~query-body))

(defmacro defmutation [query-name query-args query-body]
  `(defoperation :mutation ~query-name ~query-args ~query-body))

(defquery oneLoan [customer-id :- types/Str
                   loanId :- types/Str]
  {(field :customer {:id customer-id})
   {(field :name) types/Str
    (field :type) types/Str
    (field :loan {:name :loanId})
                  {(field :amount) types/Int}
    (on :NewCustomer)
                  {(field :age) types/Int}}})

(defquery oneLoan2 []
  {(field :customer)
   {(field :name) types/Str
    (field :type) types/Str
    (field :loan {:name :loanId})
                  {(field :amount) types/Int}
    (on :NewCustomer)
                  {(field :age) types/Int}}})



