(ns clj-graphql.types
  (:require [schema.core :as s]))

(defprotocol GraphQLType
  (schema [this]))

(defrecord GraphQLScalar [type-name schema]
  GraphQLType
  (schema [this] (:schema this)))

(defrecord GraphQLEnum [type-name values]
  GraphQLType
  (schema [this] (apply s/enum values)))

(defn list [type] (with-meta type {:graphql.type/list true}))
(defn maybe [type] (with-meta type {:graphql.type/maybe true}))

(def Str (->GraphQLScalar :String s/Str))
(def ID (->GraphQLScalar :ID s/Str))
(def Bool (->GraphQLScalar :Boolean s/Bool))
(def Int (->GraphQLScalar :Int s/Int))
(def Num (->GraphQLScalar :Float s/Num))