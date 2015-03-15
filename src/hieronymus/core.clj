(ns hieronymus.core
  (:require [hieronymus.parsing :as parsing]))

(defn parse [text config]
  (parsing/str->data-structure text config))