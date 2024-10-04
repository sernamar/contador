(ns dev
  (:require [contador.core :as contador]
            [clojure.java.io :as io])
  (:import [java.util Locale]))

(comment
  (contador/read-journal (io/resource "examples/euros-with-germany-locale-formatting.ledger") Locale/GERMANY)
  (contador/read-journal (io/resource "examples/no-currency-with-germany-locale-formatting.ledger") Locale/GERMANY)
  
  (contador/read-journal (io/resource "examples/pounds-with-uk-locale-formatting.ledger") Locale/UK)
  (contador/read-journal (io/resource "examples/pounds-with-germany-locale-formatting.ledger") Locale/GERMANY :gbp)
  )

