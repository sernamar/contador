(ns contador.core
  (:require [dinero.core :as dinero]
            [dinero.parse :as parse]
            [clojure.string :as str])
  (:import [java.util Currency Locale]
           [java.text DecimalFormat]))

(defn- currency-code
  "Returns the currency code for the given locale."
  [locale]
  (-> locale
      Currency/getInstance
      Currency/.getCurrencyCode
      str/lower-case
      keyword))

(defn- parse-amount
  "Parses the given amount string as a number."
  [string locale]
  (parse/assert-valid-grouping string locale)
  (let [formatter (DecimalFormat/getInstance locale)]
    (DecimalFormat/.parse ^DecimalFormat formatter string)))

(defn- only-amount?
  "Checks if the given string contains only an amount without currency information."
  [string]
  (re-matches #"[+-]?\d+([.,]\d+)?" string))

(defn parse-monetary-amount
  "Parses the given string as a monetary amount."
  ([string]
   (parse-monetary-amount string (Locale/getDefault)))
  ([string locale]
   (parse-monetary-amount string locale (currency-code locale)))
  ([string locale currency]
   (if (only-amount? string)
     (dinero/money-of (parse-amount string locale) currency)
     (parse/parse-string string {:locale locale :currencies [currency]}))))
