(ns contador.core
  (:require [dinero.core :as dinero]
            [dinero.parse :as parse]
            [clojure.string :as str])
  (:import [java.util Currency Locale]
           [java.text DecimalFormat]
           [java.time LocalDate]
           [java.time.format DateTimeFormatter]))

;;; Parse monetary amounts functions

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

;;; Parse transactions functions

(defn parse-entry
  "Parses the given entry string as a transaction."
  ([string]
   (parse-entry string (Locale/getDefault)))
  ([string locale]
   (parse-entry string locale (currency-code locale)))
  ([string locale currency]
   (let [[_ account amount] (re-matches #"\s+(.*?(?=\s{2,}))\s+(.*)" string)]
     {:account account
      :amount (parse-monetary-amount amount locale currency)})))

(defn parse-transaction-header
  "Parses the given header string as a header of a transaction."
  [string]
  (rest (re-matches #"(^.*?(?=\s))\s?([*!])?\s(.*)" string)))

(defn parse-date
  "Parses the given date string."
  [string]
  (LocalDate/parse string (DateTimeFormatter/ofPattern "yyyy/MM/dd")))

(defn parse-status
  "Parses the given status string."
  [string]
  (case string
    "*" :cleared
    "!" :pending
    nil nil
    (throw (ex-info "Invalid status" {:status string}))))

(defn parse-transaction
  "Parses the given lines as a transaction."
  ([lines]
   (parse-transaction lines (Locale/getDefault)))
  ([lines locale]
   (parse-transaction lines locale (currency-code locale)))
  ([lines locale currency]
   (let [[date status payee] (parse-transaction-header (first lines))
         entries (rest lines)]
     {:date (parse-date date)
      :status (parse-status status)
      :payee payee
      :entries (map #(parse-entry % locale currency) entries)})))

;;; Read journal functions

(defn- read-transactions
  "Reads the transactions from the given file."
  [file]
  (-> file
      slurp
      (str/split #"(\n\n)|(\n*$)")))

(defn read-journal
  "Reads the journal from the given file."
  ([file]
   (read-journal file (Locale/getDefault)))
  ([file locale]
   (read-journal file locale (currency-code locale)))
  ([file locale currency]
   {:locale locale
    :currency currency
    :transactions (map #(parse-transaction (str/split % #"\n") locale currency) (read-transactions file))}))
