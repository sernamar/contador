(ns contador.core-test
  (:require [contador.core :as sut]
            [clojure.test :as t]
            [dinero.core :as dinero])
  (:import [java.text ParseException]
           [java.util Locale]))

(t/deftest parse-monetary-amount
  (let [germany Locale/GERMANY]
    (t/is (= (dinero/money-of 1 :eur) (sut/parse-monetary-amount "1" germany)))
    (t/is (= (dinero/money-of 1 :eur) (sut/parse-monetary-amount "1 €" germany)))
    (t/is (= (dinero/money-of 1 :eur) (sut/parse-monetary-amount "1 EUR" germany)))
    (t/is (= (dinero/money-of -1 :eur) (sut/parse-monetary-amount "-1" germany)))
    (t/is (= (dinero/money-of -1 :eur) (sut/parse-monetary-amount "-1 €" germany)))
    (t/is (= (dinero/money-of 1.23 :eur) (sut/parse-monetary-amount "1,23" germany)))
    (t/is (= (dinero/money-of 1.23 :eur) (sut/parse-monetary-amount "1,23 €" germany)))
    (t/is (= (dinero/money-of 1234 :eur) (sut/parse-monetary-amount "1.234" germany)))
    (t/is (= (dinero/money-of 1234 :eur) (sut/parse-monetary-amount "1.234 €" germany)))
    ;; other currency
    (t/is (= (dinero/money-of 1.23 :gbp) (sut/parse-monetary-amount "1,23 £" germany :gbp)))
    ;; invalid format or grouping
    (t/is (thrown? ParseException (sut/parse-monetary-amount "1.23" germany)))
    (t/is (thrown? ParseException (sut/parse-monetary-amount "1.23 €" germany)))
    (t/is (thrown? ParseException (sut/parse-monetary-amount "1.23 €" germany)))
    (t/is (thrown? ParseException (sut/parse-monetary-amount "1.2345" germany)))
    (t/is (thrown? ParseException (sut/parse-monetary-amount "1.2345 €" germany)))
    ;; invalid currency
    (t/is (thrown? ParseException (sut/parse-monetary-amount "1 CNY" germany))))
  (let [uk Locale/UK]
    (t/is (= (dinero/money-of 1 :gbp) (sut/parse-monetary-amount "1" uk)))
    (t/is (= (dinero/money-of 1 :gbp) (sut/parse-monetary-amount "£1" uk)))
    (t/is (= (dinero/money-of 1.23 :gbp) (sut/parse-monetary-amount "£1.23" uk)))
    (t/is (= (dinero/money-of 1.23 :eur) (sut/parse-monetary-amount "€1.23" uk :eur)))))
