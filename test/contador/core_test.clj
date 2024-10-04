(ns contador.core-test
  (:require [contador.core :as sut]
            [clojure.test :as t]
            [dinero.core :as dinero])
  (:import [clojure.lang ExceptionInfo]
           [java.text ParseException]
           [java.time LocalDate]
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

(t/deftest parse-entry
  (let [germany Locale/GERMANY]
    (t/is (= {:account "Assets:Cash" :amount (dinero/money-of 1 :eur)}
             (sut/parse-entry "    Assets:Cash                                  1" germany)))
    (t/is (= {:account "Assets:Cash" :amount (dinero/money-of 1 :eur)}
             (sut/parse-entry "    Assets:Cash                                  1 €" germany)))
    (t/is (= {:account "Assets:Cash" :amount (dinero/money-of 1.23 :eur)}
             (sut/parse-entry "    Assets:Cash                                  1,23 €" germany)))))

(t/deftest parse-transaction-header
  (t/is (= ["2024/10/03" "*" "Opening Balance"]
	   (sut/parse-transaction-header "2024/10/03 * Opening Balance")))
  (t/is (= ["2024/10/04" nil "Moe's restaurant"]
	   (sut/parse-transaction-header "2024/10/04 Moe's restaurant"))))

(t/deftest parse-date
  (t/is (= (LocalDate/of 2024 10 03) (sut/parse-date "2024/10/03"))))

(t/deftest parse-status
  (t/is (= :cleared (sut/parse-status "*")))
  (t/is (= :pending (sut/parse-status "!")))
  (t/is (nil? (sut/parse-status nil)))
  (t/is (thrown? ExceptionInfo (sut/parse-status "x"))))

(t/deftest parse-transaction
  (let [germany Locale/GERMANY]
    (t/is (= {:date (LocalDate/of 2024 10 03)
              :status :cleared
              :payee "Opening Balance"
              :entries [{:account "Assets:Cash" :amount (dinero/money-of 1234.56 :eur)}
                        {:account "Assets:Debit Card" :amount (dinero/money-of 1000 :eur)}]}
             (sut/parse-transaction ["2024/10/03 * Opening Balance"
                                     "    Assets:Cash                                  1234,56 €"
                                     "    Assets:Debit Card                               1000 €"]
                                    germany)))
    (t/is (= {:date (LocalDate/of 2024 10 03)
              :status :cleared
              :payee "Opening Balance"
              :entries [{:account "Assets:Cash" :amount (dinero/money-of 1234.56 :gbp)}
                        {:account "Assets:Debit Card" :amount (dinero/money-of 1000 :gbp)}]}
             (sut/parse-transaction ["2024/10/03 * Opening Balance"
                                     "    Assets:Cash                                1234,56 £"
                                     "    Assets:Debit Card                             1000 £"]
                                    germany
                                    :gbp))))
  (let [uk Locale/UK]
    (t/is (= {:date (LocalDate/of 2024 10 03)
              :status :cleared
              :payee "Opening Balance"
              :entries [{:account "Assets:Cash" :amount (dinero/money-of 1234.56 :gbp)}
                        {:account "Assets:Debit Card" :amount (dinero/money-of 1000 :gbp)}]}
             (sut/parse-transaction ["2024/10/03 * Opening Balance"
                                     "    Assets:Cash                                   £1234.56"
                                     "    Assets:Debit Card                                £1000"]
                                    uk)))))
