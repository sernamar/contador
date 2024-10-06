(ns contador.core-test
  (:require [contador.core :as sut]
            [clojure.test :as t]
            [dinero.core :as dinero])
  (:import [clojure.lang ExceptionInfo]
           [java.text ParseException]
           [java.time LocalDate]
           [java.util Locale]))

;;; Parse monetary amounts functions

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

;;; Parse transactions functions

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
              :entries [{:account "Assets:Cash" :amount (dinero/money-of 500 :eur)}
                        {:account "Assets:Debit Card" :amount (dinero/money-of 500 :eur)}
                        {:account "Equity:Opening Balances" :amount (dinero/money-of -1000 :eur)}]}
             (sut/parse-transaction ["2024/10/03 * Opening Balance"
                                     "    Assets:Cash                                    500 €"
                                     "    Assets:Debit Card                              500 €"
                                     "    Equity:Opening Balances                      -1000 €"]
                                    germany)))
    (t/is (= {:date (LocalDate/of 2024 10 03)
              :status :cleared
              :payee "Opening Balance"
              :entries [{:account "Assets:Cash" :amount (dinero/money-of 500 :gbp)}
                        {:account "Assets:Debit Card" :amount (dinero/money-of 500 :gbp)}
                        {:account "Equity:Opening Balances" :amount (dinero/money-of -1000 :gbp)}]}
             (sut/parse-transaction ["2024/10/03 * Opening Balance"
                                     "    Assets:Cash                                    500 £"
                                     "    Assets:Debit Card                              500 £"
                                     "    Equity:Opening Balances                      -1000 £"]
                                    germany
                                    :gbp))))
  (let [uk Locale/UK]
    (t/is (= {:date (LocalDate/of 2024 10 03)
              :status :cleared
              :payee "Opening Balance"
              :entries [{:account "Assets:Cash" :amount (dinero/money-of 500 :gbp)}
                        {:account "Assets:Debit Card" :amount (dinero/money-of 500 :gbp)}
                        {:account "Equity:Opening Balances" :amount (dinero/money-of -1000 :gbp)}]}
             (sut/parse-transaction ["2024/10/03 * Opening Balance"
                                     "    Assets:Cash                                     £500"
                                     "    Assets:Debit Card                               £500"
                                     "    Equity:Opening Balances                       -£1000"]
                                    uk)))))

;;; Transactions functions

(def journal ^:private
  {:locale Locale/GERMANY
   :currency :eur
   :transactions [{:date (LocalDate/of 2024 10 03)
                   :status :cleared
                   :payee "Opening Balance"
                   :entries [{:account "Assets:Cash" :amount (dinero/money-of 500 :eur)}
                             {:account "Assets:Debit Card" :amount (dinero/money-of 500 :eur)}
                             {:account "Equity:Opening Balances" :amount (dinero/money-of -1000 :eur)}]}
                  {:date (LocalDate/of 2024 10 04)
                   :status nil
                   :payee "Moe's restaurant"
                   :entries [{:account "Expenses:Restaurant:Food" :amount (dinero/money-of 20 :eur)}
                             {:account "Expenses:Restaurant:Tips" :amount (dinero/money-of 2 :eur)}
                             {:account "Assets:Cash" :amount (dinero/money-of -12 :eur)}
                             {:account "Assets:Debit Card" :amount (dinero/money-of -10 :eur)}]}
                  {:date (LocalDate/of 2024 10 05)
                   :status nil
                   :payee "Mike's convenience store"
                   :entries [{:account "Expenses:Groceries" :amount (dinero/money-of 35.95 :eur)}
                             {:account "Assets:Cash" :amount (dinero/money-of -35.95 :eur)}]}]})

(t/deftest filter-transactions-by-date
  (let [transactions (:transactions journal)]
    (t/is (= (list (first transactions))
             (sut/filter-transactions-by-date transactions (LocalDate/of 2024 10 01) (LocalDate/of 2024 10 03))))
    (t/is (= (rest transactions)
             (sut/filter-transactions-by-date transactions (LocalDate/of 2024 10 04) (LocalDate/of 2024 10 05))))
    (t/is (= transactions
             (sut/filter-transactions-by-date transactions (LocalDate/of 2024 10 01) (LocalDate/of 2024 10 05))))))

(t/deftest filter-transactions-by-payee
  (let [transactions (:transactions journal)]
    (t/is (= (list (first transactions))
             (sut/filter-transactions-by-payee transactions "Opening Balance"))
    (t/is (= (list (second transactions))
             (sut/filter-transactions-by-payee transactions "Moe's restaurant"))
    (t/is (= (list (last transactions))
             (sut/filter-transactions-by-payee transactions "Mike's convenience store")))))))

(t/deftest filter-transactions-by-account-name
  (let [transactions (:transactions journal)]
    (t/is (= transactions
             (sut/filter-transactions-by-account-name transactions "Assets")))
    (t/is (= transactions
             (sut/filter-transactions-by-account-name transactions "Assets:Cash")))
    (t/is (= (take 2 transactions)
             (sut/filter-transactions-by-account-name transactions "Assets:Debit Card")))
    (t/is (= (rest transactions)
             (sut/filter-transactions-by-account-name transactions "Expenses")))))

(t/deftest get-transactions
  (let [transactions (:transactions journal)]
    (t/is (= (list (first transactions))
             (sut/get-transactions journal {:payee "Balance" :account-name "Cash"})))
    (t/is (= (list (first transactions))
             (sut/get-transactions journal {:account-name "Cash" :end-date (LocalDate/of 2024 10 03)})))
    (t/is (= (list (second transactions))
             (sut/get-transactions journal {:account-name "Expenses" :end-date (LocalDate/of 2024 10 04)})))
    (t/is (= (list (second transactions))
             (sut/get-transactions journal {:payee "Moe's restaurant" :account-name "Cash"})))
    (t/is (empty? (sut/get-transactions journal :start-date (LocalDate/of 2024 10 01) :end-date (LocalDate/of 2024 10 02))))))

;;; Balance functions

(t/deftest filter-entries-by-account-name
  (let [transactions (:transactions journal)
        entries (:entries (first transactions))]
    (t/is (= (list (first entries))
             (sut/filter-entries-by-account-name entries "Assets:Cash")))
    (t/is (= (take 2 entries)
             (sut/filter-entries-by-account-name entries "Assets")))
    (t/is (empty? (sut/filter-entries-by-account-name entries "Expenses")))))

(t/deftest get-balance
  (t/is (= (dinero/money-of 452.05 :eur)
           (sut/get-balance journal "Assets:Cash")))
  (t/is (= (dinero/money-of 500 :eur)
           (sut/get-balance journal "Assets:Cash"
                            {:start-date (LocalDate/of 2024 10 01) :end-date (LocalDate/of 2024 10 03)})))
  (t/is (= (dinero/money-of -12 :eur)
           (sut/get-balance journal "Assets:Cash"
                            {:start-date (LocalDate/of 2024 10 04) :end-date (LocalDate/of 2024 10 04)})))
  (t/is (= (dinero/money-of -47.95 :eur)
           (sut/get-balance journal "Assets:Cash"
                            {:start-date (LocalDate/of 2024 10 04) :end-date (LocalDate/of 2024 10 05)}))))
