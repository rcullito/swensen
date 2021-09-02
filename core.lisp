(defvar yale-model
  '((us-equities 30)
    (foreign-equities 15)
    (emerging-equities 5)
    (inter-treasuries 15)
    (tips 15)
    (reits 20)))

(defvar all-weather
  '((us-equities 30)
    (inter-treasuires 15)
    (long-treasuries 40)
    (gold 7.5)
    (commodities 7.5)))

(defvar budget 10000)

(defun percent (n)
  (/ n 100))

(defun max-asset-class-value (asset-class model)
  (let ((percentage (percent (cadr (assoc asset-class model)))))
    (* budget
       percentage)))


;; the value of shares of emerging must be <= to
(max-asset-class-value 'emerging-equities yale-model)
(max-asset-class-value 'us-equities all-weather)






