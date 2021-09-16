(defvar yale-model
  '((us-equities .30 178)
    (foreign-equities . .15)
    (emerging-equities . .5)
    (inter-treasuries . .15)
    (tips . .15)
    (reits . .20)))


(defvar budget 10000)

;; where guesses is a list of dotteded pairs of (17 . us-equities)
(defun good-enough? (guess)
  (let* ((asset-meta (assoc (car guess) yale-model))
         (percentage (cadr asset-meta))
         (price (caddr asset-meta))
        (maximum (/ (* budget percentage) price)))
    (values
     (* percentage
        (cdr guess))
     maximum)))

(good-enough? '(us-equities . 17))

(cdr (assoc 'us-equities yale-model))


(defvar guesses '((us-equities . 17)
                  (foreign-equities . 6)
                  (emerging-equities . 4)
                  (inter-treasuries . 12)
                  (tips . 8)
                  (reits . 24)))




(mapcar #'car all-weather)
