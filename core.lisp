

(defstruct etf
  weight
  num
  price)

(defmethod current-weight (e)
  (if (zerop (etf-num e))
      0
      (/ budget (* (etf-num e) (etf-price e)))))

(defvar budget 1000)

(defvar emerging (make-etf :weight .05
                                   :num 12
                                   :price 64))

(defparameter us-equities (make-etf :weight .30
                                     :num 0
                                     :price 172))



(defun improve (best-so-far fund)
  (cond ((null fund)
         best-so-far)
        ((> (current-weight best-so-far) (current-weight (car fund)))
         (improve best-so-far (cdr fund)))
        (t (improve (car fund) (cdr fund)))))

(defun next-pick (fund)
  (improve (car fund) (cdr fund)))

(next-pick (list emerging us-equities))


