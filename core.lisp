

(defstruct etf
  weight
  num
  price)

(defmethod current-weight (e)
  (if (zerop (etf-num e))
      0
      (/ budget (* (etf-num e) (etf-price e)))))

(defmethod far-off (e)
  (- (etf-weight e) (current-weight e)))

(defvar budget 100)

(defvar emerging (make-etf :weight .30
                                   :num 0
                                   :price 10))

(defparameter us-equities (make-etf :weight .70
                                     :num 0
                                     :price 10))

(defun improve (best-so-far fund)
  (cond ((null fund)
         best-so-far)
        ((> (far-off best-so-far) (far-off (car fund)))
         (improve best-so-far (cdr fund)))
        (t (improve (car fund) (cdr fund)))))

(defun next-pick (fund)
  (improve (car fund) (cdr fund)))

(next-pick (list emerging us-equities))
