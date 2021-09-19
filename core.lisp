

(defstruct etf
  weight
  num
  price)

(defvar budget 100)

(defmethod expenditure (e)
  (* (etf-num e) (etf-price e)))


(defmethod current-weight (e)
  (if (zerop (etf-num e))
      0
      (/ budget (expenditure e))))

(defmethod far-off (e)
  (- (etf-weight e) (current-weight e)))

(defun total-expenditure (fund)
  (apply #'+ (mapcar #'expenditure fund)))

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

(defun allocate (fund budget)
  ;; TODO here
  ;; make a data structure of etfs and incremented counts
  ;; dec budget
  (if (> budget (total-expenditure fund))
      (let ((pick (next-pick fund)))
        (incf (etf-num pick))
        (allocate fund (- budget (etf-price pick))))
      fund))

(allocate (list emerging us-equities) budget)


