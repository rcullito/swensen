

(defstruct etf
  weight
  num
  price)

(defun emerging ()
    (make-etf :weight .30
              :num 0
              :price 10))

(defun us-equities ()
  (make-etf :weight .70
            :num 0
            :price 10))

(defmethod expenditure (e)
  (* (etf-num e) (etf-price e)))

(defmacro current-weight (e)
  `(if (zerop (etf-num ,e))
      0
      (/ (expenditure ,e) budget)))

(defmacro far-off (e)
  `(- (etf-weight ,e) (current-weight ,e)))

(defun total-expenditure (fund)
  (apply #'+ (mapcar #'expenditure fund)))

(defun improve (best-so-far fund budget)
  (print-object budget *standard-output*)
  (cond ((null fund)
         best-so-far)
        ((> (far-off best-so-far) (far-off (car fund)))
         (improve best-so-far (cdr fund) budget))
        (t (improve (car fund) (cdr fund) budget))))

(defun next-pick (fund budget)
  (improve (car fund) (cdr fund) budget))

(defun allocate (fund budget)
  ;; TODO here
  ;; make a data structure of etfs and incremented counts
  ;; dec budget
  (if (< budget (apply #'max (mapcar #'etf-price fund)))
      fund
      (let ((pick (next-pick fund budget)))
        (incf (etf-num pick))
        (allocate fund (- budget (etf-price pick))))))

(allocate (list (emerging) (us-equities)) 100)

(let ((budget 100))
  (current-weight emerging))

emerging






