

(defstruct etf
  weight
  num
  price
  budget)

(defmacro emerging ()
  `(make-etf :weight .30
             :num 0
             :price 10
             :budget initial-budget))

(defmacro us-equities ()
  `(make-etf :weight .70
            :num 0
            :price 10
            :budget initial-budget))

(defmethod expenditure (e)
  (* (etf-num e) (etf-price e)))

(defmacro current-weight (e)
  `(if (zerop (etf-num ,e))
      0
      (/ (expenditure ,e) (etf-budget ,e))))

(defmacro far-off (e)
  `(- (etf-weight ,e) (current-weight ,e)))

(defun total-expenditure (fund)
  (apply #'+ (mapcar #'expenditure fund)))

(defun next-pick (fund budget)
  (car (sort fund (lambda (x y)
                    (print (etf-weight x))
                    (print (current-weight x))
                    (print (etf-weight y))
                    (print (current-weight y))
                    (> (far-off x) (far-off y))))))

(defun allocate (fund budget)
  (if (< budget (apply #'max (mapcar #'etf-price fund)))
      fund
      (let ((pick (next-pick fund budget)))
        (incf (etf-num pick))
        (allocate fund (- budget (etf-price pick))))))

(let ((initial-budget 100))
  (allocate (list (emerging) (us-equities)) initial-budget))










