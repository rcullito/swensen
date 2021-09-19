

(defstruct etf
  weight
  num
  price
  budget)

(defmacro emerging ()
  `(make-etf :weight .05
             :num 0
             :price 51.44 ;; VWO
             :budget initial-budget))

(defmacro us-equities ()
  `(make-etf :weight .30
             :num 0
             :price 229.13 ;; VTI
             :budget initial-budget))

(defmacro foreign-equities ()
  `(make-etf :weight .15
             :num 0
             :price 105.15 ;; VT
             :budget initial-budget))

(defmacro inter-treasuries ()
  `(make-etf :weight .15
             :num 0
             :price 67.96 ;; VGIT
             :budget initial-budget))

(defmacro tips ()
  `(make-etf :weight .15
             :num 0
             :price 52.66 ;; VTIP
             :budget initial-budget))

(defmacro reits ()
  `(make-etf :weight .20
             :num 0
             :price 106.37 ;; VNQ
             :budget initial-budget))

(defmethod expenditure (e)
  (* (etf-num e) (etf-price e)))

(defun total-expenditure (fund)
  (apply #'+ (mapcar #'expenditure fund)))

(defmacro current-weight (e)
  `(if (zerop (etf-num ,e))
      0
      (/ (expenditure ,e) (etf-budget ,e))))

(defmacro far-off (e)
  `(- (etf-weight ,e) (current-weight ,e)))


(defun next-pick (fund budget)
  (car (sort fund (lambda (x y)
                    (> (far-off x) (far-off y))))))

(defun allocate (fund budget)
  (if (< budget (apply #'max (mapcar #'etf-price fund)))
      fund
      (let ((pick (next-pick fund budget)))
        (incf (etf-num pick))
        (allocate fund (- budget (etf-price pick))))))

(let ((initial-budget 4000))
  (allocate (list (emerging)
                  (us-equities)
                  (foreign-equities)
                  (inter-treasuries)
                  (tips)
                  (reits))
            initial-budget))










