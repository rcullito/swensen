

(defstruct etf
  ticker
  asset-class
  weight
  num
  price
  budget)

(defmacro emerging ()
  `(make-etf :ticker 'VWO
             :asset-class 'emerging
             :weight .05
             :num 0
             :price 51.44
             :budget initial-budget))

(defmacro us-equities ()
  `(make-etf :asset-class 'us-equities
             :ticker 'VTI
             :weight .30
             :num 0
             :price 229.13 
             :budget initial-budget))

(defmacro foreign-equities ()
  `(make-etf :asset-class 'foreign-equities
             :ticker 'VT
             :weight .15
             :num 0
             :price 105.15
             :budget initial-budget))

(defmacro inter-treasuries ()
  `(make-etf :ticker 'VGIT
             :weight .15
             :num 0
             :price 67.96 
             :budget initial-budget))

(defmacro tips ()
  `(make-etf :ticker 'VTIP
             :weight .15
             :num 0
             :price 52.66 
             :budget initial-budget))

(defmacro reits ()
  `(make-etf :ticker 'VNQ
             :weight .20
             :num 0
             :price 106.37
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










