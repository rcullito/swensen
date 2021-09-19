

(defstruct etf
  ticker
  asset-class
  weight
  num
  price
  budget)

(defvar swensen-model
  '((emerging .05 VWO 51.44)
    (us-equities .30 VTI 229.13)
    (foreign-equities .15 VT 105.15)
    (inter-treasuries .15 VGIT 67.96)
    (tips .15 VTIP 52.66 )
    (reits .20 VNQ 106.37)))

(defun create-etf (asset-class weight ticker price)
  `(defmacro ,asset-class ()
     (make-etf :ticker ,ticker
               :asset-class ,asset-class
               :weight ,weight
               :num 0
               :price ,price
               :budget initial-budget)))

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

(defun initialize (model)
  (mapc (lambda (asset)
        (apply #'create-etf asset))
      model))

(let ((initial-budget 4000)
      (model 'swensen-model))
  (initialize model)
  (allocate (list (emerging)
                  (us-equities)
                  (foreign-equities)
                  (inter-treasuries)
                  (tips)
                  (reits))
            initial-budget))










