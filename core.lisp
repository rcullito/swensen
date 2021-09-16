


(defvar percentage .30)
(defvar price 178)
(defvar budget 10000)
(defvar maximum-shares (floor (/ (* budget percentage) price)))

(defun good-enough? (guess)
  (or (= maximum-shares guess)
      (= (1- maximum-shares) guess)))

(defun improve (guess)
  (cond ((> guess maximum-shares) (1- guess))
        ((< (1+ guess) maximum-shares) (1+ guess))
        (t guess)))

(defun pick (guess)
  (if (good-enough? guess)
      guess
      (pick (improve guess))))


(pick 8)
