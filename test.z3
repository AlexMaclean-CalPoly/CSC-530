(declare-const a1 Int)
(declare-const a2 Int)
(declare-const a3 Int)
(declare-const a4 Int)
(declare-const a5 Int)
(declare-const a6 Int)

(declare-const l1 Int)
(declare-const l2 Int)
(declare-const l Int)

(assert (= (+ (- (* 50 a1 l1) (* a3 l1) l1) (- (* 50 a4 l2) (* a6 l2) l2)) (- l)))
(assert (= 0 (+ (* a2 l1) (* a5 l2))))
(assert (and (>= l1 0) (>= l2 0) (> l 0)))

(check-sat)
(get-model)


