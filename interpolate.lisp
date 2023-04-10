(defun lerp (a b s)
  (+ a (* s (- b a))))

(defun smooth (s)
  (* s s (- 3 (* s 2))))

(defun smoothstep (a b s)
  (lerp a b (smooth s)))

(defun smoother (s)
  (* s s s (+ (* s (- (* s 6) 15)) 10)))

(defun smootherstep (a b s)
  (lerp a b (smoother s)))
