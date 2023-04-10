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


(defun test ()
  (let ((a 0) (b 100))
    (loop :for i :from 0 :to 1 :by 0.1 :do 
          (format t "~a~%" 
                  (smootherstep a b i)))))
