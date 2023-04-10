(defun c-to-f (celsius)
  "converts celsius to fahrenheit."
  (+ (* celsius 1.8) 32))

(defun f-to-c (fahr)
  "converts fahrenheit to celsius."
  (/ (- fahr 32) 1.8))

(defun fn-to-table (fn &key (lower 0) (upper 10))
  "uses a funcion to create a alist of input/output"
  (loop :for i :from lower :to upper :collect
        (list i (funcall fn i))))

(defun plot-table (title table)
  "pretty-prints a table"
  (format t "~a~%" title)
  (format t "--------------~%")
  (format t "~:{ ~a~10t~a~%~}" table))

(defun plot-func (title func &key (lower 0) (upper 10))a
  "pretty-prints a table from a function"
  (plot-table title (fn-to-table func :lower lower :upper upper)))
