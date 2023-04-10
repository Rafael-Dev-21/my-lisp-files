(load #p"interpolate.lisp")

(defparameter *seed* 0)

(defparameter *hash* #(151 160 137 91  90  15  131 13  201 95  96  53  194 233 7
                       225 140 36  103 30  69  142 8   99  37  240 21  10  23  190
                       6   148 247 120 234 75  0   26  197 62  94  252 219 203 117
                       35  11  32  57  177 33  88  237 149 56  87  174 20  125 136
                       171 168 68  175 74  165 71  134 139 48  27  166 77  146 158
                       231 83  111 229 122 60  211 133 230 220 105 92  41  55  46
                       245 40  244 102 143 54  65  25  63  161 1   216 80  73  209
                       76  132 187 208 89  18  169 200 196 135 130 116 188 159 86
                       164 100 109 198 173 186 3   64  52  217 226 250 124 123 5
                       202 38  147 118 126 255 82  85  212 207 206 59  227 47  16
                       58  17  182 189 28  42  223 183 170 213 119 248 152 2   44
                       154 163 70  221 153 101 155 167 43  172 9   129 22  39  253
                       19  98  108 110 79  113 224 232 178 185 112 104 218 246 97
                       228 251 34  242 193 238 210 144 12  191 179 162 241 81  51
                       145 235 249 14  239 107 49  192 214 31  181 199 106 157 184
                       84  204 176 115 121 50  45  127   4 150 254 138 236 205 93
                       222 114 67  29  24  72  243 141 128 195 78  66  215 61  156 
                       180))

(defun grad2 (x y)
  (let (( yindex (mod (+ y *seed*) 256)))
    (when (< yindex 0)
      (incf yindex 255))
    (let ((xindex (mod (+ (aref *hash* yindex) x *seed*) 256)))
      (when (< xindex 0)
        (incf xindex 255))
      (aref *hash* xindex))))

(defun value2 (x y)
  (let* ((xint (floor x))
        (yint (floor y))
        (xfrac (- x xint))
        (yfrac (- y yint)))
    (/ (smootherstep 
         (smootherstep 
           (grad2 xint yint)
           (grad2 (1+ xint) yint)
           xfrac)
         (smootherstep
           (grad2 xint (1+ yint))
           (grad2 (1+ xint) (1+ yint))
           xfrac)
         yfrac) 256.0)))

(defun dot2 (grad x y)
  (case (mod grad 8)
    (0 x)
    (1 (+ x y))
    (2 y)
    (3 (+ (- x) y))
    (4 (- x))
    (5 (- (- x) y))
    (6 (- y))
    (7 (- x y))))

(defun perlin2 (x y)
  (let* ((xint (floor x))
         (yint (floor y))
         (xfrac (- x xint))
         (yfrac (- y yint)))
    (* (+ (smootherstep
            (smootherstep
              (dot2 
                (grad2 xint yint)
                xfrac
                yfrac)
              (dot2
                (grad2 (1+ xint) yint)
                (1- xfrac)
                yfrac)
              xfrac)
            (smootherstep
              (dot2
                (grad2 xint (1+ yint))
                xfrac
                (1- yfrac))
              (dot2
                (grad2 (1+ xint) (1+ yint))
                (1- xfrac)
                (1- yfrac))
              xfrac)
            yfrac) 1.0) 0.5)))


(defun fbm (fn x y &key (frequency 1) (amplitude 1) (lacunarity 2) (persistence 0.5) (octaves 3))
  (let ((xa (* x frequency))
        (ya (* y frequency))
        (amp amplitude)
        (result 0.0)
        (div 0))
    (loop :repeat octaves :do
          (incf result (* (funcall fn xa ya) amp))
          (incf div amp)
          (setf xa (* ya lacunarity) ya (* ya lacunarity) amp (* amp persistence)))
    (/ result div)))
