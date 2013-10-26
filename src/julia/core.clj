(ns julia.core
  (:use [quil.core]))

;; This is complex

(deftype complex [^double real ^double imag])

(defn plus [^complex z1 ^complex z2]
  (let [x1 (double (.real z1))
        y1 (double (.imag z1))
        x2 (double (.real z2))
        y2 (double (.imag z2))]
    (complex. (+ x1 x2) (+ y1 y2))))

(defn times [^complex z1 ^complex z2]
  (let [x1 (double (.real z1))
        y1 (double (.imag z1))
        x2 (double (.real z2))
        y2 (double (.imag z2))]
    (complex. (- (* x1 x2) (* y1 y2)) (+ (* x1 y2) (* y1 x2)))))

(defn mag2 [^complex z]
  ;; magnitude squared
  (let [x (double (.real z))
        y (double (.imag z))]
    (+ (* x x) (* y y))))



;; The iteration function

(defn ifn [c p]
  (fn [^complex z]
    ;; example of an iteration function
    ;; z_n = z_(n-1)^p + c
    ;; c controls the "shape" of the set
    ;; p controls the degree of rotational symmetry (ie number of arms)
    (plus c (reduce times (repeat p z)))))


;; the iteration & thresholding

(defn do-iterations [max-its escape z_n itfn]
  (count (take-while #(< (mag2 %) escape)
                     (take max-its
                           (iterate itfn z_n)))))

;; Quil stuff

(defn setup []
  (smooth)
  (frame-rate 20)
  (color-mode :hsb 100))

(defn pt-to-plane [x y w h]
  (let [s 2]
    (complex. (* 2 s (- (double (/ x w)) 0.5))
              (* 2 s (- (double (/ y h)) 0.5)))))

(defn col-of [v]
  [(int (* 10 v)) 100 100])

(defn draw []
  (time
   (let [w (width)
         h (height)
         c (pt-to-plane (mouse-x) (mouse-y) w h)]
     (doseq [x (range w)
             y (range h)]
       (let [iteration-fn (ifn c 5)
             v (do-iterations 10 4 (pt-to-plane x y w h) iteration-fn)]
         (apply stroke (col-of v))
         (point x y))))))


(defsketch julia
  :title "Julia"
  :setup setup
  :draw draw
  :size [200 200])
