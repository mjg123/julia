(ns julia.core
  (:use [quil.core]))

;; This is complex
;; http://stackoverflow.com/questions/11824815/fast-complex-number-arithmetic-in-clojure

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
    ;; c controls the "shape" of the set, is read in from mouse position
    ;; p controls the degree of rotational symmetry (ie number of arms)
    ;;   is hard-coded in the draw fn below


    ;; NB this is hard-coded for p=5 to test performance
    (let [r (double (.real z))
          i (double (.imag z))]
      (plus c
            (complex. (+ (* r r r r r) (* -10 r r r i i) (* 5 r i i i i))
                      (+ (* i i i i i) (* -10 r r i i i) (* 5 r r r r i)))))))


;; the iteration & thresholding

(defn count-iterations [max-its escape z_n itfn]

  (let [escape-pred #(> (mag2 %) escape)]

    (loop [it-count 0
           z z_n]
      (if (or (escape-pred z)
              (< max-its it-count))
        it-count
        (recur (inc it-count) (itfn z))))))

;; Quil stuff

(defn setup []
  (frame-rate 20)
  (color-mode :hsb 100))

(defn pt-to-plane [x y w h]
  ;; maps (x,y) to a point in the range ((-2,-2),(2,2))
  (complex. (* 4 (- (/ (double x) w) 0.5))
            (* 4 (- (/ (double y) h) 0.5))))

(defn hue-of [v]
  (* 5 v))

(defn draw []
  (time
   (let [w (width)
         h (height)
         c (pt-to-plane (mouse-x) (mouse-y) w h)
         iteration-fn (ifn c 5)]
     (doseq [x (range w)
             y (range h)]
       (let [v (count-iterations 10 4 (pt-to-plane x y w h) iteration-fn)]
         (stroke (hue-of v) 100 100)
         (point x y))))))


(defsketch julia
  :title "Julia"
  :setup setup
  :draw draw
  :size [480 480])
