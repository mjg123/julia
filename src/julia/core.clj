(ns julia.core
  (:use [quil.core]))

;; Advice to use deftype from:
;; http://stackoverflow.com/questions/11824815/fast-complex-number-arithmetic-in-clojure
;; but 'plus and 'times are not needed as ifn does its own expansion now.

(deftype complex [^double real ^double imag])

(defn mag2 [^complex z]
  ;; magnitude squared
  (let [x (double (.real z))
        y (double (.imag z))]
    (+ (* x x) (* y y))))



;; The iteration function

(defn ifn [^complex c p]
  (let [rc (.real c)
        ic (.imag c)]

    ;; Iteration functions are usually of the form:
    ;;     z_n = z_(n-1)^p + c
    ;;
    ;; c controls the "shape" of the set, is read in from mouse position
    ;;
    ;; p controls the degree of rotational symmetry (ie number of arms)
    ;;   is hard-coded in the draw fn below
    ;;
    ;; I've hard-coded the expansion of z_(n-1)^5 here for performance,
    ;; other degrees could be hard-coded, or generated by macro?

    (condp = p ;; will throw exception if there is no match

      5 (fn [^complex z]
          (let [r (double (.real z))
                i (double (.imag z))]
            (complex. (+ rc (* r r r r r) (* -10 r r r i i) (* 5 r i i i i))
                      (+ ic (* i i i i i) (* -10 r r i i i) (* 5 r r r r i))))))))


;; the iteration & thresholding

(defn count-iterations [max-its escape z_n itfn]

  ;; this used to be done with a combination of count, take-while,
  ;; iterate & repeat but LazySeq was a bottleneck so manual looping
  ;; and counting iterations

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
  ;; maps (x,y) to a point on the complext plane
  ;; in the range ((-2,-2),(2,2))

  ;; this was actually the fn which cost us the most time.
  ;; It used to say: (double (/ x w)) but we spent a lot of time
  ;; in clojure.lang.Ratio#doubleVal so coercing the numerator
  ;; to a double before the division avoids the creation & use of
  ;; the Ratio

  (complex. (* 4 (- (/ (double x) w) 0.5))
            (* 4 (- (/ (double y) h) 0.5))))

(defn hue-of [v]
  (+ 15 (* 5 v)))

(defn draw []
  (time
   (let [w (width)
         h (height)
         c (pt-to-plane (mouse-x) (mouse-y) w h)
         iteration-fn (ifn c 5)]
     (doseq [x (range w)
             y (range h)]
       (let [v (count-iterations 10 4 (pt-to-plane x y w h) iteration-fn)]
         (stroke (hue-of v) 70 70)
         (point x y))))))


(defsketch julia
  :title "Julia"
  :setup setup
  :draw draw
  :size [480 480])
