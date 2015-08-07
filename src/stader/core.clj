(ns stader.core
  (:gen-class)
  (:require [mikera.image.core :as img]
            [mikera.image.filters :as imgf]
            [mikera.image-matrix :as imat]
            [clojure.core.matrix :as mat]
            [clojure.core.matrix.operators :as matop]
            [clojure.math.combinatorics :as combo]))

(defn filter-puzzle [puzzle]
  (-> puzzle
    (img/filter-image (imgf/grayscale))
    (img/filter-image (imgf/->Filter
    (com.jhlabs.image.ThresholdFilter. (float 220.0))))
    (img/filter-image (imgf/->Filter (com.jhlabs.image.SharpenFilter.)))))

(defn delta [x y m]
  (if-not (mat/equals x y)
    (let [x-value (/ (bit-and (apply img/get-pixel m x) 0xff) 255.0)
          y-value (/ (bit-and (apply img/get-pixel m y) 0xff) 255.0)]
          (Math/abs (- x-value y-value)))
    0.0))

(defn counter-term [center x radius]
  (let [distance (mat/distance center x)]
      (if (<= distance radius)
        1
        0)))

(defn make-neighborhood [center r]
  (let [[start-x start-y] (map #(Math/ceil %) (matop/- center r))
        ;; range exclusive => floor + inc == ceil
        [end-x end-y] (map #(Math/ceil %) (matop/+ center r))]
      (for [x (range start-x end-x) y (range start-y end-y)]
        [x y])))

(defn term [ct d]
  (+ (* ct d)
     (* (- 1.0 ct)
        (- 1.0 d))))

(defn J [puzzle r center]
  (mat/esum
      (for [x (make-neighborhood center (+ r 2))]
        (let [ct (counter-term center x r)
              d (delta center x puzzle)]
          #_(println x ct d (term ct d))
          (term ct d)))))

(defn process [puzzle pos]
    (if (zero? (mod (first pos) 1000)) (do (print ".") (flush)))
    (if (== -16777216 (apply img/get-pixel puzzle pos))
        {:location pos :score (J puzzle 8 pos)}
        {:location pos :score 1000.0}))

(defn scores [puzzle]
  (pmap
    (partial process puzzle)
    (combo/cartesian-product
        (range 16 (- (mat/column-count puzzle) 16))
        (range 16 (- (mat/row-count puzzle) 16)))))

(defn best-near [results point]
  (->> (for [[x y] (make-neighborhood point 8)] (filter (fn [{[x_ y_] :location}] (and (== x_ x) (== y_ y))) results))
      (filter #(not (empty? %)))
      (map first)
      (sort-by :score)))

(defn best [dots]
  (pmap #(first (best-near dots (:location %))) dots))

(defn unique [dots]
    (reduce
      (fn [acc {location :location :as value}]
        (if (empty? (filter #(= location (:location %)) acc))
          (conj acc value)
          acc)) [] dots))

(defn find-unique-dots [candidates] ((comp unique best) candidates))

(defn offsets [direction]
  (get {:right [15 -13] :left [-55 -14] :up [-24 -42] :down [-23 11]} direction))

(defn get-image [puzzle direction position]
  (let [digit-top-left (matop/+ position (offsets direction))
        digit-bottom-right (matop/+ digit-top-left [45 29])
        dot-top-left (matop/- position 8)
        dot-bottom-right (matop/+ position 8)
        [x y] [(min (first digit-top-left) (first dot-top-left)) (min (second digit-top-left) (second dot-top-left))]
        [x_ y_] [(max (first digit-bottom-right) (first dot-bottom-right)) (max (second digit-bottom-right) (second dot-bottom-right))]]
    (img/sub-image puzzle x y (- x_ x) (- y_ y))))

(defn get-digit [puzzle x y]
  (->>
    (mat/select puzzle (range y (+ y 29)) (range x (+ x 15)))
    (mat/to-vector)
    (map #(first (vec %)))))

(defn get-digits [puzzle direction pos]
  (for [n (range 3) :let [[x y] (matop/+ pos (offsets direction) [(* 15 n) 0])]]
    (if (and (> x 0) (> y 0) (< (+ x 15) (.getWidth puzzle)) (< (+ y 29) (.getHeight puzzle)))
      (let [digit (get-digit puzzle x y)]
        (if-not (every? #(= 1.0 %) digit)
            {:digit digit
            :image (img/sub-image puzzle x y 15 29)}
            nil))
        nil)))


(defn closeness [source target] (mat/distance source target))

(defn recognize-digit [training-data digit]
  (if-not (nil? digit)
      (let [recognized (apply min-key :distance
              (map (fn [[data value]] {:digit value
                      :distance (closeness digit data)})
                training-data))]
        (if (< (:distance recognized) 10.0) recognized { :digit 0 :distance nil }))
      {:digit 0 :distance nil}))

(defn recognize-number [training-data digits]
  (dissoc
      (reduce (fn [{number :number coeff :coeff distance :distance} val]
          {
          :number (+ number (* coeff (:digit val)))
          :coeff (if-not (nil? (:distance val)) (* coeff 10) coeff)
          :distance (conj distance (:distance val))
          })
          {:number 0 :coeff 1 :distance []}
          (reverse (map (partial recognize-digit training-data) digits)))
      :coeff))

(defn variance [direction]
  (direction
     {:up [[-8 8] [-8 8]]
      :left [[-8 8] [-8 8]]
      :right [[-8 8] [-8 8]]
      :down [[-8 8] [-8 8]]}))

(defn guesses [training-data puzzle pos]
    (flatten
      (for [direction [:left :right :up :down]]
        (for [offset-x (apply range (first (variance direction))) offset-y (apply range (second (variance direction)))
           :let [digits (map :digit (get-digits puzzle direction
                             (matop/+ pos [offset-x offset-y])))]
           :when (not-every? nil? digits)]
        (assoc (recognize-number training-data digits)
          :direction direction
          :dot-location pos
          :location (matop/+ pos [offset-x offset-y]))))))

(defn best-guess [training-data puzzle pos]
  (let [guesses (guesses training-data puzzle pos)
        num-digits (apply max (map #(count (remove nil? (:distance %))) guesses))
        filtered-guesses (filter #(= num-digits (count (remove nil? (:distance %)))) guesses)]
  (apply min-key #(mat/esum (remove nil? (:distance %))) filtered-guesses)))

(defn find-dots [puzzle]
  (->> puzzle
      (filter-puzzle)
      (scores)
      (filter #(<= (:score %) 50.0))
      (best)
      (unique)))

(defn recognize-numbers [puzzle training-data dots]
  (let [gs-puzzle (filter-puzzle puzzle)]
    (pmap (comp (partial best-guess training-data gs-puzzle) :location) dots)))

; pulled from part 1
(def training-data
  (let [gs-puzzle (filter-puzzle (img/load-image-resource "puzzle1-part.png"))]
      (map (fn [[[x y] result]] [(get-digit gs-puzzle x y) result])
        [[[521 319] 0] [[482 217] 1] [[467 217] 2] [[352 478] 3]
         [[513 663] 4] [[452 217] 5] [[318 615] 6] [[394 547] 7]
         [[416 676] 8] [[386 311] 9] [[319 615] 6] [[387 311] 9]])))

(defn correct-numbers [puzzle numbers]
  (let [duplicates (for [[number freq] (frequencies (map :number numbers)) :when (> freq 1)] number)]
    (for [number numbers]
      (if (some #(= (:number number) %) duplicates)
          (do
             (img/show (get-image puzzle (:direction number) (:location number)))
             (println "Recognized number " (:number number))
             (println "Enter correct number: ")
             (assoc number :number (Integer/parseInt (read-line))))
      number))))

(defn -main
  [& args]
  (println "STADER Puzzle solver 1.0")
  (println "Locating circles and detecting numbers. This might take a while...")

  (if-not (= (count args) 2)
    (do
      (println "usage: lein run [puzzle-resource] [output-csv]")
      (System/exit -1))

    (let [start (System/currentTimeMillis)
          puzzle (img/load-image-resource (first args))
          dots (find-dots puzzle)
          numbers (sort-by :number (recognize-numbers puzzle training-data dots))
          corrected-numbers (sort-by :number (correct-numbers puzzle numbers))]

        (println (str "Completed in " (-> (System/currentTimeMillis)
                                                (- start)
                                                (/ 1000.0)) " seconds"))

        (with-open [w (java.io.FileWriter. (java.io.File. (second args)))]
            (.write w "num,x,y\n")
            (doseq [{[x y] :dot-location number :number} corrected-numbers]
                (.write w (str number "," (int x) "," (int y) "\n"))))

        (System/exit 0))))

; test test test
(comment
    (defn check [expected]
        (reduce (fn [acc val] (if (not= (first val) (:number (second val))) (conj acc val) acc))
            []
            (->> (recognize-numbers puzzle training-data wow)
                  (interleave expected)
                  (partition 2))))

    (def g (guesses training-data (filter-puzzle puzzle) (:location (first wow))))
    (let [num-digits (apply max (map #(count (remove nil? (:distance %))) g))]
      num-digits)

    (filter #(= 2 (count (remove nil? (:distance %)) )) g)
    (filter #(= 31 (:number %)) g)

    ; part 1
    (def puzzle (img/load-image-resource "puzzle1-part.png"))
    (def wow (doall (find-dots puzzle)))
    (check [641 634 528 635 636 519 524 527 521 518 517 522 526 520 523 637 516 525 640 638])

    ; part 2
    (def puzzle (img/load-image-resource "puzzle1-part2.png"))
    (def wow (doall (find-dots puzzle)))
    (check [731 711 710 718 719 730 708 709 696 707 712 734 729 727 706 725 726 705 728 713 703 704 732 733 701 722 702 720 721 714 724 735 700 313 698 699 715 723 138 311 312 716 142 139 736 141 697])

    ; part 3
    (def puzzle (img/load-image-resource "puzzle1-part3.png"))
    (def wow (doall (find-dots puzzle)))
    (check [])

    ; part 4
    (def puzzle (img/load-image-resource "puzzle1-part4.png"))
    (def wow (doall (find-dots puzzle)))
    (check [573 607 604 574 614 615 606 665 664 572 616 623 621 622 603 601 620 619 571 618 625 617 661 626 624 613 600 378 660 663 598 646 570 647 597 651 627 594 608 650 576 649 645 612 593 575 561 577 567 568 560 569 611 590 662 551 554 555 550 549 548 589 610 558 552 566 557 556 553 586 563 579 562 559 578 542 565 564 609 395 580 581 391 583 582 394 585 584 399 541 540 400 536 396 488 489 408 629 490 628 403])

    (def puzzle (img/load-image-resource "puzzle1-part5.png"))
    (def wow (doall (find-dots puzzle)))
    (check [])

    (correct-numbers puzzle [{:number 1 :location [100 100] :direction :up} {:number 1 :location [100 100] :direction :up} {:number 20 :location [100 100] :direction :up}])


    ; verification tools & general debugging stuff
    (img/show (get-image puzzle :right [70 267]))

    (img/show (:image (nth (get-digits puzzle :right [67 267]) 2)))
    (mat/esum
      (:distance
        (recognize-number training-data (map :digit (get-digits (filter-puzzle puzzle) :left [67 267])))))
    (best-guess training-data (filter-puzzle puzzle) [82 577])

    (for [{location :location} wow
                    :let [gs-puzzle (filter-puzzle puzzle)
                          number (best-guess training-data gs-puzzle location)]]
      (do
         (img/show (get-image puzzle (:direction number) location))
         (println location "number:" number)
         (read-line)))

    (defn mark [image [x y]]
      (img/set-pixel image x y 0xffff0000)
      (img/set-pixel image (inc x) y 0xffff0000)
      (img/set-pixel image (dec x) y 0xffff0000)
      (img/set-pixel image x (inc y) 0xffff0000)
      (img/set-pixel image x (dec y) 0xffff0000))

    (let [marked-image (img/load-image-resource "puzzle1-part3.png")]
        (doall (for [{location :location} (sort-by :score wow)]
          (mark marked-image location)))
        (img/save marked-image "/tmp/marked.png"))

    ; circle debugging
    (defn delta-mat [x y m]
      (if-not (mat/equals x y)
        (let [x-value (apply mat/mget m x)
              y-value (apply mat/mget m y)]
              (- x-value y-value))
        0.0))

    (defn J-mat [puzzle r center]
      (mat/esum
          (for [x (make-neighborhood center r)]
            (let [ct (counter-term center x r)
                  d (delta-mat center x puzzle)]
              (term ct d)))))

    (def test-matrix [[11 12 13 14 15 16 17 18]
                      [21 22 23 24 25 26 27 28]
                      [31 32 33 34 35 36 37 38]
                      [41 42 43 44 45 46 47 48]
                      [51 52 53 54 55 56 57 58]
                      [61 62 63 64 65 66 67 68]
                      [71 72 73 74 75 76 77 78]
                      [81 82 83 84 85 86 87 88]])

    (map (fn [[x y]] (mat/mget test-matrix x y)) (make-neighborhood [3 1] 3/2))
    (map (fn [[x y]] [(int x) (int y)]) (make-neighborhood [4 4] 3/2))
    (map #(Math/ceil %) (matop/- [4 4] 3/2))
    (map #(Math/floor %) (matop/+ [4 4] 3/2))
    (mat/mget test-matrix 4 4)

    (def candidates (filter #(<= (:score %) 30.0) (scores filtered-puzzle)))

    (defn inside [x y x0 y0 r]
      (<= (+ (* (- x x0) (- x x0))
         (* (- y y0) (- y y0)))
          (* r r)))

    (for [y (range 16) x (range 16)]
      (do
          (if (inside x y 7 7 4)
            (print 1)
            (print 0))
          (if (== (mod x 16) 15)
            (print "\n")
            (print " ")))

    (def perfect-circle
      [[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
      [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
      [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
      [0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0]
      [0 0 0 0 0 1 1 1 1 1 0 0 0 0 0 0]
      [0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0]
      [0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0]
      [0 0 0 1 1 1 1 1 1 1 1 1 0 0 0 0]
      [0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0]
      [0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0]
      [0 0 0 0 0 1 1 1 1 1 0 0 0 0 0 0]
      [0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0]
      [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
      [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
      [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
      [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]])

    (delta-mat [7 7] [0 0] perfect-circle)
    (delta-mat [7 7] [3 3] perfect-circle)
    (counter-term [7 7] [3 3] 4)
    (term 1 1)
    (J-mat perfect-circle 4 [7 7])

    (def noisy-circle
      [[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
      [0 0 0 1 1 0 0 0 0 0 0 1 0 0 0 0]
      [0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
      [0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0]
      [0 0 0 0 0 1 1 1 1 1 0 0 1 0 0 0]
      [0 0 1 0 1 1 1 1 1 1 1 0 0 0 0 0]
      [0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0]
      [0 0 0 1 1 1 1 1 1 1 1 1 0 0 0 0]
      [0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0]
      [0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0]
      [0 0 1 0 0 1 1 1 1 1 0 0 0 0 0 0]
      [0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0]
      [0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0]
      [0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0]
      [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
      [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]])

    (J-mat noisy-circle 4 [7 7])

    (clojure.pprint/pprint
      (partition 10
        (map (fn [[x y]] (mat/mget perfect-circle x y)) (make-neighborhood [7 7] 5)))))

    (closeness [0.0 0.0 1.0] [0.0 0.0 1.0])

    (defn frm-save
       "Save a clojure form to file."
         [#^java.io.File file form]
           (with-open [w (java.io.FileWriter. file)]
                 (binding [*out* w *print-dup* true] (prn form))))

    (defn frm-load
        "Load a clojure form from file."
          [#^java.io.File file]
            (with-open [r (java.io.PushbackReader.
                                 (java.io.FileReader. file))]
                   (let [rec (read r)]
                           rec))))

