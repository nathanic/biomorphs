(ns biomorphs.genetics
  (:require [biomorphs.utils :refer [log]]
            [goog.math :refer [clamp angleDx angleDy lerp]]
            ))

;; TERMINOLOGY
;; - gene: a particular [numeric] value that is expressed somehow in the
;; appearance of the resulting creature
;; - genotype: a description of all the genes; a vector of gene definitions
;; - gene definition (gdef): a hash describing the properties of a gene
;; - genome: a vector of gene values, one for each gene in the genotype
;; - creature: a seq of line segment descriptors representing the phenotype

(def BIOMORPH-COUNT 9)
(def CHILD-COUNT (dec BIOMORPH-COUNT))
;; (def MUTATION-RATE 0.10)
(def MUTATION-RATE 0.05)
(def BASE-BRANCH-LEN 30)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Genotype Definition & Gene Manipulation

; this genotype was originally inspired by http://www.annanardella.it/biomorph.html
(def GENOTYPE
  [{:name    :angle-front
    :index    0
    :default  45
    :min      0
    :max      360
    ; angle of :ne/:nw strokes
    }
   {:name    :angle-rear
    :index    1
    :default  45
    :min      0
    :max      360
    ; angle of :se/sw strokes
    }
   {:name    :elongation-front
    :index   2
    :default 1.0
    :min     -9.0
    :max     9.0
    ; length factor for :ne/:nw strokes
    }
   {:name    :elongation-rear
    :index   3
    :default 1.0
    :min     -9.0
    :max     9.0
    ; length factor for :se/sw strokes
    }
   {:name    :expansion-x
    :index   4
    :default 1.0
    :min     -9.0
    :max     9.0
    ; general width scale factor
    }
   {:name    :expansion-y
    :index   5
    :default 1.0
    :min     -9.0
    :max     9.0
    ; general height scale factor
    }
   {:name    :iterations
    :index   6
    :default 2
    :min     1
    :max     10
    ;; :type    :int
    :mutation-rate 0.2
    ; count of branchings, where 0 is Y-shaped
    }
   {:name    :gradient
    :index   7
    :default 1.0
    :min     0
    :max     1.5
    ; length factor applied with each successive iteration
    }
   {:name    :color
    :index   8
    :default 180
    :min     0
    :max     360
    ; hue of creature
    }
   ; maybe also
   ; :segments
   ; :segment-gradient
   ; :symmetry
   ; :drawing-primitive
   ; dots or other shapes at some or all vertices?
   ; perhaps only origin, or only terminals, or anywhere
   ]
  )

; lookup table in an attempt to optimize speed
(def GENOTYPE-LUT
  (reduce (fn [lut gene]
            (assoc lut (:name gene) gene))
          GENOTYPE))

(comment
  ; could build :index automatically...
  ; but it's kind of nice to have it explicit for reference
  (map-indexed
    (fn [idx gdef]
      (assoc gdef :index idx))
    GENOTYPE)
  )

(defn get-gene-def [gene-name]
  (get GENOTYPE-LUT gene-name))

(defn get-gene [genome gene-name]
  (get genome (:index (get GENOTYPE-LUT gene-name))))

(defn get-gene-index [gene-name]
  (:index (get GENOTYPE-LUT gene-name)))

(defn default-genome []
  (mapv :default GENOTYPE))

(defn random-gene-value [{:keys [min max]}]
  (+ min (rand (- max min))))

(defn random-genome []
  (mapv random-gene-value GENOTYPE)
  )

(defn valid-genome? [genome]
  (and (= (count genome) (count GENOTYPE))
       (every? number? genome)))

(comment
  (default-genome)
  (random-genome)
  (in-ns 'biomorphs.genetics)
  (def g (default-genome))
  (bench 1000000 #(get-gene g :color))
  ;=> 0.688 ns
  (bench 1000000 #(get-genetic-angle g :se))
  ;=> 3.57 ns

  (bench 1000000 #(get-genetic-expansion g))
  ;=> 1.397 ns

  (bench 1000000 #(get-genetic-gradient g 5))
  ;=> 1.467 ns
  )

(comment
  (defn fixup-gene [gdef gval]
    (let [fixer (case (:type gdef)
                  :int  Math/floor
                  identity
                  )]
      (fixer (clamp gval (:min gdef) (:max gdef)))))

  (defn fixup-genome [genome]
    (map fixup-gene
         GENOTYPE
         genome))
  )

(defn mutate-gene
  "given a gene definition and gene value, randomly increase or decrease it
  based on its mutation rate (or the global default MUTATION-RATE)"
  [{:keys [min max mutation-rate]} gval]
  (let [variance  (* (- max min)
                     (or mutation-rate MUTATION-RATE)
                     (rand-nth [-1 1])) ]
    (clamp (+ gval variance) min max)))

; pick a random gene and give it a nudge
; this does not ensure uniqueness...
; could pull from a lazy seq into a set
; until it's of a certain size...
; could deterministically vary the genes...
(defn mutate-genome
  [genome]
  (let [idx (rand-int (count genome)) ]
    (assoc genome
           idx
           (mutate-gene (get GENOTYPE idx)
                        (get genome idx)))))

(defn update-gene
  "produce an updated version of a genome via updater function
  f :: gene -> gene"
  [genome gene-name f]
  (assoc genome
         (get-gene-index gene-name)
         (f (get-gene genome gene-name))))


(defn make-children [parent-genome]
  (for [_ (range CHILD-COUNT)]
    (mutate-genome parent-genome)))

; lame way to ensure uniqueness...
; hopefully i'll think of something better later.
(defn make-children-unique [parent-genome]
  (loop [children #{}]
    (if (< (count children) CHILD-COUNT)
      (recur (conj children (mutate-genome parent-genome)))
      (vec children))))


(defn- interpolate-gene
  [coeff a b]
  (lerp a b coeff)
  )

(defn interpolate-genomes
  "produce an interpolated intermediate genome between the given genomes
  where coeff=0 means you get purely genome-a, and 1.0 gives you genome-b"
  [genome-a genome-b coeff]
  {:pre [(<= 0.0 coeff) (<= coeff 1.0)]}
  (mapv (partial interpolate-gene coeff) genome-a genome-b))


; seems to work but could use some tweaking
; there is apparently some gene it spends time on that doesn't produce much visible change
(defn interpolate-genomes-gradual
  "produce an interpolated intermediate genome between the given genomes
  where coeff=0 means you get purely genome-a, and 1.0 gives you genome-b"
  [genome-a genome-b coeff]
  {:pre [(<= 0.0 coeff) (<= coeff 1.0)]}
  (let [num-genes (count GENOTYPE)
        prod      (* coeff num-genes)
        coeff-idx (Math/floor prod)
        sub-coeff (- prod coeff-idx) ]
    (loop [i 0, out []]
      (cond
        (>= i num-genes)
        out,
        (< i coeff-idx)
        (recur (inc i)
               (conj out (get genome-b i))),
        (= i coeff-idx)
        (recur (inc i)
               (conj out
                     (interpolate-gene sub-coeff
                                       (get genome-a i)
                                       (get genome-b i)))),
        :else
        (recur (inc i)
               (conj out (get genome-a i)))
        ))))


(comment
  (def coeff 0.6)
  (def g-idx (Math/floor (* coeff (count GENOTYPE))))
  (def prod (* coeff (count GENOTYPE)))
  (def sub-coeff (- prod (Math/floor prod)))


  (defn interpolations
    "returns a lazy seq of n interpolated genomes,
    varying linearly from genome-a to genome-b"
    [n genome-a genome-b]
    (for [x (range (inc n))]
      (interpolate-genomes genome-a genome-b (/ x n))))

  (interpolate-gene 1 10 20)
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Phenotypic Properties

(defn get-genetic-iterations [genome]
  (inc (Math/round (get-gene genome :iterations))))

(defn get-genetic-gradient
  "get the cumulative gradient factor for a branch at this depth"
  [genome depth]
  ; TODO: probably needs adjustment
  ; 0 iterations -> still one branching
  (Math/pow (get-gene genome :gradient) depth))


; this could stand to be a bit more dramatic
(defn get-genetic-color
  "the color will be the hue gene, with lightness and alpha affected by the gradient gene, returns an hsla 4-vector"
  [genome depth]
  (let [grad (get-genetic-gradient genome depth)]
    [(* grad (get-gene genome :hue)) (* 1.0) (* grad 0.5) (* 1.0)]
    ))

; TODO? normalize angle to [0,360)
(defn get-genetic-angle
  "consult the genome to calculate an angle in degrees for the given nominal direction"
  [genome dir]
  (case dir
    :n  90
    :ne (-  90 (get-gene genome :angle-front))
    :e  0
    :se (+ 270 (get-gene genome :angle-rear))
    :s  270
    :sw (- 270 (get-gene genome :angle-rear))
    :w  180
    :nw (+  90 (get-gene genome :angle-front))
    ))

(defn get-genetic-elongation
  "consult the genome to find the elongation factor for this direction"
  [genome dir]
  (case dir
    :ne (get-gene genome :elongation-front)
    :nw (get-gene genome :elongation-front)
    :se (get-gene genome :elongation-rear)
    :sw (get-gene genome :elongation-rear)
    1.0))

(defn get-genetic-expansion
  [genome]
  [(get-gene genome :expansion-x)
   (get-gene genome :expansion-y)])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Creature Morphology

; a little symbolic navigation graph
(def DIRECTIONS
    {:n   {:left :nw, :right :ne},
     :ne  {:left :n,  :right :e},
     :e   {:left :ne, :right :se},
     :se  {:left :e,  :right :s},
     :s   {:left :se, :right :sw},
     :sw  {:left :s,  :right :w}
     :w   {:left :sw, :right :nw},
     :nw  {:left :w,  :right :n},
     })

(defn- turn-direction
  "given an initial direction, turn :left or :right by 45 degrees
  (turn-direction :ne :right) => :e "
  [dir leftright]
  (-> DIRECTIONS (get dir) (get leftright)))


(defn- calc-branch-vector
  "reckon the vector required to make a branch in direction `dir`,
  after `depth` iterations"
  [genome depth dir]
  (let [angle   (get-genetic-angle genome dir)
        elong   (get-genetic-elongation genome dir)
        grad    (get-genetic-gradient genome depth)
        len     (* elong grad BASE-BRANCH-LEN)
        [ex ey] (get-genetic-expansion genome) ]
    [(* ex (angleDx angle len))
     (* ey (angleDy angle len))]))


; we easily hit creatures too big to fit in their cells
; the nardella version seems to compensate the scale genes to try to keep things fitting

; maybe call it subtree-seq
(defn- stream-subtree [genome [x y] dir depth]
  (when (< depth (get-genetic-iterations genome))
    (let [[dx dy] (calc-branch-vector genome depth dir)
          [x' y'] [(+ x dx) (+ y dy)]
          segment {:x0 x, :y0 y, :x1 x', :y1 y',
                   :color (get-genetic-color genome depth)
                   } ]
      (cons segment
            (concat (lazy-seq (stream-subtree genome [x' y']
                                              (turn-direction dir :left) (inc depth)))
                    (lazy-seq (stream-subtree genome [x' y']
                                              (turn-direction dir :right) (inc depth))))))))

; i'm thinking i should look to stream-subtree for speed optimization
; maybe do it without seqy business



; maybe call it creature-seq
(defn stream-creature [genome]
  (stream-subtree genome [0 0] :n 0))

(comment
  (in-ns 'biomorphs.genetics)
  (stream-creature [45 45 1 1 1 1 3 0.9 180])
  )


(defn- extreme-coords
  [[xmin ymin, xmax ymax] {:keys [x0 y0, x1 y1]}]
  [(if xmin (min xmin x0 x1) (min x0 x1))
   (if ymin (min ymin y0 y1) (min y0 y1))
   (if xmax (max xmax x0 x1) (max x0 x1))
   (if ymax (max ymax y0 y1) (max y0 y1))
    ])
;; NB: min and max seem to treat nil==0
;; profiling on chrome showed the above to be faster
;; than the arguably more elegant (min (or xmin x0) x0 x1)

(defn measure-creature [creature]
  (reduce extreme-coords [nil nil nil nil] creature))

; NB these coords are in creature-space
(defn creature-centroid [creature]
  (let [[x0 y0, x1 y1] (measure-creature creature)]
    [(* 0.5 (+ x0 x1))
     (* 0.5 (+ y0 y1))]))

(comment

  (def critter [ {:x0 -10, :y0 5, :x1 10, :y1 15} ])
  (def critter (doall (stream-creature [65 25 1 1 1 1 9 0.9 180])))
  (count critter) ; 1023 segments

  (def bench biomorphs.utils/bench)
  (measure-creature critter)
  (:mean (bench 1000 #(creature-centroid critter)))
  ;=> 6.7 ms
  (:mean (bench 1000 #(creature-centroid (stream-creature [65 25 1 1 1 1 9 0.9 180]))))
  ;=> 65 ms

  ; simple reduction for lower bound on speed
  (:mean (bench 1000 (fn [] (reduce #(inc %) 0 critter))))
  ;=> 0.832 ms

  )


; first make it possible
; then make it pretty
; then make it fast (if necessary)

(defn scale-genome-from-dims
  "generate a version of the supplied genome that would scale to
  dimensions [dest-w dest-h], given its already-calculated dimensions"
  [genome creature-w creature-h dest-w dest-h]
  (-> genome
      (update-gene :expansion-x #(* % (/ dest-w creature-w)))
      (update-gene :expansion-y #(* % (/ dest-h creature-h)))
      ))

; more expensive version
(defn scale-genome-to-fit
  "generate a genome related to the given genome that has scale genes set to
  fit within the desired area."
  [genome w h]
  (let [creature      (stream-creature genome)
        [x0 y0 x1 y1] (measure-creature creature)
        cx            (- x1 x0)
        cy            (- y1 y0)
        scalex        (/ w cx)
        scaley        (/ h cy) ]
    (scale-genome-from-dims genome (- x1 x0) (- y1 y0) w h)))


(comment
  (measure-creature (stream-creature (default-genome)))
  ; [-51.21320343559642 0 51.21320343559643 81.21320343559643]
  (-> (default-genome)
      (scale-genome-to-fit 40 40)
      stream-creature
      measure-creature
      )
  ; [-20 0 20 40]
  )

(defn- extreme-coords'
  [[xmin ymin, xmax ymax] [x0 y0, x1 y1]]
  [(if xmin (min xmin x0 x1) (min x0 x1))
   (if ymin (min ymin y0 y1) (min y0 y1))
   (if xmax (max xmax x0 x1) (max x0 x1))
   (if ymax (max ymax y0 y1) (max y0 y1))
    ])

(defn- measure-subtree [extrema genome [x y] dir depth]
  (if (< depth (get-genetic-iterations genome))
    (let [[dx dy] (calc-branch-vector genome depth dir)
          [x' y'] [(+ x dx) (+ y dy)]
          ]
      (-> extrema
          (measure-subtree genome [x' y'] (turn-direction dir :left) (inc depth))
          (measure-subtree genome [x' y'] (turn-direction dir :right) (inc depth))
          (extreme-coords' [x y x' y'])))
    extrema))


(defn measure-creature-alt [genome]
  (measure-subtree [nil nil nil nil] genome [0 0] :n 0))

(comment
  (= (measure-creature (stream-creature (default-genome)))
     (measure-creature-alt (default-genome)))

  (def g (default-genome))
  (def gc (doall (stream-creature g)))
  (count gc)

  ; less trivial creature with 2047 body elements
  (def g [103.2341,248.7688,4.2983,-2.6653,4.3936,0.4085,10.0000,0.9389,50.2353])

  ; 11 ns for less trivial
  (bench 1000 #(measure-creature gc))
  ; 55 ns for less trivial
  (bench 1000 #(measure-creature-alt g))

  )
