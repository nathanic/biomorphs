(ns biomorphs.genetics
  (:require [biomorphs.utils :refer [index-of index-of-by]]
            [goog.math :refer [clamp angleDx angleDy lerp]]
            )
  )

; logic for defining and manipulating genomes
; and calculating their corresponding phenotypes
; (but no canvas/GUI code)

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

; this genotype is lifted from http://www.annanardella.it/biomorph.html
; everything is implicitly :type :double unless specified otherwise
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
    :min     0
    :max     9
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
  (map-indexed
    (fn [idx gdef]
      (assoc gdef :index idx))
    GENOTYPE)
  )

(defn get-gene-def [gene-name]
  (get GENOTYPE-LUT gene-name))

(defn get-gene [genome gene-name]
  (get genome (:index (get GENOTYPE-LUT gene-name))))

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
  )

; TODO: consider improving performance by not doing O(n) lookups
; could scan GENOTYPE and build a hash (gene-name -> index)
; (but oh well, n is always small here)
; UPDATE: profiling suggests we spend a lot of time doing this
; so it'd be worth speeding up
; we don't *really* need a concept of ordinality with this new genome
; could just do it as hash-maps from name -> gdef and name -> gene
; i *think* small hash-maps are implemented as JS objects
; though we might need to define an order for succinct url hashes
(defn get-gene-index
  "gene-name -> ordinal position in genome vector, else nil"
  [gene-name]
  (index-of-by (fn [{:keys [name]}]
                 (= name gene-name))
               GENOTYPE))

#_(defn get-gene-def
  "gene-name -> gdef, else nil"
  [gene-name]
  (get GENOTYPE (get-gene-index gene-name)))

#_(defn get-gene
  "retrieve a gene value from the supplied genome given a gene name.
  returns nil if not found."
  [genome gene-name]
  (get genome (get-gene-index gene-name)))


(comment
  (get-gene (default-genome) :color)
  (get-gene (default-genome) :iterations)
  (get-gene (default-genome) :angle-front)
  (get-gene (default-genome) :elongation-front)

  (index-of-by odd? [2 4 6 8 9 4])
  )


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


(defn make-children [parent-genome]
  (for [_ (range CHILD-COUNT)]
    (mutate-genome parent-genome)))

; lame way to ensure uniqueness...
(defn make-children-unique [parent-genome]
  (loop [children #{}]
    (if (< (count children) CHILD-COUNT)
      (recur (conj children (mutate-genome parent-genome)))
      (vec children))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Phenotype Expression Support

; so other modules don't have to care about gene names
; also: rounding gets around an infinite loop bug
(defn get-genome-iterations [genome]
  (Math/round (get-gene genome :iterations)))

(defn get-genetic-gradient
  "get the cumulative gradient factor for a branch at this depth"
  [genome depth-remain]
  ; TODO: probably needs adjustment
  ; 0 iterations -> still one branching
  ; also i don't like writing in terms of depth-remain...
  (Math/pow (get-gene genome :gradient)
            (- (get-genome-iterations genome) depth-remain)))


(defn color-for-depth
  "calculate the color of this branch based on both the genome and the branch depth"
  [genome depth]
  ; TODO impl this for real
  ; that is, keeping the creature's hue,
  ; but reduce lightness or value or something
  (nth (cycle [[10 10 240]
               [200 100 10]
               [10 200 10]
               [200 50 50]
               [200 200 10]
               ])
       depth))

; (in-ns 'biomorphs.genetics)
; this isn't coming out dramatic enough
(defn color-for-depth'
  "the color will be the hue gene, with lightness and alpha affected by the gradient gene"
  [genome depth-remain]
  (let [grad (get-genetic-gradient genome depth-remain)]
    [(* grad (get-gene genome :hue)) (* 1.0) (* grad 0.5) (* 1.0)]
    ))

; ordering of directions we walk through
(def DIRECTIONS [:n :ne :e :se :s :sw :w :nw])
; XXX we end up linearly searching this a lot
; is there a way we can make that faster?
; could programmatically build a JS object like
; directions.ne = { left: 'n', :right :'e' } etc.
; or we could build a JS array and just linearly search it
; might be worth benchmarking

(comment

  (defn maps->js
    "transform nested hashmaps into nested js objects"
    [m]
    (let [out (js-obj)]
      (doseq [[k v] m]
        (aset out
              (if (keyword? k) (name k) k)
              (cond
                (map? v)     (my-js-map v)
                (keyword? v) (name v)
                :else        v)))
      out))


  ; optimization attempt: build a js object for looking up directions
  (def DIRECTIONS'
    (let [dirs [:n :ne :e :se :s :sw :w :nw]
          len  (count dirs)
          out  (js-obj)
          ]
      (loop [i 0]
        (when (> len i)
          (aset out
                (name (get dirs i))
                (doto (js-obj)
                  (aset! "left"  (name (get dirs (mod (dec i) len))))
                  (aset! "right" (name (get dirs (mod (inc i) len))))))
          (recur (inc i))))
      out))


  (set! (.-f js/window)
        (doto (js-obj)
          (aset "yo" "gabba")
          (aset "gabba" "gabba")
          ))

  (defn make-dir-lut-map []
    (let [len (count DIRECTIONS)]
      (loop [i 0, m {}]
        (if (<= len i)
          m     ; exit loop
          (recur (inc i)
                 (assoc m
                        (get DIRECTIONS i)
                        {:left  (get DIRECTIONS (mod (dec i) len))
                         :right (get DIRECTIONS (mod (inc i) len))
                         }))))))

  (def d (make-dir-lut-map))
  (my-js-map d)

  (set! (.-d js/window) (my-js-map d))

  (-> (.-dirs js/window)
      (aget "se")
      (aget "right")
      )

  (defn turn-direction [dir leftright]
    (-> (.-dirs js/window)
        (aget (name dir))
        (aget (name leftright))
        keyword
        ))

)

(comment
  ; dang this is so much simpler than what i had been doing
  (def hashdirs
    {:n   {:left :nw, :right :ne},
     :ne  {:left :n,  :right :e},
     :e   {:left :ne, :right :se},
     :se  {:left :e,  :right :s},
     :s   {:left :se, :right :sw},
     :sw  {:left :s,  :right :w}
     :w   {:left :sw, :right :nw},
     :nw  {:left :w,  :right :n},
     })

  (defn turn-direction-hash
    [dir leftright]
    (get-in hashdirs [dir leftright]))

  ; let's try this instead of get-in
  ; (which requires building a vector)
  (defn turn-direction-hash2
    [dir leftright]
    (-> hashdirs (get dir) (get leftright)))

  (def jsdirs
    (maps->js
      {:n   {:left :nw, :right :ne},
       :ne  {:left :n,  :right :e},
       :e   {:left :ne, :right :se},
       :se  {:left :e,  :right :s},
       :s   {:left :se, :right :sw},
       :sw  {:left :s,  :right :w}
       :w   {:left :sw, :right :nw},
       :nw  {:left :w,  :right :n},
       }))


(defn turn-direction-js
  [dir leftright]
  (-> jsdirs
      (aget (name dir))
      (aget (name leftright))
      keyword
      ))


(def DIRECTIONS-old [:n :ne :e :se :s :sw :w :nw])
(defn- turn-direction-old [dir leftright]
  (let [idx (index-of DIRECTIONS-old dir)
        op  (if (= leftright :left) dec inc) ]
    (if idx
      (get DIRECTIONS-old (mod (op idx) (count DIRECTIONS-old)))
      (throw (js/Error. (str dir " is not a valid direction.")))
      )))

  (bench 1000000 (fn [] (turn-direction-hash :s :left)))
  ; mean 3.796 ns
  (bench 1000000 (fn [] (turn-direction-js :s :left)))
  ; mean 5.83 ns
  (bench 1000000 (fn [] (turn-direction :s :left)))
  ; mean 5.966 ns

  (bench 1000000 (fn [] (turn-direction-hash2 :s :left)))
  ; mean 0.596 ns!  holy crap

  ; very surprised the JS version is not much faster than the old linear search one
  ; seems like hash maps is the way to go



)

(comment
  (defn bench [iters f]
    (let [start   (.getTime (js/Date.))
          result  (f)
          _       (dotimes [_ (dec iters)] (f))
          end     (.getTime (js/Date.))
          elapsed (- end start)]
      {:result  result,
       :elapsed elapsed,
       :mean    (/ elapsed iters)
       }))
  )

; (in-ns 'biomorphs.genetics)
; move to the next or previous nominal direction based on :left or :right
; that is, a 45 degree movement like :n -> ne
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
(defn turn-direction
    [dir leftright]
    (-> DIRECTIONS (get dir) (get leftright)))


; i guess cljs doesn't have this?
(defn double [x] x)

#_(defn calc-branch-vector [genome depth dir]
  (let [max-branch-len  200 ;; (Math/floor (/ CXCELL 2 8))
        [lx ly]         (length-genes-for-depth genome depth)
        [dx dy]         (DIR-VECTORS dir) ]

    ; TODO REWRITE ME
    (mapv #(Math/floor (double %))
          [(* lx dx (/ max-branch-len MAX-GENE ))
           (* ly dy (/ max-branch-len MAX-GENE )) ])
    ;; [(* dx 100) (* dy 100)]
    ))

; TODO? normalize angle to [0,360)
; TODO: adjust for the fact that 90 degrees is actually downward in canvas coords
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
  "consult the geneome to find the elongation factor for this direction"
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

(defn calc-branch-vector
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

; apply expansion here or just scale the canvas while drawing?
; what *is* expansion?  just a coefficients on the branch vector components?

; also re: scaling, we easily hit creatures too big to fit in their cells
; the nardella version seems to compensate the scale genes to try to keep things fitting
; we could basically walk through the drawing algorithm but only save the extrema points...
; UPDATE: doing measurement now, can definitely do this

; so do we follow nardella and dynamically cap the expansion genes?

; might be interesting to change the drawing algorithm such that the genetics module
; generates a lazy seq of line segments, and the drawing algorithm just interprets that...

; maybe call it subtree-seq
(defn stream-subtree [genome [x y] dir depth-remain]
  (when (pos? depth-remain)
    (let [[dx dy] (calc-branch-vector genome depth-remain dir)
          [x' y'] [(+ x dx) (+ y dy)]
          segment {:x0 x, :y0 y, :x1 x', :y1 y',
                   :color (color-for-depth' genome depth-remain)
                   }
          ]
      (cons segment
            (concat (lazy-seq (stream-subtree genome [x' y'] (turn-direction dir :left) (dec depth-remain)))
                    (lazy-seq (stream-subtree genome [x' y'] (turn-direction dir :right) (dec depth-remain))))))))

; maybe creature-seq
(defn stream-creature [genome]
  (stream-subtree genome [0 0] :n (inc (get-genome-iterations genome))))

(comment
  (in-ns 'biomorphs.genetics)
  (stream-creature [45 45 1 1 1 1 3 0.9 180])

  )


; okay, armed with our streaming creature description,
; let's see how hard it is to measure it
; it's a monoid if you squint (ignoring map vs vector)
(defn- extreme-coords
  [[xmin ymin, xmax ymax] {:keys [x0 y0, x1 y1]}]
  [(min (or xmin x0) x0 x1)
   (min (or ymin y0) y0 y1)
   (max (or xmax x0) x0 x1)
   (max (or ymax y0) y0 y1) ])
;; NB: min and max seem to treat nil==0

(defn measure-creature [creature]
  (reduce extreme-coords [nil nil nil nil] creature))

; NB these coords are in creature-space
(defn creature-centroid [creature]
  (let [[x0 y0, x1 y1] (measure-creature creature)]
    [(* 0.5 (+ x0 x1))
     (* 0.5 (+ y0 y1))]))

(comment

  (def critter [ {:x0 -10, :y0 5, :x1 10, :y1 15} ])

  (measure-creature critter)
  (creature-centroid critter)
  )


(defn interpolate-gene
  [coeff a b]
  (lerp a b coeff)
  )

(defn interpolate-genomes
  "produce an interpolated intermediate genome between the given genomes
  where coeff=0 means you get purely genome-a, and 1.0 gives you genome-b"
  [genome-a genome-b coeff]
  {:pre [(<= 0.0 coeff) (<= coeff 1.0)]}
  (mapv (partial interpolate-gene coeff) genome-a genome-b))

(defn interpolations
  "returns a lazy seq of n interpolated genomes,
  varying linearly from genome-a to genome-b"
  [n genome-a genome-b]
  (for [x (range (inc n))]
    (interpolate-genomes genome-a genome-b (/ x n))))

(comment
  (interpolate-gene 1 10 20)
  )
; what about three.js and webgl?
