(ns biomorphs.genetics
  (:require [biomorphs.utils :refer [index-of index-of-by]]
            [goog.math :refer [clamp angleDx angleDy]]
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

(def BIOMORPH-COUNT 9)
(def CHILD-COUNT (dec BIOMORPH-COUNT))
(def MUTATION-RATE 0.05)
(def BASE-BRANCH-LEN 30)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Genotype Definition & Gene Manipulation

; this genotype is lifted from http://www.annanardella.it/biomorph.html
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
   ]
  )

(defn default-genome []
  (mapv :default GENOTYPE))

(defn random-gene-value [{:keys [min max]}]
  (+ min (rand (- max min))))

(defn random-genome []
  (mapv random-gene-value GENOTYPE)
  )

(comment
  (default-genome)
  (random-genome)
  )

; TODO: consider improving performance by not doing O(n) lookups
; could scan GENOTYPE and build a hash (gene-name -> index)
; (but oh well, n is always small here)
(defn get-gene-index
  "gene-name -> ordinal position in genome vector, else nil"
  [gene-name]
  (index-of-by (fn [{:keys [name]}]
                 (= name gene-name))
               GENOTYPE))

(defn get-gene-def
  "gene-name -> gdef, else nil"
  [gene-name]
  (get GENOTYPE (get-gene-index gene-name)))

(defn get-gene
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


(defn clamp-genome [genome]
  (map (fn [{:keys [min max]} gene]
         (clamp gene min max))
       genome
       GENOTYPE))

(defn mutate-gene
  "given a gene definition and gene value, randomly increase or decrease it
  based on the mutation rate"
  [{:keys [min max]} gval]
  (let [variance  (* (- max min)
                     MUTATION-RATE
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
(defn get-genome-iterations [genome]
  (get-gene genome :iterations))

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

; ordering of directions we walk through
(def DIRECTIONS [:n :ne :e :se :s :sw :w :nw])

; move to the next or previous nominal direction based on :left or :right
; that is, a 45 degree movement like :n -> ne
(defn- turn-direction [dir leftright]
  (let [idx (index-of DIRECTIONS dir)
        op  (if (= leftright :left) dec inc) ]
    (if idx
      (nth DIRECTIONS (mod (op idx) (count DIRECTIONS)))
      (throw (js/Error. (str dir " is not a valid direction.")))
      )))


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

(defn get-genetic-gradient
  "get the cumulative gradient factor for a branch at this depth"
  [genome depth-remain]
  ; TODO: probably needs adjustment
  ; because we are really passing in depth-remain
  ; and 0 -> one branching still
  (Math/pow (get-gene genome :gradient)
            (- (get-genome-iterations genome) depth-remain)))

(defn calc-branch-vector
  "reckon the vector required to make a branch in direction `dir`,
  after `depth` iterations"
  [genome depth dir]
  (let [angle (get-genetic-angle genome dir)
        elong (get-genetic-elongation genome dir)
        grad  (get-genetic-gradient genome depth)
        len   (* elong grad BASE-BRANCH-LEN) ]
    [(angleDx angle len)
     (angleDy angle len)]
    ))

; apply expansion here or just scale the canvas while drawing?


