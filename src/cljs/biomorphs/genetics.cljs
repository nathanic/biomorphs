(ns biomorphs.genetics)

; logic for defining and manipulating genomes
; and calculating their corresponding phenotypes
; but no canvas/GUI code

; might not need most of these anymore
;; (def CX 1024)
;; (def CY 1024)
(def BIOMORPH-COUNT 9)
(def CHILD-COUNT (dec BIOMORPH-COUNT))
;; (def CXCELL (int (/ CX BIOMORPH-COUNT)))
;; (def CYCELL (int (/ CY BIOMORPH-COUNT)))

(def MIN-GENE 0)
(def MAX-GENE 19)
(def MAX-DEPTH 10)

; the order of directions we cycle through
(def directions [:n :ne :e :se :s :sw :w :nw])
; and the vectors they represent
(def dir-vectors
  {:n  [ 0 -1]
   :ne [ 1 -1]
   :e  [ 1  0]
   :se [ 1  1]
   :s  [ 0  1]
   :sw [-1  1]
   :w  [-1  0]
   :nw [-1 -1]
   })

;; there's probably a better way to do this
;; but i was relying on a JVMism in my previous impl
(defn index-of [coll item]
  (loop [idx 0, c coll]
    (if-not (empty? c)
      (if (= item (first c))
        idx
        (recur (inc idx) (rest c)))
      -1)))

(comment
  (index-of [:a :b :c] :b)
  (index-of [:a :b :c] :c)
  (index-of [:a :b :c] :farts)
  )
; make a 45 degree right turn
; based on :left or :right
(defn turn-direction [dir leftright]
  (let [idx (index-of directions dir)
        op  (if (= leftright :left) dec inc)
        ]
    (if-not (= idx -1)
      (nth directions (mod (op idx) (count directions)))
      (throw (js/Error. (str dir " is not a valid direction.")))
      )))

(comment
  (turn-direction :n :right)
  (turn-direction :n :left)
  (turn-direction :e :right)
  (turn-direction :e :left)
  (turn-direction "farts" :left)
  )

;; he uses the following genome
;; newgenes = [];
;; newgenes[0] = rand(); // depth
;; newgenes[1] = rand(); // x-scale levels 3, 6, 9
;; newgenes[2] = rand(); // x-scale levels 1, 4, 7
;; newgenes[3] = rand(); // x-scale levels 2, 5, 8
;; newgenes[4] = rand(); // y-scale levels 3, 6, 9
;; newgenes[5] = rand(); // y-scale levels 1, 4, 7
;; newgenes[6] = rand(); // y-scale levels 2, 5, 8
;; newgenes[7] = rand(); // red
;; newgenes[8] = rand(); // green
;; newgenes[9] = rand(); // blue
;; newgenes[10] = rand(); // width

(def GENES
  [:depth
   :xscale369
   :xscale147
   :xscale258
   :yscale369
   :yscale147
   :yscale258
   :red           ; what about hue here instead?
   :green
   :blue
   :width
   ])
; other ideas:
; segmentation,
; segment gradient,
; symmetries
; drawing primitive (line, ellipse, rect, filled/empty variants)
;
; see also http://www.annanardella.it/biomorph.html
; http://www.phy.syr.edu/courses/mirror/biomorph/

(defn gene-index [gene-id]
  (let [idx (index-of GENES gene-id)]
    (if-not (= -1 idx)
      idx
      (throw (js/Error. (str gene-id " is not a valid gene id!")))
      )))

(defn get-gene [genome gene-id]
  (get genome (gene-index gene-id)))

(defn random-genome []
  (vec (repeatedly (count GENES) #(+ MIN-GENE (rand-int MAX-GENE)))))

(comment
  (random-genome)
  )

(defn length-genes-for-depth
  "returns a pair of gene values coding for length at the specified depth"
  [genome depth]
  (let [xidx (+ (gene-index :xscale369) (mod depth 3))]
    [(get genome xidx)
     (get genome (+ 3 xidx))]))

(defn color-for-depth
  "calculate the color of this branch based on both the genome and the branch depth"
  [genome depth]
  ; TODO impl this for real
  (nth (cycle [[10 10 240]
               [200 100 10]
               [10 200 10]
               [200 50 50]
               [200 200 10]
               ])
       depth))

(comment
  (def mygenome [0 1 2 3 4 5 6 7 8 9 10])
  (def mygenome [1 1 1 1 1 1 1 1 1 1 1])
  ; example taken from the javascript debugger
  (def mygenome [0, 3, 20, 15, 4, 1, 16, 17, 7, 17, 3])
  (get-gene mygenome :depth)

  (map #(length-genes-for-depth mygenome %1) (range 10))
  )


; i guess cljs doesn't have this
(defn double [x] x)

(defn genome-depth [genome]
  (mod (int (get-gene genome :depth)) MAX-DEPTH))


(defn calc-branch-vector [genome depth dir]
  (let [max-branch-len  200 ;; (Math/floor (/ CXCELL 2 8))
        [lx ly]         (length-genes-for-depth genome depth)
        [dx dy]         (dir-vectors dir) ]
    ;; var i = x + Math.floor( xoffset*( genes[xGene] * 2579 ) % maxSegmentLen );
    ;; var j = y + Math.floor( yoffset*( genes[yGene] * 5051 ) % maxSegmentLen );
    ;; [(Math/floor (mod (* lx dx 2579) max-branch-len))
    ;;  (Math/floor (mod (* ly dy 5051) max-branch-len))]

    (mapv #(Math/floor (double %))
          [(* lx dx (/ max-branch-len MAX-GENE ))
           (* ly dy (/ max-branch-len MAX-GENE )) ])
    ;; [(* dx 100) (* dy 100)]
    ))


; ensure a gene does not go out of bounds
; (or should genes wrap around?)
(defn saturate-gene [gene]
  (max MIN-GENE (min MAX-GENE gene)))

; pick a random gene and give it +/- 1
; this does not ensure uniqueness...
; could pull from a lazy seq into a set
; until it's of a certain size...
; could deterministically vary the genes...
(defn mutate-genome
  [genome]
  (let [idx    (rand-int (count genome))
        newval (+ (get genome idx) (rand-nth [-1 1])) ]
      (assoc genome idx (saturate-gene newval))))

(defn make-children [parent-genome]
  (for [_ (range CHILD-COUNT)]
    (mutate-genome parent-genome)))

; lame way to ensure uniqueness...
(defn make-children-unique [parent-genome]
  (loop [children #{}]
    (if (< (count children) CHILD-COUNT)
      (recur (conj children (mutate-genome parent-genome)))
      (vec children))))

(comment

  (mutate-genome [1, 1 1 1, 1 1 1, 1 1 1])

  (mutate-genome [MAX-GENE, MAX-GENE MAX-GENE MAX-GENE, MAX-GENE MAX-GENE MAX-GENE, MAX-GENE MAX-GENE MAX-GENE])

  (make-children [1, 1 1 1, 1 1 1, 1 1 1])
  (make-children-unique [1, 1 1 1, 1 1 1, 1 1 1])

  )

