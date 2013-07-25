(ns biomorphs.biomorphs
  (:require [mondrian.anim :as anim]
            [mondrian.canvas :as canvas]
            [mondrian.color :as color]
            [mondrian.math :as math]
            [mondrian.plot :as plot]
            [mondrian.ui :as ui]
            [monet.canvas :as m]
            [biomorphs.genetics :as g]
            )
  (:use-macros [mondrian.macros :only [defmondrian]]))

(comment
  ;; nmap ,e va(<c-c><c-c>
  (browser-repl)
  (in-ns 'biomorphs.biomorphs)
  )



; might not need most of these anymore
(def CX 1024)
(def CY 1024)
(def BIOMORPH-COUNT 9)
(def CXCELL (/ CX BIOMORPH-COUNT))
(def CYCELL (/ CY BIOMORPH-COUNT))

(defn line' 
  "a version of line that works on upside down coordinates (like math rather than like canvas)"
  [ctx x y x' y']
  ;; (line (- CX x) (- CY y) (- CX x') (- CY y'))
  (-> ctx
      (m/save)
      ;; (m/scale 1 -1)
      (m/move-to x y)
      (m/line-to x' y')
      (m/stroke)
      (m/restore))
  ctx)

(defn draw-subtree
  [ctx [x y] genome dir depth-remain]
  (let [[bx by]   (g/calc-branch-vector genome depth-remain dir)
        [x' y']   [(+ x bx) (+ y by)]
        [r g b]     (g/color-for-depth genome depth-remain)
        ]
    (when (pos? depth-remain)
      (-> ctx
          ;; (println "draw-subtree depth-remain" depth-remain " line: " x y bx by)
          ;;; there must be a better way than string concatenation
          (m/stroke-style (str "rgb(" r "," g "," b ")"))
          ;; (apply stroke color)
          (line' x y x' y')
          (draw-subtree [x' y'] genome (g/turn-direction dir :left)  (dec depth-remain))
          (draw-subtree [x' y'] genome (g/turn-direction dir :right) (dec depth-remain))
          )
      ))
  ctx)

; NB: we could potentially blow the stack
; but the JS impl I saw gets away with it.
(defn draw-creature [{:keys [ctx genome pos]}]
  ;; (println "BEGIN drawing genome")
  (draw-subtree ctx pos genome :n (inc (g/genome-depth genome)))
  ;; (println "END drawing genome")
  )


;; ---------------------------------------------------------------------
;; Update pipeline
;;
;; Initial state:
;;    :drawing -- The top-level DOM element that contains the canvas,
;;      the sliders, etc.
;;    :ctx -- The canvas context.
;;    :w -- The width of the canvas.
;;    :h -- The height of the canvas.
;;
;; Configuration values (as defined on the mondrian element):
;;    :persist-image -- Whether or not to clear the background in
;;      between frames.
;;    ... fill this in ...
;;
;; After merge-control-values:
;;    :... -- One key for each control (identified by the control name).
;;      Zero or more controls may be present under the mondrian element,
;;      in which case the missing values must have been provided in as
;;      defaults in the element itself.
;;
;; After ...:

(defn merge-control-values
  "Merge the current values of the controls into state."
  [{:keys [drawing] :as state}]
  (merge state (ui/update-controls drawing)))

(defn update-pipeline
  [state]
  (-> state
      merge-control-values))


;; ---------------------------------------------------------------------
;; Render stack
;;

(defn clear-background
  [{:keys [ctx w h persist-image]}]
  (when-not persist-image
    (-> ctx
        (m/fill-style "rgba(25,29,33,0.75)") ;; Alpha adds motion blur
        (m/fill-rect {:x 0 :y 0 :w w :h h}))))

(defn draw-ball 
  [{:keys [ctx]}]
  (-> ctx
      (m/fill-style "green")
      (m/circle {:x 100, :y 100, :r 30})
      (m/text {:text "repl hacksessss", :x 10, :y 10})
      m/begin-path
      (m/stroke-style "blue")
      (m/fill-style "rgba(10,10,200,0.45)")
      (m/move-to 100 100)
      (m/line-to 200 200)
      (m/line-to 100 200)
      m/close-path
      m/stroke
      m/fill
      ))

(defn render-stack
  [state]
  (clear-background state)
  ; TODO move genome from that atom into the state
  (draw-creature state)
  ;; (draw-ball state)
  )

;; ---------------------------------------------------------------------
;; Main entry point
;;

(defmondrian biomorphs-anim
  {:cx 1024       ;; might be able to just derive these from ctx
   :cy 1024
   :biomorph-count 9
   :pos [100 100]
   :genome [4 1 1 1 1 1 1 1 1 1 1]
   }
  update-pipeline
  render-stack)


(comment
  (do (require 'cemerick.piggieback) (cemerick.piggieback/cljs-repl))
  (js/alert "yoyo")
  (+ 2 2)
  (stop-updating)
  )
