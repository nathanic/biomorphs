(ns biomorphs.utils
  (:require
    [cljs.core.async :as async
     :refer [<! >! chan close! sliding-buffer put! alts!]]
    [jayq.core :refer [$ append ajax inner $deferred when done resolve pipe on] :as jq]
    ;; [jayq.util :refer [log]]
    )
  (:require-macros [cljs.core.async.macros :as m :refer [go alt!]])
  )

;; having trouble getting core.async to work
;; got it building though, by pulling the source
;; and adding a macro for the missing `exists?` definition
;; however, that means this project will only run on
;; machines where i've done a `lein install` of my modified version
;; UPDATE: `exists?` is supplied by clojurescript in a newer version

(defn data-from-event [event]
  (-> event .-currentTarget $ .data (js->clj :keywordize-keys true)))

(defn click-chan [selector msg-name]
  (let [rc (chan)]
    (on ($ "body") :click selector {}
        (fn [e]
          (jq/prevent e)
          (put! rc [msg-name (data-from-event e)])))
    rc))

(defn fields-value-map [form-selector fields]
  (into {} (map
            (fn [fld]
              [fld (jq/val ($ (str form-selector " input[name=" (name fld) "]")))] )
            fields)))

(defn form-submit-chan [form-selector msg-name fields]
  (let [rc (chan)]
    (on ($ "body") :submit form-selector {}
        (fn [e]
          (jq/prevent e)
          (put! rc [msg-name (fields-value-map form-selector fields)])))
    rc))

(defn merge-chans [& chans]
  (let [rc (chan)]
    (go
     (loop []
       (put! rc (first (alts! chans)))
       (recur)))
    rc))

(defn filter-chan [pred channel]
  (let [rc (chan)]
    (go (loop []
          (let [val (<! channel)]
            (if (pred val) (put! rc val))
            (recur))
          ))
    rc))

; maybe use jayq.util/log
(defn log
  "writes to js console.log"
  [& stuff]
  (.log js/console (apply str (interpose " " stuff)))
  )

(defn clog
  "version of log that ignores its first parameter but returns it;
  suitable for use in (-> ctx ...) blocks."
  [ctx & stuff]
  (apply log stuff)
  ctx)


;; there's probably a better way to do this
;; but i was relying on a JVMism (.indexOf) previously
#_(defn index-of
  "return the index of the supplied item, or nil"
  [coll item]
  (loop [idx 0, c coll]
    (if-not (empty? c)
      (if (= item (first c))
        idx
        (recur (inc idx) (rest c)))
      nil)))

; let's try a vector-specialized version
; this is inspired by reading cljs.core
(defn index-of
  "return the index of the supplied item, or nil"
  [v item]
  (let [len (count v)]
    (loop [i 0]
      (cond
        (<= len i)         nil,
        (= item (get v i)) i,
        :else              (recur (inc i ))))))


(defn index-of-by
  "return the index of the first item that matches the supplied predicate, or nil."
  [pred coll]
  (first
    (keep-indexed (fn [idx x]
                    (if (pred x)
                      idx
                      nil))
                  coll)))


; now using goog.math/clamp
;; (defn clamp [x xmin xmax]
;;   (max xmin (min xmax x)))

(comment
  ;; maybe a macro like:
  (defmacro saving-> [ctx & forms]
    `(-> ctx
         (m/save)
         ~@forms
         (m/restore)
         ))
  )

