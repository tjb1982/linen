(ns flax.core)


(def ^:dynamic *dry-run* nil)
(def ^:dynamic *skip* nil)
(def ^:dynamic *isolate* nil)
(def this-ns *ns*)
(def genv (atom {}))
(def returns (atom []))


(defn false-or
  "Will set the value to false if it's explicitly false,
   or prev or current, whatever they happen to be."
  [c p]
  (if (false? c)
    false
    (or p c)))


(defmacro wrap-binding
  [what & forms]
  `(binding [*dry-run* (or *dry-run* (:dry-run ~what)) ;; once it's marked dry-run = true, it can't be changed
             *skip* (false-or (:skip ~what) *skip*) 
             sh/*throw* (false-or (:throw ~what) sh/*throw*)]
    (binding [*dry-run* (or *dry-run* *skip*)] ;; skip and dry-run mean the same thing
      ~@forms)))

