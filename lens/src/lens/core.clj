(ns lens.core)

(defmacro def-curry-fn [name args & body]
  {:pre [(not-any? #{'&} args)]}
  (if (empty? args)
    `(defn ~name ~args ~@body)
    (let [rec-funcs (reduce (fn [l v]
                              `(letfn [(helper#
                                         ([] helper#)
                                         ([x#] (let [~v x#] ~l))
                                         ([x# & rest#] (let [~v x#]
                                                         (apply (helper# x#) rest#))))]
                                 helper#))
                            `(do ~@body) (reverse args))]
      `(defn ~name [& args#]
         (let [helper# ~rec-funcs]
           (apply helper# args#))))))

(defrecord Address [street city postcode])

(defrecord Person [name age address])

(defrecord User [uid username identity password])

; get :: a -> b, set :: a -> b -> a
(defrecord Lens [getter setter])

(defn lget [lens a]
  ((:getter lens) a))

(defn lset [lens a b]
  ((:setter lens) a b))

(defn lmodify [lens a func]
  ((:setter lens) a (func ((:getter lens) a))))

(def lpostcode (->Lens (fn [person] (:postcode person)) (fn [person postcode] (assoc-in person [:postcode] postcode))))

;; (\i -> p { _y = i}) <$> f (_y p)
;; forall f. Functor f => (b -> f b') -> a -> a'

;; (\i -> p { _y = i}) <$> f (_y p)
;;(defn mklens [get set]
;;   ())

(def-curry-fn add [x y] (+ x y))

(defprotocol Functor
    (fmap [functor f] "fmap :: f a -> (a -> b) -> f b"))

;; data Identity a = Identity { runIdentity :: a }
(defrecord Identity [runIdentity]
    Functor
    (fmap [functor f]
        (Identity. (f (:runIdentity functor)))))

(defrecord Const [runConst]
    Functor
    (fmap [functor f]
        (Const. (:runConst functor))))
