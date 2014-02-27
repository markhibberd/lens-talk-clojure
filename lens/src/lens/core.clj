(ns lens.core
  (:require clojure.pprint digest))

(defn show [a] (clojure.pprint/pprint a))

(defrecord Address [street city postcode])

(defrecord Person [name age address])

(defrecord User [uid username identity password])

(defprotocol Functor
    (fmap [functor f] "fmap :: f a -> (a -> b) -> f b"))

;; data Id a = Id { runId :: a }
(defrecord Id [runId]
    Functor
    (fmap [functor f]
        (Id. (f (:runId functor)))))

;; data Const x a = Const { runConst :: x }
(defrecord Const [runConst]
    Functor
    (fmap [functor f]
        (Const. (:runConst functor))))

;; get a value
(defn -get [lens a]
  (:runConst ((lens (fn [z] (->Const z))) a)))

;; modify a value with f
(defn -modify [lens a f]
  (:runId ((lens (fn [z] (->Id (f z)))) a)))

;; set a value with b
(defn -set [lens a b] (-modify lens a (fn [bb] b)))

;; build a lens from a get and set function

(defn lens [getter, setter]
  (fn [b-fb]
    (fn [a]
      (fmap (b-fb (getter a))
        (fn [b] (setter a b))))))


;;;;;; the actual lens functions, someone who can do macros should get rid of these in about 2 minutes

(defn mklens [func]
  (lens
    (fn [a] (func a))
    (fn [a b] (assoc-in a [func] b))))

(def -postcode (mklens :postcode))
(def -city (mklens :city))
(def -street (mklens :street))
(def -address (mklens :address))
(def -age (mklens :age))
(def -name (mklens :name))
(def -uid (mklens :uid))
(def -username (mklens :username))
(def -identity (mklens :identity))
(def -password (mklens :password))

; FIX this is only a getter, need to specialize functor by hand
(def -plaintext
  (lens
    (fn [user] (:password user))
    (fn [user plain] (assoc-in user [:password] (digest/md5 plain)))))




;;;;; naive lens implementation - this is silly, but informative ;;;;;


; getter :: a -> b, setter :: a -> b -> a
(defrecord Lens [getter setter])

(defn lget [lens a]
  ((:getter lens) a))

(defn lset [lens a b]
  ((:setter lens) a b))

(defn lmodify [lens a func]
  ((:setter lens) a (func ((:getter lens) a))))

(defn lcompose [lens-a-b lens-b-c]
  (->Lens
    (fn [a] (lget lens-b-c (lget lens-a-b a)))
    (fn [a c] (lset lens-a-b a
                (lset lens-b-c
                  (lget lens-a-b a) c)))))

(def lpostcode
  (->Lens
    (fn [address] (:postcode address))
    (fn [address postcode] (assoc-in address [:postcode] postcode))))

(def laddress
  (->Lens
    (fn [person] (:address person))
    (fn [person address] (assoc-in person [:address] address))))


;;;; fully expaned -get, -set, -modify


;;; get a value
;(defn -get [lens a]
;  (let [b-cb (fn [z] (->Const z)) ;- b -> Const b b
;        a-ca (lens b-cb)          ;- a -> Const b a
;        ca   (a-ca a)]            ;- Const b a
;       (:runConst ca)))           ;- b
;
;
;;; modify a value with f
;(defn -modify [lens a f]
;  (let [b-ib (fn [bb] (->Id (f bb))) ;- b -> Id b
;        a-ia (lens b-ib   )      ;- a -> Id a
;        ia   (a-ia a)]           ;- Id a
;       (:runId ia)))             ;- a
;
;;; set a value with b
;(defn -set [lens a b]
;  (let [b-ib (fn [bb] (->Id b)) ;- b -> Id b
;        a-ia (lens b-ib   )     ;- a -> Id a
;        ia   (a-ia a)]          ;- Id a
;       (:runId ia)))            ;- a
;


(defmacro mkLens [ob, fn]
  `(def ~(symbol (str "-" fn))
    (lens
      (fn [x#] (~(keyword fn) x#))
      (fn [o# x#] (assoc-in o# [~(keyword fn)] x#)))))
