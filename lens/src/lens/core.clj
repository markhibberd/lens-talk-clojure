(ns lens.core
  (:require clojure.pprint))

(defn show [a] (clojure.pprint/pprint a))

(defrecord Address [street city postcode])

(defrecord Person [name age address])

(defrecord User [uid username identity password])

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

(defn -get [lens a]
  (let [b-cb (fn [z] (->Const z)) ;- b -> Const b b
        a-ca (lens b-cb)          ;- a -> Const b a
        ca   (a-ca a)]            ;- Const b a
       (:runConst ca)))           ;- b

(defn -set [lens a b]
  (let [b-ib (fn [bb] (->Id b)) ;- b -> Id b
        a-ia (lens b-ib   )     ;- a -> Id a
        ia   (a-ia a)]          ;- Id a
       (:runId ia)))            ;- a

(defn -modify [lens a f]
  (let [b-ib (fn [bb] (->Id (f bb))) ;- b -> Id b
        a-ia (lens b-ib   )      ;- a -> Id a
        ia   (a-ia a)]           ;- Id a
       (:runId ia)))             ;- a

(defn lens [getter, setter]
  (fn [b-fb]
    (fn [a]
      (fmap (b-fb (getter a))
        (fn [b] (setter a b))))))

(def -postcode
  (lens
    (fn [address] (:postcode address))
    (fn [address postcode] (assoc-in address [:postcode] postcode))))

(def -city
  (lens
    (fn [address] (:city address))
    (fn [address city] (assoc-in address [:city] city))))

(def -street
  (lens
    (fn [address] (:street address))
    (fn [address street] (assoc-in address [:street] street))))

(def -address
  (lens
    (fn [person] (:address person))
    (fn [person address] (assoc-in person [:address] address))))

(def -age
  (lens
    (fn [person] (:age person))
    (fn [person age] (assoc-in person [:age] age))))

(def -name
  (lens
    (fn [person] (:name person))
    (fn [person name] (assoc-in person [:name] name))))

(def -uid
  (lens
    (fn [user] (:uid user))
    (fn [user uid] (assoc-in user [:uid] uid))))

(def -username
  (lens
    (fn [user] (:username user))
    (fn [user username] (assoc-in user [:username] username))))

(def -identity
  (lens
    (fn [user] (:identity user))
    (fn [user identity] (assoc-in user [:identity] identity))))

(def -password
  (lens
    (fn [user] (:password user))
    (fn [user password] (assoc-in user [:password] password))))

;(def -plaintext
;  (lens
;    (fn [user] ("can't get plaintext, this is a getter only"))
;    (fn [user plain] (assoc-in user [:identity] (digest/md5 plain)))))
