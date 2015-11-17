(ns BakersClojure.main
(:require [clojure.core.async
             :as async
             :refer [>! <! >!! <!! go chan buffer close! thread
                     alts! alts!! timeout]]))

(defrecord Customer
  [id n fib-of-n server])

(defn make-customer [id]
  (println "Got a customer")
  (let [n (+ 30 (rand-int 10))
        customer (->Customer id n (atom nil) (atom nil))]
    (add-watch (:fib-of-n customer) id
               (fn [key atom old-value new-value]
                 (println (str "Customer " key " changed from " old-value " to " new-value "."))))
    customer))

(defn make-customers [num-customers]
    (println "customers")
    (async/map make-customer (range num-customers)))

(defn set-fib-value
  [customer value]
  (reset! (:fib-of-n customer) value))

(defn fib
  ([n]
     (fib [0 1] n))
  ([x, n]
     (if (< (count x) n)
       (fib (conj x (+ (last x) (nth x (- (count x) 2)))) n)
       x)))

(defrecord Server
  [id customers-served])

(defn make-server [id]
  (let [server (->Server id (atom []))]
    (add-watch (:customers-served server) id
               (fn [key atom old-value new-value]
                 (println (str "Server " key " has now served " new-value "."))))
    server))

(defn make-servers [num-servers]
  (println "servers")
  (make-server num-servers))

(defn serve [server customer num-servers]
  (println "Your mom"))

(defn add-served-customer [server customer-id n]
  (swap! (:customers-served server) conj {:customer-id customer-id, :n n}))


(defn manage-bakery
  [num-servers num-customers]
  (let [customers (make-customers num-customers)
        servers (make-servers num-servers)
        pairs (async/map (fn [customer server]
                           {:customer customer,
                            :server server,
                            :server-channel servers})
                         [customers servers])]
    (loop []
      (when-let [{server :server, customer :customer, servers :server-channel}
                 (<!! pairs)]
        (go
         (serve server customer servers))
        (recur)))))


(defn -main []
  (manage-bakery 100 2000))
