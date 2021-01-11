(ns assign2.sales
  (:gen-class))
(:require 'clojure.java.io)

(def cust {})
(def pro {})
(def sale {})


(defn init []
  (def file1 (slurp "C:/Users/patel/Assignme/customer.txt"))
  (def lines (clojure.string/split-lines file1 ))
  (def a (count lines))
  (def flag1 a)
  (defn custom [a]
	   (def b(get lines (dec a)))
	   (def li (clojure.string/replace b #"\|" ","))
	   (let [k (clojure.string/split li #",")]
      (def fir (Integer/parseInt(nth k 0)))
      (def res (subs li 2))
      (if(= flag1 a)
        (def cust { fir res})
        (def cust (assoc cust fir res)))
      (if (< 1 a)
        (custom (dec a)))))
  (custom a)
  
  (def file2 (slurp "C:/Users/patel/Assignment2/product.txt"))
  (def lines1 (clojure.string/split-lines file2 ))
  (def a1 (count lines1))
  (def flag2 a1)
  (defn prod [a1]
    (def b1(get lines1 (dec a1)))
    (def li1 (clojure.string/replace b1 #"\|" ","))
    (let [k1 (clojure.string/split li1 #",")]
      (def fir1 (Integer/parseInt(nth k1 0)))
      (def res1 (subs li1 2))
      (if(= flag2 a1)
        (def pro {fir1 res1})
        (def pro (assoc pro fir1 res1)))
      (into (sorted-map) pro)
      (if (< 1 a1)
        (prod (dec a1)))))
  (prod a1)
  
  (def file3 (slurp "C:/Users/patel/Assignment2/sales.txt"))
  (def lines2 (clojure.string/split-lines file3 ))
  (def a2 (count lines2))
  (def flag3 a2)
  (defn sales [a2]
    (def b2(get lines2 (dec a2)))
    (def li2 (clojure.string/replace b2 #"\|" ","))
    (let [k2 (clojure.string/split li2 #",")]
      (def fir2 (Integer/parseInt(nth k2 0)))
      (def res2 (subs li2 2))
      (if(= flag3 a2)
        (def sale {fir2 res2})
        (def sale (assoc sale fir2 res2)))
      (into (sorted-map) sale)
      (if (< 1 a2)
        (sales (dec a2)))))
  (sales a2))
(init)


(defn displayCustomer []
  (def newcust (reverse (sort (keys cust))))
  (def c1 (count newcust))
  (defn loop1 [c1]
    (def t1 (get cust (nth newcust (dec c1))))
    (println (nth newcust (dec c1)) ":[" t1"]")
    (if(< 1 c1)
      (loop1 (dec c1))))
  (loop1 c1))


(defn displayProduct []
  (def newpro (reverse (sort (keys pro))))
  (def c1 (count newpro))
  (defn loop1 [c1]
    (def t1 (get pro (nth newpro (dec c1))))
    (println (nth newpro (dec c1)) ":[" t1"]")
    (if(< 1 c1)
      (loop1 (dec c1))))
  (loop1 c1))


(defn displaySales []
  (def newsale (reverse (sort (keys sale))))
  (def c1 (count newsale))
  (defn loop1 [c1]
    (def t1 (get sale (nth newsale (dec c1))))
    (let [k (clojure.string/split t1 #",")]
      (def rest1 (Integer/parseInt(nth k 0)))
      (def temp1 (get cust rest1))
      (def temp (clojure.string/split temp1 #"," ))
      (def cus1 (get temp 0))
      (def rest2(Integer/parseInt(nth k 1)))
      (def temp2 (get pro rest2))
      (def tem (clojure.string/split temp2 #"," ))
      (def prod1 (get tem 0))
      (def rest3 (Integer/parseInt(nth k 2)))
      (println (str (nth newsale (dec c1))":""["cus1","prod1","rest3"]"))
      (if(< 1 c1)
        (loop1 (dec c1)))))
  (loop1 c1))


(defn totalSales []	  
  (println "Enter customer name:")
  (def cost 0)
  (def fa 1)
  (def fname)
  (let[cname (read-line)]
				(println cname)
				(def cuid 0)
    (def fname cname)
    (def b(count cust))
    (def lis (keys cust))
    (defn getCid [b]
      (def tpid(nth lis (dec b)))
      (def t(get cust (nth lis (dec b))))
      (def m (clojure.string/split t #","))
      (def k1(get m 0))
      (if (= cname k1)
        (do(def cuid tpid)
          (def fa 0))) 
      (if(< 1 b)
        (getCid (dec b))))
    (getCid b))
  (if( = fa 0)
    (do (def b1(count sale))
      (def lis1 (keys sale))
      (def vect ())
      (def pri ())
      (def tot 0)
      (defn getPid [b1] 
        (def tpid1(nth lis1 (dec b1)))
        (def t1(get sale (nth lis1 (dec b1))))
        (def m1 (clojure.string/split t1 #","))
        (def k1 (Integer/parseInt(nth m1 0)))
        (if (= cuid k1)
          (def vect (conj vect (Integer/parseInt(nth m1 1)))))
        (if (= cuid k1)
          (def pri (conj pri (nth m1 2))))
        (if(< 1 b1)
          (getPid (dec b1))))
      (getPid b1)
      
      (if (empty? vect)
        (do(println fname ":$"tot))
        (do(def con (count vect))
          (defn getTotal [con]
            (def t2(get pro (nth vect (dec con))))
            (def m33 (clojure.string/split t2 #","))
            (def cost (Double. (get m33 1)))
            (def y (Double. (nth pri (dec con))))
            (def tot (+ (* cost y) tot))
            (if( < 1 con)
              (getTotal (dec con))))
          (getTotal con)
          
          (println fname ": $"tot))))
    (do (println "The customer dont exists!"))))



(defn productCount []
  (println "Enter product name:")
  (def pid 0)
  (def cost 0)
  (def fo 1)
  (def fname)
  (let[pname (read-line)]
    (println pname)
    (def fname pname)
    (def b(count pro))
    (def lis (keys pro))
    (defn getPid [b]
      (def tpid(nth lis (dec b)))
      (def t(get pro (nth lis (dec b))))
      (def m (clojure.string/split t #","))
      (def k1(get m 0))
      (if (= pname k1)
        (do (def pid tpid)
          (def fo 0)))
      (if(< 1 b)
        (getPid (dec b))))
    (getPid b))
  (if(= fo 0)
    (do (def abc (count sale))
      (def bca (keys sale))
      (def timee 0)
      (defn getfinal [abc]
        (def tsale(nth bca (dec abc)))
        (def t1(get sale tsale ))
        (def m1 (clojure.string/split t1 #","))
        (def k2(Integer/parseInt(nth m1 1)))
        (def k3(get m1 2))
        (if (= pid k2)
          (def timee (+ timee (Long/valueOf k3))))
        (if(< 1 abc)
          (getfinal (dec abc))))
      (getfinal abc)
      (println fname ":" timee))
    (do (println "No product exists!"))))



(defn exit1 []
  (println "Goodbye!!!")
  (System/exit 0))

(defn menu []
  (println)
  (println 
    "*** Sales Menu ***
------------------
1. Display Customer Table
2. Display Product Table
3. Display Sales Table
4. Total Sales for Customer
5. Total Count for Product
6. Exit")
  (println "Enter your choice:") 
  (let[cid (read-line)]
    (println cid)
    (case cid "1" (displayCustomer)
      "2" (displayProduct)
      "3" (displaySales)
      "4" (totalSales)
      "5" (productCount)
      "6" (exit1)
      (menu))
    (menu)))
(menu)


