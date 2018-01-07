;; Adi:Devrim
;; Soyadi:AKINCI
;; Numarasi:141044052

;;Programin calismasi
;; Programin en son satirinda ornekte verildigi sekilde yazmaniz yeterlidir.
;; Cagrilma Sekli: (lexer filename)
;; Ornek: (lexer "CoffeeSample.coffee")

;;findIndexLeftParanthesis fonksiyonu
;;Bu fonksiyon Sol parantezlerin string icerisindeki indexini bulur ve onları vector'e ekler.
;;Parametre -> string

(defn findIndexLeftParanthesis [string]
  (def myVector [])
  (loop [i 0]
    (when (< i (count string))
      (cond
        (= \( (nth string i)) (def myVector (conj myVector i))
        )
      (recur (+ i 1))))
  myVector
  )

;;findIndexRightParanthesis fonksiyonu
;;Bu fonksiyon Sağ parantezlerin string icerisindeki indexini bulur ve onları vector'e ekler.
;;Parametre -> string

(defn findIndexRightParanthesis [string]
  (def myVector [])
  (loop [i 0]
    (when (< i (count string))
      (cond
        (= \) (nth string i)) (def myVector (conj myVector i))
        )
      (recur (+ i 1))))
  myVector
  )

;;insertString fonksiyonu
;;Bu fonksiyon string'e verilen elemani, verilen index'e ekler.
;;Parametre -> string
;;             element - Eklenecek Eleman
;;             index - Eklenecek Yer

(defn insertString [string element index]
  (def newString (str (subs string 0 index) element (subs string index (count string))))
  newString
  )

;;countLeftParanthesis fonksiyonu
;;Bu fonksiyon verilen listede sol parantezlerin sayisini bulur.
;;Parametre -> searchList - Arananacak Liste


(defn countLeftParanthesis [searchList]
  (def retVal 0)
  (loop [i 0]
    (when (< i (count searchList))
      (if (= "(" (nth searchList i))
        (def retVal (+ retVal 1)))
      (recur (+ i 1))))
  retVal
  )

;;countRightParanthesis fonksiyonu
;;Bu fonksiyon verilen listede sağ parantezlerin sayisini bulur.
;;Parametre -> searchList - Arananacak Liste

(defn countRightParanthesis [searchList]
  (def retVal 0)
  (loop [i 0]
    (when (< i (count searchList))
      (if (= ")" (nth searchList i))
        (def retVal (+ retVal 1)))
      (recur (+ i 1))))
  retVal
  )

;;isInteger fonksiyonu
;;Bu fonksiyon verilen stringin integer olup olmadigini kontrol eder.
;;Parametre -> string

(defn isInteger [string]
  (def integer true)
  (def i 0)
  (while (and (< i (count string)) (= integer true))
    (do
      (cond
        (= \0 (nth string i)) (def integer false)
        (= \- (nth string i)) (def i (+ i 1))
        (= \0 (nth string i)) (def i (+ i 1))
        (= \1 (nth string i)) (def i (+ i 1))
        (= \2 (nth string i)) (def i (+ i 1))
        (= \3 (nth string i)) (def i (+ i 1))
        (= \4 (nth string i)) (def i (+ i 1))
        (= \5 (nth string i)) (def i (+ i 1))
        (= \6 (nth string i)) (def i (+ i 1))
        (= \7 (nth string i)) (def i (+ i 1))
        (= \8 (nth string i)) (def i (+ i 1))
        (= \9 (nth string i)) (def i (+ i 1))
        :else (def integer false)
        )
      )
    )
    (if (and (= (count string) 1) (= \0 (nth string 0)))
      (def integer true))
  integer
  )

;;isKeyword fonksiyonu
;;Bu fonksiyon verilen stringin keyword olup olmadigini kontrol eder.
;;Parametre -> string
;;             keywordList

(defn isKeyword [string keywordList]
  (def validKeyword false)
  (loop [i 0]
    (when (and (< i (count keywordList)) (= validKeyword false))
      (if (= string (nth keywordList i))
        (def validKeyword true))
      (recur (+ i 1))))
  validKeyword
  )

;;isOperator fonksiyonu
;;Bu fonksiyon verilen stringin operator olup olmadigini kontrol eder.
;;Parametre -> string
;;             operatorList

(defn isOperator [string operatorList]
  (def validOperator false)
  (loop [i 0]
    (when (and (< i (count operatorList)) (= validOperator false))
      (if (= string (nth operatorList i))
        (def validOperator true))
      (recur (+ i 1))))
  validOperator
  )

;;isList fonksiyonu
;;Bu fonksiyon verilen stringin identifier olup olmadigini kontrol eder.
;;Parametre -> string
;;             idList

(defn isID [string idList]
  (def validID true)
  (doseq [i string]
    (if (= (.indexOf idList i) -1)
      (def validID false)
      )
    )
  validID
  )

;;isValid fonksiyonu
;;Bu fonksiyon verilen token listesinin dogru olup olmadigini kontrol eder.
;;Parametre -> tokenList keywordList operatorList idList

(defn isValid [tokenList keywordList operatorList idList]
  (def valid true)
  (loop [i 0]
    (when (and (< i (count tokenList)) (= valid true))
        (def flagKeyword (isKeyword (nth tokenList i) keywordList))
        (def flagOperator (isOperator (nth tokenList i) operatorList))
        (def flagInteger  (isInteger (nth tokenList i)))
        (def flagID (isID (nth tokenList i) idList))
        (if (= (or flagKeyword flagOperator flagInteger flagID) false)
          (def valid false))
      (recur (+ i 1))))
  valid
  )

;;removeElementVector fonksiyonu
;;Bu fonksiyon verilen index'e gore vector'den eleman siler.
;;Parametre -> vector, index

(defn removeElementVector [vector index]
  (def newVector (vec (concat (subvec vector 0 index) (subvec vector (+ index 1) (count vector)))))
  newVector
  )

;;ignoreCommentOneSharp fonksiyonu
;;Bu fonksiyon vector'deki tek satirlik comment'leri siler.
;;Parametre -> myVector

(defn ignoreCommentOneSharp [myVector]
  (def sharpFlag false)
  (def newLineFlag false)
  (def exit true)
  (def returnVector myVector)
  (def sharpIndex -1)
  (def newLineIndex -1)
  (def size (count returnVector))
  (while (= exit true)
    (do
      (loop [j 0]
        (when (and (< j size) (= sharpFlag false))
          (if (= \# (nth returnVector j))
            (do
              (def sharpFlag true)
              (def sharpIndex j)
              )
            )
          (recur (+ j 1))))
      (if (not= sharpIndex -1)
        (do
          (loop [k sharpIndex]
            (when (and (< k size) (= newLineFlag false))
              (if (= \newline (nth returnVector k))
                (do
                  (def newLineFlag true)
                  (def newLineIndex k)
                  )
                )
              (recur (+ k 1))))
          (loop [m 0]
            (when (< m (- newLineIndex sharpIndex))
              (def returnVector (removeElementVector returnVector sharpIndex))
              (recur (+ m 1))))
          )
        (do
          (def exit false)))
      (def size (count returnVector))
      (def sharpFlag false)
      (def newLineFlag false)
      (def sharpIndex -1)
      )
    )
  returnVector
  )

;;ignoreCommentTripleSharp fonksiyonu
;;Bu fonksiyon vector'deki çoklu satir olan comment'leri siler.
;;Parametre -> myVector

(defn ignoreCommentTripleSharp [myVector]
  (def exit true)
  (def tripleSharp false)
  (def lastTripleSharpFlag false)
  (def returnVector myVector)
  (def firstTripleSharp -1)
  (def lastTripleSharp -1)
  (def size (count returnVector))
  (while (= exit true)
    (do
      (loop [i 0]
        (when (and (< i (- size 2)) (= tripleSharp false))
          (if (= \# (nth returnVector i))
            (if (= \# (nth returnVector (+ i 1)))
              (if (= \# (nth returnVector (+ i 2)))
                (do
                  (def firstTripleSharp i)
                  (def tripleSharp true)))))
          (recur (+ i 1))))
      (if (not= firstTripleSharp -1)
        (do
          (loop [j (+ firstTripleSharp 1)]
            (when (and (< j (- size 2)) (= lastTripleSharpFlag false))
              (if (= \# (nth returnVector j))
                (if (= \# (nth returnVector (+ j 1)))
                  (if (= \# (nth returnVector (+ j 2)))
                    (do
                      (def lastTripleSharp (+ j 2))
                      (def lastTripleSharpFlag true)))))
              (recur (+ j 1))))
          (if (= lastTripleSharp -1)
            (do
              (def lastTripleSharp (count returnVector))
              (loop [m 0]
                (when (< m (- lastTripleSharp firstTripleSharp))
                  (def returnVector (removeElementVector returnVector firstTripleSharp))
                  (recur (+ m 1))))
              )
            (do
              (loop [m 0]
                (when (<= m (- lastTripleSharp firstTripleSharp))
                  (def returnVector (removeElementVector returnVector firstTripleSharp))
                  (recur (+ m 1))))
              ))
          )
        (do
          (def exit false)))
      (def size (count returnVector))
      (def firstTripleSharp -1)
      (def lastTripleSharp -1)
      (def lastTripleSharpFlag false)
      (def tripleSharp false)
      )
    )
  returnVector
  )

;;ignoreComment fonksiyonu
;;Bu fonksiyon string'in icerisindeki butun commentleri siler.
;;Parametre -> string

(defn ignoreComment [string]
  (def newString "")
  (def myVector (apply vector string))
  (def myVector (ignoreCommentTripleSharp myVector))
  (def myVector (ignoreCommentOneSharp myVector))
  (loop [i 0]
    (when (< i (count myVector))
      (def newString (str newString (nth myVector i)))
      (recur (+ i 1))))
  newString
  )

;;lexer fonksiyonu
;;Bu fonksiyon verilen dosyayı okur ve okudugu dosyanin icerisindeki kodu tokenlerine ayirir.
;; Return olarak token listesi dondurur.
;;Parametre -> filename - Dosya ismi

(defn lexer [filename]
  (def keywordList (list "and" "or" "not" "equal" "append" "concat" "set" "defvar"
                         "deffun" "for" "while" "if" "then" "else" "true" "false"))
  (def operatorList (list "+" "-" "/" "*" "(" ")"))
  (def idList (list \a\b\c\d\e\f\g\h\i\j\k\l\m\n\o\p\q\r\s\t\u\v\w\x\y\z
                    \A\B\C\D\E\F\G\H\I\J\K\L\M\N\O\P\Q\R\S\T\U\V\W\X\Y\Z))
  (def tokenList [])
  (def string "")
  (def string (slurp filename))
  (def string (ignoreComment string))
  (def leftParanthesisVector (findIndexLeftParanthesis string))
  (def rightParanthesisVector (findIndexRightParanthesis string))
  (loop [i 0]
    (when (< i (count leftParanthesisVector))
      (def string (insertString string " " (+ (nth leftParanthesisVector i) 1)))
      (def leftParanthesisVector (findIndexLeftParanthesis string))
      (def rightParanthesisVector (findIndexRightParanthesis string))
      (recur (+ i 1))))
  (loop [i 0]
    (when (< i (count rightParanthesisVector))
      (def string (insertString string " " (nth rightParanthesisVector i)))
      (def leftParanthesisVector (findIndexLeftParanthesis string))
      (def rightParanthesisVector (findIndexRightParanthesis string))
      (recur (+ i 1))))
  (def tokenList (clojure.string/split string #"\s+"))
  (def numberLeftParanthesis (countLeftParanthesis tokenList))
  (def numberRightParanthesis (countRightParanthesis tokenList))
  (if (= numberLeftParanthesis numberRightParanthesis)
    (do
      (if (= (isValid tokenList keywordList operatorList idList) true)
        (do
          (def tokenList (apply list tokenList)))
        (do
          (def tokenList ())
          (println "Invalid keyword or operator or integer value or identifier."))))
    (do
      (println "Number of left paranthesis is not equal to number of right paranthesis.")
      (def tokenList ())
      )
    )
  tokenList
  )

(println (lexer "CoffeeSample.coffee"))