;; Adi: Devrim
;; Soyadi: Akinci
;; Numarasi: 141044052

;;NOT:Hocam, include.clj dosyasının içerisindeki fonksiyonları kullandım.

;; utility functions 
(load-file "include.clj") ;; "c2i and "i2c"


(use 'clojure.java.io)
(import java.io.BufferedReader)
(import java.io.FileReader)

;;read-as-list fonksiyonu
;;"Reads a file containing one word per line and returns a list of words (each word is in turn a list of characters)."
;;Parametre -> filename - Dosya ismi
(defn read-as-list
	[filename]
	(def myList [])
	;;Dosya okuma islemi
	(with-open [rdr (BufferedReader. (FileReader. filename))]
		;;Satir satir okur
		(doseq [line (line-seq rdr)]
			;;Okuduklarini listeye cevirir ve baska bir listeye atar
			(def myList (conj myList (apply list (seq line))))))
	(apply list myList))

;; -----------------------------------------------------
;; HELPERS
;; *** PLACE YOUR HELPER FUNCTIONS BELOW ***

;;readDocument fonksiyonu
;;Bu fonksiyon verilen dokumani okur.
;;Parametre -> documentName - Dokuman ismi

(defn readDocument [documentName]
	(def myList [])
	(def orgList [])
	(def myList (clojure.string/split (slurp documentName) #"\s+"))
	(loop [i 0]
		(when (< i (count myList))
			(def orgList (conj orgList (seq (nth myList i))))
			(recur (inc i))))
	(apply list orgList))

;;spell-checker-0 fonksiyonu
;;Bu fonksiyon verilen kelimeyi sozlukte arar eger varsa true yoksa false return eder.
;;Parametre -> word - Kelime ismi

(defn spell-checker-0
	[word]
	;;Sozlugun okunmasi
	(def checkList (read-as-list "dictionary2.txt"))
	(def flag true)
	(def found false)
	;;Linear search ile kelimenin sozlukte aranmasi
	(loop [j 0]
		(when (and (< j (count checkList)) flag)
			(def wordList (nth checkList j))
			(if (= word wordList)
				(do (def flag false)
						(def found true))
				)
		(recur (inc j))))
	(boolean found)
	)

;;spell-checker-1 fonksiyonu
;;Bu fonksiyon verilen kelimeyi sozlukte arar eger varsa true yoksa false return eder.
;;(spell-checker-0'a gore daha hizli arar.)
;;Parametre -> word - Kelime ismi

(defn spell-checker-1
	[word]
	;;Sozlugun okunmasi
	(def checkList (read-as-list "dictionary2.txt"))
	(def found false)
	;;Kelimenin sozlukte aranmasi
	(if (not= (.indexOf checkList word) -1)
		(do
			(def found true)))
	(boolean found)
	)

;;createMap fonksiyonu
;;Bu fonksiyon alfabedeki harflerin hangi harfe karsilik geldigini map eder.(shuffle fonksiyonunu kullanarak)
;;Parametre -> isShuffle - Listeyi karistirmak icin tutulan boolean bir degisken

(defn createMap [isShuffle]
	(def mainAlphabet (list \a\b\c\d\e\f\g\h\i\j\k\l\m\n\o\p\q\r\s\t\u\v\w\x\y\z))
	(if (= isShuffle true)
		(do
			;;Sifrenin olusturulmasi
			(def cipherAlphabet (shuffle mainAlphabet))
			(def cipherAlphabet (apply list cipherAlphabet))
			;;Sifrenin gercek alfabeye map etmesi
			(def mapAlphabet (zipmap cipherAlphabet mainAlphabet)))
		)
	mapAlphabet
	)

;;converter fonksiyonu
;;Bu fonksiyon verilen sifreli kelimeyi, map ile cevirir ve yeni bir kelime return eder.
;;Parametre -> mapAlphabet - Alfabe mapi , encodedWord - Sifreli kelime

(defn converter [mapAlphabet encodedWord]
	(def checkList [])
	(loop [i 0]
		(when (< i (count encodedWord))
			;;Yeni kelimenin uretilmesi
			(def checkList (conj checkList (get mapAlphabet (nth encodedWord i))))
			(recur (inc i))))
	(apply list checkList))

;;countLetter fonksiyonu
;;Bu fonksiyon bir metnin icindeki harflerin sayisini sayar ve bir map dondurur.
;;Parametre -> paragraph - Metin

(defn countLetter [paragraph]
	(def mainAlphabet (sorted-map \a 0\b 0\c 0\d 0\e 0\f 0\g 0\h 0\i 0\j 0\k 0\l 0\m 0\n 0\o 0\p 0\q 0\r 0\s 0\t 0\u 0\v 0\w 0\x 0\y 0\z 0))
	(loop [i 0]
		(when (< i (count paragraph))
			(def word (nth paragraph i))
			;;Metnin icindeki harfleri sayma islemi
			(loop [j 0]
				(when (< j (count word))
					(loop [k 0]
						(when (< k 26)
							(if (= (nth word j) (i2c k))
								(def mainAlphabet (update mainAlphabet (nth word j) inc)))
						(recur (inc k))))
					(recur (inc j))))
			(recur (inc i))))
	(sort-by val > mainAlphabet)
	)

;;match fonksiyonu
;;Bu fonksiyon verilen map ile kelimeyi donusturur ve donusturdugu kelimeyi sozlukte arar.Eger sozlukte kelimeyi bulursa
;;o kelimeyi dondurur
;;Parametre -> cipherAlphabet - Alfabe mapi , encodedWord - Sifreli kelime

(defn match [cipherAlphabet encodedWord]
	(def decodedWord (converter cipherAlphabet encodedWord))
	(def found (spell-checker-1 decodedWord))
	(if (not= found true)
		(def decodedWord nil))
	decodedWord
	)

;;occurenceMap fonksiyonu
;;Bu fonksiyon ingilizce alfabesine gore sifreli metinde en cok tekrar eden 6 harfi sira ile e t a o i n ye map eder,
;;geri kalan 20 harfi kendi arasinda siralayarak yeni bir map olusturur ve bunu return eder.
;;Parametre -> occurList - Sifreli metinde gecen harflerin sayisi

(defn occurenceMap [occurList]
	(def cipherAlphabet [])
	(def alphabetVector [])
	(def occurenceAlphabet (list \e\t\a\o\i\n))
	(def otherAlphabet (list \s\h\r\d\l\c\u\m\w\f\g\y\p\b\v\k\j\x\q\z))
	(loop [i 0]
		(when (< i (count occurList))

			(if (> i 5)
				;;En cok tekrar eden 6 harfi listeye atar.
				(def alphabetVector (conj alphabetVector (first (nth occurList i))))
				;;Diger 20 harfi listeye atar.
				(def cipherAlphabet (conj cipherAlphabet (first (nth occurList i))))
				)
			(recur (inc i))))
	(def cipherAlphabet (apply list cipherAlphabet))
	(def alphabetVector (apply list alphabetVector))
	;;En cok tekrar eden 6 harfi map eder.
	(def occurenceAlphabetMap (zipmap cipherAlphabet occurenceAlphabet))
	;;Diger harfleri map eder.
	(def otherAlphabetMap (zipmap alphabetVector otherAlphabet))
	;;İki mapi birlestirir ve return eder.
	(merge occurenceAlphabetMap otherAlphabetMap)
	)

;; -----------------------------------------------------
;; DECODE FUNCTIONS


;;Gen-Decoder-A fonksiyonu
;;Bu fonksiyon verilen sifreli metnin icindeki kelimeleri cozmesi icin bir fonksiyon return eder.
;;Parametre -> paragraph - Sifreli metin
(defn Gen-Decoder-A
	[paragraph]
	;;Donguden cikmak icin tanimlanan boolean degisken
	(def exit true)
	(def doShuffle true)
	;;decodeWord fonksiyonu
	;;Bu fonksiyon sifreli bir kelimeyi cozer ve cozdugu kelimeyi return eder.
	;;Parametre -> word - Sifreli kelime
	(defn decodeWord [word]
		(while (= exit true)
			(do
				(def alphabet (createMap doShuffle))
				(def decodedWord (match alphabet word))
				(if (not= decodedWord nil)
					(def exit false))
				)
			)
		decodedWord)
	decodeWord)

;;Gen-Decoder-B-0 fonksiyonu
;;Bu fonksiyon verilen sifreli metnin icindeki kelimeleri cozmesi icin bir fonksiyon return eder.
;;Parametre -> paragraph - Sifreli metin

(defn Gen-Decoder-B-0
	[paragraph]
	;;Donguden cikmak icin tanimlanan boolean degisken
	(def exit true)
	;;decodeWord fonksiyonu
	;;Bu fonksiyon sifreli bir kelimeyi cozer ve cozdugu kelimeyi return eder.
	;;Parametre -> word - Sifreli kelime
	(defn decodeWord [word]
		(def v1 [])
		(def v2 [])
		;;Sifreli metinde gecen harflerin sayisinin bulunmasi
		(def countLetterMap (countLetter paragraph))
		;;Gercek alfabenin sifreli alfabeye map edilmesi
		(def cipherMap (occurenceMap countLetterMap))
		(loop [i 0]
			(when (< i 6)
				(def entry (find cipherMap (first (nth countLetterMap i))))
				;;Harfleri map den cikarmadan once degerlerinin vektorde tutulmasi
				;;v1 -> Keyleri tutan vector
				(def v1 (conj v1 (first entry)))
				;;v2 -> Valuelari tutan vector
				(def v2 (conj v2 (second entry)))
				;;En cok tekrar eden 6 harfin map den cikarilmasi
				(def cipherMap (dissoc cipherMap (first (nth countLetterMap i))))
				(recur (inc i))))
		(while (= exit true)
			(do
				;;Mapin valuelarini tutan liste
				(def mapVal (vals cipherMap))
				;;Mapin keylerini tutan liste
				(def mapKey (keys cipherMap))
				;;Valuelari tutan listenin karistirilmasi
				(def mapVal (apply list (shuffle mapVal)))
				;;Yeni bir map olusturulmasi
				(def newMap (zipmap mapKey mapVal))
				(def tempMap (zipmap v1 v2))
				(def newMap (merge tempMap newMap))
				;;Sifreli kelimenin donusturulmesi
				(def decodedWord (converter newMap word))
				;;Donusturulen kelimenin sozlukte aranmasi
				(def found (spell-checker-1 decodedWord))
				(if (= found true)
					(do
						(loop [i 0]
							(when (< i (count word))
								(if (= (contains? cipherMap (nth word i)) true)
									(do
										;;Bulunan kelimenin degistirilecek harfin map de bulunması
										(def pair (find newMap (nth word i)))
										(if (not= (.contains v1 (first pair)) true)
											;;Pair ilk elemaninin v1 e eklenmesi
											(def v1 (conj v1 (first pair))))
										(if (not= (.contains v2 (second pair)) true)
											;;Pair ikinci elemaninin v2 ye eklenmesi
											(def v2 (conj v2 (second pair))))
										(def newCh (get cipherMap (nth word i)))
										;;cipherMap de pair ikinci elemanina karsilik gelen keyin bulunmasi
										(def mapkey (filter (comp #{(second pair)} cipherMap) (keys cipherMap)))
										(def mapkey (first mapkey))
										(if (= (contains? cipherMap mapkey) true)
											;;cipherMap e eleman eklenmesi
											(def cipherMap (assoc cipherMap mapkey newCh))
											)
										;;Bulunan kelimenin harflerini map den cikarmasi
										(def cipherMap (dissoc cipherMap (nth word i)))
										)
									)
								(recur (inc i))))
						(def exit false)
						)
					(do
						(def decodedWord nil))))
			)
		decodedWord
		)
	decodeWord
	)

(defn Gen-Decoder-B-1
	[paragraph]
  	;you should implement this function
)

;;Code-Breaker fonksiyonu
;;Bu fonksiyon şifreli bir metni cozer ve anlamlı bir metin return eder.
;;Parametre -> document - Dosya ismi, decoder - Sifreyi cozecek fonksiyon(Gen-Decoder-A,Gen-Decoder-B-0,Gen-Decoder-B-1)

(defn Code-Breaker
	[document decoder]
	(def plainText [])
	(def doc (readDocument document))
	(loop [i 0]
		 (when (< i (count doc))
			 (def decodedWord ((decoder doc) (nth doc i)))
			 (def plainText (conj plainText decodedWord))
			 (recur (inc i))))
	(apply list plainText)
	)

;; -----------------------------------------------------
;; Test code...

(defn test_on_test_data
	[]
	;;spell-checker-0 in test edilmesi
	;;Sozlukte car kelimesinin aranmasi (Beklenen output:True)
	(println (spell-checker-0 (list \c\a\r)))
	;;Sozlukte xyz kelimesinin aranmasi (Beklenen outpu:False)
	(println (spell-checker-0 (list \x\y\z)))
	;;spell-checker-1 in test edilmesi
	;;Sozlukte electric kelimesinin aranmasi (Beklenen output:True)
	(println (spell-checker-1 (list \e\l\e\c\t\r\i\c)))
	;;Sozlukte abcdxyz kelimesinin aranmasi (Beklenen output:False)
	(println (spell-checker-1 (list \a\b\c\d\x\y\z)))
	;;GenDecoderB-0 nin test edilmesi
	(println "GenDecoder-B-0 ->" (Code-Breaker "document1.txt" Gen-Decoder-B-0))
	;;GenDecoderA nin test edilmesi
	(println "GenDecoder-A ->" (Code-Breaker "document1.txt" Gen-Decoder-A))
	)
;; test code...
(test_on_test_data)
