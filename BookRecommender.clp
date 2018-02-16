;; main Module


(deftemplate question 

    (slot text)
    (slot type)
    (slot ident))

(deftemplate answer 

    (slot ident)
    (slot text))

(deftemplate recommendation 
    (slot novel)
    (slot author))

(deffacts question-data 

    "The questions the system can ask."
      (question (ident autobigraphy) (type yes-no)(text " Do you like autobiography ?"))
      (question (ident adventure) (type yes-no)(text " Do you like adventure?"))
      (question (ident comedy) (type yes-no)(text " Do you like comedy?"))
      (question (ident crime) (type yes-no)(text " Do you like crime?"))
      (question (ident fantasy) (type yes-no)(text " Do you like fantasy?"))
      (question (ident graphic) (type yes-no)(text " Do you like graphic?"))
      (question (ident horror) (type yes-no)(text " Do you like horror?"))
      (question (ident Psychological ) (type yes-no)(text " Do you like Psychological novels ?"))
      (question (ident Romance ) (type yes-no)(text " Do you like Romance novels ?"))
      (question (ident Science_fiction ) (type yes-no)(text " Do you like Science fiction books?"))
      (question (ident Thriller ) (type yes-no)(text " Do you like Thriller novels?"))
      (question (ident War ) (type yes-no)(text " Do you like War Novels ?"))
	  )

(defglobal ?*crlf* = "
")




;; ask Module

(defmodule ask)

(deffunction is-of-type (?answer ?type)

    "Check that the answer has the right form"
    (if (eq ?type yes-no) then
        (return (or (eq ?answer yes) (eq ?answer no)))
    elif (eq ?type number) then
        (return (numberp ?answer))
    else (return (> (str-length ?answer) 0))))

(deffunction ask-user (?question ?type)

  "Ask a question, and return the answer"
  (bind ?answer "")
  (while (not (is-of-type ?answer ?type)) do
         (printout t ?question " ")
         (if (eq ?type yes-no ) then
           (printout t "(yes or no) "))
         (bind ?answer (read)))
  (return ?answer))

   
(defrule ask::ask-question-by-id 

  "Given the identifier of a question, ask it and assert the answer"
  (declare (auto-focus TRUE))
  (MAIN::question (ident ?id) (text ?text) (type ?type))
  (not (MAIN::answer (ident ?id)))
  ?ask <- (MAIN::ask ?id)
  =>
  (bind ?answer (ask-user ?text ?type))
  (assert (answer (ident ?id) (text ?answer)))
  (retract ?ask)
  (return))


;; interview Module
(defmodule Novel_genre)

(defrule request-autobigraphy => (assert (ask autobigraphy)))
(defrule request-adventure =>(assert (ask adventure)))
(defrule request-comedy =>(assert (ask comedy)))
(defrule request-crime =>(assert (ask crime)))
(defrule request-fantasy => (assert (ask fantasy)))
(defrule request-graphic =>(assert (ask graphic)))
(defrule request-horror =>(assert (ask horror)))
(defrule request-Psychological =>(assert (ask Psychological )))
(defrule request-Romance =>(assert (ask Romance )))
(defrule request-Science_fiction =>(assert (ask Science_fiction )))
(defrule request-Thriller =>(assert (ask Thriller )))
(defrule request-War => (assert (ask War)))


(defmodule Welcome)

(defrule banner 
   
    =>
    (printout t "Please enter your name and then press enter key> ")
    (bind ?name (read))
    (printout t crlf " " crlf)
    (printout t " Hello, " ?name "." crlf)
    (printout t " Welcome to the Novel Recommender" crlf)
    (printout t " Here is a list of recommended novels :)." crlf)
    (printout t " " crlf crlf))


;; startup Module



;; recommend Module

(defmodule recommend)

(defrule genre_de_autobiography
 (answer (ident autobiography ) (text yes))
    =>
    (assert
        (recommendation (novel " Fear and Loathing in Las Vegas ") (author ""))))

(defrule genre_de_adventure

    (answer (ident adventure) (text yes))

    =>
    (assert
        (recommendation (novel " The Count of Monte Cristo ") (author "Alexandre Dumas"))))

(defrule genre_de_comedy

    (answer (ident comedy ) (text yes))

    =>
    (assert
        (recommendation (novel " Dirk Gently's Holistic Detective Agency ") (author "Douglas Adams"))))

(defrule genre_de_crime

    (answer (ident crime) (text yes))

    =>
    (assert
        (recommendation (novel "Donkey Punch") (author "Ray Banks"))))


(defrule genre_de_fantasy

    (answer (ident fantasy ) (text yes))

    =>
    (assert
        (recommendation (novel " Alice's Adventures in Wonderland ") (author "Lewis Carroll"))))

(defrule genre_de_graphic

    (answer (ident graphic) (text yes))

    =>
    (assert
        (recommendation (novel "100 Bullets") (author "Brian Azzarello"))))

(defrule genre_de_horror

    (answer (ident horror ) (text yes))

    =>
    (assert
        (recommendation (novel "The Dark Tower (series)  ") (author "Stephen King"))))

(defrule genre_de_psychological

    (answer (ident psychological) (text yes))

    =>
    (assert
        (recommendation (novel "Crime and Punishment  ") (author "Fyodor Dostoevsky"))))

(defrule genre_de_Romance

    (answer (ident ) (text yes))

    =>
    (assert
        (recommendation (novel "The Food of the Gods and How It Came to Earth ") (author "H. G. Wells"))))
(defrule genre_de_Science_fiction

    (answer (ident Science_fiction ) (text yes))

    =>
    (assert
        (recommendation (novel " Insurgent (novel) ") (author "Veronica Roth"))))
(defrule genre_de_Thriller

    (answer (ident thriller ) (text yes))

    =>
    (assert
        (recommendation (novel "Hannibal Rising") (author "Thomas Harris"))))
(defrule genre_de_War

    (answer (ident War) (text yes))

    =>
    (assert
        (recommendation (novel "Death to the French ") (author "C. S. Forester"))))


;; report Module

(defmodule report)

(defrule sort-and-print 
    ?r1 <- (recommendation (novel ?f1) (author ?e))
    (not (recommendation (novel ?f2&:(< (str-compare ?f2 ?f1) 0))))
    =>
    (printout t crlf " " crlf)
    (printout t " These are the books according to your choice -> " crlf ?f1 )
    (printout t " Author: "  crlf ?e crlf crlf)
    (retract ?r1))

;; run Module
(deffunction run-system () 

    (reset)
    (focus Novel_genre Welcome recommend report)
    (run))

(while TRUE
    (run-system))
