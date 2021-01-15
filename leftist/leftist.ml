(************************************************)
(**********KRYSTYNA GASINSKA (grupa 3)***********)
(***** ZADANIE 2 DRZEWA LEWICOWE 25.11.2020 *****)
(* code review w parze z: Jan Wojtach (grupa 3) *)
(************************************************)

(* wierzcholki drzew skladaja sie z pary liczb - pierwsza liczba (oznaczana w 
kodzie przez "a") to priorytet, druga (oznaczana w kodzie przez "b" - typu int) 
to glebokosc prawego poddrzewa *)
type 'a queue =
  | Node of 'a queue * 'a * int *'a queue
  | Null;;

(* konstruktor pustej kolejki *)
let empty = 
  Null;;

(* funkcja pomocnicza dla funkcji join. sprawdza, czy drzewo jest lewicowe, 
a w jego korzeniu znajduje sie poprawna wysokosc prawego poddrzewa,
jesli nie - poprawia je. Tzn: jesli lewe i prawe poddrzewa nie sa puste
sprawdzane sa ich wysokosci - nastepnie zwracane jest wyjsciowe drzewo
z wysokoscia prawego poddrzewa (tj. wysokosc jego prawego poddrzewa+1) 
w korzeniu oraz ew. zamienia prawe i lewe poddrzewo. W przypadku gdy 
jedno z poddrzew jest puste zwraca drzewo, ktorego lewym poddrzewem
jest niepuste drzewo, prawym puste, a druga wartosc w wierzcholku to 0.*)
let pomoc q =
  match q with 
  | Null -> Null
  | Node(ql,a,_,Null) -> Node(ql,a,0,Null) 
  | Node(Null,x,_,qr) -> Node(qr,x,0,Null)  
  | Node (Node(qll,al,bl,qlr),a,b,Node(qrl,ar,br,qrr)) ->
    (
    if (bl>=br) then (Node (Node(qll,al,bl,qlr),a,br+1,Node(qrl,ar,br,qrr))) 
    else (Node (Node(qrl,ar,br,qrr),a,bl+1,Node(qll,al,bl,qlr)))
      );;
  
(* zwraca drzewo lewicowe bedace polaczeniem dwoch drzew lewicowych. 
Jesli jedno z drzew jest puste - zwraca drugie z nich. Jesli zadne nie
jest puste: Mamy niepuste drzewa q1, q2. Bierzemy drzewo Q, ktore
w lewym poddzrewie ma lewe poddrzewo tego z drzew q1, q2 - ktorego
priorytet wierzcholka jest lepszy (mniejsza lizba), w korzeniu 
ma korzen tego drzewa natomiast w prawym poddrzewie rekurencyjne 
polaczenie drugiego z drzew i pozostalego prawego poddrzewa. Zwracane jest
pomoc(drzewa) aby wynik byl poprawnym drzewem lewicowym z poprawna wartoscia
w korzeniu. *)
let rec join q1 q2 = 
  match q1, q2 with
  | Null, q2 -> q2
  | q1, Null -> q1
  | Node(q1l,a1,b1,q1r), Node(q2l,a2,b2,q2r) ->
    (
    if (a1>a2) then (pomoc(Node(q2l,a2,b2,(join q2r q1)))) 
    else (pomoc(Node(q1l,a1,b1,(join q1r q2))))
    );;

(* dolacza element do kolejki i zwraca nowa kolejke. Korzysta z funkcji join *)
let add e q =
  join q (Node(Null,e,0,Null));;

(* wyjatek podnoszony w przypadku proby usuniecia elementu z pustej kolejki
- patrz funkcja delete_min *)
exception Empty;;

(* usuwa najmniejszy element do kolejki, zwraca nowa kolejke oraz
usuniety element. Korzysta z funkcji join. W przypadku gdy kolejka jest pusta
podnoszony jest wyjatek Empty. *)
let delete_min q =
  match q with 
  | Null -> raise(Empty)
  | Node(q1,x,y,q2) -> (x,(join q1 q2));;

(* Zwraca true jesli dana kolejka jest pusta. W przeciwnym razie false *)
let is_empty q =
  match q with
  | Null -> true
  | _ -> false;;