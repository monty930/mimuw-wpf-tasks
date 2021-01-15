(*****************************************************************************)
(************************ KRYSTYNA GASINSKA (grupa 3) ************************)
(*********************** ZADANIE 4 ORIGAMI 15.12.2020 ************************)
(************** code review w parze z: Jan Kolibabski (grupa 3) **************)
(*****************************************************************************)

open List;;
open Float;;

(* typ reprezentujacy punkt na plaszczyznie *)
type point = float * float;;

(* typ reprezentujacy zlazana kartke. jest to funkcja, ktora dla danego punktu
na plaszczyznie zwraca, ile razy szpilka wbita w tym punkcie przebije zlozona 
kartke. *)
type kartka = point -> int;;

(* zmienna do ustalenia dokladnosci *)
let eps = 100. *. epsilon;;

(* prostokat p1 p2 zwraca kartke reprezentujaca domkniety prostokat o bokach 
rownoległych do osi układu wspołrzednych i lewym dolnym rogu p1, a prawym 
gornym p2. punkt p1 powinien byc nieostro na lewo i w doł od punktu p2 *)
let prostokat ((x1, y1) : point) ((x2, y2) : point) = 
  assert (x1 <= x2 && y1 <= y2);
  (function (xf, yf) -> 
    if xf >= x1 -. eps && xf -. eps <= x2 && 
        yf >= y1 -. eps && yf -. eps <= y2
    then 1 else 0 
  : kartka);;

(* kolko p r zwraca kartke reprezentujaca domkniete kolo o srodku w punkcie p
i promieniu r *)
let kolko ((x1, y1) : point) (r : float) =
  assert (r >= 0.);
  (function ((xf, yf) : point) ->
    let d1 = (xf -. x1) in
    let d2 = (yf -. y1) in
    if sqrt ((d1 *. d1) +. (d2 *. d2)) -. eps <= r
    then 1 else 0 
  : kartka);;

(* typ reprezentujacy polozenie punktu wzgledem prostej *)
type strona = Lewa | Na | Prawa;;

(* polozenie p1 p2 p3 - zwraca polozenie punktu p3 wzgledem prostej przechodza-
cej przez punkty p1 i p2. zwraca wynik typu "strona". Wynik zwracany jest z
dokladnoscia do eps. punkty p1 i p2 powinny byc rozne. *)
let polozenie ((x1, y1) : point) ((x2, y2) : point) ((x3, y3) : point) =
  assert (x1 <> x2 || y1 <> y2);
  if abs (x1 -. x3) < eps &&  abs (y1 -. y3) < eps then Na 
  else if abs (x2 -. x3) < eps &&  abs (y2 -. y3) < eps then Na 
  else
    let a1 = x2 -. x1 in
    let a2 = y2 -. y1 in
    let b1 = x3 -. x1 in
    let b2 = y3 -. y1 in
    let dlugosc_a = sqrt (a1 *. a1 +. a2 *. a2) in
    let dlugosc_b = sqrt (b1 *. b1 +. b2 *. b2) in
    let wyzn = (a1 *. b2) -. (a2 *. b1) in 
    let sinus = wyzn /. (dlugosc_a *. dlugosc_b) in
    if sinus > (-1.) *. eps && sinus < eps then Na 
    else if sinus > (-1.) *. eps then Lewa 
    else Prawa;;

(* odbicie p1 p2 p3 - zwraca punkt bedacy odbiciem punktu p3 wzgledem prostej
przechodzacej przez p1 i p2. punkty p1 i p2 powinny byc rozne. *)
let odbicie ((x1, y1) : point) ((x2, y2) : point) ((x3, y3) : point) =
  assert (x1 <> x2 || y1 <> y2);
  let a = y1 -. y2 in
  let b = x2 -. x1 in
  let c = y1 *. (x1 -. x2) +. x1 *. (y2 -. y1) in
  let d = b *. x3 -. a *. y3 in
  let s1 = (-1.) *. ((a *. c -. b *. d) /. (a *. a +. b *. b)) in
  let s2 = (-1.) *. ((b *. c +. a *. d) /. (a *. a +. b *. b)) in
  let xw = 2. *. s1 -. x3 in
  let yw = 2. *. s2 -. y3 in
  (xw, yw);;

(* zloz p1 p2 krt sklada kartke krt wzdluz prostej przechodzacej przez punkty 
p1 i p2. Papier jest przekladany z prawej strony prostej na lewa (patrzac w 
kierunku od p1 do p2). punkty p1 i p2 powinny byc rozne. *)
let zloz ((x1, y1) : point) ((x2, y2) : point) (krt : kartka) = 
  assert (x1 <> x2 || y1 <> y2);
  (function (xf, yf) -> 
    match polozenie (x1, y1) (x2, y2) (xf, yf) with
    | Na -> krt (xf, yf)
    | Lewa -> krt (xf, yf) + krt (odbicie (x1, y1) (x2, y2) (xf, yf))
    | Prawa -> 0 
  : kartka);;

(* skladaj lst krt zwraca zlozenie kartki krt kolejno wzdluz wszystkich prostych
z listy lst. lista powinna zawierac pary roznych punktow *)
let skladaj (lst : (point * point) list) (krt : kartka) =
  let pom acc (p1, p2) = 
    assert (p1 <> p2);
    zloz p1 p2 acc
  in
  fold_left pom krt lst;;

(**********************************************************************************************)