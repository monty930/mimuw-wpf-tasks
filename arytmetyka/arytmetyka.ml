(***************************************************)
(*********** KRYSTYNA GASINSKA (grupa 3) ***********)
(********* ZADANIE 1 ARYTMETYKA 18.11.2020 *********)
(* code review w parze z: Piotr Kaminski (grupa 1) *)
(***************************************************)

open Float;;

(*Typ wartosc. Zbior mozliwych wartosci liczby: zbior pusty, przedzial lub dopelnienie.
Zakres (x, y) oznacza zbior wartosci (x; y), x<=y, Dopelnienie (x, y) oznacza zbior wartosci: (-inf;x)u(y;inf), x<y *)
type wartosc = 
  | Pusty 
  | Zakres of float * float 
  | Dopelnienie of float * float;;

(*Wyjatek podnoszony w przypadku, gdy pewne funkcje otrzymaja argument niezgodny z ich dziedzina.
Stworzony, aby nie pojawial sie warning: pattern-matching (patrz: funkcje suma4 i suma)
Podczas prawidłowego wykonania programu wyjatek nie powinien zostac podniesiony.*)
exception Matching_error of wartosc;;

(*FUNKJE POMOCNICZE:*) 
(*zwraca wartosc bezwzgledna liczby x*)
let abs x = 
  if x > 0.0 then x 
  else (x *. (-1.0));;

(*zwraca minimum wartosci z listy*)
(*funkcja miary procedury rekurencyjnej "pom" jest dlugosc listy lspom, ktora przy pierwszym wywolaniu ma taka dlugosc, jak
lista ls bedaca argumentem funkcji min, a po zakonczeniu dzialania pom, ma dlugosc 0.*)
let min ls =
  let rec pom lspom mintmp =
    match lspom with
    | [] -> mintmp 
    | glowa :: [] -> if (mintmp>glowa) then glowa else mintmp 
    | glowa :: ogon -> if (mintmp>glowa) then pom ogon glowa else pom ogon mintmp 
  in pom ls infinity;;

(*zwraca maximum wartosci z listy*)
(*funkcja miary procedury rekurencyjnej "pom" jest dlugosc listy lspom, ktora przy pierwszym wywolaniu ma taka dlugosc, jak
lista ls bedaca argumentem funkcji max, a po zakonczeniu dzialania pom, ma dlugosc 0.*)
let max ls =
  let rec pom lspom maxtmp =
    match lspom with
    | [] -> maxtmp 
    | glowa :: [] -> if (maxtmp<glowa) then glowa else maxtmp 
    | glowa :: ogon -> if (maxtmp<glowa) then pom ogon glowa else pom ogon maxtmp 
  in pom ls neg_infinity;;

(*zwraca minimum tych wartosci z listy ls, które są większe od -inf. Jeśli wszystkie są równe -inf lub inf, funkcja zwraca -inf*)
(*funkcja miary procedury rekurencyjnej "pom" jest dlugosc listy lspom, ktora przy pierwszym wywolaniu ma taka dlugosc, jak
lista ls bedaca argumentem funkcji mininf, a po zakonczeniu dzialania pom, ma dlugosc 0.*)
let mininf ls =
    let rec pom lspom mintmp =
      match lspom with
      | [] -> (if (mintmp=infinity) then neg_infinity else mintmp) 
      | glowa :: ogon -> if (mintmp>glowa && glowa<>neg_infinity) then pom ogon glowa else pom ogon mintmp 
    in pom ls infinity;;

(*zwraca maximum tych wartosci z listy ls, które są mniejsze od inf. Jeśli wszystkie są równe -inf lub inf, funkcja zwraca inf*)
(*funkcja miary procedury rekurencyjnej "pom" jest dlugosc listy lspom, ktora przy pierwszym wywolaniu ma taka dlugosc, jak
lista ls bedaca argumentem funkcji maxinf, a po zakonczeniu dzialania pom, ma dlugosc 0.*)
let maxinf ls =
      let rec pom lspom maxtmp =
        match lspom with
        | [] -> (if (maxtmp=neg_infinity) then infinity else maxtmp) 
        | glowa :: ogon -> if (maxtmp<glowa && glowa<>infinity) then pom ogon glowa else pom ogon maxtmp 
      in pom ls neg_infinity;;

(*przeciwny x - zwraca zbior wartosci { -a  : in_wartosc x a }*)
let przeciwny w =
  match w with 
  | Zakres (a, b) -> Zakres ((-1.)*.b, (-1.)*.a)
  | Dopelnienie (c, d) -> Dopelnienie ((-1.)*.d, (-1.)*.c)
  | Pusty -> Pusty;;

(*odwrotny x - zwraca zbior wartosci { 1/a  : in_wartosc x a }*)
let odwrotny w =
  match w with 
  | Zakres (a, b) ->
   (if (a=0. && b>0.) then (Zakres(1./.b, infinity)) else 
   if (a<0. && b=0.) then (Zakres(neg_infinity, 1./.a)) else 
   if (a*.b>0.) then (Zakres(min[1./.a;1./.b], max[1./.a;1./.b])) else 
   if (a=0. && b=0.) then Pusty else (Dopelnienie(min[1./.a;1./.b], max[1./.a;1./.b])))
  | Dopelnienie (a, b) -> 
   (if (a=0. && b>0.) then (Zakres(neg_infinity, 1./.b)) else 
   if (a<0. && b=0.) then (Zakres(1./.a, infinity)) else 
   if (a*.b>0.) then (Dopelnienie(min[1./.a;1./.b], max[1./.a;1./.b])) else 
   if (a=0. && b=0.) then Zakres (neg_infinity, infinity) else (Zakres(min[1./.a;1./.b], max[1./.a;1./.b])))
  | Pusty -> Pusty;;

(* suma - zwraca sume mnogosciowa zbiorow w1 w2, gdzie w1 i w2 mają postać (-inf;a) lub (b;inf) 
Zauwazmy, ze jesli w1 i w2 maja poprawna postac - wyjatek nie powinien zostac podniesiony *)
let suma w1 w2 =
    match w1 with 
     |Zakres (a,b) -> 
       (match w2 with 
       |Zakres (c,d) -> 
        (if (classify_float(a)=FP_infinite && classify_float(d)=FP_infinite) then 
        if (b<c) then Dopelnienie(b,c) else Zakres (neg_infinity,infinity) else
        if (classify_float(a)=FP_infinite && classify_float(c)=FP_infinite) then 
        Zakres (neg_infinity,max [b;d]) else 
        if (classify_float(b)=FP_infinite && classify_float(d)=FP_infinite) then 
        Zakres (min[a;c],infinity) else
        if (a>d) then Dopelnienie(d,a) else Zakres (neg_infinity,infinity))
       |Dopelnienie (c,d) -> raise (Matching_error w2)
       |Pusty -> Pusty)
     |Dopelnienie (c,d) -> raise (Matching_error w2)
     |Pusty -> Pusty

(* suma - zwraca sume mnogosciowa zbiorow w1 w2 w3 w4, gdzie w1 w2 w3 w4 mają postać (-inf;a) lub (b;inf)
Zauwazmy, ze jesli w1 i w2 maja poprawna postac - wyjatek nie powinien zostac podniesiony *)
let suma4 w1 w2 w3 w4 =
  match w1 with 
  | Zakres (a,b) -> 
    (match w2 with 
    | Zakres (c,d) -> 
      (match w3 with 
      | Zakres (e,f) -> 
        (match w4 with  
        | Zakres (g,h) -> 
          (if ((classify_float(a)=FP_infinite && classify_float(b)=FP_infinite) || (classify_float(c)=FP_infinite && classify_float(d)=FP_infinite) || (classify_float(e)=FP_infinite && classify_float(f)=FP_infinite)|| (classify_float(g)=FP_infinite && classify_float(h)=FP_infinite)) then (Zakres(neg_infinity,infinity)) else 
          if ((mininf[a;c;e;g])<=(maxinf[b;d;f;h])) then Zakres(neg_infinity,infinity) else Dopelnienie((maxinf[b;d;f;h]),(mininf[a;c;e;g])))
        | Dopelnienie (g,h) -> raise (Matching_error w2)
        | Pusty -> Pusty)
      | Dopelnienie (e,f) -> raise (Matching_error w2)
      | Pusty -> Pusty)
    | Dopelnienie (c,d) -> raise (Matching_error w2)
    | Pusty -> Pusty)
  | Dopelnienie (a,b) -> raise (Matching_error w2)
  | Pusty -> Pusty;;

(*FUNKCJE:*)
(*1. konstruktory*)
(* wartosc_dokladnosc - zwraca typ "wartosc" - "zakres": x +/- p%. Warunek poczatkowy: p>0 *)
let wartosc_dokladnosc x p = 
  assert (p >= 0.);
  Zakres (x-.(p/.100.)*.abs(x), x+.(p/.100.)*.abs(x));;

(* wartpsc_od_do - zwraca typ "wartosc" - "zakres": od x do y. Warunek poczatkowy: x<=y *)
let wartosc_od_do x y = 
  assert (x <= y);
  Zakres (x, y);;

(* wartosc_dokladna - zwraca typ "wartosc" - "zakres": od x do x *)
let wartosc_dokladna x = 
  Zakres (x, x);;

(*2. selektory*)
(* in_wartosc - zwraca true jesli wartosc w moze byc rowna x (x zawiera sie w zbiorze w) *)
let in_wartosc w x = 
  if (classify_float(x)=FP_nan) then false else
  match w with
  | Pusty -> false
  | Zakres (a, b) -> if (x<a || x>b) then false else true
  | Dopelnienie (a, b) -> if (x>a && x<b) then false else true;;

(* min_wartosc - zwraca najmniejsza mozliwa wartosc liczby lub neg_infinity jesli zbior nie jest ograniczony od dolu *)
let min_wartosc w =
  match w with
  | Pusty -> infinity
  | Zakres (a, b) -> a
  | Dopelnienie (a, b) -> neg_infinity;;

(* max_wartosc - zwraca najwieksza mozliwa wartosc liczby lub infinity jesli zbior nie jest ograniczony od gory *)
let max_wartosc w =
  match w with
  | Pusty -> neg_infinity
  | Zakres (a, b) -> b
  | Dopelnienie (a, b) -> infinity;;

(* sr_wartosc - zwraca srednia arytmetyczna min_wartosc i max_wartosc przedzialu lub nan jesli nie są one okreslone lub nie sa skonczone *)
let sr_wartosc w =
  match w with
  | Pusty -> nan
  | Zakres (a, b) -> if (classify_float(a)=FP_infinite && classify_float(b)=FP_infinite) then nan else ((a+.b)/.2.)
  | Dopelnienie (a, b) -> nan (* krancowe wartosci przedzialu nie sa skonczone *)
 
(*modyfikatory*)
(* plus x y - zwraca zbior wartosci { a + b : in_wartosc x a ∧ in_wartosc y b } *) 
let plus w1 w2 =
  let rec plus2 w1 w2 =
    match w1 with
    | Pusty -> Pusty 
    | Zakres (a, b) -> (match w2 with
      | Pusty -> Pusty
      | Zakres (c, d) -> 
        Zakres (min [(a+.c); (a+.d); (b+.c); (b+.d)], max[(a+.c); (a+.d); (b+.c); (b+.d)])
      | Dopelnienie (c, d) -> 
        if (c+.b>=d+.a) then Zakres (neg_infinity, infinity) else
        Dopelnienie (c+.b, d+.a))
    | Dopelnienie (e, f) -> (match w2 with
      | Pusty -> Pusty
      | Zakres (g, h) -> plus2 w2 w1
      | Dopelnienie (g, h) -> 
        if (e+.h>=f+.g || e+.h<f+.g) then Zakres (neg_infinity, infinity) else
        Dopelnienie (e+.h, e+.g))
    in plus2 w1 w2;;

(* minus x y - zwraca zbior wartosci { a - b : in_wartosc x a ∧ in_wartosc y b } *) 
let minus w1 w2 =
  plus w1 (przeciwny w2);;

(* razy x y - zwraca zbior wartosci { a * b : in_wartosc x a ∧ in_wartosc y b } *) 
let razy w1 w2 =
  let rec razy2 w1 w2 =
    match w1 with
    | Pusty -> Pusty 
    | Zakres (a, b) -> 
      (match w2 with
      | Pusty -> Pusty
      | Zakres (c, d) -> 
        if ((a=b && a=0.) || (c=d && c=0.)) then (Zakres(0.,0.)) else
        Zakres (min [(a*.c); (a*.d); (b*.c); (b*.d)], max[(a*.c); (a*.d); (b*.c); (b*.d)])
      | Dopelnienie (c, d) -> 
        if (a=b && a=0.) then (Zakres(0.,0.)) else
        (suma (razy2 w1 (Zakres(neg_infinity,c))) (razy2 w1 (Zakres(d,infinity)))))
    | Dopelnienie (e, f) -> (match w2 with
      | Pusty -> Pusty
      | Zakres (g, h) -> razy2 w2 w1
      | Dopelnienie (g, h) -> 
        (suma4
        (razy2 (Zakres(neg_infinity,e)) (Zakres(neg_infinity,g)))
        (razy2 (Zakres(neg_infinity,e)) (Zakres(h,infinity)))
        (razy2 (Zakres(f,infinity)) (Zakres(neg_infinity,g)))
        (razy2 (Zakres(f,infinity)) (Zakres(h,infinity)))))
    in razy2 w1 w2;;

(* podzielic x y - zwraca zbior wartosci { a/b : in_wartosc x a ∧ in_wartosc y b } *) 
let podzielic w1 (w2:wartosc) =
  if (w2=Zakres (0.0,0.0)) then Pusty else 
  razy w1 (odwrotny w2);;