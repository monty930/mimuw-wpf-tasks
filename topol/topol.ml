(**************************************************)
(********** KRYSTYNA GASINSKA (grupa 3) ***********)
(** ZADANIE 5 SORTOWANIE TOPOLOGICZNE 12.01.2021 **)
(*** code review w parze z: Iga Janik (grupa 6) ***)
(**************************************************)

(* wyjatek podnoszony gdy graf jest cykliczny *)
exception Cykliczne;;

(* typ reprezentujacy stan danego wierzcholka, uzyty w przeszukiwaniu wglab
- Nieodwiedzony, Odwiedzony (w danym wywolaniu przeszukiwania), Przetworzony
(po rozpatrzeniu synow) - taki wierzcholek nie jest elementem cyklu *)
type stan =
  | Nieodwiedzony
  | Odwiedzony
  | Przetworzony;;

(* tworz_mape g tworzy strukture typu polymorphic map. kluczami sa wierzcholki 
grafu g, a wartosciami: pary. pierwszy element pary to lista synow wierzcholka,
drugi - jego stan, dla wszystkich wierzcholkow - Nieodwiedzony. Zwraca pare:
(mapa, lista wszystkich wierzcholkow grafu - kazdy dokladnie raz).
wejsciowy graf ma postac [(a_1,[a_11;...;a_1n]); ...; (a_m,[a_m1;...;a_mk])]. *)
let tworz_mape g = 
  let wierzcholki = ref [] in
  let dodaj_wierzcholek m (k, v) =
    let (lst, st) = 
      try PMap.find k m with Not_found -> 
        begin
          wierzcholki := k::!wierzcholki;
          ([], Nieodwiedzony)
        end in
    PMap.add k (v@lst, Nieodwiedzony) m in
  let m = List.fold_left dodaj_wierzcholek PMap.empty g in 
  let dodaj_liste m (_, v) = 
    let dodaj_syna m s = 
      let (lst, st) = 
        try PMap.find s m with Not_found -> 
          begin
            wierzcholki := s::!wierzcholki;
            ([], Nieodwiedzony)
          end in
    PMap.add s (lst, Nieodwiedzony) m in
    List.fold_left dodaj_syna m v in
  (List.fold_left dodaj_liste m g, !wierzcholki);;

(* topol g zwraca liste wierzcholkow grafu g w kolejnosci topologicznej,
czyli: "dla danej listy [(a_1,[a_11;...;a_1n]); ...; (a_m,[a_m1;...;a_mk])] 
    zwraca liste, na ktorej kazdy z elementow a_i oraz a_ij wystepuje
    dokladnie raz i ktora jest uporzadkowana w taki sposob, ze kazdy
    element a_i jest przed kazdym z elementow a_i1 ... a_il".
jesli w grafie wystepuje cykl - podnoszony jest wyjatek. *)
let topol g =
  let wynik = ref [] 
  and (graf, wierzcholki) = 
    let (gr, wi) = tworz_mape g in
    (ref gr, wi) in
  let rec dfs w =
    let (lst, st) = try PMap.find w !graf with Not_found -> assert false in
    if st = Odwiedzony then raise Cykliczne 
    else if st = Nieodwiedzony then 
      begin
        graf := PMap.add w (lst, Odwiedzony) !graf;
        List.iter dfs lst;
        wynik := w::!wynik;
        graf := PMap.add w (lst, Przetworzony) !graf;
      end in
  List.iter dfs wierzcholki;
  !wynik;;

(* UWAGA. w powyższej implementacji:
wartosci a_1, a_2, ... a_m nie musza byc rozne oraz wartosci a_1, a_2, ... a_m nie
musza pokrywac wszystkich wierzcholkow grafu (moze sie zdazyc ze wierzcholek
pojawi sie tylko na liscie synow - jesli sam nie ma synow) np: 
[ (1, [3]); (1, [2]); (2, []); (2, []); (2, [3]) ]
jest, w powyższej implementacji, poprawna reprezentacja pewnego grafu o 
wierzcholkach 1, 2, 3. *)