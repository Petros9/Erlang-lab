-module(pollution).
-author("Svatopluk").

% {(wpółrzędne geograficzne), nazwa stacji pomiarowych, poziom zanieczyszczenia(dwa rozne)/temperatura, data i godzina pomiaru} jako krotka
% nie powinno być możliwe:
% - dodanie dwoch staji o tej samej nazwie lub wspolrzednych
% - dananie pomiarow o tych samych wspolrzednych, czasie, type
% - dodanie pomiaru do nieistniejacej stacji

% Stacja jest listą: imie, wspolrzedne, wartosci (wartosci sa zrealizowane jako hashmapa)

-export([createMonitor/0, addStation/3, addValue/5, removeValue/4, getOneValue/4,
  getStationMean/3, getDailyMean/3, addMeasurement/4, getDailyPeak/3, getMean/2,
  getBelowTheAverage/2]).

-define(coordCorrect(Coord), (is_tuple(Coord) andalso (size(Coord)==2) andalso is_number(element(1, Coord)) andalso is_number(element(2, Coord)))).
-record(key, {datetime, type}).
createMonitor() -> [].

% nazwa, wspolrzedne geograficzne, lista stacji
% w poniszej funkcji sprawdzamy, czy stacja z danymi parametami juz istnieje, jesli nie, to dodajemy jej do listy
addStation(Name, _, _) when not is_list(Name) -> throw('Name is supposed to be a list!');
addStation(_,Coord,_) when not ?coordCorrect(Coord) -> throw('Incorect coordinates!');
addStation(Name, {Xcoord, Ycoord}, StationList) ->
  DoesInvolve = fun(#{name := TName, coord := {TXcoord, TYcoord}})-> (TName == Name) or ({TXcoord, TYcoord} == {Xcoord, Ycoord})end,
  case lists:any(DoesInvolve, StationList) of
    false -> [ #{name => Name, coord => {Xcoord, Ycoord}, measurements => #{}} | StationList];
    _ -> throw('Cannot add, station with such parameters has been added!')
  end.


% wspolrzedne/nazwa, data, typ pomiaru, wartosc, lista
% w poniszczych funkcjach najpierw ustalamy daną stację, do której chcemy dodać pomiar (w addValue)
% w addMesurment dodajemy pomiar
addMeasurement(Date, Type, Value, Measurements) ->
  DoesInvolve = maps:is_key(#key{datetime = Date, type = Type}, Measurements),
  case DoesInvolve of
    false ->  maps:put(#key{datetime = Date, type = Type}, Value, Measurements);
    _ -> throw('Cannot add, measurments with such parameters have been added!')
  end.

addValue(_,_,_,_,[]) -> throw('There is no station with such parameters');
addValue(Name, Date, Type, Measurement, [Station = #{name := Name, measurements := Measurements} | StationList]) ->
  [Station#{measurements => addMeasurement(Date, Type, Measurement, Measurements)} | StationList];
addValue(Coord, Date, Type, Measurement, [Station = #{coord := Coord, measurements := Measurements} | StationList]) ->
  [Station#{measurements => addMeasurement(Date, Type, Measurement, Measurements)} | StationList];
addValue(ID, Date, Type, Measurement, [H |StationList]) -> [H| addValue(ID, Date, Type, Measurement, StationList)].


% wspolrzedne/nazwa, data, typ pomiaru, lista
% ponizsza funkcja najpeirw ustala stacje, z ktorej usunac mamy pomiar
% po ustaleniu stacji usuwamy wartosc, która kryje się pod kluczem rownym podanym parametrom

removeValue(_, _, _, []) ->throw('There is no station with such parameters');
removeValue(Name, Date, Type, [Station = #{name := Name, measurements := Measurements} | StationList]) ->
  [Station#{measurements => maps:remove(#key{datetime = Date, type = Type}, Measurements)} | StationList];
removeValue(Coord, Date, Type, [Station = #{coord := Coord, measurements := Measurements} | StationList]) ->
  [Station#{measurements => maps:remove(#key{datetime = Date, type = Type}, Measurements)} | StationList];
removeValue(ID, Date, Type, [H |StationList]) -> [H| removeValue(ID, Date, Type, StationList)].

% wsporzedne/nazwa, data, typ, lista
% analogicznie jak w poprzedniej funkcji, tylko zamiast usuwac wartosci, to jes pobieramy funkcją get

getOneValue(_, _, _, [])  -> throw('No station with such parameters');
getOneValue(Name, Date, Type,[#{name := Name, measurements := Measurments} | _ ])  ->
  maps:get(#key{datetime = Date, type = Type}, Measurments, 'No value');
getOneValue(Coord, Date, Type,[#{coord := Coord, measurements := Measurments} | _ ])  ->
  maps:get(#key{datetime = Date, type = Type}, Measurments, 'No value');
getOneValue(ID, Date, Type, [_| StationList]) -> getOneValue(ID, Date, Type, StationList).


% wsporzedne/nazwa, parametr, lista

% w tej funkcji filtrowane sa wartosci z hashmapy, których kategoria zgadza sie z podaną kategorią parametrem
% nastepnie wartosci sa sumowane i wyliczna jest srednia
getMean(Type, Measurments) ->
  Filtered = maps:filter(fun(#key{type = T}, _) -> T==Type end, Measurments),
  Sum = maps:fold( fun(_, V, Acc) -> V+Acc end, 0, Filtered),
  case maps:size(Filtered) of
    0 -> 0;
    _-> Sum/maps:size(Filtered)
  end.

getStationMean(_, _, []) -> throw('No station with such parameters');
getStationMean(Name, Type, [#{name := Name, measurements := Measurments} | _]) -> getMean(Type, Measurments);
getStationMean(Coord, Type,[#{coord := Coord, measurements := Measurments} | _ ])  -> getMean(Type, Measurments);
getStationMean(ID, Type, [_|StationList]) -> getStationMean(ID, Type, StationList).

% daily mean
% kazdej stacji z listy sprawdzamy czy posiada ona pomiary w hahsmapie, ktore nas interesuja (funkcja maps:filter)
% nastepnie z tych wszytkich wartosci tworzona jest jedna lista Values
% średnia wyliczamy z listy Values
getDailyMean(Day, Type, StationList) ->
  TheDayValues = fun(#{measurements := Measurments},Type) ->
    maps:values(maps:filter(fun(#key{datetime = {TDay, _}, type = TType},_) -> (TDay == Day) andalso (TType==Type)end, Measurments))end,
  Values = lists:foldl(fun(Station, Acc)-> Acc++TheDayValues(Station, Type) end ,[], StationList),
  case length(Values) of
    0 -> 0;
    _ -> lists:sum(Values)/length(Values)
  end.

% NIESPODZIANKA
% get aplitude max i min w danym dniu
% identycznie jak w powyższej funkcji tworzymy listę TheValues, a następnie za pomocą funkcji foldl wynajdujemy wartość najwieksza
getDailyPeak(Day ,Type, StationList) ->
  TheValues = fun(#{measurements := Measurments},Type) ->
    maps:values(maps:filter(fun(#key{datetime = {TDay, _}, type = TType},_) -> (TDay == Day) andalso (TType==Type)end, Measurments))end,
  Values = lists:foldl(fun(Station, Acc)-> Acc++TheValues(Station, Type) end ,[], StationList),
  MaxVal = lists:foldl(fun(Val, Acc)-> max(Val, Acc) end ,0,Values),
  MaxVal.

% stacje z pomiarami ponizej sredniej
% funkcja getTotalMean ma za zadanie policzyć srednia ze wszystkich stacji za wszystkich pomiarow
% funkcja getBelowTheAverage zwraca liste stacji, których calosciowa srednia jest ponizej sredniej pomiarów ze wszysktich stacji
getTotalMean(Type, StationList)->
  TheDayValues = fun(#{measurements := Measurments},Type) ->
    maps:values(maps:filter(fun(#key{type = TType},_) ->(TType==Type)end, Measurments))end,
  Values = lists:foldl(fun(Station, Acc)-> Acc++TheDayValues(Station, Type) end ,[], StationList),
  case length(Values) of
    0 -> 0;
    _ -> lists:sum(Values)/length(Values)
  end.

getBelowTheAverage(Type, StationList)->
  GetHelper = fun(#{name := Name},Type, StationList) -> getStationMean(Name, Type, StationList)end,
  [Station || Station <-StationList, GetHelper(Station, Type, StationList) < getTotalMean(Type,StationList)].