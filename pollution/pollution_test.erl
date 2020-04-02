
-module(pollution_test).
-author("Svatopluk").

-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

total_test()->
  P = pollution:createMonitor(),
  P1 = pollution:addStation("Kangur", {0, 0}, P),
  P2 = pollution:addValue("Kangur", calendar:local_time(),"PM10", 0,P1),
  P3 = pollution:addStation("Koala", {0, 1}, P2),
  P4 = pollution:addValue("Koala", calendar:local_time(),"PM10", 1, P3),
  P5 = pollution:addValue("Koala", {{2020,2,1},{1,2,3}},"PM10", 2, P4),
  P6 = pollution:addValue("Koala", {{2020,2,1},{1,2,5}},"PM10", 9, P5),
  P7 = pollution:removeValue("Koala", {{2020,2,1},{1,2,3}},"PM10", P6),

  [ ?assertThrow('Name is supposed to be a list!', pollution:addStation(123, {1, 1}, P2)),
    ?assertThrow('Incorect coordinates!', pollution:addStation("Mysz", {"erlang", 1}, P2)),
    ?assertThrow('Cannot add, station with such parameters has been added!', pollution:addStation("Koala", {1, 2}, P7)),
    ?assertThrow('Cannot add, station with such parameters has been added!', pollution:addStation("Szczur", {0, 0}, P7)),
    ?assertEqual(0.5, pollution:getDailyMean({2020,04,02},"PM10", P7)), %sprawdzamy dzialie dailyMean
    ?assertEqual(5.0, pollution:getStationMean("Koala","PM10", P7)), %spradzamy, czy dobrze liczy oraz czy usunal wczesniej wartosc 2
    ?assertThrow('No station with such parameters',pollution:getStationMean("Leopard","PM10", P7)),
    ?assertEqual(P2,pollution:getBelowTheAverage("PM10", P7)), %funkcja niespodziankowa
    ?assertEqual(9,pollution:getOneValue("Koala",{{2020,2,1},{1,2,5}},"PM10",P7)),
    pollution:getDailyPeak({2020,2,1}, "PM10", P7) % nie umiem napisac testu assert, ale najwysza wartoscia tegodnia jest 9
].