-module(pollution_gen_server_test).
-author("Svatopluk").
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").


start_test()->
  pollution_gen_server:start().

finish_test()->
  pollution_gen_server:stop().

all_tests()->
  start_test(),
  pollution_gen_server:addStation("Pies", {12,12}),
  pollution_gen_server:addStation("Wiewiorka", {0,0}),
  pollution_gen_server:addValue("Pies", {{2020, 1, 1}, {11,11,11}}, "PM10", 3),
  pollution_gen_server:addValue({12, 12}, {{2020, 1, 3}, {5, 3, 1}}, "PM10", 9),
  pollution_gen_server:addValue("Wiewiorka", {{2020, 1, 1}, {7, 8, 9}}, "PM10", 1),
  pollution_gen_server:addValue("Wiewiorka", {{2020, 1, 3}, {4, 5, 6}}, "PM10", 5),
  pollution_gen_server:addValue("Pies", {{2020, 5, 1}, {1, 2, 3}}, "PM10", 5),
  pollution_gen_server:addStation("Ryba", {9,9}),

  pollution_gen_server:addValue("Ryba", {{2020, 5, 1}, {1, 2, 3}}, "PM10", 100),
  pollution_gen_server:addValue("Wiewiorka", {{2020, 5, 1}, {4, 5, 6}}, "PM10", 3),
[
  ?assertEqual({error,'Name is supposed to be a list!'}, pollution_gen_server:addStation(1,{1,2})),
  ?assertEqual({error, 'Incorect coordinates!'}, pollution_gen_server:addStation("Kot", "Kot")),
  ?assertEqual({error, 'Cannot add, station with such parameters has been added!'}, pollution_gen_server:addStation("Pies", {12,12})),
  ?assertEqual({error, 'Cannot add, station with such parameters has been added!'}, pollution_gen_server:addStation("Lis", {0,0})),
  ?assertEqual({error, 'Cannot add, measurments with such parameters have been added!'}, pollution_gen_server:addValue( "Pies", {{2020, 1, 1}, {11, 11, 11}}, "PM10", 3)),
  ?assertEqual({error, 'There is no station with such parameters'}, pollution_gen_server:addValue({1, 1}, {{2020, 1, 1}, {11, 11, 11}}, "PM10", 3)),
  ?assertEqual({error, 'There is no station with such parameters'}, pollution_gen_server:removeValue("Kura", {{2020, 1,1}, {11, 11, 11}}, "PM10")),
  ?assertEqual(2.0, pollution_gen_server:getDailyMean({2020, 1, 1}, "PM10")),
  pollution_gen_server:removeValue("Pies", {{2020, 1, 1}, {11, 11, 11}}, "PM10"),
  ?assertEqual(1.0, pollution_gen_server:getDailyMean({2020, 1, 1}, "PM10")),
  ?assertEqual({error, 'No station with such parameters'}, pollution_gen_server:getOneValue({2,5}, {{2020, 1, 1}, {11, 11, 11}}, "PM10")),
  ?assertEqual(100, pollution_gen_server:getOneValue({9,9}, {{2020, 5, 1}, {1, 2, 3}}, "PM10")),
  ?assertEqual(3.0, pollution_gen_server:getStationMean("Wiewiorka", "PM10")),
  ?assertEqual({error, 'No station with such parameters'}, pollution_gen_server:getStationMean("Rekin", "PM10")),
  ?assertEqual(100, pollution_gen_server:getDailyPeak({2020, 5, 1}, "PM10")),
  ?assertEqual('No value', pollution_gen_server:getOneValue("Pies", {{2019, 1, 1}, {1, 1, 1}}, "PM10"))
].
