-module(pollution_gen_server).
-author("Svatopluk").

-behaviour(gen_server).

-export([start_link/1, start/0, clean/0, crash/0, stop/0, init/1,
  handle_cast/2, handle_call/3, terminate/2]).

-export([addStation/2, addValue/4, removeValue/3, getOneValue/3,
  getStationMean/2, getDailyMean/2, getDailyPeak/2,
  getBelowTheAverage/1]).

-define(SERVER, ?MODULE).


%pollution functions

addStation(Name, Coord) ->
  gen_server:call(?MODULE, {addStation, Name, Coord}).

addValue(ID, Date, Type, Measurement)->
  gen_server:call(?MODULE,{addValue, ID, Date, Type, Measurement}).

removeValue(ID, Date, Type)->
  gen_server:call(?MODULE, {removeValue, ID, Date, Type}).

getOneValue(ID, Date, Type)->
  gen_server:call(?MODULE, {getOneValue, ID, Date, Type}).

getStationMean(ID, Type)->
  gen_server:call(?MODULE, {getStationMean, ID, Type}).

getDailyMean(Day, Type)->
  gen_server:call(?MODULE, {getDailyMean, Day, Type}).

getDailyPeak(Day, Type)->
  gen_server:call(?MODULE, {getDailyPeak, Day, Type}).

getBelowTheAverage(Type)->
  gen_server:call(?MODULE, {getBelowTheAverage, Type}).

% gen_server functions
start_link(InitialState)->
  gen_server:start_link({local, pollution_gen_server}, ?MODULE, InitialState, []).

init(InitialState)->
  {ok, InitialState}.

crash()->
  gen_server:cast(?MODULE, {crash}).

start()->
  start_link(pollution:createMonitor()).

stop()->
  gen_server:cast(?MODULE, {stop}).

clean()->
  gen_server:call(?MODULE, {clean}).


% pollution and non pollution handlers

handle_call({addStation, Name, Coord}, _FROM, Monitor)->
  {Status, ResultMonitor} = try pollution:addStation(Name, Coord, Monitor) of
                           NewMonitor -> {ok, NewMonitor}
                            catch
                              Throw -> {{error, Throw}, Monitor}
                         end,
  {reply, Status, ResultMonitor};

handle_call({addValue, ID, Date, Type, Value}, _FROM, Monitor)->
  {Status, ResultMonitor} = try pollution:addValue(ID, Date, Type, Value, Monitor) of
                           NewMonitor -> {ok, NewMonitor}
                            catch
                              Throw -> {{error, Throw}, Monitor}
                         end,
  {reply, Status, ResultMonitor};

handle_call({removeValue, ID, Date, Type}, _FROM, Monitor)->
  {Status, ResultMonitor} = try  pollution:removeValue(ID, Date, Type, Monitor) of
                           NewMonitor -> {ok, NewMonitor}
                            catch
                              Throw -> {{error, Throw}, Monitor}
                         end,
  {reply, Status, ResultMonitor};

handle_call({getOneValue, ID, Date, Type}, _FROM, Monitor)->
  Result = try pollution:getOneValue(ID, Date, Type, Monitor) of
             Number -> Number
           catch
             Throw -> {error, Throw}
           end,
  {reply, Result, Monitor};

handle_call({getStationMean, ID, Type}, _FROM, Monitor)->
  Result = try pollution:getStationMean(ID, Type, Monitor) of
             Number -> Number
           catch
             Throw -> {error, Throw}
           end,
  {reply, Result, Monitor};

handle_call({getDailyMean, Day, Type}, _FROM, Monitor)->
  Result = pollution:getDailyMean(Day, Type, Monitor),
  {reply, Result, Monitor};

handle_call({getDailyPeak, Day, Type}, _FROM, Monitor)->
  Result = pollution:getDailyPeak(Day, Type, Monitor),
  {reply, Result, Monitor};

handle_call({getBelowTheAverage, Type}, _FROM, Monitor)->
  {Status, Stations} = {ok, pollution:getBelowTheAverage(Type, Monitor)},
  {reply, Status, Stations};

handle_call({clean}, _FROM, _Monitor)->
  {Status, NewMonitor} = {ok, pollution:createMonitor()},
  {reply, Status, NewMonitor}.

handle_cast({crash}, State)->
  1/0,
  {noreply, State};

handle_cast({stop}, State)->
  {stop, normal, State}.

terminate(_Reason, _State)->
  ok.
