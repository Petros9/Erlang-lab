
-module(qsort).
-author("Svatopluk").

-export([lessThan/2, grtEqThan/2, qs/1, randomElems/3, compareSpeeds/3]).

lessThan(List, Arg) -> [X || X<-List, X < Arg].
grtEqThan(List, Arg) -> [X || X<-List, X >= Arg].
qs([])->[];
qs([Pivot|Tail]) -> qs(lessThan(Tail,Pivot)) ++ [Pivot] ++ qs(grtEqThan(Tail,Pivot)).

randomElems(N,Min,Max)->[rand:uniform(Max-Min)+Min|| _<-lists:seq(1,N)].

compareSpeeds(List, Fun1, Fun2) ->
  {Time1, _} = timer:tc(Fun1, [List]),
  {Time2, _} = timer:tc(Fun2, [List]),
  io:format("Our quicksort: ~B and library sort: ~B~n", [Time1, Time2]).