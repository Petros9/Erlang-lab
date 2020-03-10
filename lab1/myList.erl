
-module(myList).
-author("Svatopluk").

-export([contains/2, dupplicateElements/1, sumFloats/2]).

contains([],_) ->
  false;
contains([Head | Tail],X) ->
  case Head of
    X -> true;
    _ -> contains(Tail, X)
  end.

dupplicateElements([]) ->
  [];
dupplicateElements([H|T]) ->
  [H,H] ++ dupplicateElements(T).


sumFloats([],Sum) ->
  Sum;
sumFloats([H|T], Sum) ->
  sumFloats(T,H+Sum).
