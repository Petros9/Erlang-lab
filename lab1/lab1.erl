-module(lab1).

-export([power/2]).

power(N,0) ->
  1;
power(N,M) ->
  N * power(N, M-1).
