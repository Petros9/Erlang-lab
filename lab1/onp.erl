-module(onp).
-author("Svatopluk").


-export([calconp/1]).

calconp(Text) ->
  calconp(string:tokens(Text, " "), []).

calconp([], [Result]) -> Result;

calconp(["+" | T1], [A, B | T2]) -> calconp(T1, [A + B | T2]);

calconp(["-" | T1], [A,B | T2]) -> calconp(T1, [B - A | T2]);

calconp(["*" | T1], [A,B | T2]) -> calconp(T1, [A * B | T2]);

calconp(["/" | T1], [A,B | T2]) -> calconp(T1, [B / A | T2]);

calconp(["^" | T1], [A,B | T2]) -> calconp(T1, [ lab1:power(B,A) | T2]);

calconp(["sqrt" | T1], [A | T2]) -> calconp(T1, [ math:sqrt(A) | T2]);

calconp(["sin" | T1], [A | T2]) -> calconp(T1, [ math:sin(A) | T2]);

calconp(["cos" | T1], [A | T2]) -> calconp(T1, [ math:cos(A) | T2]);

calconp(["tg" | T1], [A | T2]) -> calconp(T1, [ math:tan(A) | T2]);

calconp(["ctg" | T1], [A | T2]) -> calconp(T1, [1/math:tan(A) | T2]);

calconp([H | T], S) ->
  if is_number(H) ->  calconp(T, [H | S]);
    true ->
      Ok = string:str(H, ".") > 0,
      if Ok -> calconp(T, [list_to_float(H) | S]);
        true -> calconp(T, [list_to_integer(H) | S])
      end
  end.

