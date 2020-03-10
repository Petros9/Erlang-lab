-module(onp).
-author("Svatopluk").


-export([calconp/1]).

calconp(Text) ->
  calconp(string:tokens(Text, " "), []).

calconp([], [Result]) ->
  Result;

calconp(["+" | T1], [A, B | T2]) ->
  if is_number(A) ->
    if is_number(B)->
      calconp(T1, [A + B | T2]);
    true ->
      calconp(T1, [A + list_to_integer(B) | T2])
    end;
  true ->
    if is_number(B)->
      calconp(T1, [list_to_integer(A) + B | T2]);
      true ->
        calconp(T1, [list_to_integer(A) + list_to_integer(B) | T2])
    end
  end;

calconp(["-" | T1], [A,B | T2]) ->
  if is_number(A) ->
    if is_number(B)->
      calconp(T1, [B - A | T2]);
      true ->
        calconp(T1, [list_to_integer(B) - A | T2])
    end;
    true ->
      if is_number(B)->
        calconp(T1, [B - list_to_integer(A) | T2]);
        true ->
          calconp(T1, [list_to_integer(B) - list_to_integer(A) | T2])
      end
  end;

calconp(["*" | T1], [A,B | T2]) ->
  if is_number(A) ->
    if is_number(B)->
      calconp(T1, [A * B | T2]);
      true ->
        calconp(T1, [A * list_to_integer(B) | T2])
    end;
    true ->
      if is_number(B)->
        calconp(T1, [list_to_integer(A) * B | T2]);
        true ->
          calconp(T1, [list_to_integer(A) * list_to_integer(B) | T2])
      end
  end;

calconp(["/" | T1], [A,B | T2]) ->
  if is_number(A) ->
    if is_number(B)->
      calconp(T1, [B / A | T2]);
      true ->
        calconp(T1, [list_to_integer(B) / A | T2])
    end;
    true ->
      if is_number(B)->
        calconp(T1, [ B / list_to_integer(A) | T2]);
        true ->
          calconp(T1, [list_to_integer(B) / list_to_integer(A)  | T2])
      end
  end;
calconp(["sqrt" | T1], [A,B | T2]) ->
  if is_number(A) ->
    if is_number(B)->
      calconp(T1, [B / A | T2]);
      true ->
        calconp(T1, [list_to_integer(B) / A | T2])
    end;
    true ->
      if is_number(B)->
        calconp(T1, [ B / list_to_integer(A) | T2]);
        true ->
          calconp(T1, [list_to_integer(B) / list_to_integer(A)  | T2])
      end
  end;


calconp(["^" | T1], [A,B | T2]) ->
  if is_number(A) ->
    if is_number(B)->
      calconp(T1, [ lab1:power(B,A) | T2]);
      true ->
        calconp(T1, [ lab1:power(list_to_integer(B), A) | T2])
    end;
    true ->
      if is_number(B)->
        calconp(T1, [ lab1: power(B, list_to_integer(A)) | T2]);
        true ->
          calconp(T1, [ lab1:power(list_to_integer(B), list_to_integer(A))  | T2])
      end
  end;

calconp(["sqrt" | T1], [A | T2]) ->
  if is_number(A) ->
    calconp(T1, [ math:sqrt(A) | T2]);
    true ->
      calconp(T1, [math:sqrt(list_to_integer(A)) | T2])
  end;

calconp(["sin" | T1], [A | T2]) ->
  if is_number(A) ->
    calconp(T1, [ math:sin(A) | T2]);
    true ->
      calconp(T1, [math:sin(list_to_integer(A)) | T2])
  end;

calconp(["cos" | T1], [A | T2]) ->
  if is_number(A) ->
    calconp(T1, [ math:cos(A) | T2]);
    true ->
      calconp(T1, [math:cos(list_to_integer(A)) | T2])
  end;

calconp(["tg" | T1], [A | T2]) ->
  if is_number(A) ->
      calconp(T1, [ math:tan(A) | T2]);
    true ->
      calconp(T1, [math:tan(list_to_integer(A)) | T2])
  end;

calconp(["ctg" | T1], [A | T2]) ->
  if is_number(A) ->
    calconp(T1, [ 1/(math:tan(A)) | T2]);
    true ->
      calconp(T1, [1/math:tan((list_to_integer(A))) | T2])
  end;

calconp([H | T], S) ->
  calconp(T, [H | S]).
