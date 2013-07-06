-module(ring).
-export([start/2]).

start(N, M) ->
  statistics(runtime),
  start(N, M, N, spawn_link(fun() -> loop(N, M, []) end)).

start(_, M, 0, Next) ->
  send(Next, hey, M);
start(N, M, NC, Next) ->
  start(N, M, NC-1, spawn_link(fun() -> loop(N, M, Next) end)).

send(Pid, _, 0) ->
  Pid ! done,
  Pid;
send(Pid, Message, M) ->
  Pid ! Message,
  send(Pid, Message, M-1).

loop(N, M, Next) ->
  receive
    done when is_pid(Next) ->
      Next ! done;
    done ->
      Z = N * M,
      {_, Time} = statistics(runtime),
      io:format("~p messages sent in ~p ms~n", [Z, Time]),
      exit(self(), ok);
    hey when is_pid(Next) ->
      Next ! hey,
      loop(N, M, Next);
    _ ->
      loop(N, M, Next)
  end.
