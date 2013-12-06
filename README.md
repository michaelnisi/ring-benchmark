# ring-benchmark

In his book *Programming Erlang*, Joe Armstrong demands:

> Write a ring benchmark. Create N processes in a ring. Send a message round the ring M times so that a total of N * M messages get sent. Time how long this takes for different values of N and M.

So, here goes:
```Erlang
-module(ring).
-export([send/2]).

%% @doc Send M messages through a ring of N processes.
send(M, N) ->
  statistics(runtime),
  H = lists:foldl(
    fun(Id, Pid) -> spawn_link(fun() -> loop(Id, Pid, M) end) end,
    self(),
    lists:seq(N, 2, -1)),
  {_, Time} = statistics(runtime),
  io:format("~p processes spawned in ~p ms~n", [N, Time]),
  statistics(runtime),
  H ! M,
  loop(1, H, M).

loop(Id, Pid, M) ->
  receive
    1 ->
      {_, Time} = statistics(runtime),
      io:format("~p messages sent in ~p ms~n", [M, Time]),
      exit(self(), ok);
    Index ->
      Pid ! Index - 1,
      loop(Id, Pid, M)
  end.
```

To try this in shell:
```
$ erl
> c(ring).
{ok,ring}
> ring:send(100000,10000).
10000 processes spawned in 50 ms
100000 messages sent in 170 ms
** exception exit: ok
3>
```
