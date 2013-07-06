# ring-benchmark

Well, a ring benchmark ...

> Write a ring benchmark. Create N processes in a ring. Send a message round the ring M times so that a total of N * M messages get sent. Time how long this takes for different values of N and M.

> Write a similar program in some other programming language you are familiar with. Compare the results. Write a blog, and publish the results on the Internet!

    -module(ring1).
    -export([start/2, loop/3]).

    start(N, M) ->
      statistics(runtime),
      H = lists:foldl(
        fun(Id, Pid) -> spawn_link(?MODULE, loop, [Id, Pid, M]) end,
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

Let's try this again. This time a real ring:

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

As a Node module:

    var cluster = require('cluster')
      , http = require('http')

    module.exports = function (n, m) {
      if (cluster.isMaster) {
        console.time('in')
        var worker, ni = n, mi = m
        while (ni--) worker = new Worker(worker)
        while (mi--) worker.hello()

        cluster.on('exit', function (worker, code, signal) {
          if (!Object.keys(cluster.workers).length) {
            console.log('%d messages sent through ring', n * m)
            console.timeEnd('in')
          }
        })
      } else {
        var mCount = 0
        process.on('message', function (msg) {
          if (++mCount >= m) {
            cluster.worker.kill()
          }
        })
      }
    }

    function Worker(next) {
      var me = cluster.fork()
      me.hello = function () {
        me.send('hello')
        if (next) {
          next.hello()
        }
      }
      return me
    }


