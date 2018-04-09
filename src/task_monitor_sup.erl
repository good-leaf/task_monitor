-module(task_monitor_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([
	init/1,
	new/4,
	worker/2,
	workers/1,
	terminate_child/2
]).

-define(WORKER_NAME_ARGS(N, I, Args), {N, {I, 'start_link', Args}, temporary, 5 * 1000, 'worker', [I]}).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec new(atom(), atom(), atom(), list()) -> Result :: {'ok', _Child} | {'error', _Error}.
new(SupName, Name, M, A) ->
	case worker(SupName, Name) of
		undefined ->
			supervisor:start_child(SupName, ?WORKER_NAME_ARGS(Name, M, A));
		Pid ->
			{ok, Pid}
	end.

-spec workers(atom()) -> list().
workers(SupName) ->
	[Pid || {_, Pid, 'worker', [_]} <- supervisor:which_children(SupName)].

-spec worker(atom(), atom()) -> pid().
worker(SupName, PName) ->
	case [Pid || {Worker, Pid, 'worker', [_]} <- supervisor:which_children(SupName), Worker =:= PName] of
		[] -> 'undefined';
		[P |_] -> P
	end.

-spec terminate_child(atom(), atom()) -> Result :: 'ok' | {'error', _Error}.
terminate_child(SupName, ChildName) ->
	timer:sleep(20),
	supervisor:terminate_child(SupName, ChildName),
	supervisor:delete_child(SupName, ChildName).

init([]) ->
	{ok, {{one_for_one, 1, 5}, []}}.
