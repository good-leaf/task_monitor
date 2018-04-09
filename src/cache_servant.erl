%%%-------------------------------------------------------------------
%%% @author yangyajun03
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 四月 2018 上午10:11
%%%-------------------------------------------------------------------
-module(cache_servant).
-author("yangyajun03").

-include("task_monitor.hrl").
%% API
-export([
    create_table/1,
    insert/3,
    lookup/2,
    delete/2
]).

create_table(CacheTable) ->
    ets:new(CacheTable, [
        set,
        public,
        named_table,
        {write_concurrency, true},
        {read_concurrency, true}
    ]).

insert(CacheTable, TaskId, Data) ->
    error_logger:info_msg("save cache_table:~p, taskid:~p, data:~p", [CacheTable, TaskId, Data]),
    true = ets:insert(CacheTable, {TaskId, common:milltimestamp(), Data}).

lookup(CacheTable, TaskId) ->
    case ets:lookup(CacheTable, TaskId) of
        [] ->
            [];
        [{TaskId, _Time, Data}] ->
            Data
    end.

delete(CacheTable, TaskId) ->
    true = ets:delete(CacheTable, TaskId).