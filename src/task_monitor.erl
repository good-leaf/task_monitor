%%%-------------------------------------------------------------------
%%% @author yangyajun03
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 四月 2018 下午2:25
%%%-------------------------------------------------------------------
-module(task_monitor).
-author("yangyajun03").

%% API
-export([
    start/3,
    start/2,
    register_recv/2,
    stop/1,
    record_task/3,
    cancle_task/2,
    query_task/2,

    test_start/1,
    test_create_data/3
]).

-spec start(atom(), atom()) -> {ok, pid()} | {error, term()}.
start(TableName, SaveMethod) ->
    task_monitor_sup:new(task_monitor_sup, TableName, task_manager, [[TableName, SaveMethod]]).

-spec start(atom(), atom(), pid()) -> {ok, pid()} | {error, term()}.
start(TableName, SaveMethod, RecvPid) ->
    {ok, _Pid} = task_monitor_sup:new(task_monitor_sup, TableName, task_manager, [[TableName, SaveMethod]]),
    task_monitor_sup:new(task_monitor_sup, common:retry_name(TableName), task_notice, [[TableName, RecvPid]]).

-spec register_recv(atom(), any()) -> {ok, pid()} | {error, term()}.
register_recv(TableName, RecvPid) ->
    task_monitor_sup:new(task_monitor_sup, common:retry_name(TableName), task_notice, [[TableName, RecvPid]]).

stop(TableName) ->
    case task_monitor_sup:worker(task_monitor_sup, TableName) of
        undefined ->
            ok;
        Pid when is_pid(Pid) ->
            %%通知定时器
            chronos:stop(common:timer_name(TableName)),
            task_monitor_sup:terminate_child(task_monitor_sup, TableName)
    end,

    RetryName = common:retry_name(TableName),
    case task_monitor_sup:worker(task_monitor_sup, RetryName) of
        undefined ->
            ok;
        RetryPid when is_pid(RetryPid) ->
            task_monitor_sup:terminate_child(task_monitor_sup, RetryName)
    end.

record_task(TableName, Data, HandleExpiredTime) ->
    task_manager:record_task(TableName, Data, HandleExpiredTime).

cancle_task(TableName, TaskId) ->
    task_manager:cancle_task(TableName, TaskId).

query_task(TableName, TaskId) ->
    task_manager:query_task(TableName, TaskId).



test_start(TableName) ->
    start(TableName, cache),
    {ok, RecvPid} = register_recv(TableName, self()),
    task_notice:register_recv(common:retry_name(TableName), RecvPid).

test_create_data(TableName, Num, MilExpired) ->
    Fun = fun(N) ->
            D = list_to_binary(integer_to_list(N) ++ "_test"),
            task_monitor:record_task(TableName, D, MilExpired)
        end,
    [Fun(N) || N <- lists:seq(1, Num)].

