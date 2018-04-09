%%%-------------------------------------------------------------------
%%% @author yangyajun03
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 四月 2018 上午10:14
%%%-------------------------------------------------------------------
-module(disk_servant).
-author("yangyajun03").

-include("task_monitor.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
%% API
-export([
    create_table/1,
    insert/3,
    delete/2,
    lookup/2,
    timer_insert/2,
    clear_table/1,
    query_select/2,
    clean_select/2
]).

create_table(DiskTable) ->
    %新建dets目录
    file:make_dir(?DETS_DIR),
    Kfile = ?DETS_DIR ++ atom_to_list(DiskTable),
    {ok, _Tab} = dets:open_file(DiskTable, [{type, set}, {file, Kfile}]).

insert(DiskTable, TaskId, Data) ->
    error_logger:info_msg("save distable:~p, taskid:~p, data:~p", [DiskTable, TaskId, Data]),
    ok = dets:insert(DiskTable, {TaskId, common:milltimestamp(), Data}).

timer_insert(DiskTable, TaskId) ->
    case cache_servant:lookup(DiskTable, TaskId) of
        [] ->
            ok;
        Data ->
            insert(DiskTable, TaskId, Data)
    end,
    %%删除缓存数据
    true = cache_servant:delete(DiskTable, TaskId).


delete(DiskTable, TaskId) ->
    error_logger:info_msg("delete distable:~p, taskid:~p", [DiskTable, TaskId]),
    ok = dets:delete(DiskTable, TaskId).

lookup(DiskTable, TaskId) ->
    case dets:lookup(DiskTable, TaskId) of
        [] ->
            [];
        [{TaskId, _SaveTime, Data}] ->
            Data
    end.

clear_table(DiskTable) ->
    dets:delete_all_objects(DiskTable).

query_select(DiskTable, DurnTime) ->
    StartTime = common:milltimestamp() - DurnTime,
    Fun = ets:fun2ms(fun({_TaskId, TimeStamp , Data } )  when  TimeStamp >= StartTime -> Data end),
    dets:select(DiskTable, Fun).

clean_select(DiskTable, DurnTime) ->
    StartTime = common:milltimestamp() - DurnTime,
    Fun = ets:fun2ms(fun({TaskId, TimeStamp , _Data } )  when  TimeStamp < StartTime -> TaskId end),
    [delete(DiskTable, TaskId) || TaskId <- dets:select(DiskTable, Fun)].

