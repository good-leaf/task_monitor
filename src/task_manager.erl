%%%-------------------------------------------------------------------
%%% @author yangyajun03
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 四月 2018 上午10:05
%%%-------------------------------------------------------------------
-module(task_manager).
-author("yangyajun03").

-behaviour(gen_server).

-include("task_monitor.hrl").
%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-record(state, {
    method,
    tserver
}).

-export([
    record_task/3,
    cancle_task/2,
    query_task/2,
    set_save_method/2
]).
%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(list()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link([ServerName, SaveMethod]) ->
    gen_server:start_link({local, ServerName}, ?MODULE, [ServerName, SaveMethod], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([ServerName, SaveMethod]) ->
    {ok, _Pid} = chronos:start_link(common:timer_name(ServerName)),
    cache_servant:create_table(ServerName),
    disk_servant:create_table(ServerName),
    Method = case SaveMethod of
                 cache ->
                     cache;
                 disk ->
                     disk;
                 _ ->
                     cache
             end,
    {ok, #state{method = Method, tserver = ServerName}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_call({record_task, Data, HandleExpiredTime}, _From, #state{method = Method, tserver = TServer} = State) ->
    TaskId = common:generate_uuid(),
    error_logger:info_msg("manager srvname:~p, record_task taskid:~p, handleExpireTime:~p, data:~p",
        [TServer, TaskId, HandleExpiredTime, Data]),
    try
        case Method of
            cache ->
                cache_servant:insert(TServer, TaskId, Data),
                %%数据处理超时，存入磁盘;
                ok = chronos:start_timer(common:timer_name(TServer), TaskId, HandleExpiredTime,
                    {disk_servant, timer_insert, [TServer, TaskId]});
            disk ->
                disk_servant:insert(TServer, TaskId, Data)
        end
    catch
        E:R ->
            error_logger:error_msg("manager srvname:~p, record_task error:~p, reason:~p, data:~p",
                [TServer, E, R, Data])
    end,
    {reply, {taskid, TaskId}, State};
handle_call({cancle_task ,TaskId}, _From, #state{method = Method, tserver = TServer} = State) ->
    try
        chronos:stop_timer(common:timer_name(TServer), TaskId),
        case Method of
            cache ->
                cache_servant:delete(TServer, TaskId);
            _Mode ->
                ok
        end,
        disk_servant:delete(TServer, TaskId)
    catch
        E:R  ->
            error_logger:error_msg("manager srvname:~p, cancle_task error:~p, reason:~p, taskid:~p",
                [TServer, E, R, TaskId])
    end,
    {reply, ok, State};
handle_call({query_task ,TaskId}, _From, #state{method = Method, tserver = TServer} = State) ->
    NewData = case Method of
               cache ->
                   %%默认表名和任务名称相同
                   case cache_servant:lookup(TServer, TaskId) of
                       [] ->
                           disk_servant:lookup(TServer, TaskId);
                       Data ->
                           Data
                   end;
               disk ->
                   disk_servant:lookup(TServer, TaskId)
           end,
    {reply, {ok, NewData}, State};
handle_call({set_save_method ,SaveMethod}, _From, #state{method = Method, tserver = TServer} = State) ->
    Method = case SaveMethod of
                 cache ->
                     cache;
                 disk ->
                     disk;
                 _ ->
                     error_logger:error_msg("manager srvname:~p, cancle_task method:~p",
                         [TServer, cache]),
                     cache
             end,
    {reply, ok, State#state{method = Method}};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec record_task(atom(), term(), integer()) -> {taskid, binary()}.
record_task(ServerName, Data, HandleExpiredTime) ->
    gen_server:call(ServerName, {record_task, Data, HandleExpiredTime}).

cancle_task(ServerName, TaskId) ->
    gen_server:call(ServerName, {cancle_task ,TaskId}).

query_task(ServerName, TaskId) ->
    gen_server:call(ServerName, {query_task ,TaskId}).

set_save_method(ServerName, Method) ->
    gen_server:call(ServerName, {set_save_method ,Method}).
%%%===================================================================
%%% Internal functions
%%%===================================================================
