%%%-------------------------------------------------------------------
%%% @author yangyajun03
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 四月 2018 下午2:21
%%%-------------------------------------------------------------------
-module(task_notice).
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
    server,
    recv_pid
}).

-export([
    register_recv/2
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
start_link([ServerName, RecvPid]) ->
    gen_server:start_link({local, common:retry_name(ServerName)}, ?MODULE, [ServerName, RecvPid], []).

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
init([ServerName, RecvPid]) ->
    erlang:send_after(?TIMEVAL, self(), retry_data),
    erlang:send_after(?CLENTIE, self(), clean_data),
    {ok, #state{server = ServerName, recv_pid = RecvPid}}.

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
handle_call({register, RecvPid}, _From, State) ->
    {reply, ok, State#state{recv_pid = RecvPid}};
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
handle_info(retry_data, #state{server = ServerName, recv_pid = RecvPid} = State) ->
    try
        retry_data(ServerName, RecvPid)
    catch
        E:R  ->
            error_logger:error_msg("notice srvname:~p retry_data error:~p, reason:~p",
                [ServerName, E, R])
    end,
    erlang:send_after(?TIMEVAL, self(), retry_data),
    {noreply, State};
handle_info(clean_data, #state{server = ServerName} = State) ->
    try
        disk_servant:clean_select(ServerName, ?QUERY_TIME)
    catch
        E:R  ->
            error_logger:error_msg("notice srvname:~p clean_data error:~p, reason:~p",
                [ServerName, E, R])
    end,
    erlang:send_after(?CLENTIE, self(), clean_data),
    {noreply, State};
handle_info(Info, #state{server = ServerName} = State) ->
    error_logger:warning_msg("notice srvname:~p retry_data, ignore message:~p",
        [ServerName, Info]),
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

register_recv(ServerName, RecvPid) ->
    gen_server:call(ServerName, {register, RecvPid}).

%%%===================================================================
%%% Internal functions
%%%===================================================================
retry_data(ServerName, RecvPid) when is_pid(RecvPid) ->
    case is_process_alive(RecvPid) of
        true ->
            Fun = fun(V) ->
                RecvPid ! {retry, V}
                  end,
            [Fun(V) || V <- disk_servant:query_select(ServerName, ?QUERY_TIME)];
        false ->
            error_logger:error_msg("notice srvname:~p retry_data recv_pid:~p dead", [ServerName, RecvPid])
    end;
retry_data(ServerName, RecvPid) ->
    error_logger:error_msg("notice srvname:~p retry_data recv_pid:~p disabled", [ServerName, RecvPid]).