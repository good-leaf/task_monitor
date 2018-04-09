%%%-------------------------------------------------------------------
%%% @author yangyajun03
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 四月 2018 上午10:31
%%%-------------------------------------------------------------------
-module(common).
-author("yangyajun03").

%% API
-export([
    timestamp/0,
    milltimestamp/0,
    umilltimestamp/0,
    generate_uuid/0,
    timer_name/1,
    retry_name/1
]).

timestamp() ->
    {M, S, MS} = os:timestamp(),
    M * 1000000 + S + MS div 1000.

milltimestamp() ->
    {M, S, MS} = os:timestamp(),
    M * 1000000000 + S * 1000 + MS div 1000.

umilltimestamp() ->
    {M, S, MS} = os:timestamp(),
    M * 1000000000000 + S * 1000000 + MS.

generate_uuid() ->
    Bin = crypto:strong_rand_bytes(12),
    Time = integer_to_binary(umilltimestamp()),
    NewBin = <<Bin/binary, Time/binary>>,
    Sig = erlang:md5(NewBin),
    iolist_to_binary([io_lib:format("~2.16.0b", [S]) || S <- binary_to_list(Sig)]).

timer_name(ServerName) ->
    list_to_atom(atom_to_list(ServerName) ++ "_timer").

retry_name(ServerName) ->
    list_to_atom(atom_to_list(ServerName) ++ "_retry").