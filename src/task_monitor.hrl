%%%-------------------------------------------------------------------
%%% @author yangyajun03
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 四月 2018 上午10:12
%%%-------------------------------------------------------------------
-author("yangyajun03").

-define(APPNAME, task_monitor).
-define(DETS_DIR, begin  application:get_env(?APPNAME, dets_dir, "/opt/logs/task_monitor/") end).

-define(TIMEVAL, begin  application:get_env(?APPNAME, check_period, 5000) end).
-define(CLENTIE, begin  application:get_env(?APPNAME, clean_period, 600 * 1000) end).
-define(QUERY_TIME, begin  application:get_env(?APPNAME, retry_period, 60 * 1000) end).