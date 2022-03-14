%%%-------------------------------------------------------------------
%% @doc blockchain_crawler public API
%% @end
%%%-------------------------------------------------------------------

-module(blockchain_crawler_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    blockchain_crawler_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
