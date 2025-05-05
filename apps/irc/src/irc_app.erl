%%%-------------------------------------------------------------------
%% @doc irc public API
%% @end
%%%-------------------------------------------------------------------

-module(irc_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    irc_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
