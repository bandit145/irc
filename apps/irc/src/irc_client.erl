-module(irc_client).
-behavior(gen_server).

-export([init/1, start_link/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2]).

-record(server_state,{socket, client_name, nick, authenticated, connection_password}).
-record(irc_message, {prefix, command, params}).
-record(irc_server_prefix, {name}).
-record(irc_name_prefix, {name}).

start_link(Args) ->
	gen_server:start_link(?MODULE, Args, []).

init({Socket, Name}) ->
	{ok,#server_state{socket=Socket, client_name=Name, authenticated=false}}.

handle_cast(Msg=#irc_message{command="PASS", params=Params}, State) when Msg#server_state.authenticated =:= undefined ->
	{noreply, State#server_state{connection_password=lists:nth(1, Params)}};

handle_cast(Msg=#irc_message{command="JOIN"}, State) ->
	io:format("~nJOIN", []),
	{noreply, State};

handle_cast(_,State) ->
	{noreply, State}.

handle_call(_,_,State) ->
	{noreply, State}.

handle_info({tcp, Socket, Data}, State) ->
	IRCMessages = [parse_message(X) || X <- binary:split(Data, <<"\r\n">>, [global]), X =/= <<>>],
	io:format("~p", [IRCMessages]),
	[gen_server:cast(self(), X) || X <- IRCMessages],
	{noreply, State};

handle_info({tcp_closed, _}, State) ->
	io:format("~p Is going down!~n", [State#server_state.client_name]),
	{stop, normal , State};

handle_info(Info, State) ->
	io:format("I Should not be here ~p ~n", [Info]),
	{noreply, State}.
	
terminate(_,#server_state{socket=Socket}) ->
	gen_tcp:close(Socket),
	ok.

%%private module
parse_message(Data) ->
	parse_message(Data, [],#irc_message{params=[]}).

parse_message(<<>>, [], Msg) ->
	Msg;
parse_message(<<>>, Accum, Msg) ->
	Msg#irc_message{params=Msg#irc_message.params ++ [lists:reverse(Accum)] };
parse_message(<<58:8, Data/binary>>, Accum, Msg) when Msg#irc_message.command =/= undefined, Msg#irc_message.params =:= [] ->
	parse_message(Data, [], Msg#irc_message{params=string:split(lists:reverse(Accum), " ", all)});
parse_message(<<32:8, Data/binary>>, Accum, Msg) when Msg#irc_message.command =:= undefined ->
	parse_message(Data, [], Msg#irc_message{command=lists:reverse(Accum)});
parse_message(<<Char:8,Data/binary>>, Accum, Msg) ->
	io:format("~p~n", [Accum]),
	parse_message(Data, [Char | Accum], Msg).

