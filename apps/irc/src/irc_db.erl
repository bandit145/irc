-module(irc_db).
-export([check_password/2]).
-record(users, {nick, user, real_name, password, host, hostmask, status}).
-record(rooms, {name, members, private, allowed_members}).

check_password(User, Password) ->
	F = fun() ->
		case mnesia:read(#users{user=User, password=Password}) of
			[] ->false;
			_ -> true
		end
	    end,
	mnesia:activity(transaction, F).

