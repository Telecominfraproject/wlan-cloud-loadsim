%%%---------------------------------------------------------------------------------
%%% @author helge
%%% @copyright (C) 2020, Arilia Wireless Inc.
%%% @doc
%%% 
%%% @end
%%% Created : 17. November 2020 @ 10:06:58
%%%---------------------------------------------------------------------------------
-module(owls_timers).
-author("helge").

-include("../include/common.hrl").

%% DATA STRUCTURES

-type t_stamp() :: {Name::string(), Stamp::integer()}.
-type t_stamps() :: [t_stamp(),...].

-record (tms,{
	resolution :: second | millisecond | microsecond | nanosecond,
	units_per_sec :: integer(),
	stamps :: t_stamps()
}).
-opaque tms() :: #tms{}.
-export_type([tms/0]).


%% API
-export([new/1,mark/2]).
-export([stamp/2,delta/2,delta/3,fmt_stamp/2,fmt_duration/2]).



%--------new/1-----------------------------create a new empty timer data structure
-spec new (Resolution) -> tms() when
		Resolution :: second | millisecond | microsecond | nanosecond.

new (R) ->
	Ups = erlang:convert_time_unit(1, second, native),
	#tms{resolution=R,stamps=[{"*start*",os:system_time()}],units_per_sec=Ups}.



%--------mark/2----------------------------marks a local timestamp for later analytics
-spec mark (Name, Timer) -> NewTimer when
		Name :: string(),
		Timer :: tms(),
		NewTimer :: tms().

mark (Name,Timer) ->
	Timer#tms{stamps=[{Name,os:system_time()}|Timer#tms.stamps]}.



%--------stamp/2-----------------------get the value of a specific stamp in units of resolution
-spec stamp (Name, Timer) -> invalid | Stamp when
		Name :: string(),
		Timer :: tms(),
		Stamp :: integer().

stamp (Name,Timer) ->
	case get_stamp(Name,Timer#tms.stamps) of
		{ok, Ts, _} ->
			erlang:convert_time_unit(Ts, native, Timer#tms.resolution);
		_ ->
			invalid
	end.



%--------delta/3-------------------------gets time difference between two stamps in units of resolution
-spec delta (Earlier, Later, Timer) -> Delta when
		Earlier :: default | string(),		% Earlier must have been added before Later (default means start) or in other words 
		Later :: default |string(),			% Later must have occured after Earlier in the timeline (default means now)
		Timer :: tms(),
		Delta :: invalid | integer().

delta (default,L,T) ->
	delta("*start*",L,T);

delta (E,default,T) ->
	delta(E,"now",T);

delta (default,default,T) ->
	delta("start","now",T);

delta (E,L,T) ->
	case get_stamp(L,T#tms.stamps) of
		{ok, T2, R} ->
			case get_stamp(E,R) of
				{ok, T1, _} ->
					erlang:convert_time_unit(T2 - T1, native, T#tms.resolution);
				_ ->
					invalid
			end;
		_ ->
			invalid
	end.



%--------delta/2-------------------------gets time difference between now and a stamp in the past
-spec delta (Stamp, Timer) -> Delta when
		Stamp :: string(),		
		Timer :: tms(),
		Delta :: integer().

delta (S,T) ->
	{ok, T1, _} = get_stamp(S,T#tms.stamps),
	T2 = os:system_time(),
	erlang:convert_time_unit(T2 - T1, native, T#tms.resolution).




%--------fmt_stamp/2--------------------format a timestamp to Y-m-d H:i:s.u
-spec fmt_stamp (Ts,Timer) -> Stamp when
		Ts :: integer(),	% Tsas gotten from stamp/2
		Timer :: tms(),
		Stamp :: string().

fmt_stamp (Ts,Timer) ->	
	{{Y,M,D},{H,I,S}} = calendar:system_time_to_universal_time(Ts, Timer#tms.resolution),
	case Timer#tms.resolution of
		millisecond ->
			Usec = Ts rem 1000,
			io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B.~3..0B",[Y,M,D,H,I,S,Usec]);
		microsecond ->
			Usec = Ts rem 1000000,
			io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B.~6..0B",[Y,M,D,H,I,S,Usec]);
		nanosecond ->
			Usec = Ts rem 1000000000,
			io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B.~9..0B",[Y,M,D,H,I,S,Usec]);
		_ ->
			io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B",[Y,M,D,H,I,S])
	end.



%--------fmt_duration/2------------------formats a timedifference to Days-H:I:S.u
-spec fmt_duration (Delta,Timer) -> Stamp when
		Delta :: integer(),
		Timer :: tms(),
		Stamp :: string().

fmt_duration (Delta,Timer) ->
	Sec = erlang:convert_time_unit(Delta, Timer#tms.resolution, second),
	{D, {H,I,S}} = calendar:seconds_to_daystime(Sec),
	case Timer#tms.resolution of
		millisecond ->
			Usec = Delta rem 1000,
			io_lib:format("~B-~2..0B:~2..0B:~2..0B.~3..0B",[D,H,I,S,Usec]);
		microsecond ->
			Usec = Delta rem 1000000,
			io_lib:format("~B-~2..0B:~2..0B:~2..0B.~6..0B",[D,H,I,S,Usec]);
		nanosecond ->
			Usec = Delta rem 1000000000,
			io_lib:format("~B-~2..0B:~2..0B:~2..0B.~9..0B",[D,H,I,S,Usec]);
		_ ->
			io_lib:format("~B-~2..0B:~2..0B:~2..0B",[D,H,I,S])
	end.



%%-----------------------------------------------------------------------------
%% internal functions
%%-----------------------------------------------------------------------------

%--------get_stamp--------------------get the timestamp of named stamp in microseconds
-spec get_stamp (Name,Stamps) -> {ok, integer(), EarlierStamps} | invalid when
		Name :: string(),
		Stamps :: t_stamps(),
		EarlierStamps :: [] | t_stamps().

get_stamp (_,[]) ->
	invalid;

get_stamp ("now",T) ->
	{ok, os:system_time(), T};

get_stamp (Stamp,[{Stamp,Value}|T]) ->
	{ok, Value, T};

get_stamp (Name,[_|T]) ->
	get_stamp(Name,T).
