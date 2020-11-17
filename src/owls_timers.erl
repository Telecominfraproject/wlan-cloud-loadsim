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

-type t_stamp() :: {Name::term(), Stamp::integer()}.
-type t_stamps() :: [] | [t_stamp(),...].

-record (tms,{
	start :: integer(),
	stamps = [] :: t_stamps()
}).
-opaque tms() :: #tms{}.
-export_type([tms/0]).


%% API
-export([new/0,stats/1]).
-export([mark/2,get_stamp/2,get_elapsed/3,get_delta/3]).


%--------new-----------------------------create a new empty timer data structure
-spec new () -> tms().

new () ->
	#tms{start=os:system_time(microsecond)}.



%--------mark----------------------------marks a local timestamp for later analytics
-spec mark (Name::term(), Timer::tms()) -> NewTimer::tms().

mark (Name,Timer) ->
	Timer#tms{stamps=[{Name,os:system_time(microsecond)}|Timer#tms.stamps]}.



%--------get_stamp-----------------------get formatted timestamp of one mark
-spec get_stamp (Name::term(), Timer::tms()) -> Stamp::string().

get_stamp (Name,Timer) ->
	{ok, Ts, _} = get_stamp_us(Name,Timer#tms.stamps),
	format_stamp(Ts). 



%--------get_elapsed---------------------formats the elapsed time between two stamps
-spec get_elapsed (Older_TS, Newer_TS, Timer) -> Stamp when
		Older_TS :: term(),
		Newer_TS :: term(),
		Timer :: tms(),
		Stamp :: string().

get_elapsed (Old,New,Timer) ->
	D = get_delta(Old,New,Timer),
	format_delta(D).



%--------stats---------------------------statistics of the timer
-spec stats (Timer) -> {Stamp, Marks} when
		Timer :: tms(),
		Stamp :: string(),
		Marks :: integer().

stats (Timer) ->
	D = os:system_time(microsecond) - Timer#tms.start,
	{format_delta(D),length(Timer#tms.stamps)}.


%--------get_delta-----------------------gets the delta in microseconds between two makred stamps
-spec get_delta (Older_TS, Newer_TS, Timer) -> Delta when
		Older_TS :: term(),		% older timestamp is the one that was marked first in the timeline 
		Newer_TS :: term(),		% newer timestamp is the one marked closer to now
		Timer :: tms(),			
		Delta :: integer().

get_delta (Old,New,Timer) ->
	{ok, New_TS, T} = get_stamp_us(New,Timer#tms.stamps),
	{ok, Old_TS, _} = get_stamp_us(Old,T),
	New_TS - Old_TS.


%%-----------------------------------------------------------------------------
%% internal functions
%%-----------------------------------------------------------------------------

%--------get_stamp_us--------------------get the timestamp of named stamp in microseconds
-spec get_stamp_us (Name::term(),Stamps::t_stamps()) -> {ok, integer(), Tail::t_stamps()} | invalid.

get_stamp_us (_,[]) ->
	invalid;

get_stamp_us (Stamp,[{Stamp,Value}|T]) ->
	{ok, Value, T};

get_stamp_us (Name,[_|T]) ->
	get_stamp_us(Name,T).




%--------format_stamp--------------------format a timestamp to Y-m-d H:i:s.u
-spec format_stamp (Ts) -> Stamp when
		Ts :: integer(),	% Ts in microseconds
		Stamp :: string().

format_stamp (Ts) ->
	Usec = Ts rem 1000000,
	{{Y,M,D},{H,I,S}} = calendar:system_time_to_universal_time(Ts, microsecond),
	io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B.~6..0B",[Y,M,D,H,I,S,Usec]).


%--------format_delta--------------------formats a timedifference to D, H:I:S.u
-spec format_delta (Delta) -> Stamp when
		Delta :: integer(),  % delta time in  microseconds
		Stamp :: string().

format_delta (Delta) ->
	Sec = Delta div 1000000,
	Usec = Delta rem 1000000,
	{D, {H,I,S}} = calendar:seconds_to_daystime(Sec),
	io_lib:format("~B, ~2..0B:~2..0B:~2..0B.~6..0B",[D,H,I,S,Usec]).