%%%-----------------------------------------------------------------------------
%%% @author helge
%%% @copyright (C) 2020, Arilia Wireless Inc.
%%% @doc
%%% 
%%% @end
%%% Created : 05. December 2020 @ 17:27:13
%%%-----------------------------------------------------------------------------
-module(ovsdb_client_stats).
-author("helge").

-include("../include/common.hrl").
-include("../include/ovsdb_definitions.hrl").


-export ([prepare_statistics/0,update_statistics/2,close/0]).

-record (statistics, {
	seq :: non_neg_integer(),				%% sequence number of record
	stamp :: integer(), 					%% os timestmap
	configured :: non_neg_integer(),		%% configured clients
	running :: non_neg_integer(),			%% actively running clients
	paused :: non_neg_integer(),			%% currently paused clients
	dropped :: non_neg_integer(),			%% number of dropped connections since last interval
	recon :: non_neg_integer(),				%% number of reconnections
	avg_tx :: non_neg_integer(),			%% average trasnmitted bytes (of all access points)
	avg_rx :: non_neg_integer(),			%% average received bytes (of all access points)
	peak_tx :: non_neg_integer(),			%% maximum bytes transmitted
	peak_rx :: non_neg_integer()			%% maximum bytes received
}).


-spec prepare_statistics () -> ok | {error, Reason::term()}.
prepare_statistics () ->
	File = filename:join([code:priv_dir(?OWLS_APP),"ovsdb","handler_stats.dets"]),
	Exists = filelib:is_file(File),
	case dets:open_file(?MODULE,[{file,File},{type,set},{keypos,2}]) of
		{ok, ?MODULE} ->
			case Exists of
				true -> ok;
				false -> ok = dets:insert(?MODULE,{seq,seq,0})
			end;
		{error, Reason} ->
			?L_E(?DBGSTR("Can't open statistics table: '~p'",[File])),
			{error, Reason}
	end.


-spec update_statistics (Clients, Stats) -> ok  when
		Clients :: ets:tid(),
		Stats :: [#ap_statistics{}].
update_statistics (CRef, Stats) ->
	[{_,_,Seq}] = dets:lookup(?MODULE,seq),
	Clients = ets:match_object(CRef,#ap_client{_='_'}),
	Entry = create_stats_entry (Seq+1,Clients,Stats),		
	{ok,_} = timer:apply_after(?MGR_REPORT_INTERVAL,gen_server,call,[ovsdb_client_handler,update_stats]),
	ok = dets:insert(?MODULE,[{seq,seq,Seq+1},Entry]).
	


-spec create_stats_entry (Seq,Clients,Stats) -> Entry when
		Seq :: non_neg_integer(),	% the key in the DETS must be unique
		Clients :: [#ap_client{}],
		Stats :: [#ap_statistics{}],
		Entry :: #statistics{}.
create_stats_entry (Seq,_Clients,_Stats) ->
	#statistics{
			seq=Seq,
			stamp = 0,
			configured = 0,
			running = 0,
			paused = 0,
			dropped = 0,
			recon = 0,
			avg_tx = 0,
			avg_rx = 0,
			peak_tx = 0,
			peak_rx = 0
		}.

-spec close() -> ok.
close () ->
	dets:close(?MODULE).