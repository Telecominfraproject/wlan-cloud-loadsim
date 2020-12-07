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


%%---------external API---------------------------------------------------------
-export ([show_statistics/1]).


%%---------internal API---------------------------------------------------------
-export ([prepare_statistics/0,update_statistics/2,close/0]).




-record (statistics, {
	seq :: ets_dont_care() | non_neg_integer(),				%% sequence number of record
	stamp :: ets_dont_care() | integer(), 					%% os timestmap
	configured :: ets_dont_care() | non_neg_integer(),		%% configured clients
	running :: ets_dont_care() | non_neg_integer(),			%% actively running clients
	paused :: ets_dont_care() | non_neg_integer(),			%% currently paused clients
	dropped :: ets_dont_care() | non_neg_integer(),			%% number of dropped connections since last interval
	recon :: ets_dont_care() | non_neg_integer(),			%% number of reconnections
	avg_tx :: ets_dont_care() | non_neg_integer(),			%% average trasnmitted bytes (of all access points)
	avg_rx :: ets_dont_care() | non_neg_integer(),			%% average received bytes (of all access points)
	peak_tx :: ets_dont_care() | non_neg_integer(),			%% maximum bytes transmitted
	peak_rx :: ets_dont_care() | non_neg_integer()			%% maximum bytes received
}).


%%------------------------------------------------------------------------------
%% external API
%%------------------------------------------------------------------------------

-spec show_statistics(NumberOfRecords :: non_neg_integer()) -> ok | {error, not_available}.
show_statistics(N) ->
	case ets:whereis(?MODULE) of
		undefined ->
			{error, not_available};
		_ ->
			[{_,_,Seq}] = ets:lookup(?MODULE,seq),
			print_statistics(max(0,Seq-N))
	end.


%%------------------------------------------------------------------------------
%% internal API
%%------------------------------------------------------------------------------

-spec prepare_statistics () -> ok | {error, Reason::term()}.
prepare_statistics () ->
	_ = ets:new(?MODULE,[ordered_set,protected,{keypos, 2},named_table]),
	ets:insert(?MODULE,{seq,seq,0}),
	ok.


-spec update_statistics (Clients, Stats) -> ok  when
		Clients :: ets:tid(),
		Stats :: [#ap_statistics{}].
update_statistics (CRef, Stats) ->
	[{_,_,Seq}] = ets:lookup(?MODULE,seq),
	Clients = ets:match_object(CRef,#ap_client{_='_'}),
	Entry = create_stats_entry (Seq+1,Clients,Stats),		
	{ok,_} = timer:apply_after(?MGR_REPORT_INTERVAL,gen_server,call,[ovsdb_client_handler,update_stats]),
	ets:insert(?MODULE,[{seq,seq,Seq+1},Entry]),
	ok.
	


-spec create_stats_entry (Seq,Clients,Stats) -> Entry when
		Seq :: non_neg_integer(),	% the key in the DETS must be unique
		Clients :: [#ap_client{}],
		Stats :: [#ap_statistics{}],
		Entry :: #statistics{}.
create_stats_entry (Seq,Clients,Stats) when length(Stats) > 0 andalso length(Clients) > 0->
	#statistics{
			seq=Seq,
			stamp = erlang:system_time(),
			configured = length(Clients),
			running = length([1||#ap_client{status=S}<-Clients,S=:=running]),
			paused = length([1||#ap_client{status=S}<-Clients,S=:=paused]),
			dropped = lists:sum([X||#ap_statistics{dropped=X}<-Stats]),
			recon = lists:sum([X||#ap_statistics{restarts=X}<-Stats]),
			avg_tx = lists:sum([X||#ap_statistics{tx_bytes=X}<-Stats]) div length(Stats),
			avg_rx = lists:sum([X||#ap_statistics{rx_bytes=X}<-Stats]) div length(Stats),
			peak_tx = lists:max([X||#ap_statistics{tx_bytes=X}<-Stats]),
			peak_rx = lists:max([X||#ap_statistics{rx_bytes=X}<-Stats])
		};
create_stats_entry (Seq,Clients,_) when length(Clients) > 0 ->
	#statistics{
			seq=Seq,
			stamp = erlang:system_time(),
			configured = length(Clients),
			running = length([1||#ap_client{status=S}<-Clients,S=:=running]),
			paused = length([1||#ap_client{status=S}<-Clients,S=:=paused]),
			dropped = 0,
			recon = 0,
			avg_tx = 0,
			avg_rx = 0,
			peak_tx = 0,
			peak_rx = 0
		};
create_stats_entry (Seq,_,_) ->
	#statistics{
			seq=Seq,
			stamp = erlang:system_time(),
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
	ets:delete(?MODULE),
	ok.


%%------------------------------------------------------------------------------
%% internal functions
%%------------------------------------------------------------------------------

-spec print_statistics (StartSequence :: non_neg_integer()) -> ok.
print_statistics (Seq) ->
	Rec = ets:select(?MODULE,[{#statistics{seq='$1',_='_'},[{'<',Seq,'$1'}],['$_']}]),
	io:format("~n~n"),
	io:format("+======================================================================================================+~n"),
	io:format("|  time stamp (UTC)   |  Conf. |   Run  | Paused |  Drop. |  Recon |  ~~TX   |  ~~RX   |  ^TX   |  ^RX   |~n"),
	io:format("+------------------------------------------------------------------------------------------------------+~n"),
	[format_row(X)||X<-Rec],
	io:format("+======================================================================================================+~n").

-spec format_row (Entry :: #statistics{}) -> ok.
format_row (Entry) ->
	{{Y,M,D},{H,I,S}} = calendar:system_time_to_universal_time(Entry#statistics.stamp,native),
	#statistics{configured=CF, running=RN, paused=PS, dropped=DR, recon=RC, avg_rx=ARX, peak_rx=PRX, avg_tx=ATX, peak_tx=PTX} = Entry,
	io:format("| ~4B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B | ~6B | ~6B | ~6B | ~6B | ~6B | ~6B | ~6B | ~6B | ~6B |~n",[Y,M,D,H,I,S,CF,RN,PS,DR,RC,ATX,ARX,PTX,PRX]).