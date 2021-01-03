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
	seq         :: ets_dont_care() | non_neg_integer(),			%% sequence number of record
	stamp       :: ets_dont_care() | non_neg_integer(), 		%% os timestmap
	configured  :: ets_dont_care() | non_neg_integer(),		  %% configured clients
	running     :: ets_dont_care() | non_neg_integer(),			%% actively running clients
	paused      :: ets_dont_care() | non_neg_integer(),			%% currently paused clients
	reconnecting:: ets_dont_care() | non_neg_integer(),     %% aps in reconnecting mode
	dropped     :: ets_dont_care() | non_neg_integer(),			%% number of dropped connections since last interval
	recon       :: ets_dont_care() | non_neg_integer(),			%% number of reconnections
	avg_tx      :: ets_dont_care() | non_neg_integer(),			%% average trasnmitted bytes (of all access points)
	avg_rx      :: ets_dont_care() | non_neg_integer(),			%% average received bytes (of all access points)
	peak_tx     :: ets_dont_care() | non_neg_integer(),			%% maximum bytes transmitted
	peak_rx     :: ets_dont_care() | non_neg_integer()			%% maximum bytes received
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
-spec prepare_statistics () -> ok | generic_error().
prepare_statistics () ->
	_ = ets:new(?MODULE,[ordered_set,protected,{keypos, 2},named_table]),
	ets:insert(?MODULE,{seq,seq,0}),
	ok.

-spec update_statistics(Clients::ovsdb_client_status_map(), Stats::[ovsdb_ap_statistics()]) -> ok.
update_statistics(Clients, Stats) ->
	[{_,_,Seq}] = ets:lookup(?MODULE,seq),
	Entry = create_stats_entry (Seq+1,Clients,Stats),
	ets:insert(?MODULE,[{seq,seq,Seq+1},Entry]),
	post_statistics(Entry),
	ok.

-spec create_stats_entry(Seq::non_neg_integer(),Clients::ovsdb_client_status_map(),Stats::[ovsdb_ap_statistics()]) -> Entry::#statistics{}.
create_stats_entry (_Seq,_Clients,[])->
	#statistics{};
create_stats_entry (Seq,Clients,Stats)->
	{Running,Paused,Reconnecting,_Ready} = maps:fold(fun(_K,V,{R,P,T,RD})->
																								case V of
																									running -> {R+1,P,T,RD};
																									paused -> {R,P+1,T,RD};
																									reconnecting -> {R,P,T+1,RD};
																									ready -> {R,P,T,RD+1};
																									_ ->
																										io:format(">>>STATS: unknown status : ~p~n",[V]),
																										{R,P,T,RD}
																								end
																						end,{0,0,0,0},Clients),
	#statistics{
			seq=Seq,
			stamp = erlang:system_time(),
			configured = maps:size(Clients),
			running = Running,
			paused = Paused,
			reconnecting = Reconnecting,
			dropped = lists:sum([X||#ap_statistics{dropped=X}<-Stats]),
			recon = lists:sum([X||#ap_statistics{restarts=X}<-Stats]),
			avg_tx = lists:sum([X||#ap_statistics{tx_bytes=X}<-Stats]) div length(Stats),
			avg_rx = lists:sum([X||#ap_statistics{rx_bytes=X}<-Stats]) div length(Stats),
			peak_tx = lists:max([X||#ap_statistics{tx_bytes=X}<-Stats]),
			peak_rx = lists:max([X||#ap_statistics{rx_bytes=X}<-Stats])
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
	io:format("+==================================================================================================================+~n"),
	io:format("|  time stamp (UTC)   |  Conf. |   Run  | Paused | Reconn. |  Drop. |  Recon |  ~~TX   |  ~~RX   |  ^TX   |  ^RX   |~n"),
	io:format("+------------------------------------------------------------------------------------------------------------------+~n"),
	[format_row(X)||X<-Rec],
	io:format("+==================================================================================================================+~n").

-spec format_row (Entry :: #statistics{}) -> ok.
format_row (Entry) ->
	{{Y,M,D},{H,I,S}} = calendar:system_time_to_universal_time(Entry#statistics.stamp,native),
	#statistics{configured=CF, running=RN, paused=PS, dropped=DR, recon=RC, avg_rx=ARX, peak_rx=PRX, avg_tx=ATX, peak_tx=PTX, reconnecting = REC } = Entry,
	io:format("| ~4B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B | ~6B | ~6B | ~6B | ~6B | ~6B | ~6B | ~6B | ~6B | ~6B | ~6B |~n",[Y,M,D,H,I,S,CF,RN,PS,REC,DR,RC,ATX,ARX,PTX,PRX]).

-spec post_statistics(Entry::#statistics{}) -> ok.
post_statistics(E) ->
	Fields = record_info(fields,statistics),
	[_|Values] = tuple_to_list(E),
	Map = maps:from_list(lists:zip(Fields,Values)),
	statistics:submit_report(ovsdb_clients,maps:remove(seq,Map)).
