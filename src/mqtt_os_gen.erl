%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2020, Arilia Wireless Inc.
%%% @doc
%%%
%%% @end
%%% Created : 12. Dec 2020 10:36 a.m.
%%%-------------------------------------------------------------------
-module(mqtt_os_gen).
-author("stephb").

-include("../include/opensync_stats.hrl").
-include("../include/inventory.hrl").

%% API
-export([gen_report/4]).

gen_report(StartTime,ClientInfo,MACs,MACSSIDList)->
	TimeStamp = os:system_time() div 1000000,
	TR = #'Report'{ nodeID = ClientInfo#client_info.serial,
	                device = [ #'Device'{
		                timestamp_ms = TimeStamp,
		                uptime = (TimeStamp - (StartTime div 1000000)) div 1000,
		                load = gen('Device.LoadAvg'),
		                mem_util = gen('Device.MemUtil'),
		                fs_util = gen('Device.FsUtil'),
		                cpuUtil = gen('Device.CpuUtil'),
		                thermal_stats = gen('Device.Thermal',TimeStamp),
		                radio_temp = gen('Device.RadioTemp'),
		                ps_cpu_util = gen('Device.PerProcessUtil',ps_cpu_util),
		                ps_mem_util = gen('Device.PerProcessUtil',ps_mem_util)
	                }],
	                neighbors = gen('Neighbor',TimeStamp),
	                clients = gen('ClientReport',ClientInfo#client_info.wan_mac0,MACs,MACSSIDList,TimeStamp,StartTime div 1000000),
	                survey = gen('Survey',TimeStamp)
	},
	opensync_stats:encode_msg(TR,'Report').

gen('Device.FsUtil')->
	[{'Device.FsUtil','FS_TYPE_ROOTFS',62388,2064},
	 {'Device.FsUtil','FS_TYPE_TMPFS',125476,288}];
gen('Device.MemUtil')->
	{'Device.MemUtil',250952,108324,0,0};
gen('Device.LoadAvg')->
	{'Device.LoadAvg',0.34,0.34,0.24};
gen('Device.CpuUtil')->
	{'Device.CpuUtil',rand:uniform(30)+40};
gen('Device.RadioTemp')->
	[{'Device.RadioTemp','BAND2G',rand:uniform(20)+40},
	{'Device.RadioTemp','BAND5GU',rand:uniform(20)+40},
	{'Device.RadioTemp','BAND5GL',rand:uniform(20)+40}].

-spec gen( atom(), Qualifier::atom()) ->any();
         ( atom(), integer()) ->any().
gen('Device.PerProcessUtil',ps_cpu_util)->
	[{'Device.PerProcessUtil',2370,"ovsdb-server",2},
	{'Device.PerProcessUtil',27,"kworker/0:1",2},
	{'Device.PerProcessUtil',2547,"wm",1},
	{'Device.PerProcessUtil',1386,"nginx",0},
	{'Device.PerProcessUtil',2558,"ovs-vswitchd",0},
	{'Device.PerProcessUtil',2541,"nm",0},
	{'Device.PerProcessUtil',3392,"hostapd",0},
	{'Device.PerProcessUtil',1234,"wpa_supplicant",0},
	{'Device.PerProcessUtil',2542,"qm",0},
	{'Device.PerProcessUtil',2544,"sm",0}];
gen('Device.PerProcessUtil',ps_mem_util)->
	[{'Device.PerProcessUtil',2541,"nm",9476},
	{'Device.PerProcessUtil',2558,"ovs-vswitchd",7108},
	{'Device.PerProcessUtil',2370,"ovsdb-server",4440},
	{'Device.PerProcessUtil',1386,"nginx",3492},
	{'Device.PerProcessUtil',2542,"qm",3428},
	{'Device.PerProcessUtil',2547,"wm",2184},
	{'Device.PerProcessUtil',3392,"hostapd",2020},
	{'Device.PerProcessUtil',1234,"wpa_supplicant",1884},
	{'Device.PerProcessUtil',2539,"cmdm",1676},
	{'Device.PerProcessUtil',2544,"sm",1592}];
gen('Device.Thermal',TimeStamp)->
	[{'Device.Thermal',
	  [{'Device.Thermal.RadioTxChainMask','BAND2G',3},
	   {'Device.Thermal.RadioTxChainMask','BAND5GU',3},
	   {'Device.Thermal.RadioTxChainMask','BAND5GL',3}],
	  0,TimeStamp}];
gen('Neighbor',TimeStamp)->
	[{'Neighbor','BAND2G','OFFCHAN_SCAN',TimeStamp,
	  [{'Neighbor.NeighborBss',"fa:8f:ca:79:da:f9","Luke  speaker.ynm",
	    4294967203,1049069056513,'CHAN_WIDTH_UNKNOWN',1,undefined},
	   {'Neighbor.NeighborBss',"ec:6c:9a:16:5d:f7","TELUS6279",4294967224,
	    3190962585833,'CHAN_WIDTH_UNKNOWN',1,undefined},
	   {'Neighbor.NeighborBss',"24:f5:a2:a0:7a:33","goldniks internet",
	    4294967204,745251431446,'CHAN_WIDTH_UNKNOWN',1,undefined},
	   {'Neighbor.NeighborBss',"ec:6c:9a:16:76:8f","TELUS6279",4294967218,
	    1143567360245,'CHAN_WIDTH_UNKNOWN',1,undefined},
	   {'Neighbor.NeighborBss',"00:1c:b3:ae:de:e5","HOBCA",4294967252,
	    27577037184,'CHAN_WIDTH_UNKNOWN',1,undefined},
	   {'Neighbor.NeighborBss',"18:82:8c:05:73:37","TELUS6279",4294967213,
	    1145548083588,'CHAN_WIDTH_UNKNOWN',1,undefined}],
	  'RAW'},
	 {'Neighbor','BAND2G','ONCHAN_SCAN',TimeStamp,
	  [{'Neighbor.NeighborBss',"70:03:7e:1d:84:cd","Shaw",4294967250,
	    1511296311303,'CHAN_WIDTH_UNKNOWN',6,undefined},
	   {'Neighbor.NeighborBss',"bc:4d:fb:1c:a4:2b","SHAW-1CA420-Guest",
	    4294967217,1217418854784,'CHAN_WIDTH_UNKNOWN',6,undefined},
	   {'Neighbor.NeighborBss',"70:03:7e:1d:84:ce",[],4294967250,
	    1511296122884,'CHAN_WIDTH_UNKNOWN',6,undefined},
	   {'Neighbor.NeighborBss',"70:03:7e:1d:84:d2",[],4294967250,
	    1511296123281,'CHAN_WIDTH_UNKNOWN',6,undefined},
	   {'Neighbor.NeighborBss',"70:03:7e:1d:84:d0",[],4294967249,
	    1511296155651,'CHAN_WIDTH_UNKNOWN',6,undefined},
	   {'Neighbor.NeighborBss',"bc:4d:fb:1c:a4:28","SHAW-1CA420",
	    4294967217,1217418854784,'CHAN_WIDTH_UNKNOWN',6,undefined},
	   {'Neighbor.NeighborBss',"a8:4e:3f:5e:8b:78","SHAW-5E8B70",
	    4294967203,5674590413184,'CHAN_WIDTH_UNKNOWN',6,undefined},
	   {'Neighbor.NeighborBss',"2c:56:dc:54:bf:40","ASUS",4294967203,
	    4516988928390,'CHAN_WIDTH_UNKNOWN',6,undefined}],
	  'RAW'},
	 {'Neighbor','BAND5GU','ONCHAN_SCAN',TimeStamp,
	  [{'Neighbor.NeighborBss',"a6:4a:a4:dc:60:c7","Telus5G60C7",
	    4294967218,365965209659,'CHAN_WIDTH_UNKNOWN',149,undefined},
	   {'Neighbor.NeighborBss',"ec:ad:e0:ac:a2:2e","Apex7188B",4294967220,
	    365928448047,'CHAN_WIDTH_UNKNOWN',149,undefined}],
	  'RAW'}];
gen('Survey',TimeStamp)->
		[{'Survey','BAND2G','ON_CHANNEL',TimeStamp,
		[{'Survey.SurveySample',6,907,undefined,undefined,11,1,undefined,
		undefined,13993715,undefined,159},
		{'Survey.SurveySample',6,773,undefined,undefined,7,1,undefined,
		undefined,13993715,undefined,159},
		{'Survey.SurveySample',6,1158,undefined,undefined,10,1,undefined,
		undefined,13993715,undefined,159},
		{'Survey.SurveySample',6,534,undefined,undefined,21,3,undefined,
		undefined,13993715,undefined,159}],
		[],'RAW'},
		{'Survey','BAND5GU','ON_CHANNEL',TimeStamp,
		[{'Survey.SurveySample',149,643,undefined,undefined,1,undefined,
		undefined,undefined,13993731,undefined,158},
		{'Survey.SurveySample',149,521,undefined,undefined,1,undefined,
		undefined,undefined,13993731,undefined,158},
		{'Survey.SurveySample',149,1310,undefined,undefined,1,undefined,
		undefined,undefined,13993731,undefined,158},
		{'Survey.SurveySample',149,1362,undefined,undefined,1,undefined,
		undefined,undefined,13993731,undefined,158}],
		[],'RAW'},
		{'Survey','BAND5GL','ON_CHANNEL',TimeStamp,
		[{'Survey.SurveySample',36,4029,undefined,undefined,undefined,
		undefined,undefined,undefined,14007260,undefined,154}],
		[],'RAW'}].

-spec gen(atom(),string(),[string()],[{atom(),[string()]}],integer(),integer())->any().
gen('ClientReport',_MAC,_MACs,MACSSIDList,TimeStamp,StartTime)->
	WanClients = lists:foldl(fun({Band,SSID,WiFiMACs},A) ->
		[gen_client_report_for_band(TimeStamp,Band,WiFiMACs,SSID,StartTime)|A]
	            end,[],MACSSIDList),
	WanClients.

gen_client_report_for_band(TimeStamp,Band,MACs,SSID,StartTime)->
 #'ClientReport'{
	'band' = Band,
	timestamp_ms = TimeStamp,
	channel = rand:uniform(16),
	client_list = lists:foldl(fun(E,A) ->
			[gen_client_report_unique_client(E,SSID,TimeStamp,StartTime)|A]
		end,[],MACs)}.

gen_client_report_unique_client(Mac,SSID,TimeStamp,StartTime)->
	#'Client'{
		mac_address = Mac,
		ssid = SSID,
		connected = true,
		connect_count = rand:uniform(5),
		disconnect_count = rand:uniform(10),
		stats = get_stats(TimeStamp,StartTime)
	}.

get_stats(TimeStamp,StartTime)->
	#'Client.Stats'{
		rx_bytes = (TimeStamp-StartTime) * (rand:uniform(5)+2),
		tx_bytes = (TimeStamp-StartTime) * (rand:uniform(2)+1),
		rx_frames = ((TimeStamp-StartTime) * (rand:uniform(5)+2)) div 1024,
		tx_frames = ((TimeStamp-StartTime) * (rand:uniform(2)+1)) div 1024,
		tx_retries = rand:uniform(5000),
		rx_retries = rand:uniform(100),
		rx_rate = 1000000,
		tx_rate = 2000000,
		rssi = rand:uniform(20)+75
	}.

