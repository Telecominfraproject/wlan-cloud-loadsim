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

gen_report(StartTime,ClientInfo,_MACs,MACSSIDList)->
	TimeStamp = os:system_time(),
	TR = #'Report'{ nodeID = ClientInfo#client_info.serial,
	                device = [ #'Device'{
		                timestamp_ms = TimeStamp,
		                uptime = TimeStamp-StartTime,
		                load = gen('Device.LoadAvg'),
		                mem_util = gen('Device.MemUtil'),
		                fs_util = gen('Device.FsUtil'),
		                cpuUtil = gen('Device.CpuUtil'),
		                thermal_stats = gen('Device.Thermal'),
		                radio_temp = gen('Device.RadioTemp'),
		                ps_cpu_util = gen('Device.PerProcessUtil',ps_cpu_util),
		                ps_mem_util = gen('Device.PerProcessUtil',ps_mem_util)
	                }],
	                neighbors = gen('Neighbor'),
	                clients = gen('ClientReport',MACSSIDList)
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
	{'Device.CpuUtil',67};
gen('Device.Thermal')->
	[{'Device.Thermal',
	[{'Device.Thermal.RadioTxChainMask','BAND2G',3},
	{'Device.Thermal.RadioTxChainMask','BAND5GU',3},
	{'Device.Thermal.RadioTxChainMask','BAND5GL',3}],
	0,1607730010978}];
gen('Device.RadioTemp')->
	[{'Device.RadioTemp','BAND2G',67},
	{'Device.RadioTemp','BAND5GU',52},
	{'Device.RadioTemp','BAND5GL',64}];
gen('Neighbor')->
	[{'Neighbor','BAND2G','OFFCHAN_SCAN',1607729947907,
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
	 {'Neighbor','BAND2G','ONCHAN_SCAN',1607729945449,
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
	 {'Neighbor','BAND5GU','ONCHAN_SCAN',1607729947914,
	  [{'Neighbor.NeighborBss',"a6:4a:a4:dc:60:c7","Telus5G60C7",
	    4294967218,365965209659,'CHAN_WIDTH_UNKNOWN',149,undefined},
	   {'Neighbor.NeighborBss',"ec:ad:e0:ac:a2:2e","Apex7188B",4294967220,
	    365928448047,'CHAN_WIDTH_UNKNOWN',149,undefined}],
	  'RAW'}].

-spec gen( atom(), Qualifier::atom()) ->any();
         ( atom(), MACSSIDList::[{atom(),string(),[string()]}] ) ->any().
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
gen('ClientReport',MACSSIDList)->
	TimeStamp = os:system_time(),
	lists:foldl(fun({Band,SSID,MACs},A) ->
		[gen_client_report_for_band(TimeStamp,Band,MACs,SSID)|A]
	            end,[],MACSSIDList).

gen_client_report_for_band(TimeStamp,Band,MACs,SSID)->
 #'ClientReport'{
	'band' = Band,
	timestamp_ms = TimeStamp,
	channel = rand:uniform(16),
	client_list = lists:foldl(fun(E,A) ->
			[gen_client_report_unique_client(E,SSID)|A]
		end,[],MACs)}.

gen_client_report_unique_client(Mac,SSID)->
	{'Client',Mac,SSID,false,1,1,40001,
	20001,0,
	{'Client.Stats',700,590,6,5,undefined,undefined,1,undefined,
	6.0e3,6.0e3,4294967239},
	[],[],[],undefined}.


