%%%-------------------------------------------------------------------
%%% @author stephb
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Oct 2020 11:34 p.m.
%%%-------------------------------------------------------------------
-author("stephb").


-record(mqtt_processor_state, { listener_pid, parent_pid, peer_ip, connection_state , secure}).
