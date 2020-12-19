%%%-----------------------------------------------------------------------------
%%% @author helge
%%% @copyright (C) 2020, Arilia Wireless Inc.
%%% @doc
%%% 
%%% @end
%%% Created : 17. December 2020 @ 11:21:29
%%%-----------------------------------------------------------------------------
-module(ovsdb_dba).
-author("helge").

-include("../include/common.hrl").
-include("../include/ovsdb_ap_tables.hrl").

-export ([select/3,select_with_key/3,delete/3,delete_ret/3,insert/3,update/4,mutate_table/4]).


%%-----------------------------------------------------------------------------
%% common types
%%-----------------------------------------------------------------------------

-type selspec() :: [any()].
-type matchspec() :: [{tuple(),list(),list()},...].
-type db_record() :: #{binary()=>term()}.
-type db_records() :: [db_record()].

-export_type ([selspec/0]).


%%-----------------------------------------------------------------------------
%% API: table / database access
%%-----------------------------------------------------------------------------


-spec select (TableName :: binary(), Where :: selspec(), Store :: ets:tid()) -> Rows :: db_records().
select (T,W,S) ->
    MSpec = create_match_spec(T,W),
    R = ets:select(S,MSpec),
    Fields = tl(rec_fields(T)),                          % drop the first field since that is our internal key index
    [ maps:from_list(lists:zip(Fields,tl(X))) || X <- R ].

-spec select_with_key (TableName :: binary(), Where :: selspec(), Store :: ets:tid()) -> [{Key :: binary(), db_record()}].
select_with_key(T,W,S) ->
    MSpec = create_match_spec(T,W),
    R = ets:select(S,MSpec),
    [ maps:take(<<"**key_id**">>,X) || X <- [ maps:from_list(lists:zip(rec_fields(T),X)) || X <- R ]].



-spec delete (TableName :: binary(), Where :: selspec(), Store :: ets:tid()) -> NumDeleted :: integer().
delete (T,W,S) ->
    MSpec = create_match_spec(T,W),
    DelSpec = [ setelement(3,X,[true]) || X <- MSpec ],
    ets:select_delete(S,DelSpec).

-spec delete_ret (TableName :: binary(), Where :: selspec(), Store :: ets:tid()) -> DeletedRows :: [tuple()].
delete_ret (T,W,S) ->
    MSpec = create_match_spec(T,W),
    DelSpec = [ setelement(3,X,[true]) || X <- MSpec ],
    R = ets:select(S,MSpec),
    _ = ets:select_delete(S,DelSpec),
    Fields = tl(rec_fields(T)),                          % drop the first field since that is our internal key index
    [ maps:from_list(lists:zip(Fields,tl(X))) || X <- R ].

-spec insert (TableName :: binary(), Records :: db_record() | db_records(), Store :: ets:tid()) -> RowUUIDs :: [binary()].
insert (T,R,S) when is_map(R) ->
    insert (T,[R],S);
insert (T,R,S) ->
    {UUIDs,ModRec} = lists:unzip([ make_row_uuid(T,X) || X <- R ]),
    check_for_ssid(T,R),
    ets:insert(S,ModRec),
    UUIDs.

-spec update (TableName :: binary(), Record :: db_record(), Where :: selspec(), Store :: ets:tid()) -> RowUUIDs :: [binary()].
update (T,R,W,S) ->
    MSpec = create_match_spec(T,W),
    DelSpec = [ setelement(3,X,[true]) || X <- MSpec ],
    Res = ets:select(S,MSpec),
    _ = ets:select_delete(S,DelSpec),
    Old = [ maps:from_list(lists:zip(rec_fields(T),X)) || X <- Res ],
    {UUIDs,ModRec} = lists:unzip([ modify_row_record(T,X,R) || X <- Old ]),
    check_for_ssid(T,R),
    ets:insert(S,ModRec),
    UUIDs.

-spec mutate_table (TableName :: binary(), Mutations :: [[any()]], Where :: selspec(), Store :: ets:tid()) -> RowsAffected :: integer().
mutate_table (Table,Mut,Where,Store) ->
    MSpec = create_match_spec(Table,Where),
    DelSpec = [ setelement(3,X,[true]) || X <- MSpec ],
    Res = ets:select(Store,MSpec),
    _ = ets:select_delete(Store,DelSpec),
    ToMutate = [ maps:from_list(lists:zip(rec_fields(Table),X)) || X <- Res ],
    Mutated = [ mutate_table_row(Table,Mut,X) || X <- ToMutate ],
    ets:insert(Store,Mutated),
    length(Mutated).

%%-----------------------------------------------------------------------------
%% internal DB access functions
%%-----------------------------------------------------------------------------


-spec make_row_uuid (TableName :: binary(), Record :: db_record()) -> {UUID :: binary(), ModRecord :: tuple()}.
make_row_uuid (T,R) ->
    Fields = rec_fields(T),
    D_map = maps:from_list(lists:zip(Fields,default_values(T))),
    Ins_map = maps:merge(D_map,R),
    UUID = utils:uuid_b(),
    F = fun ({<<"_uuid">>,[<<"uuid">>,_]}) ->
                {<<"_uuid">>,[<<"uuid">>,UUID]};
            ({<<"**key_id**">>,_}) ->
                {<<"**key_id**">>,UUID};
            (X) ->
                X
        end,
    URec = maps:from_list([ F(X) || X <- maps:to_list(Ins_map) ]),
    { UUID, map_to_record(T,URec)}.

-spec modify_row_record (TableName :: binary(), OldRecord :: db_record(), NewRecord :: db_record()) -> {UUID :: binary(), ModRecord :: tuple()}.
modify_row_record(T,O,N) ->
    Fields = rec_fields(T),
    D_map = maps:from_list(lists:zip(Fields,default_values(T))),
    Ins_map = maps:merge(maps:merge(D_map,O),N),
    #{<<"**key_id**">>:=KeyID } = O,
    UUID = case N of 
                #{<<"_uuid">>:=[<<"uuid">>,V]} ->
                    V;
                #{<<"_uuid">>:=V} when is_binary(V) ->
                    V;
                _ ->
                utils:uuid_b()
           end,
    F = fun ({<<"_uuid">>,[<<"uuid">>,_]}) ->
                {<<"_uuid">>,[<<"uuid">>,UUID]};
            ({<<"**key_id**">>,_}) ->
                {<<"**key_id**">>,KeyID};
            (X) ->
                X
        end,
    URec = maps:from_list([ F(X) || X <- maps:to_list(Ins_map) ]),    
    { KeyID, map_to_record(T,URec)}.

-spec mutate_table_row (TableName :: binary(), Mutations :: [[any()]], Where :: [any()]) -> MutatedRecord :: tuple().
mutate_table_row (T,[],MutRow) ->
    map_to_record(T,MutRow);
mutate_table_row (Table,[Mut|Tail],MutRow) ->
	Mutated = apply_mutation(Mut,MutRow),
    mutate_table_row(Table,Tail,Mutated).

-spec apply_mutation (Mutations :: [], ToMutate :: #{binary()=>any()}) -> Mutated :: #{binary()=>any()}.
apply_mutation ([Field,<<"insert">>,What],RowMap) when is_map_key(Field,RowMap) ->
    #{Field:=V} = RowMap,
    NewValue = case V of
        [K,Up] when is_list(V) ->
            [K,lists:reverse([What | lists:reverse(Up)])];
        _ ->
            V
    end,
    RowMap#{Field=>NewValue};
apply_mutation ([F,Op,_],RowMap) ->
    ?L_EA("table mutation with operation '~s' on Fields '~s' not supported!",[Op,F]),
    RowMap.

-spec check_for_ssid(TableName ::  binary(), Record ::  [#{binary()=>any()}]) -> ok.
check_for_ssid (_T,[R]) when is_map(R) andalso is_map_key(<<"ssid">>,R) ->
    #{<<"ssid">>:=SSID} = R,
    ovsdb_ap:set_ssid(self(),SSID);
check_for_ssid (_,_) ->
    ok.


%%-----------------------------------------------------------------------------
%% internal functions
%%-----------------------------------------------------------------------------

-spec create_match_spec (TableName :: binary(), Where :: selspec()) -> MatchSpec :: matchspec().
create_match_spec (R,W) ->
    Op = #{<<"==">>=>'==', <<"!=">>=>'/=', <<"<=">>=>'<=', <<"<">>=>'<', <<">=">>=>'>=', <<">">>=>'>'},
    Fields = rec_fields(R),
    MP = [binary_to_atom(list_to_binary([$$,integer_to_list(X)])) || X<-lists:seq(1,length(Fields))],
    C = [{maps:get(O,Op,'=='),field_idx(A1,Fields,1),field_idx(A2,Fields,1)}|| [A1,O,A2] <- W],
    [{list_to_tuple([binary_to_atom(R)|MP]),C,['$$']}].

field_idx (F,[],_) -> F;
field_idx (F,[F|_],N) -> binary_to_atom(list_to_binary([$$,integer_to_list(N)]));
field_idx (F,[_|T],N) -> field_idx(F,T,N+1).

-spec map_to_record (TableName :: binary(), Map :: #{binary()=>any()}) -> Record :: tuple().
map_to_record (T,M) ->
    Fields = rec_fields(T),
    List = [ maps:get(X,M,<<"###CRASH###">>) || X <- Fields],
    list_to_tuple([binary_to_atom(T) | List]).


%%------------------------------------------------------------------------------
%% record convertion helpers
-spec rec_fields (RecordName :: binary()) -> Fieldnames :: [binary()].
rec_fields (<<"Wifi_Inet_Config">>) ->
    [atom_to_binary(X)||X<-record_info(fields,'Wifi_Inet_Config')];
rec_fields (<<"Wifi_Radio_Config">>) ->
    [atom_to_binary(X)||X<-record_info(fields,'Wifi_Radio_Config')];
rec_fields (<<"Wifi_VIF_Config">>) ->
    [atom_to_binary(X)||X<-record_info(fields,'Wifi_VIF_Config')];
rec_fields (<<"Wifi_VIF_State">>) ->
    [atom_to_binary(X)||X<-record_info(fields,'Wifi_VIF_State')];
rec_fields (<<"Wifi_Associated_Clients">>) ->
    [atom_to_binary(X)||X<-record_info(fields,'Wifi_Associated_Clients')];
rec_fields (<<"Command_State">>) ->
    [atom_to_binary(X)||X<-record_info(fields,'Command_State')];
rec_fields (<<"DHCP_leased_IP">>) ->
    [atom_to_binary(X)||X<-record_info(fields,'DHCP_leased_IP')];
rec_fields (<<"Wifi_RRM_Config">>) ->
    [atom_to_binary(X)||X<-record_info(fields,'Wifi_RRM_Config')];
rec_fields (<<"Hotspot20_Icon_Config">>) ->
    [atom_to_binary(X)||X<-record_info(fields,'Hotspot20_Icon_Config')];
rec_fields (<<"Hotspot20_OSU_Providers">>) ->
    [atom_to_binary(X)||X<-record_info(fields,'Hotspot20_OSU_Providers')];
rec_fields (<<"Hotspot20_Config">>) ->
    [atom_to_binary(X)||X<-record_info(fields,'Hotspot20_Config')];
rec_fields (<<"Wifi_Stats_Config">>) ->
    [atom_to_binary(X)||X<-record_info(fields,'Wifi_Stats_Config')];
rec_fields (<<"Wifi_Radio_State">>) ->
    [atom_to_binary(X)||X<-record_info(fields,'Wifi_Radio_State')];
rec_fields (<<"Wifi_Inet_State">>) ->
    [atom_to_binary(X)||X<-record_info(fields,'Wifi_Inet_State')];
rec_fields (<<"AWLAN_Node">>) ->
    [atom_to_binary(X)||X<-record_info(fields,'AWLAN_Node')].

-spec default_record(RecordName::binary()) -> Record :: tuple().
default_record (<<"Wifi_Inet_Config">>) ->
    #'Wifi_Inet_Config'{};
default_record (<<"Wifi_Radio_Config">>) ->
    #'Wifi_Radio_Config'{};
default_record (<<"Wifi_VIF_Config">>) ->
    #'Wifi_VIF_Config'{};
default_record (<<"Wifi_VIF_State">>) ->
    #'Wifi_VIF_State'{};
default_record (<<"Wifi_Associated_Clients">>) ->
    #'Wifi_Associated_Clients'{};
default_record (<<"Command_State">>) ->
    #'Command_State'{};
default_record (<<"DHCP_leased_IP">>) ->
    #'DHCP_leased_IP'{};
default_record (<<"Wifi_RRM_Config">>) ->
    #'Wifi_RRM_Config'{};
default_record (<<"Hotspot20_Icon_Config">>) ->
    #'Hotspot20_Icon_Config'{};
default_record (<<"Hotspot20_OSU_Providers">>) ->
    #'Hotspot20_OSU_Providers'{};
default_record (<<"Hotspot20_Config">>) ->
    #'Hotspot20_Config'{};
default_record (<<"Wifi_Stats_Config">>) ->
    #'Wifi_Stats_Config'{};
default_record (<<"Wifi_Radio_State">>) ->
    #'Wifi_Radio_State'{};
default_record (<<"Wifi_Inet_State">>) ->
    #'Wifi_Inet_State'{};
default_record (<<"AWLAN_Node">>) ->
    #'AWLAN_Node'{}.

-spec default_values(RecordName::binary()) -> Values :: [term()].
default_values(T) ->
    [_|R] = tuple_to_list(default_record(T)),
    R.
    
