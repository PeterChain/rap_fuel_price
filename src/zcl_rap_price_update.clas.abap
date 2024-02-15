CLASS zcl_rap_price_update DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .

    TYPES: BEGIN OF ty_type_line,
             descritivo TYPE string,
             id         TYPE string,
           END OF ty_type_line.
    TYPES: ty_type_t  TYPE TABLE OF ty_type_line WITH EMPTY KEY.

    TYPES: BEGIN OF ty_fuel_type_line,
             descritivo     TYPE string,
             id             TYPE string,
             unidade_medida TYPE string,
           END OF ty_fuel_type_line.
    TYPES: ty_fuel_type_t TYPE TABLE OF ty_fuel_type_line WITH EMPTY KEY.

    TYPES: BEGIN OF ty_price_point,
             id               TYPE string,
             nome             TYPE string,
             tipo_posto       TYPE string,
             municipio        TYPE string,
             preco            TYPE string,
             marca            TYPE string,
             combustivel      TYPE string,
             data_atualizacao TYPE string,
             distrito         TYPE string,
             morada           TYPE string,
             localidade       TYPE string,
             cod_postal       TYPE string,
             latitude         TYPE string,
             longitude        TYPE string,
           END OF ty_price_point.
    TYPES: ty_price_point_t TYPE TABLE OF ty_price_point WITH EMPTY KEY.

    TYPES: BEGIN OF ty_json_brand,
             status    TYPE string,
             mensagem  TYPE string,
             resultado TYPE ty_type_t,
           END OF ty_json_brand.

    TYPES: BEGIN OF ty_json_station_type,
             status    TYPE string,
             mensagem  TYPE string,
             resultado TYPE ty_type_t,
           END OF ty_json_station_type.

    TYPES: BEGIN OF ty_json_fuel_type,
             status    TYPE string,
             mensagem  TYPE string,
             resultado TYPE ty_fuel_type_t,
           END OF ty_json_fuel_type.

    TYPES: BEGIN OF ty_json_prices,
             status    TYPE string,
             mensagem  TYPE string,
             resultado TYPE ty_price_point_t,
           END OF ty_json_prices.

    TYPES: ty_brands_t TYPE TABLE OF ztbrand WITH EMPTY KEY.
    TYPES: ty_stationtype_t TYPE TABLE OF ztstation_type WITH EMPTY KEY.
    TYPES: ty_fueltype_t TYPE TABLE OF ztfuel_type WITH EMPTY KEY.
    TYPES: ty_station_t TYPE TABLE OF ztstation WITH EMPTY KEY.
    TYPES: ty_fuelprice_t TYPE TABLE OF ztfuel_price WITH EMPTY KEY.
    TYPES: ty_prices_t TYPE TABLE OF ztfuel_price WITH EMPTY KEY.

    TYPES: ty_numc2 TYPE n LENGTH 2.

  PROTECTED SECTION.

  PRIVATE SECTION.
    "! <p class="shorttext synchronized" lang="en">Update Brands DB table</p>
    "!
    "! @parameter ct_msg | <p class="shorttext synchronized" lang="en">Messages</p>
    "! @raising zcx_rap_price_error | <p class="shorttext synchronized" lang="en">Error</p>
    METHODS update_brands
      CHANGING
        ct_msg TYPE symsg_tab
      RAISING
        zcx_rap_price_error.

    "! <p class="shorttext synchronized" lang="en">Update Fuel type DB table</p>
    "!
    "! @parameter ct_msg | <p class="shorttext synchronized" lang="en">Messages</p>
    "! @raising zcx_rap_price_error | <p class="shorttext synchronized" lang="en">Error</p>
    METHODS update_fuel_type
      CHANGING
        ct_msg TYPE symsg_tab
      RAISING
        zcx_rap_price_error.

    "! <p class="shorttext synchronized" lang="en">Update Station type DB table</p>
    "!
    "! @parameter ct_msg | <p class="shorttext synchronized" lang="en">Messages</p>
    "! @raising zcx_rap_price_error | <p class="shorttext synchronized" lang="en">Error</p>
    METHODS update_station_type
      CHANGING
        ct_msg TYPE symsg_tab
      RAISING
        zcx_rap_price_error.

    "! <p class="shorttext synchronized" lang="en">Update Station and prices DB table</p>
    "!
    "! @parameter ct_msg | <p class="shorttext synchronized" lang="en">Messages</p>
    "! @raising zcx_rap_price_error | <p class="shorttext synchronized" lang="en">Error</p>
    METHODS update_station_price
      CHANGING
        ct_msg TYPE symsg_tab
      RAISING
        zcx_rap_price_error.

    "! <p class="shorttext synchronized" lang="en">Get the payload for a given URL</p>
    "!
    "! @parameter iv_url | <p class="shorttext synchronized" lang="en">URL</p>
    "! @parameter es_payload | <p class="shorttext synchronized" lang="en">ABAP structure with the payload content</p>
    "! @raising zcx_rap_price_error | <p class="shorttext synchronized" lang="en">Error</p>
    METHODS get_json_payload
      IMPORTING
        iv_url     TYPE string
      EXPORTING
        es_payload TYPE any
      RAISING
        zcx_rap_price_error.

    "! <p class="shorttext synchronized" lang="en">Generate GUID</p>
    "!
    "! @parameter result | <p class="shorttext synchronized" lang="en">Generated GUID</p>
    "! @raising zcx_rap_price_error | <p class="shorttext synchronized" lang="en">Error</p>
    METHODS generate_uuid
      RETURNING
        VALUE(result) TYPE sysuuid_x16
      RAISING
        zcx_rap_price_error.

    "! <p class="shorttext synchronized" lang="en">Converts string date into SAP date format</p>
    "!
    "! @parameter iv_date_str | <p class="shorttext synchronized" lang="en">String</p>
    "! @parameter result | <p class="shorttext synchronized" lang="en">Date</p>
    METHODS convert_date
      IMPORTING
        iv_date_str   TYPE string
      RETURNING
        VALUE(result) TYPE datum.

    "! <p class="shorttext synchronized" lang="en">Converts price in string into SAP decimal format</p>
    "!
    "! @parameter iv_price_str | <p class="shorttext synchronized" lang="en">Price string</p>
    "! @parameter result | <p class="shorttext synchronized" lang="en">Price</p>
    METHODS convert_price
      IMPORTING
        iv_price_str  TYPE string
      RETURNING
        VALUE(result) TYPE zfuel_price.

    "! <p class="shorttext synchronized" lang="en">Returns the max sequential ID for a Station/Fuel type combo</p>
    "!
    "! @parameter iv_station_uuid | <p class="shorttext synchronized" lang="en">Fuel station UUID</p>
    "! @parameter iv_fuel_type_id | <p class="shorttext synchronized" lang="en">Fuel type ID</p>
    "! @parameter it_prices | <p class="shorttext synchronized" lang="en">Price table</p>
    "! @parameter result | <p class="shorttext synchronized" lang="en">Max seq ID for the Station/Fuel type combo</p>
    METHODS get_max_id
      IMPORTING
        iv_station_uuid   TYPE sysuuid_x16
        iv_fuel_type_id   TYPE zfuel_type_id
        it_prices         TYPE ty_prices_t
      RETURNING
        VALUE(result)     TYPE ty_numc2.

ENDCLASS.



CLASS zcl_rap_price_update IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.
    DATA(lt_msg) = VALUE symsg_tab( ).

    DELETE FROM ztfuel_price WHERE station_uuid IS NOT INITIAL.

    TRY.
        update_brands( CHANGING ct_msg = lt_msg ).
        update_fuel_type( CHANGING ct_msg = lt_msg ).
        update_station_type( CHANGING ct_msg = lt_msg ).
        update_station_price( CHANGING ct_msg = lt_msg ).
      CATCH zcx_rap_price_error INTO DATA(lo_error).
        out->write( |Error: { lo_error->get_text( ) }| ).
    ENDTRY.

    LOOP AT lt_msg REFERENCE INTO DATA(lo_msg).
      MESSAGE ID lo_msg->msgid TYPE lo_msg->msgty NUMBER lo_msg->msgno
        WITH lo_msg->msgv1 lo_msg->msgv2 lo_msg->msgv3 lo_msg->msgv4
        INTO DATA(lv_msg).
      out->write( |Msg: { lv_msg }| ).
    ENDLOOP.
  ENDMETHOD.


  METHOD generate_uuid.
    TRY.
        result = cl_system_uuid=>create_uuid_x16_static( ).
      CATCH cx_uuid_error.
        RAISE EXCEPTION TYPE zcx_rap_price_error MESSAGE e005(zrap_price).
    ENDTRY.
  ENDMETHOD.


  METHOD get_json_payload.
    DATA ls_msg   TYPE symsg.

    TRY.
        DATA(lo_dest) = cl_http_destination_provider=>create_by_url( iv_url ).
        DATA(lo_client) = cl_web_http_client_manager=>create_by_http_destination( lo_dest ).
      CATCH cx_static_check INTO DATA(lo_http_error).
        ls_msg = cl_message_helper=>get_t100_for_object( CAST #( lo_http_error ) ).
        RAISE EXCEPTION TYPE zcx_rap_price_error MESSAGE e001(zrap_price) EXPORTING previous = lo_http_error.
    ENDTRY.

    TRY.
        DATA(lv_response) = lo_client->execute( if_web_http_client=>get )->get_text( ).
      CATCH cx_static_check INTO DATA(lo_req_error).
        ls_msg = cl_message_helper=>get_t100_for_object( CAST #( lo_req_error ) ).
        RAISE EXCEPTION TYPE zcx_rap_price_error MESSAGE e002(zrap_price) EXPORTING previous = lo_req_error.
    ENDTRY.

    /ui2/cl_json=>deserialize(
      EXPORTING
        json             = lv_response
        pretty_name      = /ui2/cl_json=>pretty_mode-camel_case
      CHANGING
        data             = es_payload ).
  ENDMETHOD.


  METHOD update_brands.
    DATA ls_result TYPE ty_json_brand.

    get_json_payload(
      EXPORTING
        iv_url     = |https://precoscombustiveis.dgeg.gov.pt/api/PrecoComb/GetMarcas|
      IMPORTING
        es_payload = ls_result ).

    SELECT * FROM ztbrand INTO TABLE @DATA(lt_brands).

    LOOP AT ls_result-resultado ASSIGNING FIELD-SYMBOL(<fs_result>).
      IF NOT line_exists( lt_brands[ brand_id = <fs_result>-id ] ).
        APPEND VALUE #( brand_id = <fs_result>-id
                        brand_name = <fs_result>-descritivo
                      ) TO lt_brands.
      ENDIF.
    ENDLOOP.

    IF lt_brands IS NOT INITIAL.
      MODIFY ztbrand FROM TABLE @lt_brands.
      IF sy-subrc IS INITIAL.
        APPEND VALUE #( msgty = 'S'
                        msgid = 'ZRAP_PRICE'
                        msgno = 006
                        msgv1 = lines( lt_brands )
                        msgv2 = 'ZTBRAND'
                      ) TO ct_msg.
      ENDIF.
    ELSE.
      APPEND VALUE #( msgty = 'S'
                      msgid = 'ZRAP_PRICE'
                      msgno = 007
                      msgv1 = 'ZTBRAND'
                    ) TO ct_msg.
    ENDIF.
  ENDMETHOD.


  METHOD update_fuel_type.
    DATA ls_result TYPE ty_json_fuel_type.

    get_json_payload(
      EXPORTING
        iv_url     = |https://precoscombustiveis.dgeg.gov.pt/api/PrecoComb/GetTiposCombustiveis|
      IMPORTING
        es_payload = ls_result ).

    SELECT * FROM ztfuel_type INTO TABLE @DATA(lt_ftype).

    LOOP AT ls_result-resultado ASSIGNING FIELD-SYMBOL(<fs_result>).
      IF NOT line_exists( lt_ftype[ fuel_type_id = <fs_result>-id ] ).
        APPEND VALUE #( fuel_type_id = <fs_result>-id
                        fuel_type_name = <fs_result>-descritivo
                      ) TO lt_ftype.
      ENDIF.
    ENDLOOP.

    IF lt_ftype IS NOT INITIAL.
      MODIFY ztfuel_type FROM TABLE @lt_ftype.
      IF sy-subrc IS INITIAL.
        APPEND VALUE #( msgty = 'S'
                        msgid = 'ZRAP_PRICE'
                        msgno = 006
                        msgv1 = lines( lt_ftype )
                        msgv2 = 'ZTFUEL_TYPE'
                      ) TO ct_msg.
      ENDIF.
    ELSE.
      APPEND VALUE #( msgty = 'S'
                      msgid = 'ZRAP_PRICE'
                      msgno = 007
                      msgv1 = 'ZTFUEL_TYPE'
                    ) TO ct_msg.
    ENDIF.
  ENDMETHOD.


  METHOD update_station_type.
    DATA ls_result TYPE ty_json_fuel_type.

    get_json_payload(
      EXPORTING
        iv_url     = |https://precoscombustiveis.dgeg.gov.pt/api/PrecoComb/GetTiposPostos|
      IMPORTING
        es_payload = ls_result ).

    SELECT * FROM ztstation_type INTO TABLE @DATA(lt_stype).

    LOOP AT ls_result-resultado ASSIGNING FIELD-SYMBOL(<fs_result>).
      IF NOT line_exists( lt_stype[ station_type_id = <fs_result>-id ] ).
        APPEND VALUE #( station_type_id = <fs_result>-id
                        station_type_name = <fs_result>-descritivo
                      ) TO lt_stype.
      ENDIF.
    ENDLOOP.

    IF lt_stype IS NOT INITIAL.
      MODIFY ztstation_type FROM TABLE @lt_stype.
      IF sy-subrc IS INITIAL.
        APPEND VALUE #( msgty = 'S'
                        msgid = 'ZRAP_PRICE'
                        msgno = 006
                        msgv1 = lines( lt_stype )
                        msgv2 = 'ZTSTATION_TYPE'
                      ) TO ct_msg.
      ENDIF.
    ELSE.
      APPEND VALUE #( msgty = 'S'
                      msgid = 'ZRAP_PRICE'
                      msgno = 007
                      msgv1 = 'ZTSTATION_TYPE'
                    ) TO ct_msg.
    ENDIF.
  ENDMETHOD.


  METHOD update_station_price.
    DATA ls_result TYPE ty_json_prices.
    DATA lv_upd_date TYPE datum.

    get_json_payload(
      EXPORTING
        iv_url     = |https://precoscombustiveis.dgeg.gov.pt/api/PrecoComb/PesquisarPostos?qtdPorPagina=99999|
      IMPORTING
        es_payload = ls_result ).

    SELECT * FROM ztstation INTO TABLE @DATA(lt_stations).
    SELECT * FROM ztfuel_price INTO TABLE @DATA(lt_prices).

    LOOP AT ls_result-resultado ASSIGNING FIELD-SYMBOL(<fs_result>).
      IF NOT line_exists( lt_stations[ station_id = <fs_result>-id ] ).
        DATA(ls_station) = VALUE ztstation( station_uuid  = generate_uuid( )
                                            station_id    = <fs_result>-id
                                            station_name  = <fs_result>-nome
                                            municipality  = <fs_result>-municipio
                                            district      = <fs_result>-distrito
                                            address       = <fs_result>-morada
                                            city          = <fs_result>-localidade
                                            post_code     = <fs_result>-cod_postal
                                            geolon        = <fs_result>-longitude
                                            geolat        = <fs_result>-latitude  ).

        SELECT SINGLE station_type_id FROM ztstation_type
          WHERE station_type_name = @<fs_result>-tipo_posto
          INTO @ls_station-station_type.

        SELECT SINGLE brand_id FROM ztbrand
          WHERE brand_name = @<fs_result>-marca
          INTO @ls_station-brand.

        APPEND ls_station TO lt_stations.

        SELECT SINGLE fuel_type_id FROM ztfuel_type
          WHERE fuel_type_name = @<fs_result>-combustivel
          INTO @DATA(lv_fuel_type).

        lv_upd_date = convert_date( <fs_result>-data_atualizacao ).
        DATA(ls_price) = VALUE ztfuel_price( station_uuid = ls_station-station_uuid
                                             price_uuid   = generate_uuid( )
                                             fuel_type    = lv_fuel_type
                                             price_id     = |{ lv_upd_date(4) }{ ls_station-station_id }01|
                                             update_date  = lv_upd_date
                                             price        = convert_price( <fs_result>-preco )  ).

        APPEND ls_price TO lt_prices.

      ELSE.
        DATA(lv_station_uuid) = lt_stations[ station_id = <fs_result>-id ]-station_uuid.

        SELECT SINGLE fuel_type_id FROM ztfuel_type
          WHERE fuel_type_name = @<fs_result>-combustivel
          INTO @lv_fuel_type.

        IF NOT line_exists( lt_prices[ station_uuid = lv_station_uuid
                                       fuel_type = lv_fuel_type ] ).
          DATA(lv_price_seq) = get_max_id( iv_station_uuid   = lv_station_uuid
                                           iv_fuel_type_id   = lv_fuel_type
                                           it_prices         = lt_prices ).
          lv_price_seq += 1.

          lv_upd_date = convert_date( <fs_result>-data_atualizacao ).
          ls_price = VALUE ztfuel_price( station_uuid = lv_station_uuid
                                         price_uuid   = generate_uuid( )
                                         price_id     = |{ lv_upd_date(4) }{ ls_station-station_id }{ lv_price_seq }|
                                         update_date  = lv_upd_date
                                         price        = convert_price( <fs_result>-preco )
                                         fuel_type    = lv_fuel_type  ).

          APPEND ls_price TO lt_prices.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF lt_stations IS NOT INITIAL.
      MODIFY ztstation FROM TABLE @lt_stations.
      IF sy-subrc IS INITIAL.
        APPEND VALUE #( msgty = 'S'
                        msgid = 'ZRAP_PRICE'
                        msgno = 006
                        msgv1 = lines( lt_stations )
                        msgv2 = 'ZTSTATION'
                      ) TO ct_msg.
      ENDIF.
    ELSE.
      APPEND VALUE #( msgty = 'S'
                      msgid = 'ZRAP_PRICE'
                      msgno = 007
                      msgv1 = 'ZTSTATION'
                    ) TO ct_msg.
    ENDIF.

    IF lt_prices IS NOT INITIAL.
      MODIFY ztfuel_price FROM TABLE @lt_prices.
      IF sy-subrc IS INITIAL.
        APPEND VALUE #( msgty = 'S'
                        msgid = 'ZRAP_PRICE'
                        msgno = 006
                        msgv1 = lines( lt_prices )
                        msgv2 = 'ZTFUEL_PRICE'
                      ) TO ct_msg.
      ENDIF.
    ELSE.
      APPEND VALUE #( msgty = 'S'
                      msgid = 'ZRAP_PRICE'
                      msgno = 007
                      msgv1 = 'ZTFUEL_PRICE'
                    ) TO ct_msg.
    ENDIF.
  ENDMETHOD.


  METHOD convert_date.
    result = |{ iv_date_str(4) }{ iv_date_str+5(2) }{ iv_date_str+8(2) }|.
  ENDMETHOD.


  METHOD convert_price.
    DATA(lv_str) = replace( val = iv_price_str   sub = '€'   with = `` ).
    lv_str = replace( val = lv_str   sub = ','   with = '.' ).
    lv_str = condense( val = lv_str ).
    result = lv_str.
  ENDMETHOD.


  METHOD get_max_id.
    DATA lt_seq   TYPE TABLE OF ty_numc2.

    LOOP AT it_prices ASSIGNING FIELD-SYMBOL(<fs_price>)
        WHERE station_uuid = iv_station_uuid
          AND fuel_type = iv_fuel_type_id.
      CHECK strlen( <fs_price>-price_id ) >= 2.
      DATA(lv_seq) = substring( val = <fs_price>-price_id
                                off = -2 ).
      APPEND lv_seq TO lt_seq.
    ENDLOOP.

    SORT lt_seq BY table_line DESCENDING.
    result = VALUE #( lt_seq[ 1 ] OPTIONAL ).
  ENDMETHOD.

ENDCLASS.
