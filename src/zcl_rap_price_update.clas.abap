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
             data_atualizazao TYPE string,
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

ENDCLASS.



CLASS zcl_rap_price_update IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.
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
        APPEND VALUE #( brand_uuid = generate_uuid( )
                        brand_id = <fs_result>-id
                        brand_name = <fs_result>-descritivo
                      ) TO lt_brands.
      ENDIF.
    ENDLOOP.

    IF lt_brands IS NOT INITIAL.
      INSERT ztbrand FROM TABLE @lt_brands.
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
        APPEND VALUE #( fuel_type_uuid = generate_uuid( )
                        fuel_type_id = <fs_result>-id
                        fuel_type_name = <fs_result>-descritivo
                      ) TO lt_ftype.
      ENDIF.
    ENDLOOP.

    IF lt_ftype IS NOT INITIAL.
      INSERT ztfuel_type FROM TABLE @lt_ftype.
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
        APPEND VALUE #( station_type = generate_uuid( )
                        station_type_id = <fs_result>-id
                        station_type_name = <fs_result>-descritivo
                      ) TO lt_stype.
      ENDIF.
    ENDLOOP.

    IF lt_stype IS NOT INITIAL.
      INSERT ztstation_type FROM TABLE @lt_stype.
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

    get_json_payload(
      EXPORTING
        iv_url     = |https://precoscombustiveis.dgeg.gov.pt/api/PrecoComb/PesquisarPostos?qtdPorPagina=99999|
      IMPORTING
        es_payload = ls_result ).

    SELECT * FROM ztstation INTO TABLE @DATA(lt_stations).
    SELECT * FROM ztfuel_price INTO TABLE @DATA(lt_prices).

    LOOP AT ls_result-resultado ASSIGNING FIELD-SYMBOL(<fs_result>).
      IF NOT line_exists( lt_stations[ station_id = <fs_result>-id ] ).
        DATA(ls_station) = VALUE ztstation(
            station_uuid          = generate_uuid( )
            station_id            = <fs_result>-id
            station_name          = <fs_result>-nome
            municipality          = <fs_result>-municipio
            district              = <fs_result>-distrito
            address               = <fs_result>-morada
            city                  = <fs_result>-localidade
            post_code             = <fs_result>-cod_postal
            geolon                = <fs_result>-longitude
            geolat                = <fs_result>-latitude  ).

        SELECT SINGLE station_type FROM ztstation_type
          WHERE station_type_name = @<fs_result>-tipo_posto
          INTO @ls_station-station_type.

        select single brand_uuid from ztbrand
          where brand_name = @<fs_result>-marca
          into @ls_station-brand_uuid.

        APPEND ls_station TO lt_stations.

        data(ls_price) = value ztfuel_price(
            station_uuid = ls_station-station_uuid
            price_uuid   = generate_uuid( )
*            fuel_type    =
*            update_date  =
*            price        =
        ).
      ENDIF.
    ENDLOOP.

    IF lt_stations IS NOT INITIAL.
      INSERT ztstation FROM TABLE @lt_stations.
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


  ENDMETHOD.

ENDCLASS.
