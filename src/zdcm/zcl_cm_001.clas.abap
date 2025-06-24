CLASS zcl_cm_001 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF token_cookies,
             token   TYPE string,
             cookies TYPE if_web_http_request=>cookies,
           END OF token_cookies.

    TYPES: BEGIN OF if_result,
             body   TYPE string,
             status TYPE i,
           END OF if_result.


    METHODS: constructor IMPORTING  i_scenario TYPE string
                                    i_service  TYPE string
                         EXCEPTIONS no_arrangement.

    METHODS: get_token_cookies IMPORTING uri                  TYPE string OPTIONAL
                               RETURNING VALUE(token_cookies) TYPE token_cookies.

    METHODS: get_if_match
      IMPORTING uri            TYPE string
                v4             TYPE string OPTIONAL "v4는 etag양식이 달라 수정함.
      RETURNING VALUE(ifmatch) TYPE string.

    METHODS: get IMPORTING uri              TYPE string
                           json             TYPE string OPTIONAL
                 RETURNING VALUE(if_result) TYPE if_result,

      post IMPORTING json   TYPE string OPTIONAL
                     uri    TYPE string OPTIONAL
                     etag   TYPE string OPTIONAL
           EXPORTING body   TYPE string
                     status TYPE i,

      delete IMPORTING uri    TYPE string
                       etag   TYPE string OPTIONAL
             EXPORTING body   TYPE string
                       status TYPE i,

      patch IMPORTING uri    TYPE string
                      json   TYPE string
                      etag   TYPE string OPTIONAL
            EXPORTING body   TYPE string
                      status TYPE i.
    "통신을 위한 header값
    DATA: token   TYPE string,
          cookies TYPE if_web_http_request=>cookies,
          ifmatch TYPE string.

  PRIVATE SECTION.
    "통신규약 존재 확인
    DATA: scenario TYPE if_com_management=>ty_cscn_id,
          service  TYPE if_com_management=>ty_cscn_outb_srv_id,
          ca       TYPE REF TO if_com_arrangement.

    CONSTANTS:
      c_content_type TYPE string VALUE 'Content-type',
      c_json_content TYPE string VALUE 'application/json; charset=UTF-8'.
ENDCLASS.


CLASS zcl_cm_001 IMPLEMENTATION.
  METHOD constructor.
    me->scenario = i_scenario.
    me->service = i_service.

    "시나리오 이름으로 통신이 존재하는지 확인
    DATA: lr_cscn TYPE if_com_scenario_factory=>ty_query-cscn_id_range.
    lr_cscn = VALUE #( ( sign = 'I' option = 'EQ' low = me->scenario ) ).

    DATA(lo_factory) = cl_com_arrangement_factory=>create_instance( ).
    lo_factory->query_ca(
          EXPORTING
            is_query           = VALUE #( cscn_id_range = lr_cscn )
          IMPORTING
            et_com_arrangement = DATA(lt_ca)
        ).

    IF lt_ca IS NOT INITIAL.
      me->ca = lt_ca[ 1 ].
      me->ca->get_comm_system_id( ).
    ELSE.
      RAISE no_arrangement.
    ENDIF.
  ENDMETHOD.


  METHOD delete.
    TRY.
        DATA(lo_dest) = cl_http_destination_provider=>create_by_comm_arrangement(
            comm_scenario  = me->scenario
            service_id     = me->service
            comm_system_id = me->ca->get_comm_system_id( ) ).
        DATA(lo_http_client) = cl_web_http_client_manager=>create_by_http_destination( lo_dest ).

        DATA(lo_request) = lo_http_client->get_http_request( ).

        lo_request->set_uri_path( EXPORTING i_uri_path = uri ).

        LOOP AT me->cookies INTO DATA(cookie).
          lo_request->set_cookie( i_name = cookie-name i_value = cookie-value ).
        ENDLOOP.

        IF etag IS NOT INITIAL.
          lo_request->set_header_field( i_name = 'If-Match' i_value = etag ).
        ENDIF.


        lo_request->set_header_field( i_name = me->c_content_type   i_value = me->c_json_content ).
        lo_request->set_header_field( i_name = 'Accept' i_value = 'application/json' ).
        lo_request->set_header_field( i_name = 'x-csrf-token' i_value = me->token ).
        DATA(lo_response) = lo_http_client->execute( if_web_http_client=>delete ).

        body   = lo_response->get_text( ).
        status = lo_response->get_status( )-code.

      CATCH cx_http_dest_provider_error INTO DATA(error).
        " handle exception here
        DATA(lv_error) = error.

      CATCH cx_web_http_client_error INTO DATA(error2).
        " handle exception here
        DATA(lv_error2) = error2.
    ENDTRY.
  ENDMETHOD.


  METHOD get.
    TRY.
        DATA(lo_dest) = cl_http_destination_provider=>create_by_comm_arrangement(
            comm_scenario  = me->scenario
            service_id     = me->service
            comm_system_id = me->ca->get_comm_system_id( ) ).
        DATA(lo_http_client) = cl_web_http_client_manager=>create_by_http_destination( lo_dest ).

        DATA(lo_request) = lo_http_client->get_http_request( ).
        "lo_request->set_uri_path( EXPORTING i_uri_path = '?' && uri ).
        lo_request->set_header_field( i_name = me->c_content_type   i_value = me->c_json_content ).
        lo_request->set_header_field( i_name = 'Accept' i_value = 'application/json' ).
        lo_request->set_uri_path( EXPORTING i_uri_path = uri ).

        IF json IS NOT INITIAL.
          lo_request->set_text( json ).
        ENDIF.

        DATA(lo_response) = lo_http_client->execute( if_web_http_client=>get ).
        if_result-body   = lo_response->get_text( ).
        if_result-status = lo_response->get_status( )-code.

      CATCH cx_http_dest_provider_error INTO DATA(err).
        if_result-body = err->get_longtext( ).
        if_result-status = '400'.

      CATCH cx_web_http_client_error INTO DATA(err2).
        if_result-body   = err2->get_longtext( ).
        if_result-status = '400'.

    ENDTRY.
  ENDMETHOD.


  METHOD get_if_match.
    TRY.

        "GET ETAG
        DATA(lo_dest) = cl_http_destination_provider=>create_by_comm_arrangement(
                comm_scenario  = me->scenario
                service_id     = me->service
                comm_system_id = me->ca->get_comm_system_id( ) ).
        DATA(lo_http_client) = cl_web_http_client_manager=>create_by_http_destination( lo_dest ).

        DATA(lo_request) = lo_http_client->get_http_request( ).
        lo_request->set_uri_path( EXPORTING i_uri_path = uri ).

        DATA(lo_response) = lo_http_client->execute( if_web_http_client=>get ).
        DATA(res)   = lo_response->get_text( ).

        "ETAG 파싱-v2,v4 달라서 나눔
        IF v4 IS NOT INITIAL.

          IF ( res CS '"@odata.metadataEtag":' ).
            ifmatch = substring_before( val = substring_after( val = res
                                                               sub = '"@odata.metadataEtag":"' )
                                         sub = '",' ).


            REPLACE ALL OCCURRENCES OF '\' IN ifmatch WITH '' .
          ENDIF.

        ELSE.

          IF ( res CS '<entry m:etag="' ).
            ifmatch = substring_before( val = substring_after( val = res
                                                                  sub = '<entry m:etag="' )
                                           sub = '"' ).
          ENDIF.

          REPLACE ALL OCCURRENCES OF '&quot;' IN ifmatch WITH '"' .
        ENDIF.


        me->ifmatch = ifmatch.

      CATCH cx_http_dest_provider_error.
        " handle exception here
        DATA(error) = '에러'.

      CATCH cx_web_http_client_error.
        " handle exception here
        error = '에러'.

    ENDTRY.

  ENDMETHOD.

  METHOD get_token_cookies.
    TRY.
        DATA(lo_dest) = cl_http_destination_provider=>create_by_comm_arrangement(
            comm_scenario  = me->scenario
            service_id     = me->service
            comm_system_id = me->ca->get_comm_system_id( ) ).
        DATA(lo_http_client) = cl_web_http_client_manager=>create_by_http_destination( lo_dest ).

        DATA(lo_request) = lo_http_client->get_http_request( ).

        IF uri IS NOT INITIAL.
          lo_request->set_uri_path( EXPORTING i_uri_path = uri && '?$top=1' ).
        ELSE.
          lo_request->set_uri_path( EXPORTING i_uri_path = '?$top=1' ).
        ENDIF.

        lo_request->set_header_field( i_name = 'x-csrf-token' i_value = 'fetch' ).
        DATA(lo_response) = lo_http_client->execute( if_web_http_client=>get ).

        me->token   = token_cookies-token   = lo_response->get_header_field( i_name = 'x-csrf-token' ).
        me->cookies = token_cookies-cookies = lo_response->get_cookies( ).
        DATA(cookie1) = lo_response->get_cookies( ).
        DATA(header) = lo_response->get_header_fields( ).
        DATA(status) = lo_response->get_status( ).
        DATA(text) = lo_response->get_text( ).

      CATCH cx_http_dest_provider_error INTO DATA(err).
        " handle exception here
        DATA(error) = err.

      CATCH cx_web_http_client_error INTO DATA(err2).
        " handle exception here
        DATA(error2) = err2.
    ENDTRY.
  ENDMETHOD.


  METHOD patch.
    TRY.
        DATA(lo_dest) = cl_http_destination_provider=>create_by_comm_arrangement(
            comm_scenario  = me->scenario
            service_id     = me->service
            comm_system_id = me->ca->get_comm_system_id( ) ).
        DATA(lo_http_client) = cl_web_http_client_manager=>create_by_http_destination( lo_dest ).

        DATA(lo_request) = lo_http_client->get_http_request( ).

        lo_request->set_text( json ).
        lo_request->set_uri_path( EXPORTING i_uri_path = uri ).

        LOOP AT me->cookies INTO DATA(cookie).
          lo_request->set_cookie( i_name = cookie-name i_value = cookie-value ).
        ENDLOOP.

        IF etag IS NOT INITIAL.
          lo_request->set_header_field( i_name = 'If-Match' i_value = etag ).
        ENDIF.

        lo_request->set_header_field( i_name = me->c_content_type   i_value = me->c_json_content ).
        lo_request->set_header_field( i_name = 'Accept' i_value = 'application/json' ).
        lo_request->set_header_field( i_name = 'x-csrf-token' i_value = me->token ).
        DATA(lo_response) = lo_http_client->execute( if_web_http_client=>patch ).

        body   = lo_response->get_text( ).
        status = lo_response->get_status( )-code.

      CATCH cx_http_dest_provider_error.
        " handle exception here
        DATA(error) = '에러'.

      CATCH cx_web_http_client_error.
        " handle exception here
        error = '에러'.
    ENDTRY.
  ENDMETHOD.


  METHOD post.
    TRY.
        DATA(lo_dest) = cl_http_destination_provider=>create_by_comm_arrangement(
            comm_scenario  = me->scenario
            service_id     = me->service
            comm_system_id = me->ca->get_comm_system_id( ) ).
        DATA(lo_http_client) = cl_web_http_client_manager=>create_by_http_destination( lo_dest ).

        DATA(lo_request) = lo_http_client->get_http_request( ).

        IF json IS NOT INITIAL.
          lo_request->set_text( json ).
        ENDIF.

        IF uri IS NOT INITIAL.
          lo_request->set_uri_path( EXPORTING i_uri_path = uri ).
        ENDIF.

        LOOP AT me->cookies INTO DATA(cookie).
          lo_request->set_cookie( i_name = cookie-name i_value = cookie-value ).
        ENDLOOP.

        IF etag IS NOT INITIAL.
          lo_request->set_header_field( i_name = 'If-Match' i_value = etag ).
        ENDIF.

        lo_request->set_header_field( i_name = me->c_content_type   i_value = me->c_json_content ).
        lo_request->set_header_field( i_name = 'Accept' i_value = 'application/json' ).
        lo_request->set_header_field( i_name = 'x-csrf-token' i_value = me->token ).
        DATA(lo_response) = lo_http_client->execute( if_web_http_client=>post ).

        body   = lo_response->get_text( ).
        status = lo_response->get_status( )-code.

      CATCH cx_http_dest_provider_error.
        " handle exception here
        DATA(error) = '에러'.

      CATCH cx_web_http_client_error.
        " handle exception here
        error = '에러'.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
