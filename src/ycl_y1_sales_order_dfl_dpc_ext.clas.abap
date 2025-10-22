CLASS ycl_y1_sales_order_dfl_dpc_ext DEFINITION
  PUBLIC
  INHERITING FROM ycl_y1_sales_order_dfl_dpc
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS /iwbep/if_mgw_appl_srv_runtime~create_deep_entity
        REDEFINITION .
    METHODS /iwbep/if_mgw_appl_srv_runtime~execute_action
        REDEFINITION .
  PROTECTED SECTION.

    METHODS returnmessages_create_entity
        REDEFINITION .
    METHODS returnmessages_delete_entity
        REDEFINITION .
    METHODS returnmessages_get_entity
        REDEFINITION .
    METHODS returnmessages_get_entityset
        REDEFINITION .
    METHODS returnmessages_update_entity
        REDEFINITION .
    METHODS salesorderheader_create_entity
        REDEFINITION .
    METHODS salesorderheader_delete_entity
        REDEFINITION .
    METHODS salesorderheader_get_entity
        REDEFINITION .
    METHODS salesorderheader_get_entityset
        REDEFINITION .
    METHODS salesorderheader_update_entity
        REDEFINITION .
    METHODS salesorderitems_create_entity
        REDEFINITION .
    METHODS salesorderitems_delete_entity
        REDEFINITION .
    METHODS salesorderitems_get_entity
        REDEFINITION .
    METHODS salesorderitems_get_entityset
        REDEFINITION .
    METHODS salesorderitems_update_entity
        REDEFINITION .
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_y1_sales_order_dfl_dpc_ext IMPLEMENTATION.


  METHOD /iwbep/if_mgw_appl_srv_runtime~create_deep_entity.

    DATA: ls_deep_entity TYPE ycl_y1_sales_order_dfl_mpc_ext=>ty_sales_order_and_item. " Updated type name
    DATA: ls_deep_item   TYPE ycl_y1_sales_order_dfl_mpc_ext=>ts_salesorderitem.

    DATA: ls_header      TYPE y1tsalesord_dflc. " Updated table name
    DATA: lt_items       TYPE STANDARD TABLE OF y1tsalesits_dflc. " Updated table name
    DATA: ls_item        TYPE y1tsalesits_dflc. " Updated table name
    DATA: lv_operation   TYPE char1.
    DATA: lv_datetime(14) TYPE c.

    " Get message container for error handling
    DATA(lo_msg) = me->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

    " Read incoming deep entity data
    CALL METHOD io_data_provider->read_entry_data
      IMPORTING
        es_data = ls_deep_entity.

    " Process header data
    IF ls_deep_entity-orderid = 0. " Updated field name
      lv_operation = 'I'. " Insert operation

      MOVE-CORRESPONDING ls_deep_entity TO ls_header.

      " Extract date and time from timestamp

      ls_header-creation_date = sy-datum. " Updated field name
      ls_header-creation_time    = sy-uzeit. " Updated field name
      ls_header-creation_user = ls_deep_entity-creation_user. " Updated field name

      " Generate new OrderId
      SELECT SINGLE MAX( orderid ) " Updated field name
      INTO ls_header-orderid " Updated field name
      FROM y1tsalesord_dflc. " Updated table name

      ls_header-orderid = ls_header-orderid + 1. " Updated field name
    ELSE.
      lv_operation = 'U'. " Update operation

      " Load current header data
      SELECT SINGLE *
      INTO ls_header
      FROM y1tsalesord_dflc " Updated table name
      WHERE orderid = ls_deep_entity-orderid. " Updated field name

      " Update header fields
      ls_header-customerid  = ls_deep_entity-customerid. " Updated field name
      ls_header-status     = ls_deep_entity-status.
      ls_header-totalitems = ls_deep_entity-totalitems. " Updated field name
      ls_header-totalfreight = ls_deep_entity-totalfreight. " Updated field name
      ls_header-totalorder = ls_header-totalitems + ls_header-totalfreight. " Updated field names
    ENDIF.

    " Process items
    LOOP AT ls_deep_entity-toitem INTO ls_deep_item. " Updated association name
      MOVE-CORRESPONDING ls_deep_item TO ls_item.

      ls_item-orderid = ls_header-orderid. " Updated field name
      APPEND ls_item TO lt_items.
    ENDLOOP.

    " Persist header data
    IF lv_operation = 'I'.
      INSERT y1tsalesord_dflc FROM ls_header. " Updated table name
      IF sy-subrc <> 0.
        ROLLBACK WORK.

        lo_msg->add_message_text_only(
        EXPORTING
          iv_msg_type = 'E'
          iv_msg_text = 'Error inserting order header' " Updated message text
          ).

        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            message_container = lo_msg.
      ENDIF.
    ELSE.
      MODIFY y1tsalesord_dflc FROM ls_header. " Updated table name
      IF sy-subrc <> 0.
        ROLLBACK WORK.

        lo_msg->add_message_text_only(
        EXPORTING
          iv_msg_type = 'E'
          iv_msg_text = 'Error updating order header' " Updated message text
          ).

        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            message_container = lo_msg.
      ENDIF.
    ENDIF.

    " Persist items data
    DELETE FROM y1tsalesits_dflc WHERE orderid = ls_header-orderid. " Updated table and field names
    IF lines( lt_items ) > 0.
      INSERT y1tsalesits_dflc FROM TABLE lt_items. " Updated table name
      IF sy-subrc <> 0.
        ROLLBACK WORK.

        lo_msg->add_message_text_only(
        EXPORTING
          iv_msg_type = 'E'
          iv_msg_text = 'Error inserting order items' " Updated message text
          ).

        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            message_container = lo_msg.
      ENDIF.
    ENDIF.

    COMMIT WORK AND WAIT.

    " Update return deep entity

    " Header data
    ls_deep_entity-orderid = ls_header-orderid. " Updated field name
    CONVERT DATE ls_header-creation_date " Updated field name
    TIME ls_header-creation_time " Updated field name
    INTO TIME STAMP ls_deep_entity-creation_date_time " Updated field name
    TIME ZONE 'UTC'. "sy-zonlo.

    " Item data
    LOOP AT ls_deep_entity-toitem ASSIGNING FIELD-SYMBOL(<ls_deep_item>). " Updated association name
      <ls_deep_item>-orderid = ls_header-orderid. " Updated field name
    ENDLOOP.

    " Copy data to response
    CALL METHOD me->copy_data_to_ref
      EXPORTING
        is_data = ls_deep_entity
      CHANGING
        cr_data = er_deep_entity.

  ENDMETHOD.


  METHOD /iwbep/if_mgw_appl_srv_runtime~execute_action.
    DATA: lv_orderid  TYPE y1tsalesord_dflc-orderid. " Updated table and field name
    DATA: lv_status   TYPE y1tsalesord_dflc-status. " Updated table name
    DATA: lt_messages TYPE STANDARD TABLE OF ycl_y1_sales_order_dfl_mpc_ext=>messagecomplex. " Updated type name
    DATA: ls_message  TYPE ycl_y1_sales_order_dfl_mpc_ext=>messagecomplex. " Updated type name

    " Check if the action is 'UpdateOrderStatus'
    IF iv_action_name = 'UPDATE_ORDER_STATUS'. " Updated action name
      " Get parameters from the request
      lv_orderid = it_parameter[ name = 'SalesOrderID' ]-value. " Updated parameter name
      lv_status  = it_parameter[ name = 'Status' ]-value. " Updated parameter name

      " Update order status in database
      UPDATE y1tsalesord_dflc " Updated table name
      SET status = lv_status
      WHERE orderid = lv_orderid. " Updated field name

      " Prepare response messages
      IF sy-subrc = 0.
        CLEAR ls_message.
        ls_message-type       = 'S'. " Success
        ls_message-message = |ORDER { lv_orderid } status updated successfully|. " Updated message
        APPEND ls_message TO lt_messages.
      ELSE.
        CLEAR ls_message.
        ls_message-type       = 'E'. " Error
        ls_message-message = |Error updating status FOR ORDER { lv_orderid }|. " Updated message
        APPEND ls_message TO lt_messages.
      ENDIF.
    ENDIF.

    " Copy data to response
    CALL METHOD me->copy_data_to_ref
      EXPORTING
        is_data = lt_messages
      CHANGING
        cr_data = er_data.

  ENDMETHOD.


  METHOD returnmessages_create_entity.
**TRY.
*CALL METHOD SUPER->RETURNMESSAGES_CREATE_ENTITY
*  EXPORTING
*    IV_ENTITY_NAME          =
*    IV_ENTITY_SET_NAME      =
*    IV_SOURCE_NAME          =
*    IT_KEY_TAB              =
**    io_tech_request_context =
*    IT_NAVIGATION_PATH      =
**    io_data_provider        =
**  IMPORTING
**    er_entity               =
*    .
** CATCH /iwbep/cx_mgw_busi_exception .
** CATCH /iwbep/cx_mgw_tech_exception .
**ENDTRY.
  ENDMETHOD.


  METHOD returnmessages_delete_entity.
**TRY.
*CALL METHOD SUPER->RETURNMESSAGES_DELETE_ENTITY
*  EXPORTING
*    IV_ENTITY_NAME          =
*    IV_ENTITY_SET_NAME      =
*    IV_SOURCE_NAME          =
*    IT_KEY_TAB              =
**    io_tech_request_context =
*    IT_NAVIGATION_PATH      =
*    .
** CATCH /iwbep/cx_mgw_busi_exception .
** CATCH /iwbep/cx_mgw_tech_exception .
**ENDTRY.
  ENDMETHOD.


  METHOD returnmessages_get_entity.
**TRY.
*CALL METHOD SUPER->RETURNMESSAGES_GET_ENTITY
*  EXPORTING
*    IV_ENTITY_NAME          =
*    IV_ENTITY_SET_NAME      =
*    IV_SOURCE_NAME          =
*    IT_KEY_TAB              =
**    io_request_object       =
**    io_tech_request_context =
*    IT_NAVIGATION_PATH      =
**  IMPORTING
**    er_entity               =
**    es_response_context     =
*    .
** CATCH /iwbep/cx_mgw_busi_exception .
** CATCH /iwbep/cx_mgw_tech_exception .
**ENDTRY.
  ENDMETHOD.


  METHOD returnmessages_get_entityset.
**TRY.
*CALL METHOD SUPER->RETURNMESSAGES_GET_ENTITYSET
*  EXPORTING
*    IV_ENTITY_NAME           =
*    IV_ENTITY_SET_NAME       =
*    IV_SOURCE_NAME           =
*    IT_FILTER_SELECT_OPTIONS =
*    IS_PAGING                =
*    IT_KEY_TAB               =
*    IT_NAVIGATION_PATH       =
*    IT_ORDER                 =
*    IV_FILTER_STRING         =
*    IV_SEARCH_STRING         =
**    io_tech_request_context  =
**  IMPORTING
**    et_entityset             =
**    es_response_context      =
*    .
** CATCH /iwbep/cx_mgw_busi_exception .
** CATCH /iwbep/cx_mgw_tech_exception .
**ENDTRY.
  ENDMETHOD.


  METHOD returnmessages_update_entity.
**TRY.
*CALL METHOD SUPER->RETURNMESSAGES_UPDATE_ENTITY
*  EXPORTING
*    IV_ENTITY_NAME          =
*    IV_ENTITY_SET_NAME      =
*    IV_SOURCE_NAME          =
*    IT_KEY_TAB              =
**    io_tech_request_context =
*    IT_NAVIGATION_PATH      =
**    io_data_provider        =
**  IMPORTING
**    er_entity               =
*    .
** CATCH /iwbep/cx_mgw_busi_exception .
** CATCH /iwbep/cx_mgw_tech_exception .
**ENDTRY.
  ENDMETHOD.


  METHOD salesorderheader_create_entity.
    DATA: ld_lastid TYPE int4.
    DATA: ls_header TYPE y1tsalesord_dflc. " Use the new table name

    " Get message container for error handling
    DATA(lo_msg) = me->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

    " Read incoming data from the request
    io_data_provider->read_entry_data(
    IMPORTING
      es_data = er_entity
      ).

    " Map the entity data to the database table structure
    MOVE-CORRESPONDING er_entity TO ls_header.

    " Set creation date, time and user automatically
    ls_header-creation_date = sy-datum.
    ls_header-creation_time = sy-uzeit.
    ls_header-creation_user = sy-uname.

    " Get the last ORDERID to generate the next one
    SELECT SINGLE MAX( orderid )
    INTO ld_lastid
    FROM y1tsalesord_dflc.

    " Increment the last ID for the new order
    ls_header-orderid = ld_lastid + 1.

    " Insert the new order header into the database
    INSERT y1tsalesord_dflc FROM ls_header.
    IF sy-subrc <> 0.

      " Error handling if insert fails
      lo_msg->add_message_text_only(
      EXPORTING
        iv_msg_type = 'E'
        iv_msg_text = 'Error inserting order header'
        ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_msg.

    ENDIF.

    " Update the response entity with the generated data
    MOVE-CORRESPONDING ls_header TO er_entity.

    " Convert date and time to timestamp for the response
    CONVERT DATE ls_header-creation_date
    TIME ls_header-creation_time
    INTO TIME STAMP er_entity-creation_date_time
    TIME ZONE 'UTC'.

  ENDMETHOD.


  METHOD salesorderheader_delete_entity.

    DATA: ls_key_tab LIKE LINE OF it_key_tab.

    " Get message container for error handling
    DATA(lo_msg) = me->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

    " Read the OrderId from the key parameters
    READ TABLE it_key_tab INTO ls_key_tab WITH KEY name = 'SalesOrderID'.
    IF sy-subrc <> 0.
      " Error if OrderId is not provided
      lo_msg->add_message_text_only(
      EXPORTING
        iv_msg_type = 'E'
        iv_msg_text = 'OrderId not provided'
        ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_msg.
    ENDIF.

    " First delete all items associated with the order
    " Using the new table name Y1TSALESITS_DFLC
    DELETE FROM y1tsalesits_dflc WHERE orderid = ls_key_tab-value.

    " Commented validation as some orders may not have items
    " Vinicius 28/04/2024 - Commenting validation because some orders may
    " not have items
    "*  IF sy-subrc <> 0.
    "*    ROLLBACK WORK.
    "*
    "*    lo_msg->add_message_text_only(
    "*      EXPORTING
    "*        iv_msg_type = 'E'
    "*        iv_msg_text = 'Error deleting order items'
    "*    ).
    "*
    "*    RAISE EXCEPTION type /iwbep/cx_mgw_busi_exception
    "*      EXPORTING
    "*        message_container = lo_msg.
    "*  ENDIF.

    " Delete the order header from the main table
    " Using the new table name Y1TSALESORD_DFLC
    DELETE FROM y1tsalesord_dflc WHERE orderid = ls_key_tab-value.
    IF sy-subrc <> 0.
      " Rollback if order header deletion fails
      ROLLBACK WORK.

      lo_msg->add_message_text_only(
      EXPORTING
        iv_msg_type = 'E'
        iv_msg_text = 'Error deleting order header'
        ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_msg.
    ENDIF.

    " Commit the changes to database
    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD salesorderheader_get_entity.

    DATA: lv_orderid TYPE y1tsalesord_dflc-orderid. " Using new table name
    DATA: ls_key_tab LIKE LINE OF it_key_tab.
    DATA: ls_header  TYPE y1tsalesord_dflc. " Using new table name

    " Get message container for error handling
    DATA(lo_msg) = me->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

    " Read the OrderId from the key parameters
    READ TABLE it_key_tab INTO ls_key_tab WITH KEY name = 'SalesOrderID'.
    IF sy-subrc <> 0.
      " Error if OrderId is not provided
      lo_msg->add_message_text_only(
      EXPORTING
        iv_msg_type = 'E'
        iv_msg_text = 'Order ID not provided'
        ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_msg.
    ENDIF.

    lv_orderid = ls_key_tab-value.

    " Select order header data from database
    SELECT SINGLE *
    INTO ls_header
    FROM y1tsalesord_dflc
    WHERE orderid = lv_orderid.

    IF sy-subrc = 0.
      " Map database data to response entity
      MOVE-CORRESPONDING ls_header TO er_entity.

      " Set created by user in response
      er_entity-creation_user = ls_header-creation_user.

      " Convert date and time to timestamp for response
      CONVERT DATE ls_header-creation_date
      TIME ls_header-creation_time
      INTO TIME STAMP er_entity-creation_date_time
      TIME ZONE 'UTC'. "sy-zonlo.

    ELSE.
      " Error if order not found
      lo_msg->add_message_text_only(
      EXPORTING
        iv_msg_type = 'E'
        iv_msg_text = 'Order ID not found'
        ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_msg.
    ENDIF.

  ENDMETHOD.


  METHOD salesorderheader_get_entityset.

    DATA: lt_headers   TYPE STANDARD TABLE OF y1tsalesord_dflc. " New table name
    DATA: ls_header    TYPE y1tsalesord_dflc. " New table name
    DATA: ls_entityset LIKE LINE OF et_entityset.

    DATA: lt_orderby   TYPE STANDARD TABLE OF string.
    DATA: lv_orderby   TYPE string.

    DATA(lt_order_by_converted) = io_tech_request_context->get_orderby( ).
    DATA(lv_where) = io_tech_request_context->get_osql_where_clause_convert( ).

    " Build dynamic ORDER BY clause from input parameters
    "LOOP AT it_order INTO DATA(ls_order).
    LOOP AT lt_order_by_converted INTO DATA(ls_order).
      TRANSLATE ls_order-property TO UPPER CASE.
      TRANSLATE ls_order-order TO UPPER CASE.

      " Convert order direction to SQL syntax
      IF ls_order-order = 'DESC'.
        ls_order-order = 'DESCENDING'.
      ELSE.
        ls_order-order = 'ASCENDING'.
      ENDIF.

      " Append formatted order clause
      APPEND |{ ls_order-property } { ls_order-order }| TO lt_orderby.
    ENDLOOP.

    " Concatenate all order clauses with comma separator
    CONCATENATE LINES OF lt_orderby INTO lv_orderby SEPARATED BY ', '.

    " Set default ordering if none provided
    IF lv_orderby = ''.
      lv_orderby = 'ORDERID ASCENDING'. " Updated field name
    ENDIF.

    " Select data from database with dynamic filtering and ordering
    SELECT *
    FROM y1tsalesord_dflc " Updated table name
    "WHERE (iv_filter_string)
    WHERE (lv_where)
    ORDER BY (lv_orderby)
    INTO TABLE @lt_headers
    UP TO @is_paging-top ROWS
    OFFSET @is_paging-skip.

    " Process each record and prepare response
    LOOP AT lt_headers INTO ls_header.
      CLEAR ls_entityset.

      " Map database fields to response structure
      MOVE-CORRESPONDING ls_header TO ls_entityset.

      " Set created by user information
      ls_entityset-creation_user = ls_header-creation_user. " Updated field name

      " Convert date and time to timestamp
      CONVERT DATE ls_header-creation_date " Updated field name
      TIME ls_header-creation_time " Updated field name
      INTO TIME STAMP ls_entityset-creation_date_time " Updated field name
      TIME ZONE 'UTC'. "sy-zonlo.

      APPEND ls_entityset TO et_entityset.
    ENDLOOP.

  ENDMETHOD.


  METHOD salesorderheader_update_entity.

    DATA: lv_error TYPE flag.

    " Get message container for error handling
    DATA(lo_msg) = me->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

    " Read incoming data from the request
    io_data_provider->read_entry_data(
    IMPORTING
      es_data = er_entity
      ).

    " Set OrderId from key parameters
    er_entity-orderid = it_key_tab[ name = 'SalesOrderID' ]-value.

    " Field validations
    IF er_entity-customerid = 0. " Updated field name
      lv_error = 'X'.
      lo_msg->add_message_text_only(
      EXPORTING
        iv_msg_type = 'E'
        iv_msg_text = 'Customer is empty' " Updated message text
        ).
    ENDIF.

    IF er_entity-totalorder < 10. " Updated field name
      lv_error = 'X'.
      lo_msg->add_message(
      EXPORTING
        iv_msg_type   = 'E'
        iv_msg_id     = 'ZOV'
        iv_msg_number = 1
        iv_msg_v1     = 'R$ 10,00'
        iv_msg_v2     = |{ er_entity-orderid }| " Updated field name
        ).
    ENDIF.

    " Check if any validation errors occurred
    IF lv_error = 'X'.
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_msg
          http_status_code  = 500.
    ENDIF.

    " Update order header in database with new table name
    UPDATE y1tsalesord_dflc " Updated table name
    SET customerid  = er_entity-customerid   " Updated field name
    totalitems  = er_entity-totalitems   " Updated field name
    totalfreight = er_entity-totalfreight " Updated field name
    totalorder  = er_entity-totalorder   " Updated field name
    status      = er_entity-status
    WHERE orderid   = er_entity-orderid.     " Updated field name

    IF sy-subrc <> 0.
      " Error handling if update fails
      lo_msg->add_message_text_only(
      EXPORTING
        iv_msg_type = 'E'
        iv_msg_text = 'Error updating order' " Updated message text
        ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_msg.
    ENDIF.

  ENDMETHOD.


  METHOD salesorderitems_create_entity.
    DATA: ls_item TYPE y1tsalesits_dflc. " Using new table name

    " Get message container for error handling
    DATA(lo_msg) = me->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

    " Read incoming data from the request
    io_data_provider->read_entry_data(
    IMPORTING
      es_data = er_entity
      ).

    " Map the entity data to the database table structure
    MOVE-CORRESPONDING er_entity TO ls_item.

    " Generate new ItemId if not provided (0)
    IF er_entity-itemid = 0.
      SELECT SINGLE MAX( itemid )
      INTO er_entity-itemid
      FROM y1tsalesits_dflc " Updated table name
      WHERE orderid = er_entity-orderid. " Updated field name

      " Increment the last ItemId for the new item
      er_entity-itemid = er_entity-itemid + 1.
    ENDIF.

    " Insert the new order item into the database
    INSERT y1tsalesits_dflc FROM ls_item. " Updated table name
    IF sy-subrc <> 0.
      " Error handling if insert fails
      lo_msg->add_message_text_only(
      EXPORTING
        iv_msg_type = 'E'
        iv_msg_text = 'Error inserting order item' " Updated message text
        ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_msg.
    ENDIF.

  ENDMETHOD.


  METHOD salesorderitems_delete_entity.

    DATA: ls_item    TYPE y1tsalesits_dflc. " Using new table name
    DATA: ls_key_tab LIKE LINE OF it_key_tab.

    " Get message container for error handling
    DATA(lo_msg) = me->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

    " Extract OrderId and ItemId from key parameters
    ls_item-orderid = it_key_tab[ name = 'SalesOrderID' ]-value. " Updated field name and parameter
    ls_item-itemid  = it_key_tab[ name = 'SalesOrderItemNumber' ]-value. " Updated parameter name

    " Delete the specific order item from database
    DELETE FROM y1tsalesits_dflc " Updated table name
    WHERE orderid = ls_item-orderid " Updated field name
    AND itemid  = ls_item-itemid.

    IF sy-subrc <> 0.
      " Error handling if delete fails
      lo_msg->add_message_text_only(
      EXPORTING
        iv_msg_type = 'E'
        iv_msg_text = 'Error deleting order item' " Updated message text
        ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_msg.
    ENDIF.

  ENDMETHOD.


  METHOD salesorderitems_get_entity.
    DATA: ls_key_tab LIKE LINE OF it_key_tab.
    DATA: ls_item    TYPE y1tsalesits_dflc. " Using new table name
    DATA: lv_error   TYPE flag.

    " Get message container for error handling
    DATA(lo_msg) = me->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

    " Read SalesOrderHeaderID from key parameters
    READ TABLE it_key_tab INTO ls_key_tab WITH KEY name = 'SalesOrderID'. " Updated parameter name
    IF sy-subrc <> 0.
      lv_error = 'X'.
      lo_msg->add_message_text_only(
      EXPORTING
        iv_msg_type = 'E'
        iv_msg_text = 'Order ID not provided' " Updated message text
        ).
    ENDIF.
    ls_item-orderid = ls_key_tab-value. " Updated field name

    " Read SalesOrderItemNumber from key parameters
    READ TABLE it_key_tab INTO ls_key_tab WITH KEY name = 'SalesOrderItemNumber'. " Updated parameter name

    IF sy-subrc <> 0.
      lv_error = 'X'.
      lo_msg->add_message_text_only(
      EXPORTING
        iv_msg_type = 'E'
        iv_msg_text = 'Item ID not provided' " Updated message text
        ).
    ENDIF.

    ls_item-itemid = ls_key_tab-value.

    " Check if any validation errors occurred
    IF lv_error = 'X'.
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_msg.
    ENDIF.

    " Select the specific order item from database
    SELECT SINGLE *
    INTO ls_item
    FROM y1tsalesits_dflc " Updated table name
    WHERE orderid = ls_item-orderid " Updated field name
    AND itemid  = ls_item-itemid.

    IF sy-subrc = 0.
      " Map database data to response entity
      MOVE-CORRESPONDING ls_item TO er_entity.
    ELSE.
      " Error handling if item not found
      lo_msg->add_message_text_only(
      EXPORTING
        iv_msg_type = 'E'
        iv_msg_text = 'Item not found' " Updated message text
        ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_msg.
    ENDIF.

  ENDMETHOD.


  METHOD salesorderitems_get_entityset.

    DATA: lv_orderid       TYPE int4.
    DATA: lt_orderid_range TYPE RANGE OF int4.
    DATA: ls_orderid_range LIKE LINE OF lt_orderid_range.
    DATA: ls_key_tab       LIKE LINE OF it_key_tab.

    " Check if OrderId filter is provided in key parameters
    READ TABLE it_key_tab INTO ls_key_tab WITH KEY name = 'SalesOrderID'. " Updated parameter name
    IF sy-subrc = 0.
      lv_orderid = ls_key_tab-value.

      " Build range table for OrderId filter
      CLEAR ls_orderid_range.
      ls_orderid_range-sign   = 'I'.
      ls_orderid_range-option = 'EQ'.
      ls_orderid_range-low    = lv_orderid.
      APPEND ls_orderid_range TO lt_orderid_range.
    ENDIF.

    " Select order items from database with optional OrderId filter
    SELECT *
    INTO CORRESPONDING FIELDS OF TABLE et_entityset
    FROM y1tsalesits_dflc " Updated table name
    WHERE orderid IN lt_orderid_range. " Updated field name

  ENDMETHOD.


  METHOD salesorderitems_update_entity.
    " Get message container for error handling
    DATA(lo_msg) = me->/iwbep/if_mgw_conv_srv_runtime~get_message_container( ).

    " Read incoming data from the request
    io_data_provider->read_entry_data(
    IMPORTING
      es_data = er_entity
      ).

    " Set OrderId and ItemId from key parameters
    er_entity-orderid = it_key_tab[ name = 'SalesOrderID' ]-value. " Updated parameter name
    er_entity-itemid  = it_key_tab[ name = 'SalesOrderItemNumber' ]-value. " Updated parameter name

    " Calculate total price based on quantity and unit price
    er_entity-totalprice = er_entity-quantity * er_entity-unitprice. " Updated field names

    " Update order item in database with new table name
    UPDATE y1tsalesits_dflc " Updated table name
    SET material    = er_entity-material
    description = er_entity-description " Updated field name
    quantity    = er_entity-quantity    " Updated field name
    unitprice   = er_entity-unitprice   " Updated field name
    totalprice  = er_entity-totalprice  " Updated field name
    WHERE orderid   = er_entity-orderid     " Updated field name
    AND itemid      = er_entity-itemid.

    IF sy-subrc <> 0.
      " Error handling if update fails
      lo_msg->add_message_text_only(
      EXPORTING
        iv_msg_type = 'E'
        iv_msg_text = 'Error updating order item' " Updated message text
        ).

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_msg.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
