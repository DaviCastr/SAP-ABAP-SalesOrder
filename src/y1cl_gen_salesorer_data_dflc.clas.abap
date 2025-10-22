CLASS y1cl_gen_salesorer_data_dflc DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.

  PRIVATE SECTION.

    TYPES: ty_t_sales_order TYPE TABLE OF y1tsalesord_dflc WITH DEFAULT KEY.
    TYPES: ty_t_sales_order_item TYPE TABLE OF y1tsalesits_dflc WITH DEFAULT KEY.

    METHODS:
      " Method to generate example order headers
      generate_order_headers
        RETURNING VALUE(rt_headers) TYPE ty_t_sales_order,

      " Method to generate example order items
      generate_order_items
        IMPORTING it_headers      TYPE ty_t_sales_order
        RETURNING VALUE(rt_items) TYPE ty_t_sales_order_item,

      " Method to calculate total prices for items
      calculate_item_totals
        CHANGING ct_items TYPE ty_t_sales_order_item,

      " Method to update header totals based on items
      update_header_totals
        CHANGING ct_headers TYPE ty_t_sales_order
                 ct_items   TYPE ty_t_sales_order_item.

ENDCLASS.

CLASS y1cl_gen_salesorer_data_dflc IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.
    DATA: lt_headers TYPE TABLE OF y1tsalesord_dflc,
          lt_items   TYPE TABLE OF y1tsalesits_dflc.

    out->write( 'Starting example data generation...' ).

    " Generate order headers
    lt_headers = generate_order_headers( ).
    out->write( |Generated { lines( lt_headers ) } order headers| ).

    " Generate order items
    lt_items = generate_order_items( lt_headers ).
    out->write( |Generated { lines( lt_items ) } order items| ).

    " Calculate item totals
    calculate_item_totals( CHANGING ct_items = lt_items ).
    out->write( 'Calculated item totals' ).

    " Update header totals based on items
    update_header_totals( CHANGING ct_headers = lt_headers
                                   ct_items   = lt_items ).
    out->write( 'Updated header totals' ).

    " Insert data into database tables
    out->write( 'Inserting data into database...' ).

    " Delete existing data first (optional - for clean test)
    DELETE FROM y1tsalesits_dflc.
    DELETE FROM y1tsalesord_dflc.

    " Insert order headers
    INSERT y1tsalesord_dflc FROM TABLE @lt_headers.
    IF sy-subrc = 0.
      out->write( |Successfully inserted { lines( lt_headers ) } order headers| ).
    ELSE.
      out->write( 'Error inserting order headers' ).
      RETURN.
    ENDIF.

    " Insert order items
    INSERT y1tsalesits_dflc FROM TABLE @lt_items.
    IF sy-subrc = 0.
      out->write( |Successfully inserted { lines( lt_items ) } order items| ).
    ELSE.
      out->write( 'Error inserting order items' ).
      RETURN.
    ENDIF.

    COMMIT WORK.
    out->write( 'Data successfully committed to database!' ).

    " Display summary
    out->write( '--- GENERATION SUMMARY ---' ).
    out->write( |Order Headers: { lines( lt_headers ) }| ).
    out->write( |Order Items:   { lines( lt_items ) }| ).

  ENDMETHOD.

  METHOD generate_order_headers.
    DATA: ls_header TYPE y1tsalesord_dflc.
    DATA: lv_timestamp TYPE timestampl.

    " Get current timestamp
    GET TIME STAMP FIELD lv_timestamp.

    " Generate up to 20 order headers
    DO 20 TIMES.
      CLEAR ls_header.

      " Basic header data
      ls_header-orderid       = sy-index * 100. " Generate unique order IDs
      ls_header-creation_date = sy-datum - sy-index. " Different dates
      ls_header-creation_time = sy-uzeit.
      ls_header-creation_user = sy-uname.
      ls_header-customerid    = 1000 + sy-index. " Customer IDs from 1001 to 1020

      " Status: A=Active, P=Pending, C=Completed, X=Cancelled
      CASE sy-index MOD 4.
        WHEN 0.
          ls_header-status = 'A'. " Active
        WHEN 1.
          ls_header-status = 'P'. " Pending
        WHEN 2.
          ls_header-status = 'C'. " Completed
        WHEN 3.
          ls_header-status = 'X'. " Cancelled
      ENDCASE.

      " Initial totals (will be updated later)
      ls_header-totalitems   = 0.
      ls_header-totalfreight = sy-index * 5. " Freight from 5 to 100
      ls_header-totalorder   = 0.

      APPEND ls_header TO rt_headers.
    ENDDO.

  ENDMETHOD.

  METHOD generate_order_items.
    DATA: ls_item TYPE y1tsalesits_dflc.
    DATA: lv_item_counter TYPE i VALUE 1.

    " Generate 2-5 items for each order header
    LOOP AT it_headers INTO DATA(ls_header).
      CLEAR ls_item.

      " Generate random number of items per order (2 to 5)
      DATA(lv_item_count) = 2 + ( sy-index MOD 4 ).

      DO lv_item_count TIMES.
        CLEAR ls_item.

        " Basic item data
        ls_item-orderid     = ls_header-orderid.
        ls_item-itemid      = sy-index. " Item ID within the order
        ls_item-material    = |MAT{ 1000 + ( sy-index * 10 ) }|. " Material numbers
        ls_item-description = |Product { ls_item-material } - Description { sy-index }|.
        ls_item-quantity    = 1 + ( sy-index MOD 10 ). " Quantity from 1 to 10
        ls_item-unitprice   = ( 10 + ( sy-index * 5 ) ) * ls_item-quantity. " Unit price calculation

        " Total price will be calculated in separate method
        ls_item-totalprice  = 0.

        APPEND ls_item TO rt_items.
        lv_item_counter = lv_item_counter + 1.
      ENDDO.
    ENDLOOP.

  ENDMETHOD.

  METHOD calculate_item_totals.
    " Calculate total price for each item (quantity * unit price)
    LOOP AT ct_items ASSIGNING FIELD-SYMBOL(<ls_item>).
      <ls_item>-totalprice = <ls_item>-quantity * <ls_item>-unitprice.
    ENDLOOP.
  ENDMETHOD.

  METHOD update_header_totals.
    DATA: lv_order_total TYPE y1tsalesord_dflc-totalorder.

    " Update header totals based on the items
    LOOP AT ct_headers ASSIGNING FIELD-SYMBOL(<ls_header>).
      CLEAR lv_order_total.

      " Calculate total items and sum for this order
      LOOP AT ct_items INTO DATA(ls_item) WHERE orderid = <ls_header>-orderid.
        lv_order_total = lv_order_total + ls_item-totalprice.
      ENDLOOP.

      " Update header with calculated totals
      <ls_header>-totalitems = lv_order_total.
      <ls_header>-totalorder = <ls_header>-totalitems + <ls_header>-totalfreight.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
