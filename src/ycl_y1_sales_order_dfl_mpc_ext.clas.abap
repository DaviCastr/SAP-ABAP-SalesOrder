CLASS ycl_y1_sales_order_dfl_mpc_ext DEFINITION
  PUBLIC
  INHERITING FROM ycl_y1_sales_order_dfl_mpc
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_sales_order_and_item
      , orderid     TYPE i
      , creation_date_time TYPE timestamp
      , creation_user   TYPE c LENGTH 20
      , customerid   TYPE i
      , totalitems  TYPE p LENGTH 8 DECIMALS 2
      , totalfreight  TYPE p LENGTH 8 DECIMALS 2
      , totalorder  TYPE p LENGTH 8 DECIMALS 2
      , status      TYPE c LENGTH 1
      , toitem    TYPE TABLE OF ts_salesorderitem WITH DEFAULT KEY
      , END OF ty_sales_order_and_item .

    METHODS define
        REDEFINITION .
protected section.
private section.
ENDCLASS.



CLASS YCL_Y1_SALES_ORDER_DFL_MPC_EXT IMPLEMENTATION.


  METHOD define.

    DATA lo_entity_type TYPE REF TO /iwbep/if_mgw_odata_entity_typ.

    super->define( ).

    lo_entity_type = model->get_entity_type( iv_entity_name = 'SalesOrderHeader' ).
    lo_entity_type->bind_structure( iv_structure_name = 'YCL_Y1_SALES_ORDER_DFL_MPC_EXT=>TY_SALES_ORDER_AND_ITEM' ).

  ENDMETHOD.
ENDCLASS.
