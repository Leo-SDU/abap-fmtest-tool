*&---------------------------------------------------------------------*
*& 程序名称：  ZSAPLL_IF_FM_TEST
*& 程序描述：  测试接口函数
*&===============================*
*&创建日期：  2020.01.02              程序员：孙亮(微信公众号：SAP亮亮)
*&===============================*
*&修改日期    请求号   修改人   业务提交人    修改描述 *
*&---------------------------------------------------------------------*
REPORT  zsapll_if_fm_test.

TABLES sscrfields.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (30) cmt1 FOR FIELD p_if.
PARAMETERS p_if TYPE c LENGTH 30 MEMORY ID lib.
SELECTION-SCREEN PUSHBUTTON 66(6) exec USER-COMMAND exec .
SELECTION-SCREEN PUSHBUTTON (8) debug USER-COMMAND debug.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN SKIP 1.
PARAMETERS: p_conf TYPE c AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN FUNCTION KEY 1.
SELECTION-SCREEN FUNCTION KEY 2.

SELECTION-SCREEN BEGIN OF SCREEN 9100 AS WINDOW.
PARAMETER: p_del TYPE c LENGTH 3.
SELECTION-SCREEN END OF SCREEN 9100.

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS:
      handle_item_dbclick FOR EVENT item_double_click OF cl_gui_alv_tree IMPORTING fieldname node_key,
      handle_function_selected FOR EVENT function_selected OF cl_gui_toolbar IMPORTING fcode.
ENDCLASS.                    "lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD handle_item_dbclick.
    PERFORM handle_item_dbclick USING fieldname node_key.
  ENDMETHOD.                    "handle_node_double_click
  METHOD handle_function_selected.
    PERFORM handle_function_selected USING fcode.
  ENDMETHOD.                    "handle_function_selected
ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION

TYPES:BEGIN OF ty_text,
        text TYPE c LENGTH 150,
      END OF ty_text,

      BEGIN OF ty_map,
        id       TYPE n LENGTH 10,
        function TYPE rs38l_fnam,
      END OF ty_map,

      BEGIN OF ty_history,
        num   TYPE i,
        datum TYPE d,
        uzeit TYPE c LENGTH 8,
        input TYPE string,
      END OF ty_history,

      BEGIN OF ty_tree_key, "ID与树中KEY的关系
        key TYPE lvc_nkey,
        num TYPE i,
      END OF ty_tree_key.

DATA: go_docking       TYPE REF TO cl_gui_docking_container,
      go_splitter      TYPE REF TO cl_gui_splitter_container,
      go_con_left      TYPE REF TO cl_gui_container,
      go_splitter_left TYPE REF TO cl_gui_splitter_container,
      go_con_toolbar   TYPE REF TO cl_gui_container,
      go_con_tree      TYPE REF TO cl_gui_container,
      go_con_input     TYPE REF TO cl_gui_container,
      go_con_output    TYPE REF TO cl_gui_container.
DATA: go_tree           TYPE REF TO cl_gui_alv_tree,
      go_toolbar        TYPE REF TO cl_gui_toolbar,
      go_event_receiver TYPE REF TO lcl_event_receiver,
      go_input          TYPE REF TO cl_gui_textedit,
      go_output         TYPE REF TO cl_gui_textedit.
DATA: gt_tree     TYPE TABLE OF ty_history,
      gt_history  TYPE TABLE OF ty_history,
      gs_history  TYPE ty_history,
      gt_tree_key TYPE SORTED TABLE OF ty_tree_key WITH UNIQUE KEY key,
      gs_tree_key TYPE ty_tree_key,
      gt_map      TYPE TABLE OF ty_map,
      gs_map      TYPE ty_map.
DATA: g_input  TYPE string,
      g_output TYPE string.
DATA: g_old_if       TYPE string,
      g_debug        TYPE c,
      g_need_refresh TYPE c VALUE 'X',
      g_fun_log_id   TYPE indx_srtfd,
      g_9100_ok      TYPE c,
      g_ucomm        TYPE sy-ucomm.
CONSTANTS:
  gc_id_map        TYPE indx_srtfd VALUE 'ZSAPLL_IFMAP',
  gc_id_log_prefix TYPE string VALUE 'ZSAPLL_IFLOG'.

INITIALIZATION.
  sy-title = '接口函数测试(微信公众号:SAP亮亮)'.
  cmt1 = '函数模块(入参:INPUT;出参:OUTPUT)'.
  %_p_conf_%_app_%-text = '删除日志时需要确认'.
  sscrfields-functxt_01 = '@R4@XML格式输入文件'.
  sscrfields-functxt_02 = '@R4@XML格式输出文件'.
  exec = '@15@执行'.
  debug = '@15@调试执行'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_if.
  CALL FUNCTION 'RS_HELP_HANDLING'
    EXPORTING
      dynpfield                 = 'P_IF'
      dynpname                  = sy-dynnr
      object                    = 'FB'
      progname                  = sy-repid
      suppress_selection_screen = 'X'.

AT SELECTION-SCREEN OUTPUT.
  IF sy-dynnr = '1000'.
    PERFORM selection_screen_pbo.
    PERFORM init_screen.
    PERFORM update_alv_tree.
  ELSEIF sy-dynnr = '9100'.
    %_p_del_%_app_%-text = '请输入“YES”确认删除'.
    g_9100_ok = ''.
  ENDIF.

AT SELECTION-SCREEN.
  IF sy-dynnr = '1000'.
    g_ucomm = sscrfields-ucomm.
    CLEAR sscrfields-ucomm.

    g_debug = ''.
    PERFORM set_if_changed. "选择屏幕的函数名称是否发生了变化，如果变化了，需要重新取值
    CASE g_ucomm.
      WHEN 'EXEC'.
        PERFORM execute_function.
      WHEN 'DEBUG'.
        g_debug = 'X'.
        PERFORM execute_function.
      WHEN 'FC01'.
        PERFORM get_input_text.
        PERFORM display_output_xml USING g_input.
      WHEN 'FC02'.
        PERFORM display_output_xml USING g_output.
    ENDCASE.
  ELSEIF sy-dynnr = '9100'.
    IF sscrfields-ucomm = 'CRET'.
      IF p_del <> 'YES'.
        MESSAGE '输入错误' TYPE 'E'.
      ELSE.
        g_9100_ok = 'X'.
      ENDIF.
    ENDIF.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  selection_screen_pbo
*&---------------------------------------------------------------------*
FORM selection_screen_pbo.
  DATA: lt_exclude TYPE TABLE OF sy-ucomm.

  APPEND 'ONLI' TO lt_exclude.
  CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
    EXPORTING
      p_status  = sy-pfkey
      p_program = sy-cprog
    TABLES
      p_exclude = lt_exclude.
ENDFORM.                    "selection_screen_pbo
*&---------------------------------------------------------------------*
*&      Form  init_screen
*&---------------------------------------------------------------------*
FORM init_screen.
  DATA: ls_hier_header TYPE treev_hhdr,
        ls_header      TYPE treev_hhdr,
        lt_fcat_tree   TYPE lvc_t_fcat,
        ls_fcat        TYPE lvc_s_fcat,
        lt_events      TYPE cntl_simple_events,
        ls_event       TYPE cntl_simple_event.

  DEFINE build_fieldcat.
    ls_fcat-fieldname = &1.
    ls_fcat-outputlen = &2.
    ls_fcat-coltext   = &3.
    APPEND ls_fcat TO lt_fcat_tree.
    CLEAR ls_fcat.
  END-OF-DEFINITION.

  CHECK go_docking IS INITIAL.

  "创建DOCKING容器
  CREATE OBJECT go_docking
    EXPORTING
      side  = cl_gui_docking_container=>dock_at_bottom
      ratio = 90.

  "屏幕分割
  CREATE OBJECT go_splitter
    EXPORTING
      parent  = go_docking
      rows    = 1
      columns = 3.
  go_splitter->set_column_width( id = 1 width = 20 ).
  go_con_left   = go_splitter->get_container( row = 1 column = 1 ).
  go_con_input  = go_splitter->get_container( row = 1 column = 2 ).
  go_con_output = go_splitter->get_container( row = 1 column = 3 ).

  "再次分割左侧区域
  CREATE OBJECT go_splitter_left
    EXPORTING
      parent  = go_con_left
      rows    = 2
      columns = 1.
  go_con_toolbar = go_splitter_left->get_container( row = 1 column = 1 ).
  go_con_tree    = go_splitter_left->get_container( row = 2 column = 1 ).
  "设置左侧的SPLITTER的样式
  go_splitter_left->set_border( border = space ). "没有边框
  go_splitter_left->set_row_mode( mode = cl_gui_splitter_container=>mode_absolute ).
  "设置树的工具栏的样式
  go_splitter_left->set_row_sash( id    = 1
                                  type  = cl_gui_splitter_container=>type_movable
                                  value = cl_gui_splitter_container=>false ).
  go_splitter_left->set_row_height( id = 1 height = 21 ).

  "创建树
  CREATE OBJECT go_tree
    EXPORTING
      parent              = go_con_tree
      node_selection_mode = cl_gui_column_tree=>node_sel_mode_multiple
      no_html_header      = 'X'
      no_toolbar          = 'X'.

  ls_header-heading = '日期'.
  ls_header-width = 22.
  ls_header-width_pix = 'X'.
  build_fieldcat:
    'UZEIT'  12 '时间'.

  "创建事件处理对象
  CREATE OBJECT go_event_receiver.

  "注册树的事件
  ls_event-eventid = cl_gui_column_tree=>eventid_item_double_click.
  ls_event-appl_event = 'X'.
  APPEND ls_event TO lt_events.
  go_tree->set_registered_events( events = lt_events ).
  SET HANDLER go_event_receiver->handle_item_dbclick FOR go_tree.

  CALL METHOD go_tree->set_table_for_first_display
    EXPORTING
      is_hierarchy_header = ls_header
    CHANGING
      it_fieldcatalog     = lt_fcat_tree
      it_outtab           = gt_tree.

  "创建树的工具栏
  CREATE OBJECT go_toolbar
    EXPORTING
      parent = go_con_toolbar.
  go_toolbar->add_button( fcode = 'SALL'   icon = icon_select_all   butn_type = cntb_btype_button quickinfo = '全选' ).
  go_toolbar->add_button( fcode = 'DSAL'   icon = icon_deselect_all butn_type = cntb_btype_button quickinfo = '取消全选' ).
  go_toolbar->add_button( fcode = ''       icon = ''                butn_type = cntb_btype_sep    quickinfo = '' ).
  go_toolbar->add_button( fcode = 'DELETE' icon = icon_delete       butn_type = cntb_btype_button quickinfo = '删除所选数据' ).

  "注册工具栏的事件
  CLEAR lt_events.
  ls_event-eventid = cl_gui_toolbar=>m_id_function_selected.
  ls_event-appl_event = 'X'.
  INSERT ls_event INTO TABLE lt_events.
  go_toolbar->set_registered_events( events = lt_events ).
  SET HANDLER go_event_receiver->handle_function_selected FOR go_toolbar.

  "创建INPUT文本框
  CREATE OBJECT go_input
    EXPORTING
      parent                     = go_con_input
      wordwrap_mode              = cl_gui_textedit=>wordwrap_at_fixed_position
      wordwrap_position          = 150
      wordwrap_to_linebreak_mode = cl_gui_textedit=>true
    EXCEPTIONS
      OTHERS                     = 1.

  "创建OUTPUT文本框
  CREATE OBJECT go_output
    EXPORTING
      parent                     = go_con_output
      wordwrap_mode              = cl_gui_textedit=>wordwrap_at_fixed_position
      wordwrap_position          = 150
      wordwrap_to_linebreak_mode = cl_gui_textedit=>true
    EXCEPTIONS
      OTHERS                     = 1.
  go_output->set_readonly_mode( readonly_mode = 1 ).

  CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
    EXCEPTIONS
      OTHERS = 1.
ENDFORM.                    "init_screen
*&---------------------------------------------------------------------*
*&      Form  UPDATE_ALV_TREE
*&---------------------------------------------------------------------*
FORM update_alv_tree .
  DATA: ls_node_layout TYPE lvc_s_layn,
        l_parent_key   TYPE lvc_nkey,
        l_node_text    TYPE lvc_value,
        l_topkey       TYPE tv_nodekey.

  CHECK g_need_refresh = 'X'.
  g_need_refresh = ''.

  CLEAR gt_history.
  IMPORT history = gt_history FROM DATABASE indx(st) ID g_fun_log_id.
  SORT gt_history BY datum DESCENDING uzeit DESCENDING.

  CALL METHOD go_tree->delete_all_nodes( ).
  CLEAR: gt_tree_key.
  LOOP AT gt_history INTO gs_history.
    WRITE gs_history-datum TO l_node_text.
    l_parent_key = ''.

    CALL METHOD go_tree->add_node
      EXPORTING
        i_relat_node_key     = l_parent_key
        i_relationship       = cl_gui_column_tree=>relat_last_child
        i_node_text          = l_node_text
        is_outtab_line       = gs_history
        is_node_layout       = ls_node_layout
      IMPORTING
        e_new_node_key       = gs_tree_key-key
      EXCEPTIONS
        relat_node_not_found = 1
        node_not_found       = 2
        OTHERS               = 3.

    gs_tree_key-num = gs_history-num.
    INSERT gs_tree_key INTO TABLE gt_tree_key.
  ENDLOOP.

  CALL METHOD go_tree->frontend_update.
ENDFORM.                    " UPDATE_ALV_TREE
*&---------------------------------------------------------------------*
*&      Form  SET_IF_CHANGED
*&---------------------------------------------------------------------*
FORM set_if_changed .
  DATA: l_fun_id TYPE n LENGTH 10. "函数对应的ID

  CHECK g_old_if <> p_if.

  PERFORM check_function.

  g_old_if = p_if.
  g_need_refresh = 'X'.

  DEFINE save_if_map. "保存函数映射
    gs_map-id = l_fun_id.
    gs_map-function = p_if.
    APPEND gs_map TO gt_map.
    SORT gt_map BY function.
    EXPORT map = gt_map TO DATABASE indx(st) ID gc_id_map.
  END-OF-DEFINITION.

  PERFORM lock_indx USING gc_id_map.

  IMPORT map = gt_map FROM DATABASE indx(st) ID gc_id_map.  "存储函数名称与ID的对应关系（为每个函数生成一个ID）
  IF sy-subrc = 0.
    SORT gt_map BY function.
    READ TABLE gt_map BINARY SEARCH INTO gs_map WITH KEY function = p_if.
    IF sy-subrc = 0.
      l_fun_id = gs_map-id.
    ELSE. "映射关系中不存在时，取最大编码后加1，作为函数的映射编码
      SORT gt_map BY id DESCENDING.
      READ TABLE gt_map INDEX 1 INTO gs_map.
      l_fun_id = gs_map-id + 1.
      save_if_map.  "保存函数与ID的映射关系
    ENDIF.
  ELSE.
    l_fun_id = 1.                                           "初始编号为1
    save_if_map.  "保存函数与ID的映射关系
  ENDIF.
  COMMIT WORK AND WAIT.

  g_fun_log_id = gc_id_log_prefix && l_fun_id.  "保存函数日志时用的ID
ENDFORM.                    " SET_IF_CHANGED
*&---------------------------------------------------------------------*
*&      Form  check_function
*&---------------------------------------------------------------------*
FORM check_function.
  DATA: ls_fm_header TYPE header_fb,
        lt_import    TYPE rsfb_para,
        lt_export    TYPE rsfb_para,
        lt_tables    TYPE rsfb_para,
        lt_change    TYPE rsfb_para.

  DEFINE show_message.
    MESSAGE &1 TYPE 'E'.
  END-OF-DEFINITION.

  IF p_if IS INITIAL.
    show_message '请输入函数名'.
  ENDIF.

  SELECT SINGLE funcname INTO ls_fm_header-name FROM tfdir WHERE funcname = p_if.
  IF sy-subrc <> 0.
    show_message '函数不存在'.
  ENDIF.

  CALL METHOD cl_fb_parameter_db=>read
    IMPORTING
      import    = lt_import
      export    = lt_export
      tables    = lt_tables
      change    = lt_change
    CHANGING
      header    = ls_fm_header
    EXCEPTIONS
      cancelled = 1
      OTHERS    = 2.
  IF sy-subrc <> 0.
    show_message '函数错误'.
  ENDIF.

  READ TABLE lt_import WITH KEY parameter = 'INPUT' TRANSPORTING NO FIELDS.
  IF sy-subrc <> 0.
    show_message '函数输入参数中没有名为INPUT的参数'.
  ENDIF.
  READ TABLE lt_export WITH KEY parameter = 'OUTPUT' TRANSPORTING NO FIELDS.
  IF sy-subrc <> 0.
    show_message '函数输出参数中没有名为OUTPUT的参数'.
  ENDIF.
  LOOP AT lt_import TRANSPORTING NO FIELDS WHERE optional = '' AND parameter <> 'INPUT'.
    show_message '函数输入参数中有多余的必输参数'.
  ENDLOOP.
  LOOP AT lt_export TRANSPORTING NO FIELDS WHERE optional = '' AND parameter <> 'OUTPUT'.
    show_message '函数输出参数中有多余的必输参数'.
  ENDLOOP.
  READ TABLE lt_change WITH KEY optional = '' TRANSPORTING NO FIELDS.
  IF sy-subrc = 0.
    show_message '函数Change类型参数中有必输参数，无法执行'.
  ENDIF.
  READ TABLE lt_tables WITH KEY optional = '' TRANSPORTING NO FIELDS.
  IF sy-subrc = 0.
    show_message '函数表类型参数中有必输参数，无法执行'.
  ENDIF.
ENDFORM.                    "check_function
*&---------------------------------------------------------------------*
*&      Form  execute_function
*&---------------------------------------------------------------------*
FORM execute_function.
  DATA: ls_history TYPE ty_history,
        l_num      TYPE i,
        lo_exp     TYPE REF TO cx_root,
        l_message  TYPE string.

  DEFINE call_function.
    IF g_debug = 'X'.
      BREAK-POINT AT NEXT APPLICATION STATEMENT.
    ENDIF.
    TRY.
        CALL FUNCTION p_if
          EXPORTING
            input  = g_input
          IMPORTING
            output = g_output.
      CATCH cx_root INTO lo_exp.
        l_message = lo_exp->get_text( ).
        MESSAGE l_message TYPE 'E' DISPLAY LIKE 'I'.
    ENDTRY.
  END-OF-DEFINITION.

  PERFORM get_input_text.

  PERFORM lock_indx USING g_fun_log_id.

  IMPORT history = gt_history FROM DATABASE indx(st) ID g_fun_log_id.
  IF sy-subrc <> 0.
    l_num = 1.
  ELSE.
    SORT gt_history BY num DESCENDING.
    READ TABLE gt_history INDEX 1 INTO gs_history.
    l_num = gs_history-num + 1.
  ENDIF.
  ls_history-num = l_num.
  ls_history-datum = sy-datum.
  WRITE sy-uzeit TO ls_history-uzeit. "时间用HH:MM:SS的格式
  ls_history-input = g_input.
  APPEND ls_history TO gt_history.
  EXPORT history = gt_history TO DATABASE indx(st) ID g_fun_log_id.

  COMMIT WORK AND WAIT. "保存日志

  call_function.  "按F5进入函数模块调试

  go_output->set_textstream( text = g_output ).

  g_need_refresh = 'X'.
ENDFORM.                    "execute_function
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_OUTPUT_XML
*&---------------------------------------------------------------------*
FORM display_output_xml USING p_string TYPE string.
  DATA: lx_xml TYPE xstring.
  DATA: lt_text  TYPE TABLE OF ty_text WITH HEADER LINE,
        lt_tline TYPE TABLE OF tline WITH HEADER LINE.

  IF p_string IS INITIAL.
    MESSAGE '没有可显示的内容' TYPE 'S'.
    EXIT.
  ENDIF.

  CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
    EXPORTING
      text   = p_string
    IMPORTING
      buffer = lx_xml.

  CALL FUNCTION 'DISPLAY_XML_STRING'
    EXPORTING
      xml_string      = lx_xml
      title           = 'XML'
      starting_x      = 50
      starting_y      = 2
    EXCEPTIONS
      no_xml_document = 1
      OTHERS          = 2.
  IF sy-subrc <> 0.
    CALL FUNCTION 'CONVERT_STRING_TO_TABLE'
      EXPORTING
        i_string         = p_string
        i_tabline_length = 150
      TABLES
        et_table         = lt_text[].

    LOOP AT lt_text.
      lt_tline-tdline = lt_text-text.
      APPEND lt_tline.
    ENDLOOP.

    CALL FUNCTION 'COPO_POPUP_TO_DISPLAY_TEXTLIST'
      EXPORTING
        titel      = 'Text'
      TABLES
        text_table = lt_tline[].
  ENDIF.
ENDFORM.                    " DISPLAY_OUTPUT_XML
*&---------------------------------------------------------------------*
*&      Form  lock_indx
*&---------------------------------------------------------------------*
FORM lock_indx USING p_srtfd.
  DATA: l_srtfd TYPE srtfd.
  "SELECT SINGLE FOR UPDATE：写入锁。
  SELECT SINGLE FOR UPDATE srtfd INTO l_srtfd FROM indx
    WHERE relid = 'ST'
      AND srtfd = p_srtfd
      AND srtf2 = 0.
ENDFORM.                    "lock_indx
*&---------------------------------------------------------------------*
*&      Form  HANDLE_ITEM_DBCLICK
*&---------------------------------------------------------------------*
FORM handle_item_dbclick  USING    p_fieldname TYPE lvc_fname
                                   p_node_key TYPE lvc_nkey.
  READ TABLE gt_tree_key WITH KEY key = p_node_key INTO gs_tree_key.
  READ TABLE gt_history INTO gs_history WITH KEY num = gs_tree_key-num.

  go_input->set_textstream( text = gs_history-input ).
ENDFORM.                    " HANDLE_ITEM_DBCLICK
*&---------------------------------------------------------------------*
*&      Form  HANDLE_FUNCTION_SELECTED
*&---------------------------------------------------------------------*
FORM handle_function_selected  USING p_fcode TYPE ui_func.
  DATA: lt_selected_nodes	TYPE lvc_t_nkey,
        l_node_key        TYPE lvc_nkey.

  CASE p_fcode.
    WHEN 'SALL'.  "全选
      LOOP AT gt_tree_key INTO gs_tree_key.
        APPEND gs_tree_key-key TO lt_selected_nodes.
      ENDLOOP.
      go_tree->set_selected_nodes( lt_selected_nodes ).

    WHEN 'DSAL'.  "取消全选
      LOOP AT gt_tree_key INTO gs_tree_key.
        APPEND gs_tree_key-key TO lt_selected_nodes.
      ENDLOOP.
      go_tree->unselect_nodes( lt_selected_nodes ).

    WHEN 'DELETE'.  "删除
      CALL METHOD go_tree->get_selected_nodes
        CHANGING
          ct_selected_nodes = lt_selected_nodes.
      IF lt_selected_nodes IS INITIAL.
        CALL METHOD go_tree->get_selected_item
          IMPORTING
            e_selected_node = l_node_key.
      ENDIF.

      IF lt_selected_nodes IS INITIAL AND l_node_key IS INITIAL.
        MESSAGE '请至少选择一行' TYPE 'S'.
        RETURN.
      ENDIF.

      IF p_conf = 'X'.
        CALL SELECTION-SCREEN 9100 STARTING AT 20 10.
        IF g_9100_ok = ''.
          MESSAGE '没有执行删除操作' TYPE 'S'.
          RETURN.
        ENDIF.
      ENDIF.

      PERFORM lock_indx USING g_fun_log_id.
      IF lt_selected_nodes IS NOT INITIAL.
        IMPORT history = gt_history FROM DATABASE indx(st) ID g_fun_log_id.
        LOOP AT lt_selected_nodes INTO l_node_key.
          READ TABLE gt_tree_key INTO gs_tree_key WITH KEY key = l_node_key.
          DELETE gt_tree_key INDEX sy-tabix.
          DELETE gt_history WHERE num = gs_tree_key-num.
        ENDLOOP.
      ELSE.
        READ TABLE gt_tree_key INTO gs_tree_key WITH KEY key = l_node_key.
        DELETE gt_tree_key INDEX sy-tabix.
        DELETE gt_history WHERE num = gs_tree_key-num.
      ENDIF.

      EXPORT history = gt_history TO DATABASE indx(st) ID g_fun_log_id.
      COMMIT WORK AND WAIT.
      g_need_refresh = 'X'.
      MESSAGE '已删除所选日志' TYPE 'S'.
  ENDCASE.
ENDFORM.                    " HANDLE_FUNCTION_SELECTED
*&---------------------------------------------------------------------*
*&      Form  GET_INPUT_TEXT
*&---------------------------------------------------------------------*
FORM get_input_text .
  DATA: lt_input TYPE TABLE OF ty_text WITH HEADER LINE.

  CALL METHOD go_input->get_text_as_stream
    IMPORTING
      text = lt_input[].
  CLEAR: g_input.
  LOOP AT lt_input.
    CONCATENATE g_input lt_input-text INTO g_input.
  ENDLOOP.
ENDFORM.                    " GET_INPUT_TEXT
