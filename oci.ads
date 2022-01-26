--  OCI is Oracle Call Interfase
--
--  Support Oracle Client version 10.2.0 or higher.

with System;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Pointers;
with Interfaces.C.Strings;

package OCI is

   subtype Text_Ptr is Interfaces.C.Strings.chars_ptr;
   subtype Text is Interfaces.C.char_array;
   subtype OraText is Text_Ptr;

   subtype Ub1 is Interfaces.C.unsigned_char;
   subtype Sb1 is Interfaces.C.signed_char;
   subtype Eb1 is Interfaces.C.char;

   subtype Ub2 is Interfaces.C.unsigned_short;
   subtype Sb2 is Interfaces.C.short;
   subtype Eb2 is Interfaces.C.short;

   subtype Ub4 is Interfaces.C.unsigned;
   subtype Sb4 is Interfaces.C.int;
   subtype Eb4 is Interfaces.C.long;

   subtype Ub8 is Interfaces.Unsigned_64;
   subtype Sb8 is Interfaces.Integer_64;

   subtype UWord is Interfaces.C.unsigned;
   subtype SWord is Interfaces.C.int;
   subtype EWord is Interfaces.C.int;

   subtype Bool is Interfaces.C.int;

   subtype DVoid is System.Address;

   type A_Int is access all Interfaces.C.int;
   type A_Ub1 is access all Ub1;
   type A_Ub2 is access all Ub2;
   type A_Sb2 is access all Sb2;
   type A_Ub4 is access all Ub4;
   type A_Sb4 is access all Sb4;
   type A_Ub8 is access all Ub8;

   type OCITypeCode is new Ub2;

   subtype Function_Pointer is DVoid;

   subtype OCIHandle is System.Address;
   Null_OCIHandle : constant OCIHandle := System.Null_Address;

   type OCIEnv is new OCIHandle;
   Null_OCIEnv : constant OCIEnv := OCIEnv (Null_OCIHandle);

   type OCIError is new OCIHandle;
   Null_OCIError : constant OCIError := OCIError (Null_OCIHandle);

   type OCISvcCtx is new OCIHandle;
   Null_OCISvcCtx : constant OCISvcCtx := OCISvcCtx (Null_OCIHandle);

   type OCIStmt is new OCIHandle;
   Null_OCIStmt : constant OCIStmt := OCIStmt (Null_OCIHandle);

   type OCIBind is new OCIHandle;
   Null_OCIBind : constant OCIBind := OCIBind (Null_OCIHandle);

   type OCIDefine is new OCIHandle;
   Null_OCIDefine : constant OCIDefine := OCIDefine (Null_OCIHandle);

   type OCIDescribe is new OCIHandle;
   Null_OCIDescribe : constant OCIDescribe := OCIDescribe (Null_OCIHandle);

   type OCIServer is new OCIHandle;
   Null_OCIServer : constant OCIServer := OCIServer (Null_OCIHandle);

   type OCISession is new OCIHandle;
   Null_OCISession : constant OCISession := OCISession (Null_OCIHandle);

--     type OCIComplexObject is new OCIHandle;
--     type OCITrans is new OCIHandle;
--     type OCISecurity is new OCIHandle;
--     type OCISubscription is new OCIHandle;
--     subtype OCIType is OCIHandle;

--     type OCISnapshot is new OCIHandle;
--     type OCIResult is new OCIHandle;

   type OCILobLocator is new OCIHandle;
   Null_OCILobLocator : constant OCILobLocator :=
     OCILobLocator (Null_OCIHandle);

   type OCIParam is new OCIHandle;
   Null_OCIParam : constant OCIParam := OCIParam (Null_OCIHandle);

--     type OCIComplexObjectComp is new OCIHandle;
--     type OCIRowid is new OCIHandle;

   type OCIDateTime is new OCIHandle;
   Null_OCIDateTime : constant OCIDateTime := OCIDateTime (Null_OCIHandle);

--     type OCIInterval is new OCIHandle;
--     type OCIColl is new OCIHandle;

--     type OCIExtProcContext is new OCIHandle;

--     type OCIAQEnqOptions is new OCIHandle;
--     type OCIAQDeqOptions is new OCIHandle;
--     type OCIAQMsgProperties is new OCIHandle;
--     type OCIAQAgent is new OCIHandle;
--     type OCIAQNfyDescriptor is new OCIHandle;

   type OCITypeGetOpt is (OCI_TYPEGET_HEADER, OCI_TYPEGET_ALL);
   pragma Convention (C, OCITypeGetOpt);

   type Vector_Of_Ub1 is array (Integer range <>) of aliased Ub1;
   type Vector_Of_Ub4 is array (Integer range <>) of aliased Ub4;
   type Vector_Of_OraText is array (Integer range <>) of aliased OraText;
   type Vector_Of_OCIBind is array (Integer range <>) of aliased OCIBind;

   package Ub1_Ptr is new Interfaces.C.Pointers
     (Integer, Ub1, Vector_Of_Ub1, 0);
   subtype A_Vector_Of_Ub1 is Ub1_Ptr.Pointer;

   package OraText_Ptr is new Interfaces.C.Pointers
      (Integer, OraText, Vector_Of_OraText, Interfaces.C.Strings.Null_Ptr);
   subtype A_Vector_Of_OraText is OraText_Ptr.Pointer;

   package OCIBind_Ptr is new Interfaces.C.Pointers
      (Integer, OCIBind, Vector_Of_OCIBind, OCIBind (Null_OCIHandle));
   subtype A_Vector_Of_OCIBind is OCIBind_Ptr.Pointer;

   subtype Undefined is Integer;
   Empty_Undefined : constant := 0;

   --  Temporary attribute value for character set ID
   OCI_UTF8ID  : constant := 871;  --  UTF-8 character set ID.
   --  See unofficial "Oracle Character Set ID Reference":
   --  http://www.mydul.net/charsets.html
   OCI_UCS2ID  : constant := 1000; --  UCS2 character set ID
   OCI_UTF16ID : constant := 1000; --  UTF-16 character set ID.
   --  See oci.h file from Oracle Client SDK

   --  Handle types
   OCI_HTYPE_FIRST                : constant := 1;
   OCI_HTYPE_ENV                  : constant := 1;
   OCI_HTYPE_ERROR                : constant := 2;
   OCI_HTYPE_SVCCTX               : constant := 3;
   OCI_HTYPE_STMT                 : constant := 4;
   OCI_HTYPE_BIND                 : constant := 5;
   OCI_HTYPE_DEFINE               : constant := 6;
   OCI_HTYPE_DESCRIBE             : constant := 7;
   OCI_HTYPE_SERVER               : constant := 8;
   OCI_HTYPE_SESSION              : constant := 9;
   OCI_HTYPE_TRANS                : constant := 10;
   OCI_HTYPE_COMPLEXOBJECT        : constant := 11;
   OCI_HTYPE_SECURITY             : constant := 12;
   OCI_HTYPE_SUBSCRIPTION         : constant := 13;
   OCI_HTYPE_DIRPATH_CTX          : constant := 14;
   OCI_HTYPE_DIRPATH_COLUMN_ARRAY : constant := 15;
   OCI_HTYPE_DIRPATH_STREAM       : constant := 16;
   OCI_HTYPE_PROC                 : constant := 17;
   OCI_HTYPE_DIRPATH_FN_CTX       : constant := 18;
   OCI_HTYPE_DIRPATH_FN_COL_ARRAY : constant := 19;
   OCI_HTYPE_XADSESSION           : constant := 20;
   OCI_HTYPE_XADTABLE             : constant := 21;
   OCI_HTYPE_XADFIELD             : constant := 22;
   OCI_HTYPE_XADGRANULE           : constant := 23;
   OCI_HTYPE_XADRECORD            : constant := 24;
   OCI_HTYPE_XADIO                : constant := 25;
   OCI_HTYPE_CPOOL                : constant := 26;
   OCI_HTYPE_SPOOL                : constant := 27;
   OCI_HTYPE_ADMIN                : constant := 28;
   OCI_HTYPE_EVENT                : constant := 29;
   OCI_HTYPE_LAST                 : constant := 29;

   --  Descriptor types
   OCI_DTYPE_FIRST                : constant := 50;
   OCI_DTYPE_LOB                  : constant := 50;
   OCI_DTYPE_SNAP                 : constant := 51;
   OCI_DTYPE_RSET                 : constant := 52;
   OCI_DTYPE_PARAM                : constant := 53;
   OCI_DTYPE_ROWID                : constant := 54;
   OCI_DTYPE_COMPLEXOBJECTCOMP    : constant := 55;
   OCI_DTYPE_FILE                 : constant := 56;
   OCI_DTYPE_AQENQ_OPTIONS        : constant := 57;
   OCI_DTYPE_AQDEQ_OPTIONS        : constant := 58;
   OCI_DTYPE_AQMSG_PROPERTIES     : constant := 59;
   OCI_DTYPE_AQAGENT              : constant := 60;
   OCI_DTYPE_LOCATOR              : constant := 61;
   OCI_DTYPE_DATETIME             : constant := 62;
   OCI_DTYPE_INTERVAL             : constant := 63;
   OCI_DTYPE_AQNFY_DESCRIPTOR     : constant := 64;
   OCI_DTYPE_DATE                 : constant := 65;
   OCI_DTYPE_TIME                 : constant := 66;
   OCI_DTYPE_TIME_TZ              : constant := 67;
   OCI_DTYPE_TIMESTAMP            : constant := 68;
   OCI_DTYPE_TIMESTAMP_TZ         : constant := 69;
   OCI_DTYPE_TIMESTAMP_LTZ        : constant := 70;
   OCI_DTYPE_UCB                  : constant := 71;
   OCI_DTYPE_SRVDN                : constant := 72;
   OCI_DTYPE_SIGNATURE            : constant := 73;
   OCI_DTYPE_RESERVED_1           : constant := 74;
   OCI_DTYPE_AQLIS_OPTIONS        : constant := 75;
   OCI_DTYPE_AQLIS_MSG_PROPERTIES : constant := 76;
   OCI_DTYPE_CHDES                : constant := 77;
   OCI_DTYPE_TABLE_CHDES          : constant := 78;
   OCI_DTYPE_ROW_CHDES            : constant := 79;
   OCI_DTYPE_CQDES                : constant := 80;
   OCI_DTYPE_LOB_REGION           : constant := 81;
   OCI_DTYPE_RESERVED_82          : constant := 82;
   OCI_DTYPE_SHARDING_KEY         : constant := 83;
   OCI_DTYPE_SHARD_INST           : constant := 84;
   OCI_DTYPE_LAST                 : constant := 84;

   --  Lob types
   OCI_TEMP_BLOB : constant := 1;
   OCI_TEMP_CLOB : constant := 2;

   --  Object Ptr Types
   OCI_OTYPE_NAME : constant := 1;
   OCI_OTYPE_REF  : constant := 2;
   OCI_OTYPE_PTR  : constant := 3;

   --  DB Change: Operation types
   OCI_OPCODE_ALLOPS  : constant := 16#00#;
   OCI_OPCODE_ALLROWS : constant := 16#01#;
   OCI_OPCODE_INSERT  : constant := 16#02#;
   OCI_OPCODE_UPDATE  : constant := 16#04#;
   OCI_OPCODE_DELETE  : constant := 16#08#;
   OCI_OPCODE_ALTER   : constant := 16#10#;
   OCI_OPCODE_DROP    : constant := 16#20#;
   OCI_OPCODE_UNKNOWN : constant := 16#40#;

   --  Server Handle Attribute Values
   --  OCI_ATTR_SERVER_STATUS
   OCI_SERVER_NOT_CONNECTED : constant := 0;
   OCI_SERVER_NORMAL        : constant := 1;

   --  Supported Namespaces
   OCI_SUBSCR_NAMESPACE_ANONYMOUS : constant := 0; -- Anonymous Namespace
   OCI_SUBSCR_NAMESPACE_AQ        : constant := 1; -- Advanced Queues
   OCI_SUBSCR_NAMESPACE_DBCHANGE  : constant := 2; -- DB change notification
   OCI_SUBSCR_NAMESPACE_RESERVED1 : constant := 3;
   OCI_SUBSCR_NAMESPACE_MAX       : constant := 4; -- Max Name Space Number

   --  Credential Types
   OCI_CRED_RDBMS : constant := 1;  -- database username/password
   OCI_CRED_EXT   : constant := 2;  -- externally provided credentials
   OCI_CRED_PROXY : constant := 3;  -- proxy authentication

   --  Error Return Values
   OCI_SUCCESS           : constant := 0;
   OCI_SUCCESS_WITH_INFO : constant := 1;
   OCI_NO_DATA           : constant := 100;
   OCI_ERROR             : constant := -1;
   OCI_INVALID_HANDLE    : constant := -2;
   OCI_NEED_DATA         : constant := 99;
   OCI_STILL_EXECUTING   : constant := -3123;

   -- User Callback Return Values --
   OCI_CONTINUE    : constant := -24200;
   OCI_ROWCBK_DONE : constant := -24201;

   --  Parsing Syntax Types
   OCI_V7_SYNTAX  : constant := 2;
   OCI_V8_SYNTAX  : constant := 3;
   OCI_NTV_SYNTAX : constant := 1;

   --  (Scrollable Cursor) Fetch Options
   --  For non-scrollable cursor, the only valid (and default) orientation is
   --  OCI_FETCH_NEXT
   OCI_FETCH_CURRENT  : constant := 16#0001#;
   OCI_FETCH_NEXT     : constant := 16#0002#;
   OCI_FETCH_FIRST    : constant := 16#0004#;
   OCI_FETCH_LAST     : constant := 16#0008#;
   OCI_FETCH_PRIOR    : constant := 16#0010#;
   OCI_FETCH_ABSOLUTE : constant := 16#0020#;
   OCI_FETCH_RELATIVE : constant := 16#0040#;

   --  Bind and Define Options
   OCI_SB2_IND_PTR       : constant := 16#001#; --  unused
   OCI_DATA_AT_EXEC      : constant := 16#002#;
   OCI_DYNAMIC_FETCH     : constant := 16#002#;
   OCI_PIECEWISE         : constant := 16#004#;
   OCI_DEFINE_RESERVED_1 : constant := 16#008#;
   OCI_BIND_RESERVED_2   : constant := 16#010#;
   OCI_DEFINE_RESERVED_2 : constant := 16#020#;
   OCI_BIND_SOFT         : constant := 16#040#;
   OCI_DEFINE_SOFT       : constant := 16#080#;
   OCI_BIND_RESERVED_3   : constant := 16#100#;
   OCI_IOV               : constant := 16#200#;

   --  Various Modes
   --  The default value for parameters and attributes
   OCI_DEFAULT                   : constant := 0;
   --  OCICreateEnvironment Modes
   OCI_THREADED                  : constant := 16#00000001#;
   OCI_OBJECT                    : constant := 16#00000002#;
   OCI_EVENTS                    : constant := 16#00000004#;
   OCI_SHARED                    : constant := 16#00000010#;
   OCI_NO_UCB                    : constant := 16#00000040#;
   OCI_NO_MUTEX                  : constant := 16#00000080#;
   OCI_SHARED_EXT                : constant := 16#00000100#;
   OCI_ALWAYS_BLOCKING           : constant := 16#00000400#;
   OCI_USE_LDAP                  : constant := 16#00001000#;
   OCI_REG_LDAPONLY              : constant := 16#00002000#;
   OCI_UTF16                     : constant := 16#00004000#;
   OCI_AFC_PAD_ON                : constant := 16#00008000#;
   OCI_NEW_LENGTH_SEMANTICS      : constant := 16#00020000#;
   OCI_NO_MUTEX_STMT             : constant := 16#00040000#;
   OCI_MUTEX_ENV_ONLY            : constant := 16#00080000#;
   OCI_SUPPRESS_NLS_VALIDATION   : constant := 16#00100000#;
   OCI_MUTEX_TRY                 : constant := 16#00200000#;
   OCI_NCHAR_LITERAL_REPLACE_ON  : constant := 16#00400000#;
   OCI_NCHAR_LITERAL_REPLACE_OFF : constant := 16#00800000#;
   OCI_ENABLE_NLS_VALIDATION     : constant := 16#01000000#;
   --  client initiated notification listener connections, applicable only for
   --  12c queues and above
   OCI_SECURE_NOTIFICATION       : constant := 16#20000000#;
   OCI_DISABLE_DIAG              : constant := 16#40000000#;

   --  OCILogon2 Modes
   OCI_LOGON2_SPOOL     : constant := 16#0001#;
   OCI_LOGON2_STMTCACHE : constant := 16#0004#;
   OCI_LOGON2_PROXY     : constant := 16#0008#;
   OCI_LOGON2_CPOOL     : constant := 16#0200#;

   --  Statement States
   OCI_STMT_STATE_INITIALIZED  : constant := 16#0001#;
   OCI_STMT_STATE_EXECUTED     : constant := 16#0002#;
   OCI_STMT_STATE_END_OF_FETCH : constant := 16#0003#;

   --  Execution Modes
   OCI_BATCH_MODE               : constant := 16#000001#;
   OCI_EXACT_FETCH              : constant := 16#000002#;
   OCI_KEEP_FETCH_STATE         : constant := 16#000004#;
   OCI_STMT_SCROLLABLE_READONLY : constant := 16#000008#;
   OCI_DESCRIBE_ONLY            : constant := 16#000010#;
   OCI_COMMIT_ON_SUCCESS        : constant := 16#000020#;
   OCI_NON_BLOCKING             : constant := 16#000040#;
   OCI_BATCH_ERRORS             : constant := 16#000080#;
   OCI_PARSE_ONLY               : constant := 16#000100#;
   OCI_SHOW_DML_WARNINGS        : constant := 16#000400#;
   OCI_RESULT_CACHE             : constant := 16#020000#;
   OCI_NO_RESULT_CACHE          : constant := 16#040000#;
   OCI_RETURN_ROW_COUNT_ARRAY   : constant := 16#100000#;

   --  OCIStmtPrepare2 Modes
   OCI_PREP2_CACHE_SEARCHONLY    : constant := 16#0010#;
   OCI_PREP2_GET_PLSQL_WARNINGS  : constant := 16#0020#;
   OCI_PREP2_IMPL_RESULTS_CLIENT : constant := 16#0400#;
   OCI_PREP2_GET_SQL_ID          : constant := 16#2000#;

   --  OCIStmtRelease Modes
   OCI_STRLS_CACHE_DELETE : constant := 16#10#;

   --  Piece Information
   OCI_PARAM_IN  : constant := 1;
   OCI_PARAM_OUT : constant := 2;

   --  Transaction Start Flags
   OCI_TRANS_NEW          : constant := 16#00000001#;
   OCI_TRANS_JOIN         : constant := 16#00000002#;
   OCI_TRANS_RESUME       : constant := 16#00000004#;
   OCI_TRANS_PROMOTE      : constant := 16#00000008#;
   OCI_TRANS_STARTMASK    : constant := 16#000000ff#;
   OCI_TRANS_READONLY     : constant := 16#00000100#;
   OCI_TRANS_READWRITE    : constant := 16#00000200#;
   OCI_TRANS_SERIALIZABLE : constant := 16#00000400#;
   OCI_TRANS_ISOLMASK     : constant := 16#0000ff00#;
   OCI_TRANS_LOOSE        : constant := 16#00010000#;
   OCI_TRANS_TIGHT        : constant := 16#00020000#;
   OCI_TRANS_TYPEMASK     : constant := 16#000f0000#;
   OCI_TRANS_NOMIGRATE    : constant := 16#00100000#;
   OCI_TRANS_SEPARABLE    : constant := 16#00200000#;
   OCI_TRANS_OTSRESUME    : constant := 16#00400000#;
   OCI_TRANS_OTHRMASK     : constant := 16#fff00000#;

   --  Transaction End Flags
   OCI_TRANS_TWOPHASE    : constant := 16#01000000#;
   OCI_TRANS_WRITEBATCH  : constant := 16#00000001#;
   OCI_TRANS_WRITEIMMED  : constant := 16#00000002#;
   OCI_TRANS_WRITEWAIT   : constant := 16#00000004#;
   OCI_TRANS_WRITENOWAIT : constant := 16#00000008#;

   --  Attributes common to Columns and Stored Procs
   OCI_ATTR_DATA_SIZE    : constant := 1;
   OCI_ATTR_DATA_TYPE    : constant := 2;
   OCI_ATTR_DISP_SIZE    : constant := 3;
   OCI_ATTR_NAME         : constant := 4;
   OCI_ATTR_PRECISION    : constant := 5;
   OCI_ATTR_SCALE        : constant := 6;
   OCI_ATTR_IS_NULL      : constant := 7;
   OCI_ATTR_TYPE_NAME    : constant := 8;
   OCI_ATTR_SCHEMA_NAME  : constant := 9;
   OCI_ATTR_SUB_NAME     : constant := 10;
   OCI_ATTR_POSITION     : constant := 11;
   OCI_ATTR_PACKAGE_NAME : constant := 12;

   --  Attribute Types
   OCI_ATTR_FNCODE                  : constant := 1;
   OCI_ATTR_OBJECT                  : constant := 2;
   OCI_ATTR_NONBLOCKING_MODE        : constant := 3;
   OCI_ATTR_SQLCODE                 : constant := 4;
   OCI_ATTR_ENV                     : constant := 5;
   OCI_ATTR_SERVER                  : constant := 6;
   OCI_ATTR_SESSION                 : constant := 7;
   OCI_ATTR_TRANS                   : constant := 8;
   OCI_ATTR_ROW_COUNT               : constant := 9;
   OCI_ATTR_SQLFNCODE               : constant := 10;
   OCI_ATTR_PREFETCH_ROWS           : constant := 11;
   OCI_ATTR_NESTED_PREFETCH_ROWS    : constant := 12;
   OCI_ATTR_PREFETCH_MEMORY         : constant := 13;
   OCI_ATTR_NESTED_PREFETCH_MEMORY  : constant := 14;
   OCI_ATTR_CHAR_COUNT              : constant := 15;
   OCI_ATTR_PDSCL                   : constant := 16;
   OCI_ATTR_PDFMT                   : constant := 17;
   OCI_ATTR_PARAM_COUNT             : constant := 18;
   OCI_ATTR_ROWID                   : constant := 19;
   OCI_ATTR_CHARSET                 : constant := 20;
   OCI_ATTR_NCHAR                   : constant := 21;
   OCI_ATTR_USERNAME                : constant := 22;
   OCI_ATTR_PASSWORD                : constant := 23;
   OCI_ATTR_STMT_TYPE               : constant := 24;
   OCI_ATTR_INTERNAL_NAME           : constant := 25;
   OCI_ATTR_EXTERNAL_NAME           : constant := 26;
   OCI_ATTR_XID                     : constant := 27;
   OCI_ATTR_TRANS_LOCK              : constant := 28;
   OCI_ATTR_TRANS_NAME              : constant := 29;
   OCI_ATTR_HEAPALLOC               : constant := 30;
   OCI_ATTR_CHARSET_ID              : constant := 31;
   OCI_ATTR_CHARSET_FORM            : constant := 32;
   OCI_ATTR_MAXDATA_SIZE            : constant := 33;
   OCI_ATTR_CACHE_OPT_SIZE          : constant := 34;
   OCI_ATTR_CACHE_MAX_SIZE          : constant := 35;
   OCI_ATTR_PINOPTION               : constant := 36;
   OCI_ATTR_ALLOC_DURATION          : constant := 37;
   OCI_ATTR_PIN_DURATION            : constant := 38;
   OCI_ATTR_FDO                     : constant := 39;
   OCI_ATTR_POSTPROCESSING_CALLBACK : constant := 40;
   OCI_ATTR_POSTPROCESSING_CONTEXT  : constant := 41;
   OCI_ATTR_ROWS_RETURNED           : constant := 42;
   OCI_ATTR_FOCBK                   : constant := 43;
   OCI_ATTR_IN_V8_MODE              : constant := 44;
   OCI_ATTR_LOBEMPTY                : constant := 45;
   OCI_ATTR_SESSLANG                : constant := 46;

   OCI_ATTR_NUM_DML_ERRORS : constant := 73;
   OCI_ATTR_DML_ROW_OFFSET : constant := 74;

   OCI_ATTR_PARSE_ERROR_OFFSET : constant := 129;

   OCI_ATTR_SERVER_STATUS      : constant := 143;

   --  complex object retrieval parameter attributes
   OCI_ATTR_COMPLEXOBJECTCOMP_TYPE       : constant := 50;
   OCI_ATTR_COMPLEXOBJECTCOMP_TYPE_LEVEL : constant := 51;
   OCI_ATTR_COMPLEXOBJECT_LEVEL          : constant := 52;
   OCI_ATTR_COMPLEXOBJECT_COLL_OUTOFLINE : constant := 53;

   OCI_ATTR_NOCACHE     : constant := 87; -- Temporary LOBs

   OCI_ATTR_SUBSCR_NAME      : constant := 94;
   OCI_ATTR_SUBSCR_CALLBACK  : constant := 95;
   OCI_ATTR_SUBSCR_CTX       : constant := 96;
   OCI_ATTR_SUBSCR_PAYLOAD   : constant := 97;
   OCI_ATTR_SUBSCR_NAMESPACE : constant := 98;

   OCI_ATTR_CONSUMER_NAME : constant := 50;
   OCI_ATTR_DEQ_MODE      : constant := 51;
   OCI_ATTR_NAVIGATION    : constant := 52;
   OCI_ATTR_WAIT          : constant := 53;
   OCI_ATTR_DEQ_MSGID     : constant := 54;

   OCI_ATTR_QUEUE_NAME : constant := 70;
   OCI_ATTR_NFY_MSGID  : constant := 71;
   OCI_ATTR_MSG_PROP   : constant := 72;

   --  only named type attributes
   OCI_ATTR_TYPECODE                 : constant := 216;
   OCI_ATTR_COLLECTION_TYPECODE      : constant := 217;
   OCI_ATTR_VERSION                  : constant := 218;
   OCI_ATTR_IS_INCOMPLETE_TYPE       : constant := 219;
   OCI_ATTR_IS_SYSTEM_TYPE           : constant := 220;
   OCI_ATTR_IS_PREDEFINED_TYPE       : constant := 221;
   OCI_ATTR_IS_TRANSIENT_TYPE        : constant := 222;
   OCI_ATTR_IS_SYSTEM_GENERATED_TYPE : constant := 223;
   OCI_ATTR_HAS_NESTED_TABLE         : constant := 224;
   OCI_ATTR_HAS_LOB                  : constant := 225;
   OCI_ATTR_HAS_FILE                 : constant := 226;
   OCI_ATTR_COLLECTION_ELEMENT       : constant := 227;
   OCI_ATTR_NUM_TYPE_ATTRS           : constant := 228;
   OCI_ATTR_LIST_TYPE_ATTRS          : constant := 229;
   OCI_ATTR_NUM_TYPE_METHODS         : constant := 230;
   OCI_ATTR_LIST_TYPE_METHODS        : constant := 231;
   OCI_ATTR_MAP_METHOD               : constant := 232;
   OCI_ATTR_ORDER_METHOD             : constant := 233;

   OCI_ATTR_CHNF_TABLENAMES         : constant := 401;
   OCI_ATTR_CHNF_ROWIDS             : constant := 402;
   OCI_ATTR_CHNF_OPERATIONS         : constant := 403;
   OCI_ATTR_CHNF_CHANGELAG          : constant := 404;
   OCI_ATTR_CHDES_DBNAME            : constant := 405;
   OCI_ATTR_CHDES_NFYTYPE           : constant := 406;
   OCI_ATTR_CHDES_XID               : constant := 407;
   OCI_ATTR_CHDES_TABLE_CHANGES     : constant := 408;
   OCI_ATTR_CHDES_TABLE_NAME        : constant := 409;
   OCI_ATTR_CHDES_TABLE_OPFLAGS     : constant := 410;
   OCI_ATTR_CHDES_TABLE_ROW_CHANGES : constant := 411;
   OCI_ATTR_CHDES_ROW_ROWID         : constant := 412;
   OCI_ATTR_CHDES_ROW_OPFLAGS       : constant := 413;

   OCI_ATTR_CHNF_REGHANDLE    : constant := 414;
   OCI_ATTR_NETWORK_FILE_DESC : constant := 415;

   --  Other Constants
   OCI_MAX_FNS                     : constant := 100;
   OCI_SQLSTATE_SIZE               : constant := 5;
   OCI_ERROR_MAXMSG_SIZE           : constant := 1024;
   OCI_ERROR_MAXMSG_SIZE2          : constant := 3072;
   OCI_LOBMAXSIZE                  : constant := Ub4'Last;
   OCI_ROWID_LEN                   : constant := 23;
   OCI_LOB_CONTENTTYPE_MAXSIZE     : constant := 128;
   OCI_LOB_CONTENTTYPE_MAXBYTESIZE : constant := OCI_LOB_CONTENTTYPE_MAXSIZE;

   OCI_ONE_PIECE   : constant := 0;
   OCI_FIRST_PIECE : constant := 1;
   OCI_NEXT_PIECE  : constant := 2;
   OCI_LAST_PIECE  : constant := 3;

   --  LOB open modes
   OCI_LOB_READONLY      : constant := 1;
   OCI_LOB_READWRITE     : constant := 2;
   OCI_LOB_WRITEONLY     : constant := 3;
   OCI_LOB_APPENDONLY    : constant := 4;
   OCI_LOB_FULLOVERWRITE : constant := 5;
   OCI_LOB_FULLREAD      : constant := 6;

   --  OCI Statement Types
   OCI_STMT_UNKNOWN  : constant := 0;
   OCI_STMT_SELECT   : constant := 1;
   OCI_STMT_UPDATE   : constant := 2;
   OCI_STMT_DELETE   : constant := 3;
   OCI_STMT_INSERT   : constant := 4;
   OCI_STMT_CREATE   : constant := 5;
   OCI_STMT_DROP     : constant := 6;
   OCI_STMT_ALTER    : constant := 7;
   OCI_STMT_BEGIN    : constant := 8;
   OCI_STMT_DECLARE  : constant := 9;
   OCI_STMT_CALL     : constant := 10;
   OCI_STMT_MERGE    : constant := 16;
   OCI_STMT_ROLLBACK : constant := 17;
   OCI_STMT_COMMIT   : constant := 21;

   --  OCI Parameter Types
   OCI_PTYPE_UNK                : constant := 0;
   OCI_PTYPE_TABLE              : constant := 1;
   OCI_PTYPE_VIEW               : constant := 2;
   OCI_PTYPE_PROC               : constant := 3;
   OCI_PTYPE_FUNC               : constant := 4;
   OCI_PTYPE_PKG                : constant := 5;
   OCI_PTYPE_TYPE               : constant := 6;
   OCI_PTYPE_SYN                : constant := 7;
   OCI_PTYPE_SEQ                : constant := 8;
   OCI_PTYPE_COL                : constant := 9;
   OCI_PTYPE_ARG                : constant := 10;
   OCI_PTYPE_LIST               : constant := 11;
   OCI_PTYPE_TYPE_ATTR          : constant := 12;
   OCI_PTYPE_TYPE_COLL          : constant := 13;
   OCI_PTYPE_TYPE_METHOD        : constant := 14;
   OCI_PTYPE_TYPE_ARG           : constant := 15;
   OCI_PTYPE_TYPE_RESULT        : constant := 16;
   OCI_PTYPE_SCHEMA             : constant := 17;
   OCI_PTYPE_DATABASE           : constant := 18;
   OCI_PTYPE_RULE               : constant := 19;
   OCI_PTYPE_RULE_SET           : constant := 20;
   OCI_PTYPE_EVALUATION_CONTEXT : constant := 21;
   OCI_PTYPE_TABLE_ALIAS        : constant := 22;
   OCI_PTYPE_VARIABLE_TYPE      : constant := 23;
   OCI_PTYPE_NAME_VALUE         : constant := 24;
   OCI_PTYPE_HIERARCHY          : constant := 25;
   OCI_PTYPE_ANALYTIC_VIEW      : constant := 26;

   --  OCI List Types
   OCI_LTYPE_UNK           : constant := 0;
   OCI_LTYPE_COLUMN        : constant := 1;
   OCI_LTYPE_ARG_PROC      : constant := 2;
   OCI_LTYPE_ARG_FUNC      : constant := 3;
   OCI_LTYPE_SUBPRG        : constant := 4;
   OCI_LTYPE_TYPE_ATTR     : constant := 5;
   OCI_LTYPE_TYPE_METHOD   : constant := 6;
   OCI_LTYPE_TYPE_ARG_PROC : constant := 7;
   OCI_LTYPE_TYPE_ARG_FUNC : constant := 8;
   OCI_LTYPE_SCH_OBJ       : constant := 9;
   OCI_LTYPE_DB_SCH        : constant := 10;
   OCI_LTYPE_TYPE_SUBTYPE  : constant := 11;
   OCI_LTYPE_TABLE_ALIAS   : constant := 12;
   OCI_LTYPE_VARIABLE_TYPE : constant := 13;
   OCI_LTYPE_NAME_VALUE    : constant := 14;
   OCI_LTYPE_PACKAGE_TYPE  : constant := 15;

   --  Memory Cartridge Services
   OCI_MEMORY_CLEARED : constant := 1;

   --  User Callback Constants
   OCI_UCBTYPE_ENTRY   : constant := 1; --  entry callback
   OCI_UCBTYPE_EXIT    : constant := 2; --  exit callback
   OCI_UCBTYPE_REPLACE : constant := 3; --  replacement callback

   --  input data types
   SQLT_CHR      : constant := 1;   -- (ORANET TYPE) character string
   SQLT_NUM      : constant := 2;   -- (ORANET TYPE) oracle numeric
   SQLT_INT      : constant := 3;   -- (ORANET TYPE) integer
   SQLT_FLT      : constant := 4;   -- (ORANET TYPE) Floating point number
   SQLT_STR      : constant := 5;   -- zero terminated string
   SQLT_VNU      : constant := 6;   -- NUM with preceding length byte
   SQLT_PDN      : constant := 7;   -- (ORANET TYPE) Packed Decimal Numeric
   SQLT_LNG      : constant := 8;   -- long
   SQLT_VCS      : constant := 9;   -- Variable character string
   SQLT_NON      : constant := 10;  -- Null/empty PCC Descriptor entry
   SQLT_RID      : constant := 11;  -- rowid
   SQLT_DAT      : constant := 12;  -- date in oracle format
   SQLT_VBI      : constant := 15;  -- binary in VCS format
   SQLT_BFLOAT   : constant := 21;  -- Native binary float
   SQLT_BDOUBLE  : constant := 22;  -- Native binary double
   SQLT_BIN      : constant := 23;  -- binary data (DTYBIN)
   SQLT_LBI      : constant := 24;  -- long binary
   SQLT_UIN      : constant := 68;  -- unsigned integer
   SQLT_SLS      : constant := 91;  -- Display sign leading separate
   SQLT_LVC      : constant := 94;  -- Longer longs (char)
   SQLT_LVB      : constant := 95;  -- Longer long binary
   SQLT_AFC      : constant := 96;  -- Ansi fixed char
   SQLT_AVC      : constant := 97;  -- Ansi Var char
   SQLT_IBFLOAT  : constant := 100; -- binary float canonical
   SQLT_IBDOUBLE : constant := 101; -- binary double canonical
   SQLT_CUR      : constant := 102; -- cursor  type
   SQLT_RDD      : constant := 104; -- rowid descriptor
   SQLT_LAB      : constant := 105; -- label type
   SQLT_OSL      : constant := 106; -- oslabel type

   SQLT_NTY    : constant := 108; -- named object type
   SQLT_REF    : constant := 110; -- ref type
   SQLT_CLOB   : constant := 112; -- character lob
   SQLT_BLOB   : constant := 113; -- binary lob
   SQLT_BFILEE : constant := 114; -- binary file lob
   SQLT_CFILEE : constant := 115; -- character file lob
   SQLT_RSET   : constant := 116; -- result set type
   SQLT_NCO    : constant := 122; -- named collection (varray or nested table)
   SQLT_VST    : constant := 155; -- OCIString type
   SQLT_ODT    : constant := 156; -- OCIDate type

   --  datetimes and intervals
   SQLT_DATE          : constant := 184; -- ANSI Date
   SQLT_TIME          : constant := 185; -- TIME
   SQLT_TIME_TZ       : constant := 186; -- TIME WITH TIME ZONE
   SQLT_TIMESTAMP     : constant := 187; -- TIMESTAMP
   SQLT_TIMESTAMP_TZ  : constant := 188; -- TIMESTAMP WITH TIME ZONE
   SQLT_INTERVAL_YM   : constant := 189; -- INTERVAL YEAR TO MONTH
   SQLT_INTERVAL_DS   : constant := 190; -- INTERVAL DAY TO SECOND
   SQLT_TIMESTAMP_LTZ : constant := 232; -- TIMESTAMP WITH LOCAL TZ

   SQLT_PNTY : constant := 241; -- pl/sql representation of named types

   --  this has been added for backward compatibility
   SQLT_FILE  : constant := SQLT_BFILEE;
   SQLT_CFILE : constant := SQLT_CFILEE;
   SQLT_BFILE : constant := SQLT_BFILEE;

   --  CHAR/NCHAR/VARCHAR2/NVARCHAR2/CLOB/NCLOB char set "form" information
   SQLCS_IMPLICIT : constant := 1; -- CHAR, VARCHAR2, CLOB w/o a specified set
   SQLCS_NCHAR    : constant := 2; -- NCHAR, NCHAR VARYING, NCLOB
   SQLCS_EXPLICIT : constant := 3; -- CHAR, etc, with "CHARACTER SET..." syntax
   SQLCS_FLEXIBLE : constant := 4; -- PL/SQL "flexible" parameters
   SQLCS_LIT_NULL : constant := 5; -- typecheck of NULL and empty_clob() lits

   --  Type manager typecodes
   OCI_TYPECODE_REF             : constant := SQLT_REF;
   OCI_TYPECODE_DATE            : constant := SQLT_DAT;
   OCI_TYPECODE_SIGNED8         : constant := 27;
   OCI_TYPECODE_SIGNED16        : constant := 28;
   OCI_TYPECODE_SIGNED32        : constant := 29;
   OCI_TYPECODE_REAL            : constant := 21;
   OCI_TYPECODE_DOUBLE          : constant := 22;
   OCI_TYPECODE_FLOAT           : constant := SQLT_FLT;
   OCI_TYPECODE_NUMBER          : constant := SQLT_NUM;
   OCI_TYPECODE_DECIMAL         : constant := SQLT_PDN;
   OCI_TYPECODE_UNSIGNED8       : constant := SQLT_BIN;
   OCI_TYPECODE_UNSIGNED16      : constant := 25;
   OCI_TYPECODE_UNSIGNED32      : constant := 26;
   OCI_TYPECODE_OCTET           : constant := 245;
   OCI_TYPECODE_SMALLINT        : constant := 246;
   OCI_TYPECODE_INTEGER         : constant := SQLT_INT;
   OCI_TYPECODE_RAW             : constant := SQLT_LVB;
   OCI_TYPECODE_PTR             : constant := 32;
   OCI_TYPECODE_VARCHAR2        : constant := SQLT_VCS;
   OCI_TYPECODE_CHAR            : constant := SQLT_AFC;
   OCI_TYPECODE_VARCHAR         : constant := SQLT_CHR;
   OCI_TYPECODE_MLSLABEL        : constant := SQLT_LAB;
   OCI_TYPECODE_VARRAY          : constant := 247;
   OCI_TYPECODE_TABLE           : constant := 248;
   OCI_TYPECODE_OBJECT          : constant := SQLT_NTY;
   OCI_TYPECODE_OPAQUE          : constant := 58;
   OCI_TYPECODE_NAMEDCOLLECTION : constant := SQLT_NCO;
   OCI_TYPECODE_BLOB            : constant := SQLT_BLOB;
   OCI_TYPECODE_BFILE           : constant := SQLT_BFILE;
   OCI_TYPECODE_CLOB            : constant := SQLT_CLOB;
   OCI_TYPECODE_CFILE           : constant := SQLT_CFILE;

   --  the following are ANSI datetime datatypes added in 8.1
   OCI_TYPECODE_TIME          : constant := SQLT_TIME;
   OCI_TYPECODE_TIME_TZ       : constant := SQLT_TIME_TZ;
   OCI_TYPECODE_TIMESTAMP     : constant := SQLT_TIMESTAMP;
   OCI_TYPECODE_TIMESTAMP_TZ  : constant := SQLT_TIMESTAMP_TZ;
   OCI_TYPECODE_TIMESTAMP_LTZ : constant := SQLT_TIMESTAMP_LTZ;

   OCI_TYPECODE_INTERVAL_YM : constant := SQLT_INTERVAL_YM;
   OCI_TYPECODE_INTERVAL_DS : constant := SQLT_INTERVAL_DS;
   OCI_TYPECODE_UROWID      : constant := SQLT_RDD;

   OCI_TYPECODE_OTMFIRST : constant := 228;
   OCI_TYPECODE_OTMLAST  : constant := 320;
   OCI_TYPECODE_SYSFIRST : constant := 228;
   OCI_TYPECODE_SYSLAST  : constant := 235;

   OCI_TYPECODE_PLS_INTEGER : constant := 266;

   --  NOTE : The following NCHAR related codes are just short forms for saying
   --  OCI_TYPECODE_VARCHAR2 with a charset form of SQLCS_NCHAR. These codes
   --  are intended for use in the OCIAnyData API only and nowhere else.
   OCI_TYPECODE_NCHAR     : constant := 286;
   OCI_TYPECODE_NVARCHAR2 : constant := 287;
   OCI_TYPECODE_NCLOB     : constant := 288;

   --  To indicate absence of typecode being specified
   OCI_TYPECODE_NONE : constant := 0;

   --  To indicate error has to be taken from error handle - reserved for
   --  sqlplus use
   OCI_TYPECODE_ERRHP : constant := 283;

   --  OCINumberToInt/OCINumberFromInt flags
   OCI_NUMBER_UNSIGNED : constant := 0; -- Unsigned type - ubX
   OCI_NUMBER_SIGNED   : constant := 2; -- Signed type - sbX

   Null_Indicator     : constant := -1;
   Not_Null_Indicator : constant := 0;

   type OCIDuration is new Ub2;

   OCI_DURATION_BEGIN   : constant OCIDuration := 10;
   OCI_DURATION_NULL    : constant OCIDuration := OCI_DURATION_BEGIN - 1;
   OCI_DURATION_DEFAULT : constant OCIDuration := OCI_DURATION_BEGIN - 2;
   OCI_DURATION_NEXT    : constant OCIDuration := OCI_DURATION_BEGIN - 3;
   OCI_DURATION_SESSION : constant OCIDuration := OCI_DURATION_BEGIN;
   OCI_DURATION_TRANS   : constant OCIDuration := OCI_DURATION_BEGIN + 1;

   --  Oracle DATE type

   subtype OCI_Hour_Number is Ub1 range 0 .. 23;
   subtype OCI_Minute_Number is Ub1 range 0 .. 59;
   subtype OCI_Second_Number is Ub1 range 0 .. 59;

   type OCITime is record
      OCITimeHH : OCI_Hour_Number;
      OCITimeMI : OCI_Minute_Number;
      OCITimeSS : OCI_Second_Number;
   end record;
   pragma Convention (C, OCITime);

   subtype OCI_Year_Number is Sb2 range -4712 .. 9999;
   subtype OCI_Month_Number is Ub1 range 1 .. 12;
   subtype OCI_Day_Number is Ub1 range 1 .. 31;

   type OCIDate is record
      OCIDateYYYY : OCI_Year_Number;
      OCIDateMM   : OCI_Month_Number;
      OCIDateDD   : OCI_Day_Number;
      OCIDateTime : OCITime;
   end record;
   pragma Convention (C, OCIDate);

   --  Oracle NUMBER type

   OCI_NUMBER_SIZE : constant := 22;

   type OCINumber is array (1 .. OCI_NUMBER_SIZE) of Ub1;
   pragma Convention (C, OCINumber);

   type OCIString is new DVoid;

   --  OCI LOB locator
   type A_OCILobLocator is access all OCILobLocator;
   subtype OCICLOBLOCATOR is OCILobLocator;
   subtype OCIBLOBLOCATOR is OCILobLocator;
   subtype OCIBFILELOCATOR is OCILobLocator;

   --------------------------------------------------
   -- Connect, Authorize, and Initialize Functions --
   --------------------------------------------------

   function OCIEnvCreate
     (Envh      : access OCIEnv;
      Mode      : Ub4;
      Ctxp      : DVoid := Null_OCIHandle;
      Malocfp   : DVoid := Null_OCIHandle;
      Ralocfp   : DVoid := Null_OCIHandle;
      Mfreefp   : DVoid := Null_OCIHandle;
      Xtramemsz : Interfaces.C.size_t := 0;
      Usrmempp  : DVoid := Null_OCIHandle) return SWord;
   pragma Import (C, OCIEnvCreate, "OCIEnvCreate");

   function OCIEnvNlsCreate
     (Envh      : access OCIEnv;
      Mode      : Ub4;
      Ctxp      : DVoid := Null_OCIHandle;
      Malocfp   : DVoid := Null_OCIHandle;
      Ralocfp   : DVoid := Null_OCIHandle;
      Mfreefp   : DVoid := Null_OCIHandle;
      Xtramemsz : Interfaces.C.size_t := 0;
      Usrmempp  : DVoid := Null_OCIHandle;
      Charset   : Ub2 := OCI_UTF8ID;
      Ncharset  : Ub2 := OCI_UTF8ID) return SWord;
   pragma Import (C, OCIEnvNlsCreate, "OCIEnvNlsCreate");

   function OCILogoff (Svchp : OCISvcCtx; Errhp : OCIError) return SWord;
   pragma Import (C, OCILogoff, "OCILogoff");

   function OCILogon
     (Envhp      : OCIEnv;
      Errhp      : OCIError;
      Svchp      : access OCISvcCtx;
      Username   : Text;
      Uname_Len  : Ub4;
      Password   : Text;
      Passwd_Len : Ub4;
      Dbname     : Text;
      Dbname_Len : Ub4) return SWord;
   pragma Import (C, OCILogon, "OCILogon");

   function OCILogon2
     (Envhp      : OCIEnv;
      Errhp      : OCIError;
      Svchp      : access OCISvcCtx;
      Username   : Text;
      Uname_Len  : Ub4;
      Password   : Text;
      Passwd_Len : Ub4;
      Dbname     : Text;
      Dbname_Len : Ub4;
      Mode       : Ub4 := OCI_DEFAULT) return SWord;
   pragma Import (C, OCILogon2, "OCILogon2");

   function OCIServerAttach
     (Srvhp      : OCIServer;
      Errhp      : OCIError;
      Dblink     : Text;
      Dblink_len : Sb4;
      Mode       : Ub4 := OCI_DEFAULT) return SWord;
   pragma Import (C, OCIServerAttach, "OCIServerAttach");

   function OCIServerDetach
     (Srvhp : OCIServer;
      Errhp : OCIError;
      Mode  : Ub4 := OCI_DEFAULT) return SWord;
   pragma Import (C, OCIServerDetach, "OCIServerDetach");

   function OCISessionBegin
     (Svchp : OCISvcCtx;
      Errhp : OCIError;
      Usrhp : OCISession;
      Credt : Ub4 := OCI_CRED_RDBMS;
      Mode  : Ub4 := OCI_DEFAULT) return SWord;
   pragma Import (C, OCISessionBegin, "OCISessionBegin");

   function OCISessionEnd
     (Svchp : OCISvcCtx;
      Errhp : OCIError;
      Usrhp : OCISession;
      Mode  : Ub4 := OCI_DEFAULT) return SWord;
   pragma Import (C, OCISessionEnd, "OCISessionEnd");

   function OCITerminate (Mode : Ub4 := OCI_DEFAULT) return SWord;
   pragma Import (C, OCITerminate, "OCITerminate");

   -------------------------------------
   -- Handle and Descriptor Functions --
   -------------------------------------

   function OCIAttrGet
     (Trgthndlp  : OCIHandle;
      Trghndltyp : Ub4;
      Attributep : DVoid;
      Sizep      : A_Ub4;
      Attrtype   : Ub4;
      Errhp      : OCIError) return SWord;
   function OCIAttrGet
     (Trgthndlp  : OCIHandle;
      Trghndltyp : Ub4;
      Attributep : access Interfaces.C.Strings.chars_ptr;
      Sizep      : access Ub4;
      Attrtype   : Ub4;
      Errhp      : OCIError) return SWord;
   pragma Import (C, OCIAttrGet, "OCIAttrGet");

   function OCIAttrSet
     (Trgthndlp  : OCIHandle;
      Trghndltyp : Ub4;
      Attributep : Text;
      Size       : Ub4;
      Attrtype   : Ub4;
      Errhp      : OCIError) return SWord;
   function OCIAttrSet
     (Trgthndlp  : OCIHandle;
      Trghndltyp : Ub4;
      Attributep : DVoid;
      Size       : Ub4;
      Attrtype   : Ub4;
      Errhp      : OCIError) return SWord;
   pragma Import (C, OCIAttrSet, "OCIAttrSet");

   function OCIDescriptorAlloc
     (Parenth    : OCIEnv;
      Descpp     : access DVoid;
      Htype      : Ub4;
      Xtramem_Sz : Interfaces.C.size_t := 0;
      Usrmempp   : Undefined := 0) return SWord;
   pragma Import (C, OCIDescriptorAlloc, "OCIDescriptorAlloc");

   function OCIDescriptorFree (Descp : OCIHandle; Dtype : Ub4) return SWord;
   pragma Import (C, OCIDescriptorFree, "OCIDescriptorFree");

   function OCIHandleAlloc
     (Parenth    : OCIHandle;
      Hndlpp     : access OCIHandle;
      Htype      : Ub4;
      Xtramem_Sz : Interfaces.C.size_t := 0;
      Usrmempp   : Undefined := Empty_Undefined) return SWord;
   pragma Import (C, OCIHandleAlloc, "OCIHandleAlloc");

   function OCIHandleFree (Hndlp : OCIHandle; Htype : Ub4) return SWord;
   pragma Import (C, OCIHandleFree, "OCIHandleFree");

   function OCIParamGet
     (Hndlp   : OCIHandle;
      Htype   : Ub4;
      Errhp   : OCIError;
      Parmdpp : access OCIParam;
      Pos     : Ub4) return SWord;
   pragma Import (C, OCIParamGet, "OCIParamGet");

   --  pragma Import (C, OCIParamSet, "OCIParamSet");

   ------------------------------------------
   -- Bind, Define, and Describe Functions --
   ------------------------------------------

   function OCIBindByName
     (Stmtp       : OCIStmt;
      Bindpp      : access OCIBind;
      Errhp       : OCIError;
      Placeholder : Text;
      Placeh_Len  : Sb4;
      Valuep      : DVoid;
      Value_Sz    : Sb4;
      Dty         : Ub2;
      Indp        : A_Sb2;
      Alenp       : A_Ub2 := null;
      Rcodep      : A_Ub2 := null;
      Maxarr_Len  : Ub4 := 0;
      Curelep     : A_Ub4 := null;
      Mode        : Ub4 := OCI_DEFAULT) return SWord;
   pragma Import (C, OCIBindByName, "OCIBindByName");

   function OCIBindByName2
     (Stmtp       : OCIStmt;
      Bindpp      : access OCIBind;
      Errhp       : OCIError;
      Placeholder : Text;
      Placeh_Len  : Sb4;
      Valuep      : DVoid;
      Value_Sz    : Sb8;
      Dty         : Ub2;
      Indp        : access Sb2;
      Alenp       : A_Ub4 := null;
      Rcodep      : A_Ub2 := null;
      Maxarr_Len  : Ub4 := 0;
      Curelep     : A_Ub4 := null;
      Mode        : Ub4 := OCI_DEFAULT) return SWord;
   pragma Import (C, OCIBindByName2, "OCIBindByName2");

   function OCIBindByPos
     (Stmtp      : OCIStmt;
      Bindpp     : access OCIBind;
      Errhp      : OCIError;
      Position   : Ub4;
      Valuep     : DVoid;
      Value_Sz   : Sb4;
      Dty        : Ub2;
      Indp       : A_Sb2;
      Alenp      : A_Ub2 := null;
      Rcodep     : A_Ub2 := null;
      Maxarr_Len : Ub4 := 0;
      Curelep    : A_Ub4 := null;
      Mode       : Ub4 := OCI_DEFAULT) return SWord;
   pragma Import (C, OCIBindByPos, "OCIBindByPos");

   function OCIBindByPos2
     (Stmtp      : OCIStmt;
      Bindpp     : access OCIBind;
      Errhp      : OCIError;
      Position   : Ub4;
      Valuep     : DVoid;
      Value_Sz   : Sb8;
      Dty        : Ub2;
      Indp       : access Sb2;
      Alenp      : A_Ub4 := null;
      Rcodep     : A_Ub2 := null;
      Maxarr_Len : Ub4 := 0;
      Curelep    : A_Ub4 := null;
      Mode       : Ub4 := OCI_DEFAULT) return SWord;
   pragma Import (C, OCIBindByPos2, "OCIBindByPos2");

--     pragma Import (C, OCIBindDynamic, "OCIBindDynamic");
--     pragma Import (C, OCIBindObject, "OCIBindObject");

   function OCIDefineByPos
     (Stmtp    : OCIStmt;
      Defnpp   : access OCIDefine;
      Errhp    : OCIError;
      Position : Ub4;
      Value    : DVoid;
      Value_Sz : Sb4;
      Dty      : Ub2;
      Indp     : A_Sb2;
      Rlenp    : A_Ub2 := null;
      Rcodep   : A_Ub2 := null;
      Mode     : Ub4 := OCI_DEFAULT) return SWord;
   pragma Import (C, OCIDefineByPos, "OCIDefineByPos");

   function OCIDefineByPos2
     (Stmtp    : OCIStmt;
      Defnpp   : access OCIDefine;
      Errhp    : OCIError;
      Position : Ub4;
      Value    : Text_Ptr;
      Value_Sz : Sb8;
      Dty      : Ub2;
      Indp     : access Sb2;
      Rlenp    : A_Ub4 := null;
      Rcodep   : A_Ub2 := null;
      Mode     : Ub4 := OCI_DEFAULT) return SWord;
   pragma Import (C, OCIDefineByPos2, "OCIDefineByPos2");

--     pragma Import (C, OCIDefineDynamic, "OCIDefineDynamic");
--     pragma Import (C, OCIDefineObject, "OCIDefineObject");
--     pragma Import (C, OCIDescribeAny, "OCIDescribeAny");
--     pragma Import (C, OCIStmtGetBindInfo, "OCIStmtGetBindInfo");

   -------------------------
   -- Statement Functions --
   -------------------------

   function OCIStmtExecute
     (Svchp    : OCISvcCtx;
      Stmtp    : OCIStmt;
      Errhp    : OCIError;
      Iters    : Ub4;
      Rowoff   : Ub4       := 0;
      Snap_In  : OCIHandle := Null_OCIHandle;
      Snap_Out : OCIHandle := Null_OCIHandle;
      Mode     : Ub4 := OCI_DEFAULT) return SWord;
   pragma Import (C, OCIStmtExecute, "OCIStmtExecute");

   function OCIStmtFetch2
     (Stmtp       : OCIStmt;
      Errhp       : OCIError;
      Nrows       : Ub4 := 1;
      Orientation : Ub2 := OCI_DEFAULT;
      FetchOffset : Sb4 := 0;
      Mode        : Ub4 := OCI_DEFAULT) return SWord;
   pragma Import (C, OCIStmtFetch2, "OCIStmtFetch2");

--     pragma Import (C, OCIStmtGetNextResult, "OCIStmtGetNextResult");
--     pragma Import (C, OCIStmtGetPieceInfo, "OCIStmtGetPieceInfo");

   function OCIStmtPrepare
     (Stmtp    : OCIStmt;
      Errhp    : OCIError;
      Stmt     : Text;
      Stmt_Len : Ub4;
      Language : Ub4 := OCI_NTV_SYNTAX;
      Mode     : Ub4 := OCI_DEFAULT) return SWord;
   pragma Import (C, OCIStmtPrepare, "OCIStmtPrepare");

   function OCIStmtPrepare2
     (Svchp    : OCISvcCtx;
      Stmtp    : access OCIStmt;
      Errhp    : OCIError;
      Stmt     : Text;
      Stmt_Len : Ub4;
      Key      : Text_Ptr := Interfaces.C.Strings.Null_Ptr;
      KeyLen   : Ub4 := 0;
      Language : Ub4 := OCI_NTV_SYNTAX;
      Mode     : Ub4 := OCI_DEFAULT) return SWord;
   pragma Import (C, OCIStmtPrepare2, "OCIStmtPrepare2");

   function OCIStmtRelease
     (Stmtp    : OCIStmt;
      Errhp    : OCIError;
      Key      : Text_Ptr := Interfaces.C.Strings.Null_Ptr;
      KeyLen   : Ub4 := 0;
      Mode     : Ub4 := OCI_DEFAULT) return SWord;
   pragma Import (C, OCIStmtRelease, "OCIStmtRelease");

--     pragma Import (C, OCIStmtSetPieceInfo, "OCIStmtSetPieceInfo");

   -------------------
   -- LOB Functions --
   -------------------

--     pragma Import (C, OCILobAppend, "OCILobAppend");
--     pragma Import (C, OCILobAssign, "OCILobAssign");
--     pragma Import (C, OCILobCharSetForm, "OCILobCharSetForm");
--     pragma Import (C, OCILobCharSetId, "OCILobCharSetId");

   function OCILobClose
     (Svchp : OCISvcCtx;
      Errhp : OCIError;
      Locp  : OCILobLocator) return SWord;
   pragma Import (C, OCILobClose, "OCILobClose");

--     pragma Import (C, OCILobCopy2, "OCILobCopy2");
--     pragma Import (C, OCILobCreateTemporary, "OCILobCreateTemporary");
--     pragma Import (C, OCILobDisableBuffering, "OCILobDisableBuffering");
--     pragma Import (C, OCILobEnableBuffering, "OCILobEnableBuffering");
--     pragma Import (C, OCILobErase2, "OCILobErase2");
--     pragma Import (C, OCILobFileCloseAll, "OCILobFileCloseAll");
--     pragma Import (C, OCILobFileClose, "OCILobFileClose");
--     pragma Import (C, OCILobFileExists, "OCILobFileExists");
--     pragma Import (C, OCILobFileGetName, "OCILobFileGetName");
--     pragma Import (C, OCILobFileIsOpen, "OCILobFileIsOpen");
--     pragma Import (C, OCILobFileOpen, "OCILobFileOpen");
--     pragma Import (C, OCILobFileSetName, "OCILobFileSetName");
--     pragma Import (C, OCILobFlushBuffer, "OCILobFlushBuffer");
--     pragma Import (C, OCILobFreeTemporary, "OCILobFreeTemporary");
--     pragma Import (C, OCILobGetChunkSize, "OCILobGetChunkSize");

   function OCILobGetLength2
     (Svchp : OCISvcCtx;
      Errhp : OCIError;
      Locp  : OCILobLocator;
      Lenp  : A_Ub8) return SWord;
   pragma Import (C, OCILobGetLength2, "OCILobGetLength2");

--     pragma Import (C, OCILobGetStorageLimit, "OCILobGetStorageLimit");
--     pragma Import (C, OCILobIsEqual, "OCILobIsEqual");
--     pragma Import (C, OCILobIsOpen, "OCILobIsOpen");
--     pragma Import (C, OCILobIsTemporary, "OCILobIsTemporary");
--     pragma Import (C, OCILobLoadFromFile2, "OCILobLoadFromFile2");
--     pragma Import (C, OCILobLocatorAssign, "OCILobLocatorAssign");
--     pragma Import (C, OCILobLocatorIsInit, "OCILobLocatorIsInit");

   function OCILobOpen
     (Svchp : OCISvcCtx;
      Errhp : OCIError;
      Locp  : OCILobLocator;
      Mode  : Ub1) return SWord;
   pragma Import (C, OCILobOpen, "OCILobOpen");

   function OCILobRead2
     (Svchp     : OCISvcCtx;
      Errhp     : OCIError;
      Locp      : OCILobLocator;
      Byte_Amtp : A_Ub8;
      Char_Amtp : A_Ub8;
      Offset    : Ub8;
      Bufp      : System.Address;
      Bufl      : Ub8;
      Piece     : Ub1;
      Ctxp      : System.Address;
      Cbfp      : Function_Pointer;
      Csid      : Ub2;
      Csfrm     : Ub1 := SQLCS_IMPLICIT) return SWord;
   pragma Import (C, OCILobRead2, "OCILobRead2");

   function OCILobTrim2
     (Svchp  : OCISvcCtx;
      Errhp  : OCIError;
      Locp   : OCILobLocator;
      Newlen : Ub8) return SWord;
   pragma Import (C, OCILobTrim2, "OCILobTrim2");

   function OCILobWrite2
     (Svchp     : OCISvcCtx;
      Errhp     : OCIError;
      Locp      : OCILobLocator;
      Byte_Amtp : A_Ub8;
      Char_Amtp : A_Ub8;
      Offset    : Ub8;
      Bufp      : System.Address;
      Buflen    : Ub8;
      Piece     : Ub1;
      Ctxp      : System.Address;
      Cbfp      : Function_Pointer;
      Csid      : Ub2;
      Csfrm     : Ub1 := SQLCS_IMPLICIT) return SWord;
   pragma Import (C, OCILobWrite2, "OCILobWrite2");

   function OCILobWriteAppend2
     (Svchp     : OCISvcCtx;
      Errhp     : OCIError;
      Lobp      : OCILobLocator;
      Byte_Amtp : A_Ub8;
      Char_Amtp : A_Ub8;
      Bufp      : System.Address;
      Bufl      : Ub8;
      Piece     : Ub1;
      Ctxp      : System.Address;
      Cbfp      : Function_Pointer;
      Csid      : Ub2;
      Csfrm     : Ub1 := SQLCS_IMPLICIT) return SWord;
   pragma Import (C, OCILobWriteAppend2, "OCILobWriteAppend2");

   ------------------------------------------------------
   -- Advanced Queuing and Publish-Subscribe Functions --
   ------------------------------------------------------

--     function OCISubscriptionDisable
--       (Subscrhp : OCISubscription;
--        Errhp    : OCIError;
--        Mode     : Ub4 := OCI_DEFAULT) return SWord;
--     pragma Import (C, OCISubscriptionDisable, "OCISubscriptionDisable");
--
--     function OCISubscriptionEnable
--       (Subscrhp : OCISubscription;
--        Errhp    : OCIError;
--        Mode     : Ub4 := OCI_DEFAULT) return SWord;
--     pragma Import (C, OCISubscriptionEnable, "OCISubscriptionEnable");
--
--     function OCISubscriptionPost
--       (Svchp     : OCISvcCtx;
--        Subscrhpp : access OCISubscription;
--        Count     : Ub2;
--        Errhp     : OCIError;
--        Mode      : Ub4 := OCI_DEFAULT) return SWord;
--     pragma Import (C, OCISubscriptionPost, "OCISubscriptionPost");
--
--     function OCISubscriptionRegister
--       (Svchp     : OCISvcCtx;
--        Subscrhpp : access OCISubscription;
--        Count     : Ub2;
--        Errhp     : OCIError;
--        Mode      : Ub4) return SWord;
--     pragma Import (C, OCISubscriptionRegister, "OCISubscriptionRegister");
--
--     function OCISubscriptionUnRegister
--       (Svchp    : OCISvcCtx;
--        Subscrhp : OCISubscription;
--        Errhp    : OCIError;
--        Mode     : Ub4) return  SWord;
--   pragma Import (C, OCISubscriptionUnRegister, "OCISubscriptionUnRegister");

   ---------------------------
   -- Transaction Functions --
   ---------------------------

   function OCITransCommit
     (Svchp : OCISvcCtx;
      Errhp : OCIError;
      Flags : Ub4 := OCI_DEFAULT) return SWord;
   pragma Import (C, OCITransCommit, "OCITransCommit");

   function OCITransRollback
     (Svchp : OCISvcCtx;
      Errhp : OCIError;
      Flags : Ub4 := OCI_DEFAULT) return SWord;
   pragma Import (C, OCITransRollback, "OCITransRollback");

   -----------------------------
   -- Miscellaneous Functions --
   -----------------------------

   function OCIBreak (Hndlp : OCIHandle; Errhp : OCIError) return SWord;
   pragma Import (C, OCIBreak, "OCIBreak");

   procedure OCIClientVersion
     (Major_Version   : out SWord;
      Minor_Version   : out SWord;
      Update_Num      : out SWord;
      Patch_Num       : out SWord;
      Port_Update_Num : out SWord);
   pragma Import (C, OCIClientVersion, "OCIClientVersion");

   function OCIErrorGet
     (Hndlp    : OCIHandle;
      Recordno : Ub4 := 1;
      Sqlstate : Undefined := Empty_Undefined;
      Errcodep : access Sb4;
      Bufp     : Text_Ptr;
      Bufsiz   : Ub4;
      Htype    : Ub4) return SWord;
   function OCIErrorGet
     (Hndlp    : OCIError;
      Recordno : Ub4 := 1;
      Sqlstate : Undefined := Empty_Undefined;
      Errcodep : access Sb4;
      Bufp     : Text_Ptr;
      Bufsiz   : Ub4;
      Htype    : Ub4) return SWord;
   pragma Import (C, OCIErrorGet, "OCIErrorGet");

   function OCIPing
     (Svchp : OCISvcCtx;
      Errhp : OCIError;
      Mode  : Ub4 := OCI_DEFAULT) return SWord;
   pragma Import (C, OCIPing, "OCIPing");

   function OCIReset (Hndlp : OCIHandle; Errhp : OCIError) return SWord;
   pragma Import (C, OCIReset, "OCIReset");

   function OCIServerRelease
     (Hndlp    : OCIHandle;
      Errhp    : OCIError;
      Bufp     : Text_Ptr;
      Bufsz    : Ub4;
      Hndltype : Ub1;
      Version  : access Ub4) return SWord;
   pragma Import (C, OCIServerRelease, "OCIServerRelease");

   function OCIServerVersion
     (Hndlp    : OCIHandle;
      Errhp    : OCIError;
      Bufp     : Text_Ptr;
      Bufsz    : Ub4;
      Hndltype : Ub1) return SWord;
   pragma Import (C, OCIServerVersion, "OCIServerVersion");

   ------------------------------------------------
   -- OCI Date, Datetime, and Interval Functions --
   ------------------------------------------------

--     pragma Import (C, OCIDateCompare, "OCIDateCompare");
--     pragma Import (C, OCIDateFromText, "OCIDateFromText");
--     pragma Import (C, OCIDateSysDate, "OCIDateSysDate");

   function OCIDateTimeConstruct
     (Hndl            : OCIEnv;
      Err             : OCIError;
      Datetime        : access OCIDateTime;
      Year            : Sb2;
      Month           : Ub1;
      Day             : Ub1;
      Hour            : Ub1;
      Min             : Ub1;
      Sec             : Ub1;
      Fsec            : Ub4;
      Timezone        : Text;
      Timezone_Length : Interfaces.C.size_t) return SWord;
   pragma Import (C, OCIDateTimeConstruct, "OCIDateTimeConstruct");

--     pragma Import (C, OCIDateTimeFromText, "OCIDateTimeFromText");

   function OCIDateTimeGetDate
     (Hndl     : OCIEnv;
      Err      : OCIError;
      Datetime : OCIDateTime;
      Year     : access Sb2;
      Month    : access Ub1;
      Day      : access Ub1) return SWord;
   pragma Import (C, OCIDateTimeGetDate, "OCIDateTimeGetDate");

   function OCIDateTimeGetTime
     (Hndl     : OCIEnv;
      Err      : OCIError;
      Datetime : OCIDateTime;
      Hour     : access Ub1;
      Min      : access Ub1;
      Sec      : access Ub1;
      Fsec     : access Ub4) return SWord;
   pragma Import (C, OCIDateTimeGetTime, "OCIDateTimeGetTime");

   function OCIDateTimeGetTimeZoneName
     (Hndl     : OCIEnv;
      Err      : OCIError;
      Datetime : OCIDateTime;
      Buf      : Text_Ptr;
      Buf_Size : access Ub4) return SWord;
   pragma Import (C, OCIDateTimeGetTimeZoneName, "OCIDateTimeGetTimeZoneName");

   function OCIDateTimeGetTimeZoneOffset
     (Hndl     : OCIEnv;
      Err      : OCIError;
      Datetime : OCIDateTime;
      Hour     : access Sb1;
      Min      : access Sb1) return SWord;
   pragma Import
     (C, OCIDateTimeGetTimeZoneOffset, "OCIDateTimeGetTimeZoneOffset");

   function OCIDateTimeToText
     (Hndl         : OCIEnv;
      Err          : OCIError;
      Date         : OCIDateTime;
      Fmt          : Text;
      Fmt_length   : Ub1;
      Fsprec       : Ub1;
      Lang_Name    : Text_Ptr;
      Lang_Lenngth : Interfaces.C.size_t;
      Buf_Size     : access Ub4;
      Buf          : Text_Ptr) return SWord;
   pragma Import (C, OCIDateTimeToText, "OCIDateTimeToText");

   function OCIDateToText
     (Err         : OCIError;
      Date        : OCIDate;
      Fmt         : Text;
      Fmt_Length  : Ub1;
      Lang_Name   : Text_Ptr;
      Lang_Length : Ub4;
      Buf_Size    : access Ub4;
      Buf         : Text_Ptr) return SWord;
   pragma Import (C, OCIDateToText, "OCIDateToText");

   --------------------------
   -- OCI NUMBER Functions --
   --------------------------

   function OCINumberFromInt
     (Err         : OCIError;
      Inum        : DVoid;
      Inum_Length : UWord;
      Inum_S_Flag : UWord;
      Number      : access OCINumber) return SWord;
   pragma Import (C, OCINumberFromInt, "OCINumberFromInt");

   function OCINumberFromReal
     (Err         : OCIError;
      Rnum        : DVoid;
      Rnum_Length : UWord;
      Number      : access OCINumber) return SWord;
   pragma Import (C, OCINumberFromReal, "OCINumberFromReal");

--     pragma Import (C, OCINumberFromText, "OCINumberFromText");
--     pragma Import (C, OCINumberIsInt, "OCINumberIsInt");
--     pragma Import (C, OCINumberIsZero, "OCINumberIsZero");
--     pragma Import (C, OCINumberToInt, "OCINumberToInt");
--     pragma Import (C, OCINumberToReal, "OCINumberToReal");

   function OCINumberToText
     (Err        : OCIError;
      Date       : OCINumber;
      Fmt        : Text;
      Fmt_Length : Ub1;
      Nls_Name   : Text;
      Nls_Length : Ub4;
      Buf_Size   : access Ub4;
      Buf        : Text_Ptr) return SWord;
   pragma Import (C, OCINumberToText, "OCINumberToText");

end OCI;
