let _14 =
      < A
      | B
      | C
      | D
      | E
      | F
      | G
      | H
      | I
      | J
      | K
      | L
      | M
      | N
      | O
      | P
      | Q
      | R
      | S
      | T
      | U
      | V
      | W
      | X
      | Y
      | Z
      >

let _13 = _14.U

let _12 = _14.S

let _11 = _14.M

let _10 = _14.E

let _9 = _14.C

let _8 = _14.A

let _7 = { head : _14, tail : List _14 }

let _6 = < Word : _7 | Number : Natural >

let _5 = _6.Word

let _4 = { head : _7, tail : List _6 }

let _3 =
      { arraySettings :
          Optional { dimensionality : Natural, elementIsNullable : Bool }
      , scalar :
          < Primitive :
              < Bit
              | Bool
              | Box
              | Bpchar
              | Bytea
              | Char
              | Cidr
              | Circle
              | Citext
              | Date
              | Datemultirange
              | Daterange
              | Float4
              | Float8
              | Hstore
              | Inet
              | Int2
              | Int4
              | Int4multirange
              | Int4range
              | Int8
              | Int8multirange
              | Int8range
              | Interval
              | Json
              | Jsonb
              | Line
              | Lseg
              | Macaddr
              | Macaddr8
              | Money
              | Name
              | Numeric
              | Nummultirange
              | Numrange
              | Oid
              | Path
              | PgLsn
              | PgSnapshot
              | Point
              | Polygon
              | Text
              | Time
              | Timestamp
              | Timestamptz
              | Timetz
              | Tsmultirange
              | Tsquery
              | Tsrange
              | Tstzmultirange
              | Tstzrange
              | Tsvector
              | Uuid
              | Varbit
              | Varchar
              | Xml
              | Box2D
              | Box3D
              | Ltree
              | Geometry
              | Geography
              >
          | Custom : _4
          >
      }

let _2 = { name : _4, pgName : Text, isNullable : Bool, value : _3 }

let _1 = List _2

in  { space =
      { head = { head = _11, tail = [ _14.Y ] }
      , tail = [ _5 { head = _12, tail = [ _14.P, _8, _9, _10 ] } ]
      }
    , name =
      { head = { head = _11, tail = [ _13, _12, _14.I, _9 ] }
      , tail =
        [ _5
            { head = _9
            , tail = [ _8, _14.T, _8, _14.L, _14.O, _14.G, _13, _10 ]
            }
        ]
      }
    , version = { major = 1, minor = 0, patch = 1 }
    , customTypes =
        [] : List
               { name : _4
               , pgSchema : Text
               , pgName : Text
               , definition :
                   < Composite : _1
                   | Enum : List { name : _4, pgName : Text }
                   | Domain : _3
                   >
               }
    , queries =
        [] : List
               { name : _4
               , srcPath : Text
               , idempotent : Bool
               , params : _1
               , result :
                   Optional
                     { cardinality : < Optional | Single | Multiple >
                     , columns : { head : _2, tail : _1 }
                     }
               , fragments :
                   List
                     < Sql : Text
                     | Var : { name : _4, rawName : Text, paramIndex : Natural }
                     >
               }
    , migrations = [] : List { name : Text, sql : Text }
    }
