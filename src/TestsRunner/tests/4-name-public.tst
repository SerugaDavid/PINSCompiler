!compiler_flags: --dump NAME --exec NAME

!code:
var x : integer;
fun f ( y : integer ) : logical = x + y
!expected:
Defs [1:1-2:40]
  VarDef [1:1-1:16]: x
    Atom [1:9-1:16]: INT
  FunDef [2:1-2:40]: f
    Parameter [2:9-2:20]: y
      Atom [2:13-2:20]: INT
    Atom [2:25-2:32]: LOG
    Binary [2:35-2:40]: ADD
      Name [2:35-2:36]: x
        # defined at: [1:1-1:16]
      Name [2:39-2:40]: y
        # defined at: [2:9-2:20]
!end

!code:
typ t1 : t2;
typ t2 : t3;
typ t3 : logical
!expected:
Defs [1:1-3:17]
  TypeDef [1:1-1:12]: t1
    TypeName [1:10-1:12]: t2
      # defined at: [2:1-2:12]
  TypeDef [2:1-2:12]: t2
    TypeName [2:10-2:12]: t3
      # defined at: [3:1-3:17]
  TypeDef [3:1-3:17]: t3
    Atom [3:10-3:17]: LOG
!end