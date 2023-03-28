!compiler_flags: --dump NAME --exec NAME



!name: Program 1 (TypeName not defined)
!code:
var x : my_int
!failure:
99
!end



!name: Program 2 (Double definition with same name)
!code:
var x : integer;
typ x : logical
!failure:
99
!end



!name: Program 3 (VarDef passed as TypeName)
!code:
var x : integer;
var y : x
!failure:
99
!end



!name: Program 4 (Calling variable as a function)
!code:
var x : integer;
fun f(y:integer) : integer = x(10)
!failure:
99
!end



!name: Program 5 (Performing binary operations on function)
!code:
fun f(x:integer) : integer = f[10]
!failure:
99
!end



!name: Program 6 (i out of scope of where)
!code:
typ x : integer;
var y : x;
fun f(x:x, y:x) : x = (
	i + 1 {where var i : integer},
	i
)
!failure:
99
!end



!name: Program 7 (VarDef passed as TypeName in function)
!code:
var x : integer;
fun f(x:x) : x = 1
!failure:
99
!end



!name: Program 8 (Declaration order)
!code:
var x : int;
typ int : integer
!expected:
Defs [1:1-2:18]
  VarDef [1:1-1:12]: x
    TypeName [1:9-1:12]: int
      # defined at: [2:1-2:18]
  TypeDef [2:1-2:18]: int
    Atom [2:11-2:18]: INT
!end



!name: Program 9 (Scope check)
!code:
typ x : integer;
var y : x;
fun f(x:x, y:x) : x = x + y + 1
!expected:
Defs [1:1-3:32]
  TypeDef [1:1-1:16]: x
    Atom [1:9-1:16]: INT
  VarDef [2:1-2:10]: y
    TypeName [2:9-2:10]: x
      # defined at: [1:1-1:16]
  FunDef [3:1-3:32]: f
    Parameter [3:7-3:10]: x
      TypeName [3:9-3:10]: x
        # defined at: [1:1-1:16]
    Parameter [3:12-3:15]: y
      TypeName [3:14-3:15]: x
        # defined at: [1:1-1:16]
    TypeName [3:19-3:20]: x
      # defined at: [1:1-1:16]
    Binary [3:23-3:32]: ADD
      Binary [3:23-3:28]: ADD
        Name [3:23-3:24]: x
          # defined at: [3:7-3:10]
        Name [3:27-3:28]: y
          # defined at: [3:12-3:15]
      Literal [3:31-3:32]: INT(1)
!end



!name: Program 10 (where declaration order)
!code:
fun f(x : integer) : integer = (
	x + y {where var x : int; var y : int; typ int : integer }
)
!expected:
Defs [1:1-3:2]
  FunDef [1:1-3:2]: f
    Parameter [1:7-1:18]: x
      Atom [1:11-1:18]: INT
    Atom [1:22-1:29]: INT
    Block [1:32-3:2]
      Where [2:5-2:63]
        Defs [2:18-2:61]
          VarDef [2:18-2:29]: x
            TypeName [2:26-2:29]: int
              # defined at: [2:44-2:61]
          VarDef [2:31-2:42]: y
            TypeName [2:39-2:42]: int
              # defined at: [2:44-2:61]
          TypeDef [2:44-2:61]: int
            Atom [2:54-2:61]: INT
        Binary [2:5-2:10]: ADD
          Name [2:5-2:6]: x
            # defined at: [2:18-2:29]
          Name [2:9-2:10]: y
            # defined at: [2:31-2:42]
!end



!name: Program 11 (where dobule-pass check)
!code:
typ int : integer;
fun f(x : integer) : integer = (
	x + y {where var x : int; var y : int; typ int : integer }
)
!expected:
Defs [1:1-4:2]
  TypeDef [1:1-1:18]: int
    Atom [1:11-1:18]: INT
  FunDef [2:1-4:2]: f
    Parameter [2:7-2:18]: x
      Atom [2:11-2:18]: INT
    Atom [2:22-2:29]: INT
    Block [2:32-4:2]
      Where [3:5-3:63]
        Defs [3:18-3:61]
          VarDef [3:18-3:29]: x
            TypeName [3:26-3:29]: int
              # defined at: [3:44-3:61]
          VarDef [3:31-3:42]: y
            TypeName [3:39-3:42]: int
              # defined at: [3:44-3:61]
          TypeDef [3:44-3:61]: int
            Atom [3:54-3:61]: INT
        Binary [3:5-3:10]: ADD
          Name [3:5-3:6]: x
            # defined at: [3:18-3:29]
          Name [3:9-3:10]: y
            # defined at: [3:31-3:42]
!end