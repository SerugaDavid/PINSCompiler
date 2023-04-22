!compiler_flags: --dump FRM --exec FRM

!name: Program 1
!code:
fun main(args:arr[10]int):int = (
    # program, ki vrne sestevek fibonaccijevih stevil na mestih v args tabeli
    { sum = 0 },
    { for i = 0, 10, 1 :
        { sum = sum + fib(args[i]) }
    } { where var i:int },
    sum
) { where
    var sum:int
};

fun fib(n:int):int = (
    # funkcija, ki vrne n-to fibonaccijevo stevilo
    { return = 0 },
    { if n == 1 | n == 2 then
        {return = 1}
    else (
        { return = fib(n-1) + fib(n-2) }
    )
    },
    return
) { where
    var return:int
};

typ int:integer;
typ str:string;
typ bool:logical

# konec
!expected:
Defs [1:1-28:17]
  FunDef [1:1-10:2]: main
    # typed as: (ARR(10,int)) -> int
    # framed as: FRAME [main]: level=1,locals_size=8,arguments_size=8,parameters_size=8,size=20
    Parameter [1:10-1:25]: args
      # typed as: ARR(10,int)
      # accessed as: Parameter: size[4],offset[4],sl[1]
      Array [1:15-1:25]
        # typed as: ARR(10,int)
        [10]
        TypeName [1:22-1:25]: int
          # defined at: [26:1-26:16]
          # typed as: int
    TypeName [1:27-1:30]: int
      # defined at: [26:1-26:16]
      # typed as: int
    Where [1:33-10:2]
      # typed as: int
      Defs [9:5-9:16]
        VarDef [9:5-9:16]: sum
          # typed as: int
          # accessed as: Local: size[4],offset[-4],sl[1]
          TypeName [9:13-9:16]: int
            # defined at: [26:1-26:16]
            # typed as: int
      Block [1:33-8:2]
        # typed as: int
        Binary [3:5-3:16]: ASSIGN
          # typed as: int
          Name [3:7-3:10]: sum
            # defined at: [9:5-9:16]
            # typed as: int
          Literal [3:13-3:14]: INT(0)
            # typed as: int
        Where [4:5-6:26]
          # typed as: void
          Defs [6:15-6:24]
            VarDef [6:15-6:24]: i
              # typed as: int
              # accessed as: Local: size[4],offset[-8],sl[1]
              TypeName [6:21-6:24]: int
                # defined at: [26:1-26:16]
                # typed as: int
          For [4:5-6:6]
            # typed as: void
            Name [4:11-4:12]: i
              # defined at: [6:15-6:24]
              # typed as: int
            Literal [4:15-4:16]: INT(0)
              # typed as: int
            Literal [4:18-4:20]: INT(10)
              # typed as: int
            Literal [4:22-4:23]: INT(1)
              # typed as: int
            Binary [5:9-5:37]: ASSIGN
              # typed as: int
              Name [5:11-5:14]: sum
                # defined at: [9:5-9:16]
                # typed as: int
              Binary [5:17-5:35]: ADD
                # typed as: int
                Name [5:17-5:20]: sum
                  # defined at: [9:5-9:16]
                  # typed as: int
                Call [5:23-5:35]: fib
                  # defined at: [12:1-24:2]
                  # typed as: int
                  Binary [5:27-5:34]: ARR
                    # typed as: int
                    Name [5:27-5:31]: args
                      # defined at: [1:10-1:25]
                      # typed as: ARR(10,int)
                    Name [5:32-5:33]: i
                      # defined at: [6:15-6:24]
                      # typed as: int
        Name [7:5-7:8]: sum
          # defined at: [9:5-9:16]
          # typed as: int
  FunDef [12:1-24:2]: fib
    # typed as: (int) -> int
    # framed as: FRAME [fib]: level=1,locals_size=4,arguments_size=8,parameters_size=8,size=16
    Parameter [12:9-12:14]: n
      # typed as: int
      # accessed as: Parameter: size[4],offset[4],sl[1]
      TypeName [12:11-12:14]: int
        # defined at: [26:1-26:16]
        # typed as: int
    TypeName [12:16-12:19]: int
      # defined at: [26:1-26:16]
      # typed as: int
    Where [12:22-24:2]
      # typed as: int
      Defs [23:5-23:19]
        VarDef [23:5-23:19]: return
          # typed as: int
          # accessed as: Local: size[4],offset[-4],sl[1]
          TypeName [23:16-23:19]: int
            # defined at: [26:1-26:16]
            # typed as: int
      Block [12:22-22:2]
        # typed as: int
        Binary [14:5-14:19]: ASSIGN
          # typed as: int
          Name [14:7-14:13]: return
            # defined at: [23:5-23:19]
            # typed as: int
          Literal [14:16-14:17]: INT(0)
            # typed as: int
        IfThenElse [15:5-20:6]
          # typed as: void
          Binary [15:10-15:25]: OR
            # typed as: log
            Binary [15:10-15:16]: EQ
              # typed as: log
              Name [15:10-15:11]: n
                # defined at: [12:9-12:14]
                # typed as: int
              Literal [15:15-15:16]: INT(1)
                # typed as: int
            Binary [15:19-15:25]: EQ
              # typed as: log
              Name [15:19-15:20]: n
                # defined at: [12:9-12:14]
                # typed as: int
              Literal [15:24-15:25]: INT(2)
                # typed as: int
          Binary [16:9-16:21]: ASSIGN
            # typed as: int
            Name [16:10-16:16]: return
              # defined at: [23:5-23:19]
              # typed as: int
            Literal [16:19-16:20]: INT(1)
              # typed as: int
          Block [17:10-19:6]
            # typed as: int
            Binary [18:9-18:41]: ASSIGN
              # typed as: int
              Name [18:11-18:17]: return
                # defined at: [23:5-23:19]
                # typed as: int
              Binary [18:20-18:39]: ADD
                # typed as: int
                Call [18:20-18:28]: fib
                  # defined at: [12:1-24:2]
                  # typed as: int
                  Binary [18:24-18:27]: SUB
                    # typed as: int
                    Name [18:24-18:25]: n
                      # defined at: [12:9-12:14]
                      # typed as: int
                    Literal [18:26-18:27]: INT(1)
                      # typed as: int
                Call [18:31-18:39]: fib
                  # defined at: [12:1-24:2]
                  # typed as: int
                  Binary [18:35-18:38]: SUB
                    # typed as: int
                    Name [18:35-18:36]: n
                      # defined at: [12:9-12:14]
                      # typed as: int
                    Literal [18:37-18:38]: INT(2)
                      # typed as: int
        Name [21:5-21:11]: return
          # defined at: [23:5-23:19]
          # typed as: int
  TypeDef [26:1-26:16]: int
    # typed as: int
    Atom [26:9-26:16]: INT
      # typed as: int
  TypeDef [27:1-27:15]: str
    # typed as: str
    Atom [27:9-27:15]: STR
      # typed as: str
  TypeDef [28:1-28:17]: bool
    # typed as: log
    Atom [28:10-28:17]: LOG
      # typed as: log
!end

!name: Program 2
!code:
fun main(args:arr[10]int):int = (
    # sesteje vsa stevila v argumentih
    {sum = 0},
    {for i = 0, 10, 1:
        {sum = sum + args[i]}
    },
    sum
) {where
    var sum:int;
    var i:int
};

typ int:integer;
typ str:string;
typ bool:logical
!expected:
Defs [1:1-15:17]
  FunDef [1:1-11:2]: main
    # typed as: (ARR(10,int)) -> int
    # framed as: FRAME [main]: level=1,locals_size=8,arguments_size=0,parameters_size=8,size=16
    Parameter [1:10-1:25]: args
      # typed as: ARR(10,int)
      # accessed as: Parameter: size[4],offset[4],sl[1]
      Array [1:15-1:25]
        # typed as: ARR(10,int)
        [10]
        TypeName [1:22-1:25]: int
          # defined at: [13:1-13:16]
          # typed as: int
    TypeName [1:27-1:30]: int
      # defined at: [13:1-13:16]
      # typed as: int
    Where [1:33-11:2]
      # typed as: int
      Defs [9:5-10:14]
        VarDef [9:5-9:16]: sum
          # typed as: int
          # accessed as: Local: size[4],offset[-4],sl[1]
          TypeName [9:13-9:16]: int
            # defined at: [13:1-13:16]
            # typed as: int
        VarDef [10:5-10:14]: i
          # typed as: int
          # accessed as: Local: size[4],offset[-8],sl[1]
          TypeName [10:11-10:14]: int
            # defined at: [13:1-13:16]
            # typed as: int
      Block [1:33-8:2]
        # typed as: int
        Binary [3:5-3:14]: ASSIGN
          # typed as: int
          Name [3:6-3:9]: sum
            # defined at: [9:5-9:16]
            # typed as: int
          Literal [3:12-3:13]: INT(0)
            # typed as: int
        For [4:5-6:6]
          # typed as: void
          Name [4:10-4:11]: i
            # defined at: [10:5-10:14]
            # typed as: int
          Literal [4:14-4:15]: INT(0)
            # typed as: int
          Literal [4:17-4:19]: INT(10)
            # typed as: int
          Literal [4:21-4:22]: INT(1)
            # typed as: int
          Binary [5:9-5:30]: ASSIGN
            # typed as: int
            Name [5:10-5:13]: sum
              # defined at: [9:5-9:16]
              # typed as: int
            Binary [5:16-5:29]: ADD
              # typed as: int
              Name [5:16-5:19]: sum
                # defined at: [9:5-9:16]
                # typed as: int
              Binary [5:22-5:29]: ARR
                # typed as: int
                Name [5:22-5:26]: args
                  # defined at: [1:10-1:25]
                  # typed as: ARR(10,int)
                Name [5:27-5:28]: i
                  # defined at: [10:5-10:14]
                  # typed as: int
        Name [7:5-7:8]: sum
          # defined at: [9:5-9:16]
          # typed as: int
  TypeDef [13:1-13:16]: int
    # typed as: int
    Atom [13:9-13:16]: INT
      # typed as: int
  TypeDef [14:1-14:15]: str
    # typed as: str
    Atom [14:9-14:15]: STR
      # typed as: str
  TypeDef [15:1-15:17]: bool
    # typed as: log
    Atom [15:10-15:17]: LOG
      # typed as: log
!end

!name: Program 3
!code:
fun main(args:arr[10]int):int = (
    # najdi najvecjega in najmanjsega
    # izpisi razliko
    # stevila so med -10000 in 10000

    {max = najvecji(args)},
    {min = najmanjsi(args)},
    {razlika = max - min},
    razlika
) {where
    var max:int;
    var min:int;
    var razlika:int
};

fun najvecji(array:arr[10]int):int = (
    {max = -10000},
    {for i = 0, 10, 1:
        {if array[i] > max then
            {max = array[i]}
        }
    },
    max
) {where
    var max:int;
    var i:int
};

fun najmanjsi(array:arr[10]int):int = (
    {min = 10000},
    {for i = 0, 10, 1:
        {if array[i] < min then
            {min = array[i]}
        }
    },
    min
) {where
    var min:int;
    var i:int
};

typ int:integer;
typ str:string;
typ bool:logical
!expected:
Defs [1:1-44:17]
  FunDef [1:1-14:2]: main
    # typed as: (ARR(10,int)) -> int
    # framed as: FRAME [main]: level=1,locals_size=12,arguments_size=8,parameters_size=8,size=24
    Parameter [1:10-1:25]: args
      # typed as: ARR(10,int)
      # accessed as: Parameter: size[4],offset[4],sl[1]
      Array [1:15-1:25]
        # typed as: ARR(10,int)
        [10]
        TypeName [1:22-1:25]: int
          # defined at: [42:1-42:16]
          # typed as: int
    TypeName [1:27-1:30]: int
      # defined at: [42:1-42:16]
      # typed as: int
    Where [1:33-14:2]
      # typed as: int
      Defs [11:5-13:20]
        VarDef [11:5-11:16]: max
          # typed as: int
          # accessed as: Local: size[4],offset[-4],sl[1]
          TypeName [11:13-11:16]: int
            # defined at: [42:1-42:16]
            # typed as: int
        VarDef [12:5-12:16]: min
          # typed as: int
          # accessed as: Local: size[4],offset[-8],sl[1]
          TypeName [12:13-12:16]: int
            # defined at: [42:1-42:16]
            # typed as: int
        VarDef [13:5-13:20]: razlika
          # typed as: int
          # accessed as: Local: size[4],offset[-12],sl[1]
          TypeName [13:17-13:20]: int
            # defined at: [42:1-42:16]
            # typed as: int
      Block [1:33-10:2]
        # typed as: int
        Binary [6:5-6:27]: ASSIGN
          # typed as: int
          Name [6:6-6:9]: max
            # defined at: [11:5-11:16]
            # typed as: int
          Call [6:12-6:26]: najvecji
            # defined at: [16:1-27:2]
            # typed as: int
            Name [6:21-6:25]: args
              # defined at: [1:10-1:25]
              # typed as: ARR(10,int)
        Binary [7:5-7:28]: ASSIGN
          # typed as: int
          Name [7:6-7:9]: min
            # defined at: [12:5-12:16]
            # typed as: int
          Call [7:12-7:27]: najmanjsi
            # defined at: [29:1-40:2]
            # typed as: int
            Name [7:22-7:26]: args
              # defined at: [1:10-1:25]
              # typed as: ARR(10,int)
        Binary [8:5-8:26]: ASSIGN
          # typed as: int
          Name [8:6-8:13]: razlika
            # defined at: [13:5-13:20]
            # typed as: int
          Binary [8:16-8:25]: SUB
            # typed as: int
            Name [8:16-8:19]: max
              # defined at: [11:5-11:16]
              # typed as: int
            Name [8:22-8:25]: min
              # defined at: [12:5-12:16]
              # typed as: int
        Name [9:5-9:12]: razlika
          # defined at: [13:5-13:20]
          # typed as: int
  FunDef [16:1-27:2]: najvecji
    # typed as: (ARR(10,int)) -> int
    # framed as: FRAME [najvecji]: level=1,locals_size=8,arguments_size=0,parameters_size=8,size=16
    Parameter [16:14-16:30]: array
      # typed as: ARR(10,int)
      # accessed as: Parameter: size[4],offset[4],sl[1]
      Array [16:20-16:30]
        # typed as: ARR(10,int)
        [10]
        TypeName [16:27-16:30]: int
          # defined at: [42:1-42:16]
          # typed as: int
    TypeName [16:32-16:35]: int
      # defined at: [42:1-42:16]
      # typed as: int
    Where [16:38-27:2]
      # typed as: int
      Defs [25:5-26:14]
        VarDef [25:5-25:16]: max
          # typed as: int
          # accessed as: Local: size[4],offset[-4],sl[1]
          TypeName [25:13-25:16]: int
            # defined at: [42:1-42:16]
            # typed as: int
        VarDef [26:5-26:14]: i
          # typed as: int
          # accessed as: Local: size[4],offset[-8],sl[1]
          TypeName [26:11-26:14]: int
            # defined at: [42:1-42:16]
            # typed as: int
      Block [16:38-24:2]
        # typed as: int
        Binary [17:5-17:19]: ASSIGN
          # typed as: int
          Name [17:6-17:9]: max
            # defined at: [25:5-25:16]
            # typed as: int
          Unary [17:12-17:18]: SUB
            # typed as: int
            Literal [17:13-17:18]: INT(10000)
              # typed as: int
        For [18:5-22:6]
          # typed as: void
          Name [18:10-18:11]: i
            # defined at: [26:5-26:14]
            # typed as: int
          Literal [18:14-18:15]: INT(0)
            # typed as: int
          Literal [18:17-18:19]: INT(10)
            # typed as: int
          Literal [18:21-18:22]: INT(1)
            # typed as: int
          IfThenElse [19:9-21:10]
            # typed as: void
            Binary [19:13-19:27]: GT
              # typed as: log
              Binary [19:13-19:21]: ARR
                # typed as: int
                Name [19:13-19:18]: array
                  # defined at: [16:14-16:30]
                  # typed as: ARR(10,int)
                Name [19:19-19:20]: i
                  # defined at: [26:5-26:14]
                  # typed as: int
              Name [19:24-19:27]: max
                # defined at: [25:5-25:16]
                # typed as: int
            Binary [20:13-20:29]: ASSIGN
              # typed as: int
              Name [20:14-20:17]: max
                # defined at: [25:5-25:16]
                # typed as: int
              Binary [20:20-20:28]: ARR
                # typed as: int
                Name [20:20-20:25]: array
                  # defined at: [16:14-16:30]
                  # typed as: ARR(10,int)
                Name [20:26-20:27]: i
                  # defined at: [26:5-26:14]
                  # typed as: int
        Name [23:5-23:8]: max
          # defined at: [25:5-25:16]
          # typed as: int
  FunDef [29:1-40:2]: najmanjsi
    # typed as: (ARR(10,int)) -> int
    # framed as: FRAME [najmanjsi]: level=1,locals_size=8,arguments_size=0,parameters_size=8,size=16
    Parameter [29:15-29:31]: array
      # typed as: ARR(10,int)
      # accessed as: Parameter: size[4],offset[4],sl[1]
      Array [29:21-29:31]
        # typed as: ARR(10,int)
        [10]
        TypeName [29:28-29:31]: int
          # defined at: [42:1-42:16]
          # typed as: int
    TypeName [29:33-29:36]: int
      # defined at: [42:1-42:16]
      # typed as: int
    Where [29:39-40:2]
      # typed as: int
      Defs [38:5-39:14]
        VarDef [38:5-38:16]: min
          # typed as: int
          # accessed as: Local: size[4],offset[-4],sl[1]
          TypeName [38:13-38:16]: int
            # defined at: [42:1-42:16]
            # typed as: int
        VarDef [39:5-39:14]: i
          # typed as: int
          # accessed as: Local: size[4],offset[-8],sl[1]
          TypeName [39:11-39:14]: int
            # defined at: [42:1-42:16]
            # typed as: int
      Block [29:39-37:2]
        # typed as: int
        Binary [30:5-30:18]: ASSIGN
          # typed as: int
          Name [30:6-30:9]: min
            # defined at: [38:5-38:16]
            # typed as: int
          Literal [30:12-30:17]: INT(10000)
            # typed as: int
        For [31:5-35:6]
          # typed as: void
          Name [31:10-31:11]: i
            # defined at: [39:5-39:14]
            # typed as: int
          Literal [31:14-31:15]: INT(0)
            # typed as: int
          Literal [31:17-31:19]: INT(10)
            # typed as: int
          Literal [31:21-31:22]: INT(1)
            # typed as: int
          IfThenElse [32:9-34:10]
            # typed as: void
            Binary [32:13-32:27]: LT
              # typed as: log
              Binary [32:13-32:21]: ARR
                # typed as: int
                Name [32:13-32:18]: array
                  # defined at: [29:15-29:31]
                  # typed as: ARR(10,int)
                Name [32:19-32:20]: i
                  # defined at: [39:5-39:14]
                  # typed as: int
              Name [32:24-32:27]: min
                # defined at: [38:5-38:16]
                # typed as: int
            Binary [33:13-33:29]: ASSIGN
              # typed as: int
              Name [33:14-33:17]: min
                # defined at: [38:5-38:16]
                # typed as: int
              Binary [33:20-33:28]: ARR
                # typed as: int
                Name [33:20-33:25]: array
                  # defined at: [29:15-29:31]
                  # typed as: ARR(10,int)
                Name [33:26-33:27]: i
                  # defined at: [39:5-39:14]
                  # typed as: int
        Name [36:5-36:8]: min
          # defined at: [38:5-38:16]
          # typed as: int
  TypeDef [42:1-42:16]: int
    # typed as: int
    Atom [42:9-42:16]: INT
      # typed as: int
  TypeDef [43:1-43:15]: str
    # typed as: str
    Atom [43:9-43:15]: STR
      # typed as: str
  TypeDef [44:1-44:17]: bool
    # typed as: log
    Atom [44:10-44:17]: LOG
      # typed as: log
!end

!name: Program 4 (gnezdene funkcije)
!code:
fun main(args:arr[100]int):int = (
    # najde najbolj pogost element v tabeli
    # tabela ima elemente vrednosti od 1 do 10 vključno

    # napolnimo tabelo pogostosti z ničlami
    {for i = 0, 10, 1:
        {pogostost[i] = 0}
    },

    # preštejemo koliko je katerih elementov
    {for i = 0, 100, 1:
        {pogostost[args[i-1]] = pogostost[args[i-1]] + 1}
    },

    # poiščemo največjo vrednost v tabeli pogostosti
    {max = najvecji(pogostost)},

    # poiščemo indeks največje vrednosti v tabeli pogostosti
    {max_index = index(pogostost, max)},

    # vrnemo največji element
    max_index+1
) {where
    var pogostost:arr[10]int;
    var i:int;
    var max:int;
    var max_index:int;

    # definirane funkcije
    fun najvecji(array:arr[10]int):int = (
        {max = 0},
        {for i = 0, 10, 1:
            {if array[i] > max then
                {max = array[i]}
            }
        },
        max
    ) {where
        var i:int;
        var max:int
    };

    fun index(array:arr[10]int, value:int):int = (
        {returnValue = -1},
        {for i = 0, 10, 1:
            {if array[i] == value & returnValue == -1 then
                {returnValue = i}
            }
        },
        returnValue
    ) {where
        var i:int;
        var returnValue:int
    }
};

typ int:integer;
typ str:string;
typ bool:logical
!expected:
Defs [1:1-59:17]
  FunDef [1:1-55:2]: main
    # typed as: (ARR(100,int)) -> int
    # framed as: FRAME [main]: level=1,locals_size=52,arguments_size=12,parameters_size=8,size=68
    Parameter [1:10-1:26]: args
      # typed as: ARR(100,int)
      # accessed as: Parameter: size[4],offset[4],sl[1]
      Array [1:15-1:26]
        # typed as: ARR(100,int)
        [100]
        TypeName [1:23-1:26]: int
          # defined at: [57:1-57:16]
          # typed as: int
    TypeName [1:28-1:31]: int
      # defined at: [57:1-57:16]
      # typed as: int
    Where [1:34-55:2]
      # typed as: int
      Defs [24:5-54:6]
        VarDef [24:5-24:29]: pogostost
          # typed as: ARR(10,int)
          # accessed as: Local: size[40],offset[-40],sl[1]
          Array [24:19-24:29]
            # typed as: ARR(10,int)
            [10]
            TypeName [24:26-24:29]: int
              # defined at: [57:1-57:16]
              # typed as: int
        VarDef [25:5-25:14]: i
          # typed as: int
          # accessed as: Local: size[4],offset[-44],sl[1]
          TypeName [25:11-25:14]: int
            # defined at: [57:1-57:16]
            # typed as: int
        VarDef [26:5-26:16]: max
          # typed as: int
          # accessed as: Local: size[4],offset[-48],sl[1]
          TypeName [26:13-26:16]: int
            # defined at: [57:1-57:16]
            # typed as: int
        VarDef [27:5-27:22]: max_index
          # typed as: int
          # accessed as: Local: size[4],offset[-52],sl[1]
          TypeName [27:19-27:22]: int
            # defined at: [57:1-57:16]
            # typed as: int
        FunDef [30:5-41:6]: najvecji
          # typed as: (ARR(10,int)) -> int
          # framed as: FRAME [L[0]]: level=2,locals_size=8,arguments_size=0,parameters_size=8,size=16
          Parameter [30:18-30:34]: array
            # typed as: ARR(10,int)
            # accessed as: Parameter: size[4],offset[4],sl[2]
            Array [30:24-30:34]
              # typed as: ARR(10,int)
              [10]
              TypeName [30:31-30:34]: int
                # defined at: [57:1-57:16]
                # typed as: int
          TypeName [30:36-30:39]: int
            # defined at: [57:1-57:16]
            # typed as: int
          Where [30:42-41:6]
            # typed as: int
            Defs [39:9-40:20]
              VarDef [39:9-39:18]: i
                # typed as: int
                # accessed as: Local: size[4],offset[-4],sl[2]
                TypeName [39:15-39:18]: int
                  # defined at: [57:1-57:16]
                  # typed as: int
              VarDef [40:9-40:20]: max
                # typed as: int
                # accessed as: Local: size[4],offset[-8],sl[2]
                TypeName [40:17-40:20]: int
                  # defined at: [57:1-57:16]
                  # typed as: int
            Block [30:42-38:6]
              # typed as: int
              Binary [31:9-31:18]: ASSIGN
                # typed as: int
                Name [31:10-31:13]: max
                  # defined at: [40:9-40:20]
                  # typed as: int
                Literal [31:16-31:17]: INT(0)
                  # typed as: int
              For [32:9-36:10]
                # typed as: void
                Name [32:14-32:15]: i
                  # defined at: [39:9-39:18]
                  # typed as: int
                Literal [32:18-32:19]: INT(0)
                  # typed as: int
                Literal [32:21-32:23]: INT(10)
                  # typed as: int
                Literal [32:25-32:26]: INT(1)
                  # typed as: int
                IfThenElse [33:13-35:14]
                  # typed as: void
                  Binary [33:17-33:31]: GT
                    # typed as: log
                    Binary [33:17-33:25]: ARR
                      # typed as: int
                      Name [33:17-33:22]: array
                        # defined at: [30:18-30:34]
                        # typed as: ARR(10,int)
                      Name [33:23-33:24]: i
                        # defined at: [39:9-39:18]
                        # typed as: int
                    Name [33:28-33:31]: max
                      # defined at: [40:9-40:20]
                      # typed as: int
                  Binary [34:17-34:33]: ASSIGN
                    # typed as: int
                    Name [34:18-34:21]: max
                      # defined at: [40:9-40:20]
                      # typed as: int
                    Binary [34:24-34:32]: ARR
                      # typed as: int
                      Name [34:24-34:29]: array
                        # defined at: [30:18-30:34]
                        # typed as: ARR(10,int)
                      Name [34:30-34:31]: i
                        # defined at: [39:9-39:18]
                        # typed as: int
              Name [37:9-37:12]: max
                # defined at: [40:9-40:20]
                # typed as: int
        FunDef [43:5-54:6]: index
          # typed as: (ARR(10,int), int) -> int
          # framed as: FRAME [L[1]]: level=2,locals_size=8,arguments_size=0,parameters_size=12,size=16
          Parameter [43:15-43:31]: array
            # typed as: ARR(10,int)
            # accessed as: Parameter: size[4],offset[4],sl[2]
            Array [43:21-43:31]
              # typed as: ARR(10,int)
              [10]
              TypeName [43:28-43:31]: int
                # defined at: [57:1-57:16]
                # typed as: int
          Parameter [43:33-43:42]: value
            # typed as: int
            # accessed as: Parameter: size[4],offset[8],sl[2]
            TypeName [43:39-43:42]: int
              # defined at: [57:1-57:16]
              # typed as: int
          TypeName [43:44-43:47]: int
            # defined at: [57:1-57:16]
            # typed as: int
          Where [43:50-54:6]
            # typed as: int
            Defs [52:9-53:28]
              VarDef [52:9-52:18]: i
                # typed as: int
                # accessed as: Local: size[4],offset[-4],sl[2]
                TypeName [52:15-52:18]: int
                  # defined at: [57:1-57:16]
                  # typed as: int
              VarDef [53:9-53:28]: returnValue
                # typed as: int
                # accessed as: Local: size[4],offset[-8],sl[2]
                TypeName [53:25-53:28]: int
                  # defined at: [57:1-57:16]
                  # typed as: int
            Block [43:50-51:6]
              # typed as: int
              Binary [44:9-44:27]: ASSIGN
                # typed as: int
                Name [44:10-44:21]: returnValue
                  # defined at: [53:9-53:28]
                  # typed as: int
                Unary [44:24-44:26]: SUB
                  # typed as: int
                  Literal [44:25-44:26]: INT(1)
                    # typed as: int
              For [45:9-49:10]
                # typed as: void
                Name [45:14-45:15]: i
                  # defined at: [52:9-52:18]
                  # typed as: int
                Literal [45:18-45:19]: INT(0)
                  # typed as: int
                Literal [45:21-45:23]: INT(10)
                  # typed as: int
                Literal [45:25-45:26]: INT(1)
                  # typed as: int
                IfThenElse [46:13-48:14]
                  # typed as: void
                  Binary [46:17-46:54]: AND
                    # typed as: log
                    Binary [46:17-46:34]: EQ
                      # typed as: log
                      Binary [46:17-46:25]: ARR
                        # typed as: int
                        Name [46:17-46:22]: array
                          # defined at: [43:15-43:31]
                          # typed as: ARR(10,int)
                        Name [46:23-46:24]: i
                          # defined at: [52:9-52:18]
                          # typed as: int
                      Name [46:29-46:34]: value
                        # defined at: [43:33-43:42]
                        # typed as: int
                    Binary [46:37-46:54]: EQ
                      # typed as: log
                      Name [46:37-46:48]: returnValue
                        # defined at: [53:9-53:28]
                        # typed as: int
                      Unary [46:52-46:54]: SUB
                        # typed as: int
                        Literal [46:53-46:54]: INT(1)
                          # typed as: int
                  Binary [47:17-47:34]: ASSIGN
                    # typed as: int
                    Name [47:18-47:29]: returnValue
                      # defined at: [53:9-53:28]
                      # typed as: int
                    Name [47:32-47:33]: i
                      # defined at: [52:9-52:18]
                      # typed as: int
              Name [50:9-50:20]: returnValue
                # defined at: [53:9-53:28]
                # typed as: int
      Block [1:34-23:2]
        # typed as: int
        For [6:5-8:6]
          # typed as: void
          Name [6:10-6:11]: i
            # defined at: [25:5-25:14]
            # typed as: int
          Literal [6:14-6:15]: INT(0)
            # typed as: int
          Literal [6:17-6:19]: INT(10)
            # typed as: int
          Literal [6:21-6:22]: INT(1)
            # typed as: int
          Binary [7:9-7:27]: ASSIGN
            # typed as: int
            Binary [7:10-7:22]: ARR
              # typed as: int
              Name [7:10-7:19]: pogostost
                # defined at: [24:5-24:29]
                # typed as: ARR(10,int)
              Name [7:20-7:21]: i
                # defined at: [25:5-25:14]
                # typed as: int
            Literal [7:25-7:26]: INT(0)
              # typed as: int
        For [11:5-13:6]
          # typed as: void
          Name [11:10-11:11]: i
            # defined at: [25:5-25:14]
            # typed as: int
          Literal [11:14-11:15]: INT(0)
            # typed as: int
          Literal [11:17-11:20]: INT(100)
            # typed as: int
          Literal [11:22-11:23]: INT(1)
            # typed as: int
          Binary [12:9-12:58]: ASSIGN
            # typed as: int
            Binary [12:10-12:30]: ARR
              # typed as: int
              Name [12:10-12:19]: pogostost
                # defined at: [24:5-24:29]
                # typed as: ARR(10,int)
              Binary [12:20-12:29]: ARR
                # typed as: int
                Name [12:20-12:24]: args
                  # defined at: [1:10-1:26]
                  # typed as: ARR(100,int)
                Binary [12:25-12:28]: SUB
                  # typed as: int
                  Name [12:25-12:26]: i
                    # defined at: [25:5-25:14]
                    # typed as: int
                  Literal [12:27-12:28]: INT(1)
                    # typed as: int
            Binary [12:33-12:57]: ADD
              # typed as: int
              Binary [12:33-12:53]: ARR
                # typed as: int
                Name [12:33-12:42]: pogostost
                  # defined at: [24:5-24:29]
                  # typed as: ARR(10,int)
                Binary [12:43-12:52]: ARR
                  # typed as: int
                  Name [12:43-12:47]: args
                    # defined at: [1:10-1:26]
                    # typed as: ARR(100,int)
                  Binary [12:48-12:51]: SUB
                    # typed as: int
                    Name [12:48-12:49]: i
                      # defined at: [25:5-25:14]
                      # typed as: int
                    Literal [12:50-12:51]: INT(1)
                      # typed as: int
              Literal [12:56-12:57]: INT(1)
                # typed as: int
        Binary [16:5-16:32]: ASSIGN
          # typed as: int
          Name [16:6-16:9]: max
            # defined at: [26:5-26:16]
            # typed as: int
          Call [16:12-16:31]: najvecji
            # defined at: [30:5-41:6]
            # typed as: int
            Name [16:21-16:30]: pogostost
              # defined at: [24:5-24:29]
              # typed as: ARR(10,int)
        Binary [19:5-19:40]: ASSIGN
          # typed as: int
          Name [19:6-19:15]: max_index
            # defined at: [27:5-27:22]
            # typed as: int
          Call [19:18-19:39]: index
            # defined at: [43:5-54:6]
            # typed as: int
            Name [19:24-19:33]: pogostost
              # defined at: [24:5-24:29]
              # typed as: ARR(10,int)
            Name [19:35-19:38]: max
              # defined at: [26:5-26:16]
              # typed as: int
        Binary [22:5-22:16]: ADD
          # typed as: int
          Name [22:5-22:14]: max_index
            # defined at: [27:5-27:22]
            # typed as: int
          Literal [22:15-22:16]: INT(1)
            # typed as: int
  TypeDef [57:1-57:16]: int
    # typed as: int
    Atom [57:9-57:16]: INT
      # typed as: int
  TypeDef [58:1-58:15]: str
    # typed as: str
    Atom [58:9-58:15]: STR
      # typed as: str
  TypeDef [59:1-59:17]: bool
    # typed as: log
    Atom [59:10-59:17]: LOG
      # typed as: log
!end

!name: Program 5 (vsi podatkovni tipi)
!code:
fun main(args:arr[10]int):str = (
    # preverimo ce so vsa stevila deljiva z 2 ali s 3 ali z nobenim ali z obema od teh dveh
    {printString = ''},
    {deljivo[0] = preveriArr(args)[0]},
    {deljivo[1] = preveriArr(args)[1]},
    {if deljivo[0] & deljivo[1] then
        {printString = 'Deljivo z 2 in 3'}
    else {if deljivo[0] then
        {printString = 'Deljivo z 2'}
    else {if deljivo[1] then
        {printString = 'Deljivo s 3'}
    else
        {printString = 'Ni deljivo z nobenim od teh'}
    }}},
    printString
) {where
    var printString:str;
    var deljivo:arr[2]bool
};

fun deljiva(n:int, m:int):bool = (
    {returnValue = n % m == 0},
    returnValue
) {where
    var returnValue:bool
};

fun deljivaZ2(n:int):bool = deljiva(n, 2);
fun deljivaS3(n:int):bool = deljiva(n, 3);

fun preveriArr(array:arr[10]int):arr[2]bool = (
    {returnValue[0] = true},
    {returnValue[1] = true},

    {for i = 0, 10, 1: (
        {if !deljivaZ2(array[i]) | !returnValue[0] then
            {returnValue[0] = false}
        },
        {if !deljivaS3(array[i]) | !returnValue[1] then
            {returnValue[1] = false}
        }
    )},

    returnValue
) {where
    var returnValue:arr[2]bool;
    var i:int
};

typ int:integer;
typ str:string;
typ bool:logical
!expected:
Defs [1:1-52:17]
  FunDef [1:1-19:2]: main
    # typed as: (ARR(10,int)) -> str
    # framed as: FRAME [main]: level=1,locals_size=12,arguments_size=8,parameters_size=8,size=24
    Parameter [1:10-1:25]: args
      # typed as: ARR(10,int)
      # accessed as: Parameter: size[4],offset[4],sl[1]
      Array [1:15-1:25]
        # typed as: ARR(10,int)
        [10]
        TypeName [1:22-1:25]: int
          # defined at: [50:1-50:16]
          # typed as: int
    TypeName [1:27-1:30]: str
      # defined at: [51:1-51:15]
      # typed as: str
    Where [1:33-19:2]
      # typed as: str
      Defs [17:5-18:27]
        VarDef [17:5-17:24]: printString
          # typed as: str
          # accessed as: Local: size[4],offset[-4],sl[1]
          TypeName [17:21-17:24]: str
            # defined at: [51:1-51:15]
            # typed as: str
        VarDef [18:5-18:27]: deljivo
          # typed as: ARR(2,log)
          # accessed as: Local: size[8],offset[-12],sl[1]
          Array [18:17-18:27]
            # typed as: ARR(2,log)
            [2]
            TypeName [18:23-18:27]: bool
              # defined at: [52:1-52:17]
              # typed as: log
      Block [1:33-16:2]
        # typed as: str
        Binary [3:5-3:23]: ASSIGN
          # typed as: str
          Name [3:6-3:17]: printString
            # defined at: [17:5-17:24]
            # typed as: str
          Literal [3:20-3:22]: STR()
            # typed as: str
        Binary [4:5-4:39]: ASSIGN
          # typed as: log
          Binary [4:6-4:16]: ARR
            # typed as: log
            Name [4:6-4:13]: deljivo
              # defined at: [18:5-18:27]
              # typed as: ARR(2,log)
            Literal [4:14-4:15]: INT(0)
              # typed as: int
          Binary [4:19-4:38]: ARR
            # typed as: log
            Call [4:19-4:35]: preveriArr
              # defined at: [31:1-48:2]
              # typed as: ARR(2,log)
              Name [4:30-4:34]: args
                # defined at: [1:10-1:25]
                # typed as: ARR(10,int)
            Literal [4:36-4:37]: INT(0)
              # typed as: int
        Binary [5:5-5:39]: ASSIGN
          # typed as: log
          Binary [5:6-5:16]: ARR
            # typed as: log
            Name [5:6-5:13]: deljivo
              # defined at: [18:5-18:27]
              # typed as: ARR(2,log)
            Literal [5:14-5:15]: INT(1)
              # typed as: int
          Binary [5:19-5:38]: ARR
            # typed as: log
            Call [5:19-5:35]: preveriArr
              # defined at: [31:1-48:2]
              # typed as: ARR(2,log)
              Name [5:30-5:34]: args
                # defined at: [1:10-1:25]
                # typed as: ARR(10,int)
            Literal [5:36-5:37]: INT(1)
              # typed as: int
        IfThenElse [6:5-14:8]
          # typed as: void
          Binary [6:9-6:32]: AND
            # typed as: log
            Binary [6:9-6:19]: ARR
              # typed as: log
              Name [6:9-6:16]: deljivo
                # defined at: [18:5-18:27]
                # typed as: ARR(2,log)
              Literal [6:17-6:18]: INT(0)
                # typed as: int
            Binary [6:22-6:32]: ARR
              # typed as: log
              Name [6:22-6:29]: deljivo
                # defined at: [18:5-18:27]
                # typed as: ARR(2,log)
              Literal [6:30-6:31]: INT(1)
                # typed as: int
          Binary [7:9-7:43]: ASSIGN
            # typed as: str
            Name [7:10-7:21]: printString
              # defined at: [17:5-17:24]
              # typed as: str
            Literal [7:24-7:42]: STR(Deljivo z 2 in 3)
              # typed as: str
          IfThenElse [8:10-14:7]
            # typed as: void
            Binary [8:14-8:24]: ARR
              # typed as: log
              Name [8:14-8:21]: deljivo
                # defined at: [18:5-18:27]
                # typed as: ARR(2,log)
              Literal [8:22-8:23]: INT(0)
                # typed as: int
            Binary [9:9-9:38]: ASSIGN
              # typed as: str
              Name [9:10-9:21]: printString
                # defined at: [17:5-17:24]
                # typed as: str
              Literal [9:24-9:37]: STR(Deljivo z 2)
                # typed as: str
            IfThenElse [10:10-14:6]
              # typed as: void
              Binary [10:14-10:24]: ARR
                # typed as: log
                Name [10:14-10:21]: deljivo
                  # defined at: [18:5-18:27]
                  # typed as: ARR(2,log)
                Literal [10:22-10:23]: INT(1)
                  # typed as: int
              Binary [11:9-11:38]: ASSIGN
                # typed as: str
                Name [11:10-11:21]: printString
                  # defined at: [17:5-17:24]
                  # typed as: str
                Literal [11:24-11:37]: STR(Deljivo s 3)
                  # typed as: str
              Binary [13:9-13:54]: ASSIGN
                # typed as: str
                Name [13:10-13:21]: printString
                  # defined at: [17:5-17:24]
                  # typed as: str
                Literal [13:24-13:53]: STR(Ni deljivo z nobenim od teh)
                  # typed as: str
        Name [15:5-15:16]: printString
          # defined at: [17:5-17:24]
          # typed as: str
  FunDef [21:1-26:2]: deljiva
    # typed as: (int, int) -> log
    # framed as: FRAME [deljiva]: level=1,locals_size=4,arguments_size=0,parameters_size=12,size=12
    Parameter [21:13-21:18]: n
      # typed as: int
      # accessed as: Parameter: size[4],offset[4],sl[1]
      TypeName [21:15-21:18]: int
        # defined at: [50:1-50:16]
        # typed as: int
    Parameter [21:20-21:25]: m
      # typed as: int
      # accessed as: Parameter: size[4],offset[8],sl[1]
      TypeName [21:22-21:25]: int
        # defined at: [50:1-50:16]
        # typed as: int
    TypeName [21:27-21:31]: bool
      # defined at: [52:1-52:17]
      # typed as: log
    Where [21:34-26:2]
      # typed as: log
      Defs [25:5-25:25]
        VarDef [25:5-25:25]: returnValue
          # typed as: log
          # accessed as: Local: size[4],offset[-4],sl[1]
          TypeName [25:21-25:25]: bool
            # defined at: [52:1-52:17]
            # typed as: log
      Block [21:34-24:2]
        # typed as: log
        Binary [22:5-22:31]: ASSIGN
          # typed as: log
          Name [22:6-22:17]: returnValue
            # defined at: [25:5-25:25]
            # typed as: log
          Binary [22:20-22:30]: EQ
            # typed as: log
            Binary [22:20-22:25]: MOD
              # typed as: int
              Name [22:20-22:21]: n
                # defined at: [21:13-21:18]
                # typed as: int
              Name [22:24-22:25]: m
                # defined at: [21:20-21:25]
                # typed as: int
            Literal [22:29-22:30]: INT(0)
              # typed as: int
        Name [23:5-23:16]: returnValue
          # defined at: [25:5-25:25]
          # typed as: log
  FunDef [28:1-28:42]: deljivaZ2
    # typed as: (int) -> log
    # framed as: FRAME [deljivaZ2]: level=1,locals_size=0,arguments_size=12,parameters_size=8,size=16
    Parameter [28:15-28:20]: n
      # typed as: int
      # accessed as: Parameter: size[4],offset[4],sl[1]
      TypeName [28:17-28:20]: int
        # defined at: [50:1-50:16]
        # typed as: int
    TypeName [28:22-28:26]: bool
      # defined at: [52:1-52:17]
      # typed as: log
    Call [28:29-28:42]: deljiva
      # defined at: [21:1-26:2]
      # typed as: log
      Name [28:37-28:38]: n
        # defined at: [28:15-28:20]
        # typed as: int
      Literal [28:40-28:41]: INT(2)
        # typed as: int
  FunDef [29:1-29:42]: deljivaS3
    # typed as: (int) -> log
    # framed as: FRAME [deljivaS3]: level=1,locals_size=0,arguments_size=12,parameters_size=8,size=16
    Parameter [29:15-29:20]: n
      # typed as: int
      # accessed as: Parameter: size[4],offset[4],sl[1]
      TypeName [29:17-29:20]: int
        # defined at: [50:1-50:16]
        # typed as: int
    TypeName [29:22-29:26]: bool
      # defined at: [52:1-52:17]
      # typed as: log
    Call [29:29-29:42]: deljiva
      # defined at: [21:1-26:2]
      # typed as: log
      Name [29:37-29:38]: n
        # defined at: [29:15-29:20]
        # typed as: int
      Literal [29:40-29:41]: INT(3)
        # typed as: int
  FunDef [31:1-48:2]: preveriArr
    # typed as: (ARR(10,int)) -> ARR(2,log)
    # framed as: FRAME [preveriArr]: level=1,locals_size=12,arguments_size=8,parameters_size=8,size=24
    Parameter [31:16-31:32]: array
      # typed as: ARR(10,int)
      # accessed as: Parameter: size[4],offset[4],sl[1]
      Array [31:22-31:32]
        # typed as: ARR(10,int)
        [10]
        TypeName [31:29-31:32]: int
          # defined at: [50:1-50:16]
          # typed as: int
    Array [31:34-31:44]
      # typed as: ARR(2,log)
      [2]
      TypeName [31:40-31:44]: bool
        # defined at: [52:1-52:17]
        # typed as: log
    Where [31:47-48:2]
      # typed as: ARR(2,log)
      Defs [46:5-47:14]
        VarDef [46:5-46:31]: returnValue
          # typed as: ARR(2,log)
          # accessed as: Local: size[8],offset[-8],sl[1]
          Array [46:21-46:31]
            # typed as: ARR(2,log)
            [2]
            TypeName [46:27-46:31]: bool
              # defined at: [52:1-52:17]
              # typed as: log
        VarDef [47:5-47:14]: i
          # typed as: int
          # accessed as: Local: size[4],offset[-12],sl[1]
          TypeName [47:11-47:14]: int
            # defined at: [50:1-50:16]
            # typed as: int
      Block [31:47-45:2]
        # typed as: ARR(2,log)
        Binary [32:5-32:28]: ASSIGN
          # typed as: log
          Binary [32:6-32:20]: ARR
            # typed as: log
            Name [32:6-32:17]: returnValue
              # defined at: [46:5-46:31]
              # typed as: ARR(2,log)
            Literal [32:18-32:19]: INT(0)
              # typed as: int
          Literal [32:23-32:27]: LOG(true)
            # typed as: log
        Binary [33:5-33:28]: ASSIGN
          # typed as: log
          Binary [33:6-33:20]: ARR
            # typed as: log
            Name [33:6-33:17]: returnValue
              # defined at: [46:5-46:31]
              # typed as: ARR(2,log)
            Literal [33:18-33:19]: INT(1)
              # typed as: int
          Literal [33:23-33:27]: LOG(true)
            # typed as: log
        For [35:5-42:7]
          # typed as: void
          Name [35:10-35:11]: i
            # defined at: [47:5-47:14]
            # typed as: int
          Literal [35:14-35:15]: INT(0)
            # typed as: int
          Literal [35:17-35:19]: INT(10)
            # typed as: int
          Literal [35:21-35:22]: INT(1)
            # typed as: int
          Block [35:24-42:6]
            # typed as: void
            IfThenElse [36:9-38:10]
              # typed as: void
              Binary [36:13-36:51]: OR
                # typed as: log
                Unary [36:13-36:33]: NOT
                  # typed as: log
                  Call [36:14-36:33]: deljivaZ2
                    # defined at: [28:1-28:42]
                    # typed as: log
                    Binary [36:24-36:32]: ARR
                      # typed as: int
                      Name [36:24-36:29]: array
                        # defined at: [31:16-31:32]
                        # typed as: ARR(10,int)
                      Name [36:30-36:31]: i
                        # defined at: [47:5-47:14]
                        # typed as: int
                Unary [36:36-36:51]: NOT
                  # typed as: log
                  Binary [36:37-36:51]: ARR
                    # typed as: log
                    Name [36:37-36:48]: returnValue
                      # defined at: [46:5-46:31]
                      # typed as: ARR(2,log)
                    Literal [36:49-36:50]: INT(0)
                      # typed as: int
              Binary [37:13-37:37]: ASSIGN
                # typed as: log
                Binary [37:14-37:28]: ARR
                  # typed as: log
                  Name [37:14-37:25]: returnValue
                    # defined at: [46:5-46:31]
                    # typed as: ARR(2,log)
                  Literal [37:26-37:27]: INT(0)
                    # typed as: int
                Literal [37:31-37:36]: LOG(false)
                  # typed as: log
            IfThenElse [39:9-41:10]
              # typed as: void
              Binary [39:13-39:51]: OR
                # typed as: log
                Unary [39:13-39:33]: NOT
                  # typed as: log
                  Call [39:14-39:33]: deljivaS3
                    # defined at: [29:1-29:42]
                    # typed as: log
                    Binary [39:24-39:32]: ARR
                      # typed as: int
                      Name [39:24-39:29]: array
                        # defined at: [31:16-31:32]
                        # typed as: ARR(10,int)
                      Name [39:30-39:31]: i
                        # defined at: [47:5-47:14]
                        # typed as: int
                Unary [39:36-39:51]: NOT
                  # typed as: log
                  Binary [39:37-39:51]: ARR
                    # typed as: log
                    Name [39:37-39:48]: returnValue
                      # defined at: [46:5-46:31]
                      # typed as: ARR(2,log)
                    Literal [39:49-39:50]: INT(1)
                      # typed as: int
              Binary [40:13-40:37]: ASSIGN
                # typed as: log
                Binary [40:14-40:28]: ARR
                  # typed as: log
                  Name [40:14-40:25]: returnValue
                    # defined at: [46:5-46:31]
                    # typed as: ARR(2,log)
                  Literal [40:26-40:27]: INT(1)
                    # typed as: int
                Literal [40:31-40:36]: LOG(false)
                  # typed as: log
        Name [44:5-44:16]: returnValue
          # defined at: [46:5-46:31]
          # typed as: ARR(2,log)
  TypeDef [50:1-50:16]: int
    # typed as: int
    Atom [50:9-50:16]: INT
      # typed as: int
  TypeDef [51:1-51:15]: str
    # typed as: str
    Atom [51:9-51:15]: STR
      # typed as: str
  TypeDef [52:1-52:17]: bool
    # typed as: log
    Atom [52:10-52:17]: LOG
      # typed as: log
!end

!name: Program 6 (gnezdene funkcije)
!code:
# https://open.kattis.com/problems/server
# standard input is given as program arguments
# standard output is a return statement at the end of the main function

typ int:integer;
typ str:string;
typ bool:logical;

fun main(args:arr[52]int):int = (
    {n = args[0]},
    {t = args[1]},
    {returnValue = getNumberOfTasks(n, t, args)},
    returnValue
) { where
    var n:int;
    var t:int;
    var returnValue:int;

    # functions
    fun getNumberOfTasks(n:int, t:int, args:arr[52]int):int = (
        # {tasks = getTasks(n, args)},
        # zmer pozabm, da ne morm arrayou prirejat
        {sum = 0},
        {count = 0},
        { for i = 0, n, 1: (
            {sum = sum + args[i+2]},
            { if sum <= t then
                {count = count + 1}
            }
        )},
        count
    ) { where
        var tasks:arr[50]int;
        var sum:int;
        var count:int;
        var i:int;

        # functions
        fun getTasks(n:int, tasks:arr[52]int):arr[50]int = (
            { for i = 0, n, 1:
                {out[i] = tasks[i+2]}
            },
            out
        ) { where
            var out:arr[50]int
        }
    }
}
!expected:
Defs [5:1-48:2]
  TypeDef [5:1-5:16]: int
    # typed as: int
    Atom [5:9-5:16]: INT
      # typed as: int
  TypeDef [6:1-6:15]: str
    # typed as: str
    Atom [6:9-6:15]: STR
      # typed as: str
  TypeDef [7:1-7:17]: bool
    # typed as: log
    Atom [7:10-7:17]: LOG
      # typed as: log
  FunDef [9:1-48:2]: main
    # typed as: (ARR(52,int)) -> int
    # framed as: FRAME [main]: level=1,locals_size=12,arguments_size=16,parameters_size=8,size=32
    Parameter [9:10-9:25]: args
      # typed as: ARR(52,int)
      # accessed as: Parameter: size[4],offset[4],sl[1]
      Array [9:15-9:25]
        # typed as: ARR(52,int)
        [52]
        TypeName [9:22-9:25]: int
          # defined at: [5:1-5:16]
          # typed as: int
    TypeName [9:27-9:30]: int
      # defined at: [5:1-5:16]
      # typed as: int
    Where [9:33-48:2]
      # typed as: int
      Defs [15:5-47:6]
        VarDef [15:5-15:14]: n
          # typed as: int
          # accessed as: Local: size[4],offset[-4],sl[1]
          TypeName [15:11-15:14]: int
            # defined at: [5:1-5:16]
            # typed as: int
        VarDef [16:5-16:14]: t
          # typed as: int
          # accessed as: Local: size[4],offset[-8],sl[1]
          TypeName [16:11-16:14]: int
            # defined at: [5:1-5:16]
            # typed as: int
        VarDef [17:5-17:24]: returnValue
          # typed as: int
          # accessed as: Local: size[4],offset[-12],sl[1]
          TypeName [17:21-17:24]: int
            # defined at: [5:1-5:16]
            # typed as: int
        FunDef [20:5-47:6]: getNumberOfTasks
          # typed as: (int, int, ARR(52,int)) -> int
          # framed as: FRAME [L[0]]: level=2,locals_size=212,arguments_size=0,parameters_size=16,size=220
          Parameter [20:26-20:31]: n
            # typed as: int
            # accessed as: Parameter: size[4],offset[4],sl[2]
            TypeName [20:28-20:31]: int
              # defined at: [5:1-5:16]
              # typed as: int
          Parameter [20:33-20:38]: t
            # typed as: int
            # accessed as: Parameter: size[4],offset[8],sl[2]
            TypeName [20:35-20:38]: int
              # defined at: [5:1-5:16]
              # typed as: int
          Parameter [20:40-20:55]: args
            # typed as: ARR(52,int)
            # accessed as: Parameter: size[4],offset[12],sl[2]
            Array [20:45-20:55]
              # typed as: ARR(52,int)
              [52]
              TypeName [20:52-20:55]: int
                # defined at: [5:1-5:16]
                # typed as: int
          TypeName [20:57-20:60]: int
            # defined at: [5:1-5:16]
            # typed as: int
          Where [20:63-47:6]
            # typed as: int
            Defs [33:9-46:10]
              VarDef [33:9-33:29]: tasks
                # typed as: ARR(50,int)
                # accessed as: Local: size[200],offset[-200],sl[2]
                Array [33:19-33:29]
                  # typed as: ARR(50,int)
                  [50]
                  TypeName [33:26-33:29]: int
                    # defined at: [5:1-5:16]
                    # typed as: int
              VarDef [34:9-34:20]: sum
                # typed as: int
                # accessed as: Local: size[4],offset[-204],sl[2]
                TypeName [34:17-34:20]: int
                  # defined at: [5:1-5:16]
                  # typed as: int
              VarDef [35:9-35:22]: count
                # typed as: int
                # accessed as: Local: size[4],offset[-208],sl[2]
                TypeName [35:19-35:22]: int
                  # defined at: [5:1-5:16]
                  # typed as: int
              VarDef [36:9-36:18]: i
                # typed as: int
                # accessed as: Local: size[4],offset[-212],sl[2]
                TypeName [36:15-36:18]: int
                  # defined at: [5:1-5:16]
                  # typed as: int
              FunDef [39:9-46:10]: getTasks
                # typed as: (int, ARR(52,int)) -> ARR(50,int)
                # framed as: FRAME [L[1]]: level=3,locals_size=200,arguments_size=0,parameters_size=12,size=208
                Parameter [39:22-39:27]: n
                  # typed as: int
                  # accessed as: Parameter: size[4],offset[4],sl[3]
                  TypeName [39:24-39:27]: int
                    # defined at: [5:1-5:16]
                    # typed as: int
                Parameter [39:29-39:45]: tasks
                  # typed as: ARR(52,int)
                  # accessed as: Parameter: size[4],offset[8],sl[3]
                  Array [39:35-39:45]
                    # typed as: ARR(52,int)
                    [52]
                    TypeName [39:42-39:45]: int
                      # defined at: [5:1-5:16]
                      # typed as: int
                Array [39:47-39:57]
                  # typed as: ARR(50,int)
                  [50]
                  TypeName [39:54-39:57]: int
                    # defined at: [5:1-5:16]
                    # typed as: int
                Where [39:60-46:10]
                  # typed as: ARR(50,int)
                  Defs [45:13-45:31]
                    VarDef [45:13-45:31]: out
                      # typed as: ARR(50,int)
                      # accessed as: Local: size[200],offset[-200],sl[3]
                      Array [45:21-45:31]
                        # typed as: ARR(50,int)
                        [50]
                        TypeName [45:28-45:31]: int
                          # defined at: [5:1-5:16]
                          # typed as: int
                  Block [39:60-44:10]
                    # typed as: ARR(50,int)
                    For [40:13-42:14]
                      # typed as: void
                      Name [40:19-40:20]: i
                        # defined at: [36:9-36:18]
                        # typed as: int
                      Literal [40:23-40:24]: INT(0)
                        # typed as: int
                      Name [40:26-40:27]: n
                        # defined at: [39:22-39:27]
                        # typed as: int
                      Literal [40:29-40:30]: INT(1)
                        # typed as: int
                      Binary [41:17-41:38]: ASSIGN
                        # typed as: int
                        Binary [41:18-41:24]: ARR
                          # typed as: int
                          Name [41:18-41:21]: out
                            # defined at: [45:13-45:31]
                            # typed as: ARR(50,int)
                          Name [41:22-41:23]: i
                            # defined at: [36:9-36:18]
                            # typed as: int
                        Binary [41:27-41:37]: ARR
                          # typed as: int
                          Name [41:27-41:32]: tasks
                            # defined at: [39:29-39:45]
                            # typed as: ARR(52,int)
                          Binary [41:33-41:36]: ADD
                            # typed as: int
                            Name [41:33-41:34]: i
                              # defined at: [36:9-36:18]
                              # typed as: int
                            Literal [41:35-41:36]: INT(2)
                              # typed as: int
                    Name [43:13-43:16]: out
                      # defined at: [45:13-45:31]
                      # typed as: ARR(50,int)
            Block [20:63-32:6]
              # typed as: int
              Binary [23:9-23:18]: ASSIGN
                # typed as: int
                Name [23:10-23:13]: sum
                  # defined at: [34:9-34:20]
                  # typed as: int
                Literal [23:16-23:17]: INT(0)
                  # typed as: int
              Binary [24:9-24:20]: ASSIGN
                # typed as: int
                Name [24:10-24:15]: count
                  # defined at: [35:9-35:22]
                  # typed as: int
                Literal [24:18-24:19]: INT(0)
                  # typed as: int
              For [25:9-30:11]
                # typed as: void
                Name [25:15-25:16]: i
                  # defined at: [36:9-36:18]
                  # typed as: int
                Literal [25:19-25:20]: INT(0)
                  # typed as: int
                Name [25:22-25:23]: n
                  # defined at: [20:26-20:31]
                  # typed as: int
                Literal [25:25-25:26]: INT(1)
                  # typed as: int
                Block [25:28-30:10]
                  # typed as: void
                  Binary [26:13-26:36]: ASSIGN
                    # typed as: int
                    Name [26:14-26:17]: sum
                      # defined at: [34:9-34:20]
                      # typed as: int
                    Binary [26:20-26:35]: ADD
                      # typed as: int
                      Name [26:20-26:23]: sum
                        # defined at: [34:9-34:20]
                        # typed as: int
                      Binary [26:26-26:35]: ARR
                        # typed as: int
                        Name [26:26-26:30]: args
                          # defined at: [20:40-20:55]
                          # typed as: ARR(52,int)
                        Binary [26:31-26:34]: ADD
                          # typed as: int
                          Name [26:31-26:32]: i
                            # defined at: [36:9-36:18]
                            # typed as: int
                          Literal [26:33-26:34]: INT(2)
                            # typed as: int
                  IfThenElse [27:13-29:14]
                    # typed as: void
                    Binary [27:18-27:26]: LEQ
                      # typed as: log
                      Name [27:18-27:21]: sum
                        # defined at: [34:9-34:20]
                        # typed as: int
                      Name [27:25-27:26]: t
                        # defined at: [20:33-20:38]
                        # typed as: int
                    Binary [28:17-28:36]: ASSIGN
                      # typed as: int
                      Name [28:18-28:23]: count
                        # defined at: [35:9-35:22]
                        # typed as: int
                      Binary [28:26-28:35]: ADD
                        # typed as: int
                        Name [28:26-28:31]: count
                          # defined at: [35:9-35:22]
                          # typed as: int
                        Literal [28:34-28:35]: INT(1)
                          # typed as: int
              Name [31:9-31:14]: count
                # defined at: [35:9-35:22]
                # typed as: int
      Block [9:33-14:2]
        # typed as: int
        Binary [10:5-10:18]: ASSIGN
          # typed as: int
          Name [10:6-10:7]: n
            # defined at: [15:5-15:14]
            # typed as: int
          Binary [10:10-10:17]: ARR
            # typed as: int
            Name [10:10-10:14]: args
              # defined at: [9:10-9:25]
              # typed as: ARR(52,int)
            Literal [10:15-10:16]: INT(0)
              # typed as: int
        Binary [11:5-11:18]: ASSIGN
          # typed as: int
          Name [11:6-11:7]: t
            # defined at: [16:5-16:14]
            # typed as: int
          Binary [11:10-11:17]: ARR
            # typed as: int
            Name [11:10-11:14]: args
              # defined at: [9:10-9:25]
              # typed as: ARR(52,int)
            Literal [11:15-11:16]: INT(1)
              # typed as: int
        Binary [12:5-12:49]: ASSIGN
          # typed as: int
          Name [12:6-12:17]: returnValue
            # defined at: [17:5-17:24]
            # typed as: int
          Call [12:20-12:48]: getNumberOfTasks
            # defined at: [20:5-47:6]
            # typed as: int
            Name [12:37-12:38]: n
              # defined at: [15:5-15:14]
              # typed as: int
            Name [12:40-12:41]: t
              # defined at: [16:5-16:14]
              # typed as: int
            Name [12:43-12:47]: args
              # defined at: [9:10-9:25]
              # typed as: ARR(52,int)
        Name [13:5-13:16]: returnValue
          # defined at: [17:5-17:24]
          # typed as: int
!end

!name: Program 7 (ULTIMATE TEST PINS 2k69)
!code:
####### ULTIMATE TEST PINS 2k69 ######

# Inicializacija
# type_definition
typ x: integer;
typ y: string;
typ z: logical;
typ i: blablabla;
typ j: arr[10] arr[15] arr[20] blablabla;
typ blablabla: integer;
typ blabla: blablabla;
typ void: logical;

# variable_definition
var k: string; var l: integer; var q: logical; var m: blabla; var n: arr[69] arr [420] string;

# Funkcije
# function_definition
fun firstfunction(x: integer, y: string, z: logical): integer =
	( {for i = 1, 100, {i = i + 1}:
		{
			while j > (-10): {
				if j * (-10) + 20 + 30 + 10 - 40 / 50 % 100 -(-(-(-(-(+10))))) != 123 then
				    12
				else
					firstfunction(123, 'bruh', true)
			}
		# komentar komentar komentar ----------------------------------------------
		}	},
		true & false | z & true | (1 >= firstfunction(i, 'monke', true)),
		{
			if 16 > (14 - 10 + 10 * 10 / 10 - 10 + 10 + -10)
				then
					firstfunction(i, 'danes je lep dan', false)
		},

		{
		for i = 1, 10, 12 : (
			{
				while j<10: (j < 10)
			},
			{
			    if j * 10 >= 100 then {a = j - 10 == 12} else 0
			},
			{
                for j = 1, 12, {j=j*2}: firstfunction(12, 'blablabla', true)
            },
			{
                p[i+10-20*30][j-10-203] = 12
            })
		},
        12
	)
{ where
    var i:integer;
    var a: logical;
    var j: integer;
    var p: arr[69] arr[420] integer;
    fun secondfunction(x: integer):integer =
        (
            (10+20) >= (30-40) & 12 == 100, 0)
        { where
            fun thirdfunction(y: string):arr[10] string =
                (
                    {10*20=xyz},
                    ss
                )
                { where
                    var xyz: integer;
                    var ss : arr[10] string
                }
    };
    fun partition(stevila: arr[10] integer, begin: integer, end: integer): integer =
        (
	        { pivot = stevila[end] },
	        { i = (begin - 1) },
            {
                for j = begin, end, {j = j + 1}:
		            {
                        if stevila[j] <= pivot then
			                (
                                { i = i + 1 },
			                    { swapTemp = stevila[j] },
			                    { stevila[i] = stevila[j] },
			                    { stevila[j] = swapTemp }
                            )
		            }
            },
            {swapTemp = stevila[i+1]},
            {stevila[i+1] = stevila[end]},
            {stevila[end] = swapTemp},
            (i + 1)
        )
        { where
            var swapTemp: integer;
            var pivot: integer;
            var i: integer;
            var j: integer
    };
    fun izpis(besedilo: string): void =
        (
            print('Tvoj program je prestal celoten preizkus!'),
            true
        );

    fun print(besedilo: string): string = besedilo
}
!expected:
Defs [5:1-107:2]
  TypeDef [5:1-5:15]: x
    # typed as: int
    Atom [5:8-5:15]: INT
      # typed as: int
  TypeDef [6:1-6:14]: y
    # typed as: str
    Atom [6:8-6:14]: STR
      # typed as: str
  TypeDef [7:1-7:15]: z
    # typed as: log
    Atom [7:8-7:15]: LOG
      # typed as: log
  TypeDef [8:1-8:17]: i
    # typed as: int
    TypeName [8:8-8:17]: blablabla
      # defined at: [10:1-10:23]
      # typed as: int
  TypeDef [9:1-9:41]: j
    # typed as: ARR(10,ARR(15,ARR(20,int)))
    Array [9:8-9:41]
      # typed as: ARR(10,ARR(15,ARR(20,int)))
      [10]
      Array [9:16-9:41]
        # typed as: ARR(15,ARR(20,int))
        [15]
        Array [9:24-9:41]
          # typed as: ARR(20,int)
          [20]
          TypeName [9:32-9:41]: blablabla
            # defined at: [10:1-10:23]
            # typed as: int
  TypeDef [10:1-10:23]: blablabla
    # typed as: int
    Atom [10:16-10:23]: INT
      # typed as: int
  TypeDef [11:1-11:22]: blabla
    # typed as: int
    TypeName [11:13-11:22]: blablabla
      # defined at: [10:1-10:23]
      # typed as: int
  TypeDef [12:1-12:18]: void
    # typed as: log
    Atom [12:11-12:18]: LOG
      # typed as: log
  VarDef [15:1-15:14]: k
    # typed as: str
    # accessed as: Global: size[4],label[k]
    Atom [15:8-15:14]: STR
      # typed as: str
  VarDef [15:16-15:30]: l
    # typed as: int
    # accessed as: Global: size[4],label[l]
    Atom [15:23-15:30]: INT
      # typed as: int
  VarDef [15:32-15:46]: q
    # typed as: log
    # accessed as: Global: size[4],label[q]
    Atom [15:39-15:46]: LOG
      # typed as: log
  VarDef [15:48-15:61]: m
    # typed as: int
    # accessed as: Global: size[4],label[m]
    TypeName [15:55-15:61]: blabla
      # defined at: [11:1-11:22]
      # typed as: int
  VarDef [15:63-15:94]: n
    # typed as: ARR(69,ARR(420,str))
    # accessed as: Global: size[115920],label[n]
    Array [15:70-15:94]
      # typed as: ARR(69,ARR(420,str))
      [69]
      Array [15:78-15:94]
        # typed as: ARR(420,str)
        [420]
        Atom [15:88-15:94]: STR
          # typed as: str
  FunDef [19:1-107:2]: firstfunction
    # typed as: (int, str, log) -> int
    # framed as: FRAME [firstfunction]: level=1,locals_size=115932,arguments_size=16,parameters_size=16,size=115952
    Parameter [19:19-19:29]: x
      # typed as: int
      # accessed as: Parameter: size[4],offset[4],sl[1]
      Atom [19:22-19:29]: INT
        # typed as: int
    Parameter [19:31-19:40]: y
      # typed as: str
      # accessed as: Parameter: size[4],offset[8],sl[1]
      Atom [19:34-19:40]: STR
        # typed as: str
    Parameter [19:42-19:52]: z
      # typed as: log
      # accessed as: Parameter: size[4],offset[12],sl[1]
      Atom [19:45-19:52]: LOG
        # typed as: log
    Atom [19:55-19:62]: INT
      # typed as: int
    Where [20:5-107:2]
      # typed as: int
      Defs [55:5-106:51]
        VarDef [55:5-55:18]: i
          # typed as: int
          # accessed as: Local: size[4],offset[-4],sl[1]
          Atom [55:11-55:18]: INT
            # typed as: int
        VarDef [56:5-56:19]: a
          # typed as: log
          # accessed as: Local: size[4],offset[-8],sl[1]
          Atom [56:12-56:19]: LOG
            # typed as: log
        VarDef [57:5-57:19]: j
          # typed as: int
          # accessed as: Local: size[4],offset[-12],sl[1]
          Atom [57:12-57:19]: INT
            # typed as: int
        VarDef [58:5-58:36]: p
          # typed as: ARR(69,ARR(420,int))
          # accessed as: Local: size[115920],offset[-115932],sl[1]
          Array [58:12-58:36]
            # typed as: ARR(69,ARR(420,int))
            [69]
            Array [58:20-58:36]
              # typed as: ARR(420,int)
              [420]
              Atom [58:29-58:36]: INT
                # typed as: int
        FunDef [59:5-72:6]: secondfunction
          # typed as: (int) -> int
          # framed as: FRAME [L[0]]: level=2,locals_size=0,arguments_size=0,parameters_size=8,size=8
          Parameter [59:24-59:34]: x
            # typed as: int
            # accessed as: Parameter: size[4],offset[4],sl[2]
            Atom [59:27-59:34]: INT
              # typed as: int
          Atom [59:36-59:43]: INT
            # typed as: int
          Where [60:9-72:6]
            # typed as: int
            Defs [63:13-71:18]
              FunDef [63:13-71:18]: thirdfunction
                # typed as: (str) -> ARR(10,str)
                # framed as: FRAME [L[1]]: level=3,locals_size=44,arguments_size=0,parameters_size=8,size=52
                Parameter [63:31-63:40]: y
                  # typed as: str
                  # accessed as: Parameter: size[4],offset[4],sl[3]
                  Atom [63:34-63:40]: STR
                    # typed as: str
                Array [63:42-63:56]
                  # typed as: ARR(10,str)
                  [10]
                  Atom [63:50-63:56]: STR
                    # typed as: str
                Where [64:17-71:18]
                  # typed as: ARR(10,str)
                  Defs [69:21-70:44]
                    VarDef [69:21-69:37]: xyz
                      # typed as: int
                      # accessed as: Local: size[4],offset[-4],sl[3]
                      Atom [69:30-69:37]: INT
                        # typed as: int
                    VarDef [70:21-70:44]: ss
                      # typed as: ARR(10,str)
                      # accessed as: Local: size[40],offset[-44],sl[3]
                      Array [70:30-70:44]
                        # typed as: ARR(10,str)
                        [10]
                        Atom [70:38-70:44]: STR
                          # typed as: str
                  Block [64:17-67:18]
                    # typed as: ARR(10,str)
                    Binary [65:21-65:32]: ASSIGN
                      # typed as: int
                      Binary [65:22-65:27]: MUL
                        # typed as: int
                        Literal [65:22-65:24]: INT(10)
                          # typed as: int
                        Literal [65:25-65:27]: INT(20)
                          # typed as: int
                      Name [65:28-65:31]: xyz
                        # defined at: [69:21-69:37]
                        # typed as: int
                    Name [66:21-66:23]: ss
                      # defined at: [70:21-70:44]
                      # typed as: ARR(10,str)
            Block [60:9-61:47]
              # typed as: int
              Binary [61:13-61:43]: AND
                # typed as: log
                Binary [61:13-61:31]: GEQ
                  # typed as: log
                  Block [61:13-61:20]
                    # typed as: int
                    Binary [61:14-61:19]: ADD
                      # typed as: int
                      Literal [61:14-61:16]: INT(10)
                        # typed as: int
                      Literal [61:17-61:19]: INT(20)
                        # typed as: int
                  Block [61:24-61:31]
                    # typed as: int
                    Binary [61:25-61:30]: SUB
                      # typed as: int
                      Literal [61:25-61:27]: INT(30)
                        # typed as: int
                      Literal [61:28-61:30]: INT(40)
                        # typed as: int
                Binary [61:34-61:43]: EQ
                  # typed as: log
                  Literal [61:34-61:36]: INT(12)
                    # typed as: int
                  Literal [61:40-61:43]: INT(100)
                    # typed as: int
              Literal [61:45-61:46]: INT(0)
                # typed as: int
        FunDef [73:5-99:6]: partition
          # typed as: (ARR(10,int), int, int) -> int
          # framed as: FRAME [L[2]]: level=2,locals_size=16,arguments_size=0,parameters_size=16,size=24
          Parameter [73:19-73:43]: stevila
            # typed as: ARR(10,int)
            # accessed as: Parameter: size[4],offset[4],sl[2]
            Array [73:28-73:43]
              # typed as: ARR(10,int)
              [10]
              Atom [73:36-73:43]: INT
                # typed as: int
          Parameter [73:45-73:59]: begin
            # typed as: int
            # accessed as: Parameter: size[4],offset[8],sl[2]
            Atom [73:52-73:59]: INT
              # typed as: int
          Parameter [73:61-73:73]: end
            # typed as: int
            # accessed as: Parameter: size[4],offset[12],sl[2]
            Atom [73:66-73:73]: INT
              # typed as: int
          Atom [73:76-73:83]: INT
            # typed as: int
          Where [74:9-99:6]
            # typed as: int
            Defs [95:13-98:27]
              VarDef [95:13-95:34]: swapTemp
                # typed as: int
                # accessed as: Local: size[4],offset[-4],sl[2]
                Atom [95:27-95:34]: INT
                  # typed as: int
              VarDef [96:13-96:31]: pivot
                # typed as: int
                # accessed as: Local: size[4],offset[-8],sl[2]
                Atom [96:24-96:31]: INT
                  # typed as: int
              VarDef [97:13-97:27]: i
                # typed as: int
                # accessed as: Local: size[4],offset[-12],sl[2]
                Atom [97:20-97:27]: INT
                  # typed as: int
              VarDef [98:13-98:27]: j
                # typed as: int
                # accessed as: Local: size[4],offset[-16],sl[2]
                Atom [98:20-98:27]: INT
                  # typed as: int
            Block [74:9-93:10]
              # typed as: int
              Binary [75:13-75:37]: ASSIGN
                # typed as: int
                Name [75:15-75:20]: pivot
                  # defined at: [96:13-96:31]
                  # typed as: int
                Binary [75:23-75:35]: ARR
                  # typed as: int
                  Name [75:23-75:30]: stevila
                    # defined at: [73:19-73:43]
                    # typed as: ARR(10,int)
                  Name [75:31-75:34]: end
                    # defined at: [73:61-73:73]
                    # typed as: int
              Binary [76:13-76:32]: ASSIGN
                # typed as: int
                Name [76:15-76:16]: i
                  # defined at: [97:13-97:27]
                  # typed as: int
                Block [76:19-76:30]
                  # typed as: int
                  Binary [76:20-76:29]: SUB
                    # typed as: int
                    Name [76:20-76:25]: begin
                      # defined at: [73:45-73:59]
                      # typed as: int
                    Literal [76:28-76:29]: INT(1)
                      # typed as: int
              For [77:13-88:14]
                # typed as: void
                Name [78:21-78:22]: j
                  # defined at: [98:13-98:27]
                  # typed as: int
                Name [78:25-78:30]: begin
                  # defined at: [73:45-73:59]
                  # typed as: int
                Name [78:32-78:35]: end
                  # defined at: [73:61-73:73]
                  # typed as: int
                Binary [78:37-78:48]: ASSIGN
                  # typed as: int
                  Name [78:38-78:39]: j
                    # defined at: [98:13-98:27]
                    # typed as: int
                  Binary [78:42-78:47]: ADD
                    # typed as: int
                    Name [78:42-78:43]: j
                      # defined at: [98:13-98:27]
                      # typed as: int
                    Literal [78:46-78:47]: INT(1)
                      # typed as: int
                IfThenElse [79:21-87:22]
                  # typed as: void
                  Binary [80:28-80:47]: LEQ
                    # typed as: log
                    Binary [80:28-80:38]: ARR
                      # typed as: int
                      Name [80:28-80:35]: stevila
                        # defined at: [73:19-73:43]
                        # typed as: ARR(10,int)
                      Name [80:36-80:37]: j
                        # defined at: [98:13-98:27]
                        # typed as: int
                    Name [80:42-80:47]: pivot
                      # defined at: [96:13-96:31]
                      # typed as: int
                  Block [81:29-86:30]
                    # typed as: int
                    Binary [82:33-82:46]: ASSIGN
                      # typed as: int
                      Name [82:35-82:36]: i
                        # defined at: [97:13-97:27]
                        # typed as: int
                      Binary [82:39-82:44]: ADD
                        # typed as: int
                        Name [82:39-82:40]: i
                          # defined at: [97:13-97:27]
                          # typed as: int
                        Literal [82:43-82:44]: INT(1)
                          # typed as: int
                    Binary [83:33-83:58]: ASSIGN
                      # typed as: int
                      Name [83:35-83:43]: swapTemp
                        # defined at: [95:13-95:34]
                        # typed as: int
                      Binary [83:46-83:56]: ARR
                        # typed as: int
                        Name [83:46-83:53]: stevila
                          # defined at: [73:19-73:43]
                          # typed as: ARR(10,int)
                        Name [83:54-83:55]: j
                          # defined at: [98:13-98:27]
                          # typed as: int
                    Binary [84:33-84:60]: ASSIGN
                      # typed as: int
                      Binary [84:35-84:45]: ARR
                        # typed as: int
                        Name [84:35-84:42]: stevila
                          # defined at: [73:19-73:43]
                          # typed as: ARR(10,int)
                        Name [84:43-84:44]: i
                          # defined at: [97:13-97:27]
                          # typed as: int
                      Binary [84:48-84:58]: ARR
                        # typed as: int
                        Name [84:48-84:55]: stevila
                          # defined at: [73:19-73:43]
                          # typed as: ARR(10,int)
                        Name [84:56-84:57]: j
                          # defined at: [98:13-98:27]
                          # typed as: int
                    Binary [85:33-85:58]: ASSIGN
                      # typed as: int
                      Binary [85:35-85:45]: ARR
                        # typed as: int
                        Name [85:35-85:42]: stevila
                          # defined at: [73:19-73:43]
                          # typed as: ARR(10,int)
                        Name [85:43-85:44]: j
                          # defined at: [98:13-98:27]
                          # typed as: int
                      Name [85:48-85:56]: swapTemp
                        # defined at: [95:13-95:34]
                        # typed as: int
              Binary [89:13-89:38]: ASSIGN
                # typed as: int
                Name [89:14-89:22]: swapTemp
                  # defined at: [95:13-95:34]
                  # typed as: int
                Binary [89:25-89:37]: ARR
                  # typed as: int
                  Name [89:25-89:32]: stevila
                    # defined at: [73:19-73:43]
                    # typed as: ARR(10,int)
                  Binary [89:33-89:36]: ADD
                    # typed as: int
                    Name [89:33-89:34]: i
                      # defined at: [97:13-97:27]
                      # typed as: int
                    Literal [89:35-89:36]: INT(1)
                      # typed as: int
              Binary [90:13-90:42]: ASSIGN
                # typed as: int
                Binary [90:14-90:26]: ARR
                  # typed as: int
                  Name [90:14-90:21]: stevila
                    # defined at: [73:19-73:43]
                    # typed as: ARR(10,int)
                  Binary [90:22-90:25]: ADD
                    # typed as: int
                    Name [90:22-90:23]: i
                      # defined at: [97:13-97:27]
                      # typed as: int
                    Literal [90:24-90:25]: INT(1)
                      # typed as: int
                Binary [90:29-90:41]: ARR
                  # typed as: int
                  Name [90:29-90:36]: stevila
                    # defined at: [73:19-73:43]
                    # typed as: ARR(10,int)
                  Name [90:37-90:40]: end
                    # defined at: [73:61-73:73]
                    # typed as: int
              Binary [91:13-91:38]: ASSIGN
                # typed as: int
                Binary [91:14-91:26]: ARR
                  # typed as: int
                  Name [91:14-91:21]: stevila
                    # defined at: [73:19-73:43]
                    # typed as: ARR(10,int)
                  Name [91:22-91:25]: end
                    # defined at: [73:61-73:73]
                    # typed as: int
                Name [91:29-91:37]: swapTemp
                  # defined at: [95:13-95:34]
                  # typed as: int
              Block [92:13-92:20]
                # typed as: int
                Binary [92:14-92:19]: ADD
                  # typed as: int
                  Name [92:14-92:15]: i
                    # defined at: [97:13-97:27]
                    # typed as: int
                  Literal [92:18-92:19]: INT(1)
                    # typed as: int
        FunDef [100:5-104:10]: izpis
          # typed as: (str) -> log
          # framed as: FRAME [L[3]]: level=2,locals_size=0,arguments_size=8,parameters_size=8,size=12
          Parameter [100:15-100:31]: besedilo
            # typed as: str
            # accessed as: Parameter: size[4],offset[4],sl[2]
            Atom [100:25-100:31]: STR
              # typed as: str
          TypeName [100:34-100:38]: void
            # defined at: [12:1-12:18]
            # typed as: log
          Block [101:9-104:10]
            # typed as: log
            Call [102:13-102:63]: print
              # defined at: [106:5-106:51]
              # typed as: str
              Literal [102:19-102:62]: STR(Tvoj program je prestal celoten preizkus!)
                # typed as: str
            Literal [103:13-103:17]: LOG(true)
              # typed as: log
        FunDef [106:5-106:51]: print
          # typed as: (str) -> str
          # framed as: FRAME [L[4]]: level=2,locals_size=0,arguments_size=0,parameters_size=8,size=8
          Parameter [106:15-106:31]: besedilo
            # typed as: str
            # accessed as: Parameter: size[4],offset[4],sl[2]
            Atom [106:25-106:31]: STR
              # typed as: str
          Atom [106:34-106:40]: STR
            # typed as: str
          Name [106:43-106:51]: besedilo
            # defined at: [106:15-106:31]
            # typed as: str
      Block [20:5-53:6]
        # typed as: int
        For [20:7-29:15]
          # typed as: void
          Name [20:12-20:13]: i
            # defined at: [55:5-55:18]
            # typed as: int
          Literal [20:16-20:17]: INT(1)
            # typed as: int
          Literal [20:19-20:22]: INT(100)
            # typed as: int
          Binary [20:24-20:35]: ASSIGN
            # typed as: int
            Name [20:25-20:26]: i
              # defined at: [55:5-55:18]
              # typed as: int
            Binary [20:29-20:34]: ADD
              # typed as: int
              Name [20:29-20:30]: i
                # defined at: [55:5-55:18]
                # typed as: int
              Literal [20:33-20:34]: INT(1)
                # typed as: int
          While [21:9-29:10]
            # typed as: void
            Binary [22:19-22:28]: GT
              # typed as: log
              Name [22:19-22:20]: j
                # defined at: [57:5-57:19]
                # typed as: int
              Block [22:23-22:28]
                # typed as: int
                Unary [22:24-22:27]: SUB
                  # typed as: int
                  Literal [22:25-22:27]: INT(10)
                    # typed as: int
            IfThenElse [22:30-27:14]
              # typed as: void
              Binary [23:20-23:86]: NEQ
                # typed as: log
                Binary [23:20-23:79]: SUB
                  # typed as: int
                  Binary [23:20-23:60]: SUB
                    # typed as: int
                    Binary [23:20-23:44]: ADD
                      # typed as: int
                      Binary [23:20-23:39]: ADD
                        # typed as: int
                        Binary [23:20-23:34]: ADD
                          # typed as: int
                          Binary [23:20-23:29]: MUL
                            # typed as: int
                            Name [23:20-23:21]: j
                              # defined at: [57:5-57:19]
                              # typed as: int
                            Block [23:24-23:29]
                              # typed as: int
                              Unary [23:25-23:28]: SUB
                                # typed as: int
                                Literal [23:26-23:28]: INT(10)
                                  # typed as: int
                          Literal [23:32-23:34]: INT(20)
                            # typed as: int
                        Literal [23:37-23:39]: INT(30)
                          # typed as: int
                      Literal [23:42-23:44]: INT(10)
                        # typed as: int
                    Binary [23:47-23:60]: MOD
                      # typed as: int
                      Binary [23:47-23:54]: DIV
                        # typed as: int
                        Literal [23:47-23:49]: INT(40)
                          # typed as: int
                        Literal [23:52-23:54]: INT(50)
                          # typed as: int
                      Literal [23:57-23:60]: INT(100)
                        # typed as: int
                  Block [23:62-23:79]
                    # typed as: int
                    Unary [23:63-23:78]: SUB
                      # typed as: int
                      Block [23:64-23:78]
                        # typed as: int
                        Unary [23:65-23:77]: SUB
                          # typed as: int
                          Block [23:66-23:77]
                            # typed as: int
                            Unary [23:67-23:76]: SUB
                              # typed as: int
                              Block [23:68-23:76]
                                # typed as: int
                                Unary [23:69-23:75]: SUB
                                  # typed as: int
                                  Block [23:70-23:75]
                                    # typed as: int
                                    Unary [23:71-23:74]: ADD
                                      # typed as: int
                                      Literal [23:72-23:74]: INT(10)
                                        # typed as: int
                Literal [23:83-23:86]: INT(123)
                  # typed as: int
              Literal [24:21-24:23]: INT(12)
                # typed as: int
              Call [26:21-26:53]: firstfunction
                # defined at: [19:1-107:2]
                # typed as: int
                Literal [26:35-26:38]: INT(123)
                  # typed as: int
                Literal [26:40-26:46]: STR(bruh)
                  # typed as: str
                Literal [26:48-26:52]: LOG(true)
                  # typed as: log
        Binary [30:9-30:73]: OR
          # typed as: log
          Binary [30:9-30:32]: OR
            # typed as: log
            Binary [30:9-30:21]: AND
              # typed as: log
              Literal [30:9-30:13]: LOG(true)
                # typed as: log
              Literal [30:16-30:21]: LOG(false)
                # typed as: log
            Binary [30:24-30:32]: AND
              # typed as: log
              Name [30:24-30:25]: z
                # defined at: [19:42-19:52]
                # typed as: log
              Literal [30:28-30:32]: LOG(true)
                # typed as: log
          Block [30:35-30:73]
            # typed as: log
            Binary [30:36-30:72]: GEQ
              # typed as: log
              Literal [30:36-30:37]: INT(1)
                # typed as: int
              Call [30:41-30:72]: firstfunction
                # defined at: [19:1-107:2]
                # typed as: int
                Name [30:55-30:56]: i
                  # defined at: [55:5-55:18]
                  # typed as: int
                Literal [30:58-30:65]: STR(monke)
                  # typed as: str
                Literal [30:67-30:71]: LOG(true)
                  # typed as: log
        IfThenElse [31:9-35:10]
          # typed as: void
          Binary [32:16-32:61]: GT
            # typed as: log
            Literal [32:16-32:18]: INT(16)
              # typed as: int
            Block [32:21-32:61]
              # typed as: int
              Binary [32:22-32:60]: ADD
                # typed as: int
                Binary [32:22-32:54]: ADD
                  # typed as: int
                  Binary [32:22-32:49]: SUB
                    # typed as: int
                    Binary [32:22-32:44]: ADD
                      # typed as: int
                      Binary [32:22-32:29]: SUB
                        # typed as: int
                        Literal [32:22-32:24]: INT(14)
                          # typed as: int
                        Literal [32:27-32:29]: INT(10)
                          # typed as: int
                      Binary [32:32-32:44]: DIV
                        # typed as: int
                        Binary [32:32-32:39]: MUL
                          # typed as: int
                          Literal [32:32-32:34]: INT(10)
                            # typed as: int
                          Literal [32:37-32:39]: INT(10)
                            # typed as: int
                        Literal [32:42-32:44]: INT(10)
                          # typed as: int
                    Literal [32:47-32:49]: INT(10)
                      # typed as: int
                  Literal [32:52-32:54]: INT(10)
                    # typed as: int
                Unary [32:57-32:60]: SUB
                  # typed as: int
                  Literal [32:58-32:60]: INT(10)
                    # typed as: int
          Call [34:21-34:64]: firstfunction
            # defined at: [19:1-107:2]
            # typed as: int
            Name [34:35-34:36]: i
              # defined at: [55:5-55:18]
              # typed as: int
            Literal [34:38-34:56]: STR(danes je lep dan)
              # typed as: str
            Literal [34:58-34:63]: LOG(false)
              # typed as: log
        For [37:9-51:10]
          # typed as: void
          Name [38:13-38:14]: i
            # defined at: [55:5-55:18]
            # typed as: int
          Literal [38:17-38:18]: INT(1)
            # typed as: int
          Literal [38:20-38:22]: INT(10)
            # typed as: int
          Literal [38:24-38:26]: INT(12)
            # typed as: int
          Block [38:29-50:15]
            # typed as: int
            While [39:13-41:14]
              # typed as: void
              Binary [40:23-40:27]: LT
                # typed as: log
                Name [40:23-40:24]: j
                  # defined at: [57:5-57:19]
                  # typed as: int
                Literal [40:25-40:27]: INT(10)
                  # typed as: int
              Block [40:29-40:37]
                # typed as: log
                Binary [40:30-40:36]: LT
                  # typed as: log
                  Name [40:30-40:31]: j
                    # defined at: [57:5-57:19]
                    # typed as: int
                  Literal [40:34-40:36]: INT(10)
                    # typed as: int
            IfThenElse [42:13-44:14]
              # typed as: void
              Binary [43:20-43:33]: GEQ
                # typed as: log
                Binary [43:20-43:26]: MUL
                  # typed as: int
                  Name [43:20-43:21]: j
                    # defined at: [57:5-57:19]
                    # typed as: int
                  Literal [43:24-43:26]: INT(10)
                    # typed as: int
                Literal [43:30-43:33]: INT(100)
                  # typed as: int
              Binary [43:39-43:57]: ASSIGN
                # typed as: log
                Name [43:40-43:41]: a
                  # defined at: [56:5-56:19]
                  # typed as: log
                Binary [43:44-43:56]: EQ
                  # typed as: log
                  Binary [43:44-43:50]: SUB
                    # typed as: int
                    Name [43:44-43:45]: j
                      # defined at: [57:5-57:19]
                      # typed as: int
                    Literal [43:48-43:50]: INT(10)
                      # typed as: int
                  Literal [43:54-43:56]: INT(12)
                    # typed as: int
              Literal [43:63-43:64]: INT(0)
                # typed as: int
            For [45:13-47:14]
              # typed as: void
              Name [46:21-46:22]: j
                # defined at: [57:5-57:19]
                # typed as: int
              Literal [46:25-46:26]: INT(1)
                # typed as: int
              Literal [46:28-46:30]: INT(12)
                # typed as: int
              Binary [46:32-46:39]: ASSIGN
                # typed as: int
                Name [46:33-46:34]: j
                  # defined at: [57:5-57:19]
                  # typed as: int
                Binary [46:35-46:38]: MUL
                  # typed as: int
                  Name [46:35-46:36]: j
                    # defined at: [57:5-57:19]
                    # typed as: int
                  Literal [46:37-46:38]: INT(2)
                    # typed as: int
              Call [46:41-46:77]: firstfunction
                # defined at: [19:1-107:2]
                # typed as: int
                Literal [46:55-46:57]: INT(12)
                  # typed as: int
                Literal [46:59-46:70]: STR(blablabla)
                  # typed as: str
                Literal [46:72-46:76]: LOG(true)
                  # typed as: log
            Binary [48:13-50:14]: ASSIGN
              # typed as: int
              Binary [49:17-49:40]: ARR
                # typed as: int
                Binary [49:17-49:30]: ARR
                  # typed as: ARR(420,int)
                  Name [49:17-49:18]: p
                    # defined at: [58:5-58:36]
                    # typed as: ARR(69,ARR(420,int))
                  Binary [49:19-49:29]: SUB
                    # typed as: int
                    Binary [49:19-49:23]: ADD
                      # typed as: int
                      Name [49:19-49:20]: i
                        # defined at: [55:5-55:18]
                        # typed as: int
                      Literal [49:21-49:23]: INT(10)
                        # typed as: int
                    Binary [49:24-49:29]: MUL
                      # typed as: int
                      Literal [49:24-49:26]: INT(20)
                        # typed as: int
                      Literal [49:27-49:29]: INT(30)
                        # typed as: int
                Binary [49:31-49:39]: SUB
                  # typed as: int
                  Binary [49:31-49:35]: SUB
                    # typed as: int
                    Name [49:31-49:32]: j
                      # defined at: [57:5-57:19]
                      # typed as: int
                    Literal [49:33-49:35]: INT(10)
                      # typed as: int
                  Literal [49:36-49:39]: INT(203)
                    # typed as: int
              Literal [49:43-49:45]: INT(12)
                # typed as: int
        Literal [52:9-52:11]: INT(12)
          # typed as: int
!end