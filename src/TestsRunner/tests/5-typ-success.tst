!compiler_flags: --dump TYP --exec TYP

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
    Parameter [1:10-1:25]: args
      # typed as: ARR(10,int)
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
    Parameter [12:9-12:14]: n
      # typed as: int
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
    Parameter [1:10-1:25]: args
      # typed as: ARR(10,int)
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
          TypeName [9:13-9:16]: int
            # defined at: [13:1-13:16]
            # typed as: int
        VarDef [10:5-10:14]: i
          # typed as: int
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
    Parameter [1:10-1:25]: args
      # typed as: ARR(10,int)
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
          TypeName [11:13-11:16]: int
            # defined at: [42:1-42:16]
            # typed as: int
        VarDef [12:5-12:16]: min
          # typed as: int
          TypeName [12:13-12:16]: int
            # defined at: [42:1-42:16]
            # typed as: int
        VarDef [13:5-13:20]: razlika
          # typed as: int
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
    Parameter [16:14-16:30]: array
      # typed as: ARR(10,int)
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
          TypeName [25:13-25:16]: int
            # defined at: [42:1-42:16]
            # typed as: int
        VarDef [26:5-26:14]: i
          # typed as: int
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
    Parameter [29:15-29:31]: array
      # typed as: ARR(10,int)
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
          TypeName [38:13-38:16]: int
            # defined at: [42:1-42:16]
            # typed as: int
        VarDef [39:5-39:14]: i
          # typed as: int
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
    Parameter [1:10-1:26]: args
      # typed as: ARR(100,int)
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
          Array [24:19-24:29]
            # typed as: ARR(10,int)
            [10]
            TypeName [24:26-24:29]: int
              # defined at: [57:1-57:16]
              # typed as: int
        VarDef [25:5-25:14]: i
          # typed as: int
          TypeName [25:11-25:14]: int
            # defined at: [57:1-57:16]
            # typed as: int
        VarDef [26:5-26:16]: max
          # typed as: int
          TypeName [26:13-26:16]: int
            # defined at: [57:1-57:16]
            # typed as: int
        VarDef [27:5-27:22]: max_index
          # typed as: int
          TypeName [27:19-27:22]: int
            # defined at: [57:1-57:16]
            # typed as: int
        FunDef [30:5-41:6]: najvecji
          # typed as: (ARR(10,int)) -> int
          Parameter [30:18-30:34]: array
            # typed as: ARR(10,int)
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
                TypeName [39:15-39:18]: int
                  # defined at: [57:1-57:16]
                  # typed as: int
              VarDef [40:9-40:20]: max
                # typed as: int
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
          Parameter [43:15-43:31]: array
            # typed as: ARR(10,int)
            Array [43:21-43:31]
              # typed as: ARR(10,int)
              [10]
              TypeName [43:28-43:31]: int
                # defined at: [57:1-57:16]
                # typed as: int
          Parameter [43:33-43:42]: value
            # typed as: int
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
                TypeName [52:15-52:18]: int
                  # defined at: [57:1-57:16]
                  # typed as: int
              VarDef [53:9-53:28]: returnValue
                # typed as: int
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
    Parameter [1:10-1:25]: args
      # typed as: ARR(10,int)
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
          TypeName [17:21-17:24]: str
            # defined at: [51:1-51:15]
            # typed as: str
        VarDef [18:5-18:27]: deljivo
          # typed as: ARR(2,log)
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
    Parameter [21:13-21:18]: n
      # typed as: int
      TypeName [21:15-21:18]: int
        # defined at: [50:1-50:16]
        # typed as: int
    Parameter [21:20-21:25]: m
      # typed as: int
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
    Parameter [28:15-28:20]: n
      # typed as: int
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
    Parameter [29:15-29:20]: n
      # typed as: int
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
    Parameter [31:16-31:32]: array
      # typed as: ARR(10,int)
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
          Array [46:21-46:31]
            # typed as: ARR(2,log)
            [2]
            TypeName [46:27-46:31]: bool
              # defined at: [52:1-52:17]
              # typed as: log
        VarDef [47:5-47:14]: i
          # typed as: int
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