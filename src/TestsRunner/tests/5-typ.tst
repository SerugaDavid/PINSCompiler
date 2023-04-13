!compiler_flags: --dump TYP --exec TYP

!code: 
var x : integer;
fun f ( y : integer ) : integer = x + y
!expected:
Defs [1:1-2:40]
  VarDef [1:1-1:16]: x
    # typed as: int
    Atom [1:9-1:16]: INT
      # typed as: int
  FunDef [2:1-2:40]: f
    # typed as: (int) -> int
    Parameter [2:9-2:20]: y
      # typed as: int
      Atom [2:13-2:20]: INT
        # typed as: int
    Atom [2:25-2:32]: INT
      # typed as: int
    Binary [2:35-2:40]: ADD
      # typed as: int
      Name [2:35-2:36]: x
        # defined at: [1:1-1:16]
        # typed as: int
      Name [2:39-2:40]: y
        # defined at: [2:9-2:20]
        # typed as: int
!end


!code: 
typ t1 : t2;
typ t2 : t3;
typ t3 : logical
!expected:
Defs [1:1-3:17]
  TypeDef [1:1-1:12]: t1
    # typed as: log
    TypeName [1:10-1:12]: t2
      # defined at: [2:1-2:12]
      # typed as: log
  TypeDef [2:1-2:12]: t2
    # typed as: log
    TypeName [2:10-2:12]: t3
      # defined at: [3:1-3:17]
      # typed as: log
  TypeDef [3:1-3:17]: t3
    # typed as: log
    Atom [3:10-3:17]: LOG
      # typed as: log
!end

!code:
var x: integer;
var y: logical;
var z: string;
typ MyInt: integer;
typ MyLog: logical;
typ MyStr: string
!expected:
Defs [1:1-6:18]
  VarDef [1:1-1:15]: x
    # typed as: int
    Atom [1:8-1:15]: INT
      # typed as: int
  VarDef [2:1-2:15]: y
    # typed as: log
    Atom [2:8-2:15]: LOG
      # typed as: log
  VarDef [3:1-3:14]: z
    # typed as: str
    Atom [3:8-3:14]: STR
      # typed as: str
  TypeDef [4:1-4:19]: MyInt
    # typed as: int
    Atom [4:12-4:19]: INT
      # typed as: int
  TypeDef [5:1-5:19]: MyLog
    # typed as: log
    Atom [5:12-5:19]: LOG
      # typed as: log
  TypeDef [6:1-6:18]: MyStr
    # typed as: str
    Atom [6:12-6:18]: STR
      # typed as: str
!end


!code:
var izraz1 : integer;
fun main ( x : integer ) : integer = (
    {izraz1 =  (1 + 2 + 3) / (4 + 5 + 6)}
)
!expected:
Defs [1:1-4:2]
  VarDef [1:1-1:21]: izraz1
    # typed as: int
    Atom [1:14-1:21]: INT
      # typed as: int
  FunDef [2:1-4:2]: main
    # typed as: (int) -> int
    Parameter [2:12-2:23]: x
      # typed as: int
      Atom [2:16-2:23]: INT
        # typed as: int
    Atom [2:28-2:35]: INT
      # typed as: int
    Block [2:38-4:2]
      # typed as: int
      Binary [3:5-3:42]: ASSIGN
        # typed as: int
        Name [3:6-3:12]: izraz1
          # defined at: [1:1-1:21]
          # typed as: int
        Binary [3:16-3:41]: DIV
          # typed as: int
          Block [3:16-3:27]
            # typed as: int
            Binary [3:17-3:26]: ADD
              # typed as: int
              Binary [3:17-3:22]: ADD
                # typed as: int
                Literal [3:17-3:18]: INT(1)
                  # typed as: int
                Literal [3:21-3:22]: INT(2)
                  # typed as: int
              Literal [3:25-3:26]: INT(3)
                # typed as: int
          Block [3:30-3:41]
            # typed as: int
            Binary [3:31-3:40]: ADD
              # typed as: int
              Binary [3:31-3:36]: ADD
                # typed as: int
                Literal [3:31-3:32]: INT(4)
                  # typed as: int
                Literal [3:35-3:36]: INT(5)
                  # typed as: int
              Literal [3:39-3:40]: INT(6)
                # typed as: int
!end


!code:
fun swap(tab : arr[20] integer, i : integer, j : integer) : integer = (
    {tmp = tab[i]},
    {tab[i] = tab[j]},
    {tab[j] = tmp}
) {where var tmp : integer}
!expected:
Defs [1:1-5:28]
  FunDef [1:1-5:28]: swap
    # typed as: (ARR(20,int), int, int) -> int
    Parameter [1:10-1:31]: tab
      # typed as: ARR(20,int)
      Array [1:16-1:31]
        # typed as: ARR(20,int)
        [20]
        Atom [1:24-1:31]: INT
          # typed as: int
    Parameter [1:33-1:44]: i
      # typed as: int
      Atom [1:37-1:44]: INT
        # typed as: int
    Parameter [1:46-1:57]: j
      # typed as: int
      Atom [1:50-1:57]: INT
        # typed as: int
    Atom [1:61-1:68]: INT
      # typed as: int
    Where [1:71-5:28]
      # typed as: int
      Defs [5:10-5:27]
        VarDef [5:10-5:27]: tmp
          # typed as: int
          Atom [5:20-5:27]: INT
            # typed as: int
      Block [1:71-5:2]
        # typed as: int
        Binary [2:5-2:19]: ASSIGN
          # typed as: int
          Name [2:6-2:9]: tmp
            # defined at: [5:10-5:27]
            # typed as: int
          Binary [2:12-2:18]: ARR
            # typed as: int
            Name [2:12-2:15]: tab
              # defined at: [1:10-1:31]
              # typed as: ARR(20,int)
            Name [2:16-2:17]: i
              # defined at: [1:33-1:44]
              # typed as: int
        Binary [3:5-3:22]: ASSIGN
          # typed as: int
          Binary [3:6-3:12]: ARR
            # typed as: int
            Name [3:6-3:9]: tab
              # defined at: [1:10-1:31]
              # typed as: ARR(20,int)
            Name [3:10-3:11]: i
              # defined at: [1:33-1:44]
              # typed as: int
          Binary [3:15-3:21]: ARR
            # typed as: int
            Name [3:15-3:18]: tab
              # defined at: [1:10-1:31]
              # typed as: ARR(20,int)
            Name [3:19-3:20]: j
              # defined at: [1:46-1:57]
              # typed as: int
        Binary [4:5-4:19]: ASSIGN
          # typed as: int
          Binary [4:6-4:12]: ARR
            # typed as: int
            Name [4:6-4:9]: tab
              # defined at: [1:10-1:31]
              # typed as: ARR(20,int)
            Name [4:10-4:11]: j
              # defined at: [1:46-1:57]
              # typed as: int
          Name [4:15-4:18]: tmp
            # defined at: [5:10-5:27]
            # typed as: int
!end


!code:
fun main(x: integer): integer = (
  ( {for i = 0, 10, 1:
      {if i % 2 == 0 then
          print('1')
      }
    },
    12
  ) {where var i: integer}
);

fun print(s : string): integer = 0
!expected:
Defs [1:1-11:35]
  FunDef [1:1-9:2]: main
    # typed as: (int) -> int
    Parameter [1:10-1:20]: x
      # typed as: int
      Atom [1:13-1:20]: INT
        # typed as: int
    Atom [1:23-1:30]: INT
      # typed as: int
    Block [1:33-9:2]
      # typed as: int
      Where [2:3-8:27]
        # typed as: int
        Defs [8:12-8:26]
          VarDef [8:12-8:26]: i
            # typed as: int
            Atom [8:19-8:26]: INT
              # typed as: int
        Block [2:3-8:4]
          # typed as: int
          For [2:5-6:6]
            # typed as: void
            Name [2:10-2:11]: i
              # defined at: [8:12-8:26]
              # typed as: int
            Literal [2:14-2:15]: INT(0)
              # typed as: int
            Literal [2:17-2:19]: INT(10)
              # typed as: int
            Literal [2:21-2:22]: INT(1)
              # typed as: int
            IfThenElse [3:7-5:8]
              # typed as: void
              Binary [3:11-3:21]: EQ
                # typed as: log
                Binary [3:11-3:16]: MOD
                  # typed as: int
                  Name [3:11-3:12]: i
                    # defined at: [8:12-8:26]
                    # typed as: int
                  Literal [3:15-3:16]: INT(2)
                    # typed as: int
                Literal [3:20-3:21]: INT(0)
                  # typed as: int
              Call [4:11-4:21]: print
                # defined at: [11:1-11:35]
                # typed as: int
                Literal [4:17-4:20]: STR(1)
                  # typed as: str
          Literal [7:5-7:7]: INT(12)
            # typed as: int
  FunDef [11:1-11:35]: print
    # typed as: (str) -> int
    Parameter [11:11-11:21]: s
      # typed as: str
      Atom [11:15-11:21]: STR
        # typed as: str
    Atom [11:24-11:31]: INT
      # typed as: int
    Literal [11:34-11:35]: INT(0)
      # typed as: int
!end

!code:
fun main( x: integer ) : integer = (
    { if 1 == 1 & 2 == 2 | 3 == 3 | 4 == 4
    then
        (
            { for i = 0, 10, 1:
                (
                    {k = k + i}
                ) { where
                    var k : integer
                }
            }
        ) { where
            var k : integer;
            var i : integer
        }
    else
        (
            { while k < 20:
                (
                    {k = k + 1}
                )
            }
        ) { where
            var k : integer
        }
    }, 0
)
!expected:
Defs [1:1-27:2]
  FunDef [1:1-27:2]: main
    # typed as: (int) -> int
    Parameter [1:11-1:21]: x
      # typed as: int
      Atom [1:14-1:21]: INT
        # typed as: int
    Atom [1:26-1:33]: INT
      # typed as: int
    Block [1:36-27:2]
      # typed as: int
      IfThenElse [2:5-26:6]
        # typed as: void
        Binary [2:10-2:43]: OR
          # typed as: log
          Binary [2:10-2:34]: OR
            # typed as: log
            Binary [2:10-2:25]: AND
              # typed as: log
              Binary [2:10-2:16]: EQ
                # typed as: log
                Literal [2:10-2:11]: INT(1)
                  # typed as: int
                Literal [2:15-2:16]: INT(1)
                  # typed as: int
              Binary [2:19-2:25]: EQ
                # typed as: log
                Literal [2:19-2:20]: INT(2)
                  # typed as: int
                Literal [2:24-2:25]: INT(2)
                  # typed as: int
            Binary [2:28-2:34]: EQ
              # typed as: log
              Literal [2:28-2:29]: INT(3)
                # typed as: int
              Literal [2:33-2:34]: INT(3)
                # typed as: int
          Binary [2:37-2:43]: EQ
            # typed as: log
            Literal [2:37-2:38]: INT(4)
              # typed as: int
            Literal [2:42-2:43]: INT(4)
              # typed as: int
        Where [4:9-15:10]
          # typed as: void
          Defs [13:13-14:28]
            VarDef [13:13-13:28]: k
              # typed as: int
              Atom [13:21-13:28]: INT
                # typed as: int
            VarDef [14:13-14:28]: i
              # typed as: int
              Atom [14:21-14:28]: INT
                # typed as: int
          Block [4:9-12:10]
            # typed as: void
            For [5:13-11:14]
              # typed as: void
              Name [5:19-5:20]: i
                # defined at: [14:13-14:28]
                # typed as: int
              Literal [5:23-5:24]: INT(0)
                # typed as: int
              Literal [5:26-5:28]: INT(10)
                # typed as: int
              Literal [5:30-5:31]: INT(1)
                # typed as: int
              Where [6:17-10:18]
                # typed as: int
                Defs [9:21-9:36]
                  VarDef [9:21-9:36]: k
                    # typed as: int
                    Atom [9:29-9:36]: INT
                      # typed as: int
                Block [6:17-8:18]
                  # typed as: int
                  Binary [7:21-7:32]: ASSIGN
                    # typed as: int
                    Name [7:22-7:23]: k
                      # defined at: [9:21-9:36]
                      # typed as: int
                    Binary [7:26-7:31]: ADD
                      # typed as: int
                      Name [7:26-7:27]: k
                        # defined at: [9:21-9:36]
                        # typed as: int
                      Name [7:30-7:31]: i
                        # defined at: [14:13-14:28]
                        # typed as: int
        Where [17:9-25:10]
          # typed as: void
          Defs [24:13-24:28]
            VarDef [24:13-24:28]: k
              # typed as: int
              Atom [24:21-24:28]: INT
                # typed as: int
          Block [17:9-23:10]
            # typed as: void
            While [18:13-22:14]
              # typed as: void
              Binary [18:21-18:27]: LT
                # typed as: log
                Name [18:21-18:22]: k
                  # defined at: [24:13-24:28]
                  # typed as: int
                Literal [18:25-18:27]: INT(20)
                  # typed as: int
              Block [19:17-21:18]
                # typed as: int
                Binary [20:21-20:32]: ASSIGN
                  # typed as: int
                  Name [20:22-20:23]: k
                    # defined at: [24:13-24:28]
                    # typed as: int
                  Binary [20:26-20:31]: ADD
                    # typed as: int
                    Name [20:26-20:27]: k
                      # defined at: [24:13-24:28]
                      # typed as: int
                    Literal [20:30-20:31]: INT(1)
                      # typed as: int
      Literal [26:8-26:9]: INT(0)
        # typed as: int
!end

!code:
# quicksort
fun partition (tab: arr[100] integer, low: integer, high: integer) : integer = (
    {pivot = tab[high]},
    {i = (low - 1)},
    ({for j = low, high, i:
        { if tab[j] <= pivot then (

            {i = i+1},
            {tmp = tab[i]},
            {tab[j] = tab[j]},
            {tab[j] = tmp}

        ) {where var tmp: integer}}
    }) {where var j: integer},

    {tmp = tab[i+1]},
    {tab[i+1] = tab[high]},
    {tab[high] = tmp}
) {
    where var pivot: integer; 
    var i: integer; 
    var tmp: integer
};

fun sort(tab: arr[100] integer, low: integer, high: integer) : integer = (
    ({ if low < high then (
        { pi = partition(tab, low, high)},
        sort(tab, low, pi-1),
        sort(tab, pi+1, high)
        ) {where var pi: integer}
    },0)
)
!expected:
Defs [2:1-32:2]
  FunDef [2:1-23:2]: partition
    # typed as: (ARR(100,int), int, int) -> int
    Parameter [2:16-2:37]: tab
      # typed as: ARR(100,int)
      Array [2:21-2:37]
        # typed as: ARR(100,int)
        [100]
        Atom [2:30-2:37]: INT
          # typed as: int
    Parameter [2:39-2:51]: low
      # typed as: int
      Atom [2:44-2:51]: INT
        # typed as: int
    Parameter [2:53-2:66]: high
      # typed as: int
      Atom [2:59-2:66]: INT
        # typed as: int
    Atom [2:70-2:77]: INT
      # typed as: int
    Where [2:80-23:2]
      # typed as: int
      Defs [20:11-22:21]
        VarDef [20:11-20:29]: pivot
          # typed as: int
          Atom [20:22-20:29]: INT
            # typed as: int
        VarDef [21:5-21:19]: i
          # typed as: int
          Atom [21:12-21:19]: INT
            # typed as: int
        VarDef [22:5-22:21]: tmp
          # typed as: int
          Atom [22:14-22:21]: INT
            # typed as: int
      Block [2:80-19:2]
        # typed as: int
        Binary [3:5-3:24]: ASSIGN
          # typed as: int
          Name [3:6-3:11]: pivot
            # defined at: [20:11-20:29]
            # typed as: int
          Binary [3:14-3:23]: ARR
            # typed as: int
            Name [3:14-3:17]: tab
              # defined at: [2:16-2:37]
              # typed as: ARR(100,int)
            Name [3:18-3:22]: high
              # defined at: [2:53-2:66]
              # typed as: int
        Binary [4:5-4:20]: ASSIGN
          # typed as: int
          Name [4:6-4:7]: i
            # defined at: [21:5-21:19]
            # typed as: int
          Block [4:10-4:19]
            # typed as: int
            Binary [4:11-4:18]: SUB
              # typed as: int
              Name [4:11-4:14]: low
                # defined at: [2:39-2:51]
                # typed as: int
              Literal [4:17-4:18]: INT(1)
                # typed as: int
        Where [5:5-14:30]
          # typed as: void
          Defs [14:15-14:29]
            VarDef [14:15-14:29]: j
              # typed as: int
              Atom [14:22-14:29]: INT
                # typed as: int
          Block [5:5-14:7]
            # typed as: void
            For [5:6-14:6]
              # typed as: void
              Name [5:11-5:12]: j
                # defined at: [14:15-14:29]
                # typed as: int
              Name [5:15-5:18]: low
                # defined at: [2:39-2:51]
                # typed as: int
              Name [5:20-5:24]: high
                # defined at: [2:53-2:66]
                # typed as: int
              Name [5:26-5:27]: i
                # defined at: [21:5-21:19]
                # typed as: int
              IfThenElse [6:9-13:36]
                # typed as: void
                Binary [6:14-6:29]: LEQ
                  # typed as: log
                  Binary [6:14-6:20]: ARR
                    # typed as: int
                    Name [6:14-6:17]: tab
                      # defined at: [2:16-2:37]
                      # typed as: ARR(100,int)
                    Name [6:18-6:19]: j
                      # defined at: [14:15-14:29]
                      # typed as: int
                  Name [6:24-6:29]: pivot
                    # defined at: [20:11-20:29]
                    # typed as: int
                Where [6:35-13:35]
                  # typed as: int
                  Defs [13:18-13:34]
                    VarDef [13:18-13:34]: tmp
                      # typed as: int
                      Atom [13:27-13:34]: INT
                        # typed as: int
                  Block [6:35-13:10]
                    # typed as: int
                    Binary [8:13-8:22]: ASSIGN
                      # typed as: int
                      Name [8:14-8:15]: i
                        # defined at: [21:5-21:19]
                        # typed as: int
                      Binary [8:18-8:21]: ADD
                        # typed as: int
                        Name [8:18-8:19]: i
                          # defined at: [21:5-21:19]
                          # typed as: int
                        Literal [8:20-8:21]: INT(1)
                          # typed as: int
                    Binary [9:13-9:27]: ASSIGN
                      # typed as: int
                      Name [9:14-9:17]: tmp
                        # defined at: [13:18-13:34]
                        # typed as: int
                      Binary [9:20-9:26]: ARR
                        # typed as: int
                        Name [9:20-9:23]: tab
                          # defined at: [2:16-2:37]
                          # typed as: ARR(100,int)
                        Name [9:24-9:25]: i
                          # defined at: [21:5-21:19]
                          # typed as: int
                    Binary [10:13-10:30]: ASSIGN
                      # typed as: int
                      Binary [10:14-10:20]: ARR
                        # typed as: int
                        Name [10:14-10:17]: tab
                          # defined at: [2:16-2:37]
                          # typed as: ARR(100,int)
                        Name [10:18-10:19]: j
                          # defined at: [14:15-14:29]
                          # typed as: int
                      Binary [10:23-10:29]: ARR
                        # typed as: int
                        Name [10:23-10:26]: tab
                          # defined at: [2:16-2:37]
                          # typed as: ARR(100,int)
                        Name [10:27-10:28]: j
                          # defined at: [14:15-14:29]
                          # typed as: int
                    Binary [11:13-11:27]: ASSIGN
                      # typed as: int
                      Binary [11:14-11:20]: ARR
                        # typed as: int
                        Name [11:14-11:17]: tab
                          # defined at: [2:16-2:37]
                          # typed as: ARR(100,int)
                        Name [11:18-11:19]: j
                          # defined at: [14:15-14:29]
                          # typed as: int
                      Name [11:23-11:26]: tmp
                        # defined at: [13:18-13:34]
                        # typed as: int
        Binary [16:5-16:21]: ASSIGN
          # typed as: int
          Name [16:6-16:9]: tmp
            # defined at: [22:5-22:21]
            # typed as: int
          Binary [16:12-16:20]: ARR
            # typed as: int
            Name [16:12-16:15]: tab
              # defined at: [2:16-2:37]
              # typed as: ARR(100,int)
            Binary [16:16-16:19]: ADD
              # typed as: int
              Name [16:16-16:17]: i
                # defined at: [21:5-21:19]
                # typed as: int
              Literal [16:18-16:19]: INT(1)
                # typed as: int
        Binary [17:5-17:27]: ASSIGN
          # typed as: int
          Binary [17:6-17:14]: ARR
            # typed as: int
            Name [17:6-17:9]: tab
              # defined at: [2:16-2:37]
              # typed as: ARR(100,int)
            Binary [17:10-17:13]: ADD
              # typed as: int
              Name [17:10-17:11]: i
                # defined at: [21:5-21:19]
                # typed as: int
              Literal [17:12-17:13]: INT(1)
                # typed as: int
          Binary [17:17-17:26]: ARR
            # typed as: int
            Name [17:17-17:20]: tab
              # defined at: [2:16-2:37]
              # typed as: ARR(100,int)
            Name [17:21-17:25]: high
              # defined at: [2:53-2:66]
              # typed as: int
        Binary [18:5-18:22]: ASSIGN
          # typed as: int
          Binary [18:6-18:15]: ARR
            # typed as: int
            Name [18:6-18:9]: tab
              # defined at: [2:16-2:37]
              # typed as: ARR(100,int)
            Name [18:10-18:14]: high
              # defined at: [2:53-2:66]
              # typed as: int
          Name [18:18-18:21]: tmp
            # defined at: [22:5-22:21]
            # typed as: int
  FunDef [25:1-32:2]: sort
    # typed as: (ARR(100,int), int, int) -> int
    Parameter [25:10-25:31]: tab
      # typed as: ARR(100,int)
      Array [25:15-25:31]
        # typed as: ARR(100,int)
        [100]
        Atom [25:24-25:31]: INT
          # typed as: int
    Parameter [25:33-25:45]: low
      # typed as: int
      Atom [25:38-25:45]: INT
        # typed as: int
    Parameter [25:47-25:60]: high
      # typed as: int
      Atom [25:53-25:60]: INT
        # typed as: int
    Atom [25:64-25:71]: INT
      # typed as: int
    Block [25:74-32:2]
      # typed as: int
      Block [26:5-31:9]
        # typed as: int
        IfThenElse [26:6-31:6]
          # typed as: void
          Binary [26:11-26:21]: LT
            # typed as: log
            Name [26:11-26:14]: low
              # defined at: [25:33-25:45]
              # typed as: int
            Name [26:17-26:21]: high
              # defined at: [25:47-25:60]
              # typed as: int
          Where [26:27-30:34]
            # typed as: int
            Defs [30:18-30:33]
              VarDef [30:18-30:33]: pi
                # typed as: int
                Atom [30:26-30:33]: INT
                  # typed as: int
            Block [26:27-30:10]
              # typed as: int
              Binary [27:9-27:42]: ASSIGN
                # typed as: int
                Name [27:11-27:13]: pi
                  # defined at: [30:18-30:33]
                  # typed as: int
                Call [27:16-27:41]: partition
                  # defined at: [2:1-23:2]
                  # typed as: int
                  Name [27:26-27:29]: tab
                    # defined at: [25:10-25:31]
                    # typed as: ARR(100,int)
                  Name [27:31-27:34]: low
                    # defined at: [25:33-25:45]
                    # typed as: int
                  Name [27:36-27:40]: high
                    # defined at: [25:47-25:60]
                    # typed as: int
              Call [28:9-28:29]: sort
                # defined at: [25:1-32:2]
                # typed as: int
                Name [28:14-28:17]: tab
                  # defined at: [25:10-25:31]
                  # typed as: ARR(100,int)
                Name [28:19-28:22]: low
                  # defined at: [25:33-25:45]
                  # typed as: int
                Binary [28:24-28:28]: SUB
                  # typed as: int
                  Name [28:24-28:26]: pi
                    # defined at: [30:18-30:33]
                    # typed as: int
                  Literal [28:27-28:28]: INT(1)
                    # typed as: int
              Call [29:9-29:30]: sort
                # defined at: [25:1-32:2]
                # typed as: int
                Name [29:14-29:17]: tab
                  # defined at: [25:10-25:31]
                  # typed as: ARR(100,int)
                Binary [29:19-29:23]: ADD
                  # typed as: int
                  Name [29:19-29:21]: pi
                    # defined at: [30:18-30:33]
                    # typed as: int
                  Literal [29:22-29:23]: INT(1)
                    # typed as: int
                Name [29:25-29:29]: high
                  # defined at: [25:47-25:60]
                  # typed as: int
        Literal [31:7-31:8]: INT(0)
          # typed as: int
!end

!code:
typ prvic:string;
typ drugic : integer;
typ tretjic : besedilo;
typ cetrtic: arr[7] arr[7] logical;
var petic : arr [ 10 ] integer;
typ besedilo: string;
typ void: integer;
var nekaj: string;
var blablabla: logical;
var nekajbla: logical;
var bla: logical;
var logi: cetrtic;

fun prvaFunkcija (prvic:string, drugic:integer, cetrtic:arr[7] logical) : integer =
    ( drugaFunkcija(12) - 1 / 2 + 3 * 6 - 420 + 42 - 69 )
    { where
        fun drugaFunkcija(nekaj:integer):integer =
            # begin where
            ({ if nekajbla then {
                    if logi[0][0] then {
                        while nekaj >= nekaj * (-5323223) + -4454 : {
                            for nekaj = 1, -9999999, nekaj-nekaj-1-1-1-1 : nekaj
                        } # end while 
                    } # end if
                } # end if
            }, 0) { where var haha: integer }
        # end drugaFunkcija
    } # end where
#lololololol
!expected:
Defs [1:1-28:6]
  TypeDef [1:1-1:17]: prvic
    # typed as: str
    Atom [1:11-1:17]: STR
      # typed as: str
  TypeDef [2:1-2:21]: drugic
    # typed as: int
    Atom [2:14-2:21]: INT
      # typed as: int
  TypeDef [3:1-3:23]: tretjic
    # typed as: str
    TypeName [3:15-3:23]: besedilo
      # defined at: [6:1-6:21]
      # typed as: str
  TypeDef [4:1-4:35]: cetrtic
    # typed as: ARR(7,ARR(7,log))
    Array [4:14-4:35]
      # typed as: ARR(7,ARR(7,log))
      [7]
      Array [4:21-4:35]
        # typed as: ARR(7,log)
        [7]
        Atom [4:28-4:35]: LOG
          # typed as: log
  VarDef [5:1-5:31]: petic
    # typed as: ARR(10,int)
    Array [5:13-5:31]
      # typed as: ARR(10,int)
      [10]
      Atom [5:24-5:31]: INT
        # typed as: int
  TypeDef [6:1-6:21]: besedilo
    # typed as: str
    Atom [6:15-6:21]: STR
      # typed as: str
  TypeDef [7:1-7:18]: void
    # typed as: int
    Atom [7:11-7:18]: INT
      # typed as: int
  VarDef [8:1-8:18]: nekaj
    # typed as: str
    Atom [8:12-8:18]: STR
      # typed as: str
  VarDef [9:1-9:23]: blablabla
    # typed as: log
    Atom [9:16-9:23]: LOG
      # typed as: log
  VarDef [10:1-10:22]: nekajbla
    # typed as: log
    Atom [10:15-10:22]: LOG
      # typed as: log
  VarDef [11:1-11:17]: bla
    # typed as: log
    Atom [11:10-11:17]: LOG
      # typed as: log
  VarDef [12:1-12:18]: logi
    # typed as: ARR(7,ARR(7,log))
    TypeName [12:11-12:18]: cetrtic
      # defined at: [4:1-4:35]
      # typed as: ARR(7,ARR(7,log))
  FunDef [14:1-28:6]: prvaFunkcija
    # typed as: (str, int, ARR(7,log)) -> int
    Parameter [14:19-14:31]: prvic
      # typed as: str
      Atom [14:25-14:31]: STR
        # typed as: str
    Parameter [14:33-14:47]: drugic
      # typed as: int
      Atom [14:40-14:47]: INT
        # typed as: int
    Parameter [14:49-14:71]: cetrtic
      # typed as: ARR(7,log)
      Array [14:57-14:71]
        # typed as: ARR(7,log)
        [7]
        Atom [14:64-14:71]: LOG
          # typed as: log
    Atom [14:75-14:82]: INT
      # typed as: int
    Where [15:5-28:6]
      # typed as: int
      Defs [17:9-26:46]
        FunDef [17:9-26:46]: drugaFunkcija
          # typed as: (int) -> int
          Parameter [17:27-17:40]: nekaj
            # typed as: int
            Atom [17:33-17:40]: INT
              # typed as: int
          Atom [17:42-17:49]: INT
            # typed as: int
          Where [19:13-26:46]
            # typed as: int
            Defs [26:27-26:44]
              VarDef [26:27-26:44]: haha
                # typed as: int
                Atom [26:37-26:44]: INT
                  # typed as: int
            Block [19:13-26:18]
              # typed as: int
              IfThenElse [19:14-26:14]
                # typed as: void
                Name [19:19-19:27]: nekajbla
                  # defined at: [10:1-10:22]
                  # typed as: log
                IfThenElse [19:33-25:18]
                  # typed as: void
                  Binary [20:24-20:34]: ARR
                    # typed as: log
                    Binary [20:24-20:31]: ARR
                      # typed as: ARR(7,log)
                      Name [20:24-20:28]: logi
                        # defined at: [12:1-12:18]
                        # typed as: ARR(7,ARR(7,log))
                      Literal [20:29-20:30]: INT(0)
                        # typed as: int
                    Literal [20:32-20:33]: INT(0)
                      # typed as: int
                  While [20:40-24:22]
                    # typed as: void
                    Binary [21:31-21:66]: GEQ
                      # typed as: log
                      Name [21:31-21:36]: nekaj
                        # defined at: [17:27-17:40]
                        # typed as: int
                      Binary [21:40-21:66]: ADD
                        # typed as: int
                        Binary [21:40-21:58]: MUL
                          # typed as: int
                          Name [21:40-21:45]: nekaj
                            # defined at: [17:27-17:40]
                            # typed as: int
                          Block [21:48-21:58]
                            # typed as: int
                            Unary [21:49-21:57]: SUB
                              # typed as: int
                              Literal [21:50-21:57]: INT(5323223)
                                # typed as: int
                        Unary [21:61-21:66]: SUB
                          # typed as: int
                          Literal [21:62-21:66]: INT(4454)
                            # typed as: int
                    For [21:69-23:26]
                      # typed as: void
                      Name [22:33-22:38]: nekaj
                        # defined at: [17:27-17:40]
                        # typed as: int
                      Literal [22:41-22:42]: INT(1)
                        # typed as: int
                      Unary [22:44-22:52]: SUB
                        # typed as: int
                        Literal [22:45-22:52]: INT(9999999)
                          # typed as: int
                      Binary [22:54-22:73]: SUB
                        # typed as: int
                        Binary [22:54-22:71]: SUB
                          # typed as: int
                          Binary [22:54-22:69]: SUB
                            # typed as: int
                            Binary [22:54-22:67]: SUB
                              # typed as: int
                              Binary [22:54-22:65]: SUB
                                # typed as: int
                                Name [22:54-22:59]: nekaj
                                  # defined at: [17:27-17:40]
                                  # typed as: int
                                Name [22:60-22:65]: nekaj
                                  # defined at: [17:27-17:40]
                                  # typed as: int
                              Literal [22:66-22:67]: INT(1)
                                # typed as: int
                            Literal [22:68-22:69]: INT(1)
                              # typed as: int
                          Literal [22:70-22:71]: INT(1)
                            # typed as: int
                        Literal [22:72-22:73]: INT(1)
                          # typed as: int
                      Name [22:76-22:81]: nekaj
                        # defined at: [17:27-17:40]
                        # typed as: int
              Literal [26:16-26:17]: INT(0)
                # typed as: int
      Block [15:5-15:58]
        # typed as: int
        Binary [15:7-15:56]: SUB
          # typed as: int
          Binary [15:7-15:51]: ADD
            # typed as: int
            Binary [15:7-15:46]: SUB
              # typed as: int
              Binary [15:7-15:40]: ADD
                # typed as: int
                Binary [15:7-15:32]: SUB
                  # typed as: int
                  Call [15:7-15:24]: drugaFunkcija
                    # defined at: [17:9-26:46]
                    # typed as: int
                    Literal [15:21-15:23]: INT(12)
                      # typed as: int
                  Binary [15:27-15:32]: DIV
                    # typed as: int
                    Literal [15:27-15:28]: INT(1)
                      # typed as: int
                    Literal [15:31-15:32]: INT(2)
                      # typed as: int
                Binary [15:35-15:40]: MUL
                  # typed as: int
                  Literal [15:35-15:36]: INT(3)
                    # typed as: int
                  Literal [15:39-15:40]: INT(6)
                    # typed as: int
              Literal [15:43-15:46]: INT(420)
                # typed as: int
            Literal [15:49-15:51]: INT(42)
              # typed as: int
          Literal [15:54-15:56]: INT(69)
            # typed as: int
!end

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
    Atom [15:8-15:14]: STR
      # typed as: str
  VarDef [15:16-15:30]: l
    # typed as: int
    Atom [15:23-15:30]: INT
      # typed as: int
  VarDef [15:32-15:46]: q
    # typed as: log
    Atom [15:39-15:46]: LOG
      # typed as: log
  VarDef [15:48-15:61]: m
    # typed as: int
    TypeName [15:55-15:61]: blabla
      # defined at: [11:1-11:22]
      # typed as: int
  VarDef [15:63-15:94]: n
    # typed as: ARR(69,ARR(420,str))
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
    Parameter [19:19-19:29]: x
      # typed as: int
      Atom [19:22-19:29]: INT
        # typed as: int
    Parameter [19:31-19:40]: y
      # typed as: str
      Atom [19:34-19:40]: STR
        # typed as: str
    Parameter [19:42-19:52]: z
      # typed as: log
      Atom [19:45-19:52]: LOG
        # typed as: log
    Atom [19:55-19:62]: INT
      # typed as: int
    Where [20:5-107:2]
      # typed as: int
      Defs [55:5-106:51]
        VarDef [55:5-55:18]: i
          # typed as: int
          Atom [55:11-55:18]: INT
            # typed as: int
        VarDef [56:5-56:19]: a
          # typed as: log
          Atom [56:12-56:19]: LOG
            # typed as: log
        VarDef [57:5-57:19]: j
          # typed as: int
          Atom [57:12-57:19]: INT
            # typed as: int
        VarDef [58:5-58:36]: p
          # typed as: ARR(69,ARR(420,int))
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
          Parameter [59:24-59:34]: x
            # typed as: int
            Atom [59:27-59:34]: INT
              # typed as: int
          Atom [59:36-59:43]: INT
            # typed as: int
          Where [60:9-72:6]
            # typed as: int
            Defs [63:13-71:18]
              FunDef [63:13-71:18]: thirdfunction
                # typed as: (str) -> ARR(10,str)
                Parameter [63:31-63:40]: y
                  # typed as: str
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
                      Atom [69:30-69:37]: INT
                        # typed as: int
                    VarDef [70:21-70:44]: ss
                      # typed as: ARR(10,str)
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
          Parameter [73:19-73:43]: stevila
            # typed as: ARR(10,int)
            Array [73:28-73:43]
              # typed as: ARR(10,int)
              [10]
              Atom [73:36-73:43]: INT
                # typed as: int
          Parameter [73:45-73:59]: begin
            # typed as: int
            Atom [73:52-73:59]: INT
              # typed as: int
          Parameter [73:61-73:73]: end
            # typed as: int
            Atom [73:66-73:73]: INT
              # typed as: int
          Atom [73:76-73:83]: INT
            # typed as: int
          Where [74:9-99:6]
            # typed as: int
            Defs [95:13-98:27]
              VarDef [95:13-95:34]: swapTemp
                # typed as: int
                Atom [95:27-95:34]: INT
                  # typed as: int
              VarDef [96:13-96:31]: pivot
                # typed as: int
                Atom [96:24-96:31]: INT
                  # typed as: int
              VarDef [97:13-97:27]: i
                # typed as: int
                Atom [97:20-97:27]: INT
                  # typed as: int
              VarDef [98:13-98:27]: j
                # typed as: int
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
          Parameter [100:15-100:31]: besedilo
            # typed as: str
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
          Parameter [106:15-106:31]: besedilo
            # typed as: str
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

!code:
typ y: logical;
typ x: integer;
fun f(x: x): integer = 1 * x;
fun main(y: integer): integer = f(3)
!expected:
Defs [1:1-4:37]
  TypeDef [1:1-1:15]: y
    # typed as: log
    Atom [1:8-1:15]: LOG
      # typed as: log
  TypeDef [2:1-2:15]: x
    # typed as: int
    Atom [2:8-2:15]: INT
      # typed as: int
  FunDef [3:1-3:29]: f
    # typed as: (int) -> int
    Parameter [3:7-3:11]: x
      # typed as: int
      TypeName [3:10-3:11]: x
        # defined at: [2:1-2:15]
        # typed as: int
    Atom [3:14-3:21]: INT
      # typed as: int
    Binary [3:24-3:29]: MUL
      # typed as: int
      Literal [3:24-3:25]: INT(1)
        # typed as: int
      Name [3:28-3:29]: x
        # defined at: [3:7-3:11]
        # typed as: int
  FunDef [4:1-4:37]: main
    # typed as: (int) -> int
    Parameter [4:10-4:20]: y
      # typed as: int
      Atom [4:13-4:20]: INT
        # typed as: int
    Atom [4:23-4:30]: INT
      # typed as: int
    Call [4:33-4:37]: f
      # defined at: [3:1-3:29]
      # typed as: int
      Literal [4:35-4:36]: INT(3)
        # typed as: int
!end

!code:
typ x: integer;
fun g(x: x): integer = (12) {
    where var x: integer
}
!expected:
Defs [1:1-4:2]
  TypeDef [1:1-1:15]: x
    # typed as: int
    Atom [1:8-1:15]: INT
      # typed as: int
  FunDef [2:1-4:2]: g
    # typed as: (int) -> int
    Parameter [2:7-2:11]: x
      # typed as: int
      TypeName [2:10-2:11]: x
        # defined at: [1:1-1:15]
        # typed as: int
    Atom [2:14-2:21]: INT
      # typed as: int
    Where [2:24-4:2]
      # typed as: int
      Defs [3:11-3:25]
        VarDef [3:11-3:25]: x
          # typed as: int
          Atom [3:18-3:25]: INT
            # typed as: int
      Block [2:24-2:28]
        # typed as: int
        Literal [2:25-2:27]: INT(12)
          # typed as: int
!end

