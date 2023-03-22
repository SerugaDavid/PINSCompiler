!compiler_flags: --dump AST --exec AST

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
    Atom [1:8-1:15]: INT
  VarDef [2:1-2:15]: y
    Atom [2:8-2:15]: LOG
  VarDef [3:1-3:14]: z
    Atom [3:8-3:14]: STR
  TypeDef [4:1-4:19]: MyInt
    Atom [4:12-4:19]: INT
  TypeDef [5:1-5:19]: MyLog
    Atom [5:12-5:19]: LOG
  TypeDef [6:1-6:18]: MyStr
    Atom [6:12-6:18]: STR
!end


!code:
var izraz1 : integer;
fun main ( x : integer ) : integer = (
    {izraz1 =  (1 + 2 + 3) / (4 + 5 + 6)}
)
!expected:
Defs [1:1-4:2]
  VarDef [1:1-1:21]: izraz1
    Atom [1:14-1:21]: INT
  FunDef [2:1-4:2]: main
    Parameter [2:12-2:23]: x
      Atom [2:16-2:23]: INT
    Atom [2:28-2:35]: INT
    Block [2:38-4:2]
      Binary [3:5-3:42]: ASSIGN
        Name [3:6-3:12]: izraz1
        Binary [3:16-3:41]: DIV
          Block [3:16-3:27]
            Binary [3:17-3:26]: ADD
              Binary [3:17-3:22]: ADD
                Literal [3:17-3:18]: INT(1)
                Literal [3:21-3:22]: INT(2)
              Literal [3:25-3:26]: INT(3)
          Block [3:30-3:41]
            Binary [3:31-3:40]: ADD
              Binary [3:31-3:36]: ADD
                Literal [3:31-3:32]: INT(4)
                Literal [3:35-3:36]: INT(5)
              Literal [3:39-3:40]: INT(6)
!end


!code:
var izraz1 : integer;
fun main ( x : integer ) : integer = (
    {izraz1 =  (1 * 2 + 3) + (4 + 5 / 6)}
)
!expected:
Defs [1:1-4:2]
  VarDef [1:1-1:21]: izraz1
    Atom [1:14-1:21]: INT
  FunDef [2:1-4:2]: main
    Parameter [2:12-2:23]: x
      Atom [2:16-2:23]: INT
    Atom [2:28-2:35]: INT
    Block [2:38-4:2]
      Binary [3:5-3:42]: ASSIGN
        Name [3:6-3:12]: izraz1
        Binary [3:16-3:41]: ADD
          Block [3:16-3:27]
            Binary [3:17-3:26]: ADD
              Binary [3:17-3:22]: MUL
                Literal [3:17-3:18]: INT(1)
                Literal [3:21-3:22]: INT(2)
              Literal [3:25-3:26]: INT(3)
          Block [3:30-3:41]
            Binary [3:31-3:40]: ADD
              Literal [3:31-3:32]: INT(4)
              Binary [3:35-3:40]: DIV
                Literal [3:35-3:36]: INT(5)
                Literal [3:39-3:40]: INT(6)
!end


!code:
var izraz1 : integer;
fun main ( x : integer ) : integer = (
    {izraz1 =  1 + 2 - 4 * 5 / 6 % 7 + 1}
)
!expected:
Defs [1:1-4:2]
  VarDef [1:1-1:21]: izraz1
    Atom [1:14-1:21]: INT
  FunDef [2:1-4:2]: main
    Parameter [2:12-2:23]: x
      Atom [2:16-2:23]: INT
    Atom [2:28-2:35]: INT
    Block [2:38-4:2]
      Binary [3:5-3:42]: ASSIGN
        Name [3:6-3:12]: izraz1
        Binary [3:16-3:41]: ADD
          Binary [3:16-3:37]: SUB
            Binary [3:16-3:21]: ADD
              Literal [3:16-3:17]: INT(1)
              Literal [3:20-3:21]: INT(2)
            Binary [3:24-3:37]: MOD
              Binary [3:24-3:33]: DIV
                Binary [3:24-3:29]: MUL
                  Literal [3:24-3:25]: INT(4)
                  Literal [3:28-3:29]: INT(5)
                Literal [3:32-3:33]: INT(6)
              Literal [3:36-3:37]: INT(7)
          Literal [3:40-3:41]: INT(1)
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
    Parameter [1:10-1:31]: tab
      Array [1:16-1:31]
        [20]
        Atom [1:24-1:31]: INT
    Parameter [1:33-1:44]: i
      Atom [1:37-1:44]: INT
    Parameter [1:46-1:57]: j
      Atom [1:50-1:57]: INT
    Atom [1:61-1:68]: INT
    Where [1:71-5:28]
      Defs [5:10-5:27]
        VarDef [5:10-5:27]: tmp
          Atom [5:20-5:27]: INT
      Block [1:71-5:2]
        Binary [2:5-2:19]: ASSIGN
          Name [2:6-2:9]: tmp
          Binary [2:12-2:18]: ARR
            Name [2:12-2:15]: tab
            Name [2:16-2:17]: i
        Binary [3:5-3:22]: ASSIGN
          Binary [3:6-3:12]: ARR
            Name [3:6-3:9]: tab
            Name [3:10-3:11]: i
          Binary [3:15-3:21]: ARR
            Name [3:15-3:18]: tab
            Name [3:19-3:20]: j
        Binary [4:5-4:19]: ASSIGN
          Binary [4:6-4:12]: ARR
            Name [4:6-4:9]: tab
            Name [4:10-4:11]: j
          Name [4:15-4:18]: tmp
!end


!code:
# program ki presteje stevilo sodih stevil
fun main ( tab : arr[20] integer ) : integer = (
	{for i = 0, 20, 1:
		{if tab[i] % 2 == 0 then
			{stSodih = stSodih + 1}
		}
	}
) {where var stSodih : integer}
!expected:
Defs [2:1-8:32]
  FunDef [2:1-8:32]: main
    Parameter [2:12-2:33]: tab
      Array [2:18-2:33]
        [20]
        Atom [2:26-2:33]: INT
    Atom [2:38-2:45]: INT
    Where [2:48-8:32]
      Defs [8:10-8:31]
        VarDef [8:10-8:31]: stSodih
          Atom [8:24-8:31]: INT
      Block [2:48-8:2]
        For [3:5-7:6]
          Name [3:10-3:11]: i
          Literal [3:14-3:15]: INT(0)
          Literal [3:17-3:19]: INT(20)
          Literal [3:21-3:22]: INT(1)
          IfThenElse [4:9-6:10]
            Binary [4:13-4:28]: EQ
              Binary [4:13-4:23]: MOD
                Binary [4:13-4:19]: ARR
                  Name [4:13-4:16]: tab
                  Name [4:17-4:18]: i
                Literal [4:22-4:23]: INT(2)
              Literal [4:27-4:28]: INT(0)
            Binary [5:13-5:36]: ASSIGN
              Name [5:14-5:21]: stSodih
              Binary [5:24-5:35]: ADD
                Name [5:24-5:31]: stSodih
                Literal [5:34-5:35]: INT(1)
!end


!code:
fun main(x: integer): integer = (
    {for i = 0, 10, 1: (
        {if i / 2 == 1 | i & 2 != 0 | 1 == i & 'a' != 'b' & 1 != 3 then (
            print(i)
        )}
    ) {where var i: integer}}
)
!expected:
Defs [1:1-7:2]
  FunDef [1:1-7:2]: main
    Parameter [1:10-1:20]: x
      Atom [1:13-1:20]: INT
    Atom [1:23-1:30]: INT
    Block [1:33-7:2]
      For [2:5-6:30]
        Name [2:10-2:11]: i
        Literal [2:14-2:15]: INT(0)
        Literal [2:17-2:19]: INT(10)
        Literal [2:21-2:22]: INT(1)
        Where [2:24-6:29]
          Defs [6:14-6:28]
            VarDef [6:14-6:28]: i
              Atom [6:21-6:28]: INT
          Block [2:24-6:6]
            IfThenElse [3:9-5:11]
              Binary [3:13-3:67]: OR
                Binary [3:13-3:36]: OR
                  Binary [3:13-3:23]: EQ
                    Binary [3:13-3:18]: DIV
                      Name [3:13-3:14]: i
                      Literal [3:17-3:18]: INT(2)
                    Literal [3:22-3:23]: INT(1)
                  Binary [3:26-3:36]: AND
                    Name [3:26-3:27]: i
                    Binary [3:30-3:36]: NEQ
                      Literal [3:30-3:31]: INT(2)
                      Literal [3:35-3:36]: INT(0)
                Binary [3:39-3:67]: AND
                  Binary [3:39-3:58]: AND
                    Binary [3:39-3:45]: EQ
                      Literal [3:39-3:40]: INT(1)
                      Name [3:44-3:45]: i
                    Binary [3:48-3:58]: NEQ
                      Literal [3:48-3:51]: STR(a)
                      Literal [3:55-3:58]: STR(b)
                  Binary [3:61-3:67]: NEQ
                    Literal [3:61-3:62]: INT(1)
                    Literal [3:66-3:67]: INT(3)
              Block [3:73-5:10]
                Call [4:13-4:21]: print
                  Name [4:19-4:20]: i
!end


!code:
fun main( x: integer ) : integer = (
    {if 1 == 1 & 2 == 2 | 3 == 3 | 4 == 4 then (
        {for i = 0, 10, 1:
            (
                {k = k + i}
            ) {where var k : integer}
        }
    ) {where var k : integer; var i : integer}
    else (
        {while k < 20:
            (
                {k = k + 1}
            ) {where var k : integer}
        }
    )
    }, 0
)
!expected:
Defs [1:1-17:2]
  FunDef [1:1-17:2]: main
    Parameter [1:11-1:21]: x
      Atom [1:14-1:21]: INT
    Atom [1:26-1:33]: INT
    Block [1:36-17:2]
      IfThenElse [2:5-16:6]
        Binary [2:9-2:42]: OR
          Binary [2:9-2:33]: OR
            Binary [2:9-2:24]: AND
              Binary [2:9-2:15]: EQ
                Literal [2:9-2:10]: INT(1)
                Literal [2:14-2:15]: INT(1)
              Binary [2:18-2:24]: EQ
                Literal [2:18-2:19]: INT(2)
                Literal [2:23-2:24]: INT(2)
            Binary [2:27-2:33]: EQ
              Literal [2:27-2:28]: INT(3)
              Literal [2:32-2:33]: INT(3)
          Binary [2:36-2:42]: EQ
            Literal [2:36-2:37]: INT(4)
            Literal [2:41-2:42]: INT(4)
        Where [2:48-8:47]
          Defs [8:14-8:46]
            VarDef [8:14-8:29]: k
              Atom [8:22-8:29]: INT
            VarDef [8:31-8:46]: i
              Atom [8:39-8:46]: INT
          Block [2:48-8:6]
            For [3:9-7:10]
              Name [3:14-3:15]: i
              Literal [3:18-3:19]: INT(0)
              Literal [3:21-3:23]: INT(10)
              Literal [3:25-3:26]: INT(1)
              Where [4:13-6:38]
                Defs [6:22-6:37]
                  VarDef [6:22-6:37]: k
                    Atom [6:30-6:37]: INT
                Block [4:13-6:14]
                  Binary [5:17-5:28]: ASSIGN
                    Name [5:18-5:19]: k
                    Binary [5:22-5:27]: ADD
                      Name [5:22-5:23]: k
                      Name [5:26-5:27]: i
        Block [9:10-15:6]
          While [10:9-14:10]
            Binary [10:16-10:22]: LT
              Name [10:16-10:17]: k
              Literal [10:20-10:22]: INT(20)
            Where [11:13-13:38]
              Defs [13:22-13:37]
                VarDef [13:22-13:37]: k
                  Atom [13:30-13:37]: INT
              Block [11:13-13:14]
                Binary [12:17-12:28]: ASSIGN
                  Name [12:18-12:19]: k
                  Binary [12:22-12:27]: ADD
                    Name [12:22-12:23]: k
                    Literal [12:26-12:27]: INT(1)
      Literal [16:8-16:9]: INT(0)
!end


!code:
# quicksort
fun partition (tab: arr[100] integer, low: integer, high: integer) : integer = (
    {pivot = tab[high]},
    {i = (low - 1)},
    {for j = low, high, i: (
        { if tab[j] <= pivot then (

            {i = i+1},
            {tmp = tab[i]},
            {tab[j] = tab[j]},
            {tab[j] = tmp}

        ) {where var tmp: integer}}
    ) {where var j: integer}},

    {tmp = tab[i+1]},
    {tab[i+1] = tab[high]},
    {tab[high] = tmp}
) {where var pivot: integer; var i: integer; var tmp: integer};

fun sort(tab: arr[100] integer, low: integer, high: integer) : integer = (
    {if low < high then (
        {pi = partition(tab, low, high)},
        sort(tab, low, pi-1),
        sort(tab, pi+1, high)
    ) {where var pi: integer}}
)
!expected:
Defs [2:1-27:2]
  FunDef [2:1-19:63]: partition
    Parameter [2:16-2:37]: tab
      Array [2:21-2:37]
        [100]
        Atom [2:30-2:37]: INT
    Parameter [2:39-2:51]: low
      Atom [2:44-2:51]: INT
    Parameter [2:53-2:66]: high
      Atom [2:59-2:66]: INT
    Atom [2:70-2:77]: INT
    Where [2:80-19:63]
      Defs [19:10-19:62]
        VarDef [19:10-19:28]: pivot
          Atom [19:21-19:28]: INT
        VarDef [19:30-19:44]: i
          Atom [19:37-19:44]: INT
        VarDef [19:46-19:62]: tmp
          Atom [19:55-19:62]: INT
      Block [2:80-19:2]
        Binary [3:5-3:24]: ASSIGN
          Name [3:6-3:11]: pivot
          Binary [3:14-3:23]: ARR
            Name [3:14-3:17]: tab
            Name [3:18-3:22]: high
        Binary [4:5-4:20]: ASSIGN
          Name [4:6-4:7]: i
          Block [4:10-4:19]
            Binary [4:11-4:18]: SUB
              Name [4:11-4:14]: low
              Literal [4:17-4:18]: INT(1)
        For [5:5-14:30]
          Name [5:10-5:11]: j
          Name [5:14-5:17]: low
          Name [5:19-5:23]: high
          Name [5:25-5:26]: i
          Where [5:28-14:29]
            Defs [14:14-14:28]
              VarDef [14:14-14:28]: j
                Atom [14:21-14:28]: INT
            Block [5:28-14:6]
              IfThenElse [6:9-13:36]
                Binary [6:14-6:29]: LEQ
                  Binary [6:14-6:20]: ARR
                    Name [6:14-6:17]: tab
                    Name [6:18-6:19]: j
                  Name [6:24-6:29]: pivot
                Where [6:35-13:35]
                  Defs [13:18-13:34]
                    VarDef [13:18-13:34]: tmp
                      Atom [13:27-13:34]: INT
                  Block [6:35-13:10]
                    Binary [8:13-8:22]: ASSIGN
                      Name [8:14-8:15]: i
                      Binary [8:18-8:21]: ADD
                        Name [8:18-8:19]: i
                        Literal [8:20-8:21]: INT(1)
                    Binary [9:13-9:27]: ASSIGN
                      Name [9:14-9:17]: tmp
                      Binary [9:20-9:26]: ARR
                        Name [9:20-9:23]: tab
                        Name [9:24-9:25]: i
                    Binary [10:13-10:30]: ASSIGN
                      Binary [10:14-10:20]: ARR
                        Name [10:14-10:17]: tab
                        Name [10:18-10:19]: j
                      Binary [10:23-10:29]: ARR
                        Name [10:23-10:26]: tab
                        Name [10:27-10:28]: j
                    Binary [11:13-11:27]: ASSIGN
                      Binary [11:14-11:20]: ARR
                        Name [11:14-11:17]: tab
                        Name [11:18-11:19]: j
                      Name [11:23-11:26]: tmp
        Binary [16:5-16:21]: ASSIGN
          Name [16:6-16:9]: tmp
          Binary [16:12-16:20]: ARR
            Name [16:12-16:15]: tab
            Binary [16:16-16:19]: ADD
              Name [16:16-16:17]: i
              Literal [16:18-16:19]: INT(1)
        Binary [17:5-17:27]: ASSIGN
          Binary [17:6-17:14]: ARR
            Name [17:6-17:9]: tab
            Binary [17:10-17:13]: ADD
              Name [17:10-17:11]: i
              Literal [17:12-17:13]: INT(1)
          Binary [17:17-17:26]: ARR
            Name [17:17-17:20]: tab
            Name [17:21-17:25]: high
        Binary [18:5-18:22]: ASSIGN
          Binary [18:6-18:15]: ARR
            Name [18:6-18:9]: tab
            Name [18:10-18:14]: high
          Name [18:18-18:21]: tmp
  FunDef [21:1-27:2]: sort
    Parameter [21:10-21:31]: tab
      Array [21:15-21:31]
        [100]
        Atom [21:24-21:31]: INT
    Parameter [21:33-21:45]: low
      Atom [21:38-21:45]: INT
    Parameter [21:47-21:60]: high
      Atom [21:53-21:60]: INT
    Atom [21:64-21:71]: INT
    Block [21:74-27:2]
      IfThenElse [22:5-26:31]
        Binary [22:9-22:19]: LT
          Name [22:9-22:12]: low
          Name [22:15-22:19]: high
        Where [22:25-26:30]
          Defs [26:14-26:29]
            VarDef [26:14-26:29]: pi
              Atom [26:22-26:29]: INT
          Block [22:25-26:6]
            Binary [23:9-23:41]: ASSIGN
              Name [23:10-23:12]: pi
              Call [23:15-23:40]: partition
                Name [23:25-23:28]: tab
                Name [23:30-23:33]: low
                Name [23:35-23:39]: high
            Call [24:9-24:29]: sort
              Name [24:14-24:17]: tab
              Name [24:19-24:22]: low
              Binary [24:24-24:28]: SUB
                Name [24:24-24:26]: pi
                Literal [24:27-24:28]: INT(1)
            Call [25:9-25:30]: sort
              Name [25:14-25:17]: tab
              Binary [25:19-25:23]: ADD
                Name [25:19-25:21]: pi
                Literal [25:22-25:23]: INT(1)
              Name [25:25-25:29]: high
!end


!code:
typ prvic:string;
typ drugic : integer;
typ tretjic : besedilo;
typ cetrtic: arr[7] arr[7] logical;
var petic : arr [ 10 ] integer;

fun prvaFunkcija (prvic:string, drugic:integer, cetrtic:arr[7] logical) : integer =
5 + 3 - 2 == 10 & true | beseda / drugabeseda * 'danesjelepdanaaaaa' % cetrtic != petic + 5 - !e
- (-10) + (+(+(-10))) / (!cetrtic) + 100 - false | nekaj & nekaj == bla | nekaj >= blabla &
nekaj <= blablabla | nekaj + nekaj < bla & nekaj - nekaj > nekajbla { where
    fun drugaFunkcija(nekaj:integer):integer=
        {
            { if haha then {
                    if nekaj then 5+3-1-4+2 * true else {
                        while nekaj >= nekaj * (-5323223) + -44545    : {
                            for nekaj = 1, nekaj >= -9999999, nekajnekaj-1-1-1-1:nekaj
                        }
                    }
                }
             }
        = celotenprogramtideluje69 { where
            fun epicBubblesort(polje:arr[10]integer):void =
                n == dolzinaPolja &
                temp == 0 & {
                for i=0,i<n,i + 1:
                    { for j=0, j<(n-i),j + 1:
                        {
                            if polje[j-1] > polje[j] then
                                temp == polje[j-1] &
                                polje[j-1] == polje[j] &
                                polje[j] == temp &
                                blablabla #neki komentarji
                        }
                    }
                }
            }
         }
    }
#lololololol
!expected:
Defs [1:1-38:6]
  TypeDef [1:1-1:17]: prvic
    Atom [1:11-1:17]: STR
  TypeDef [2:1-2:21]: drugic
    Atom [2:14-2:21]: INT
  TypeDef [3:1-3:23]: tretjic
    TypeName [3:15-3:23]: besedilo
  TypeDef [4:1-4:35]: cetrtic
    Array [4:14-4:35]
      [7]
      Array [4:21-4:35]
        [7]
        Atom [4:28-4:35]: LOG
  VarDef [5:1-5:31]: petic
    Array [5:13-5:31]
      [10]
      Atom [5:24-5:31]: INT
  FunDef [7:1-38:6]: prvaFunkcija
    Parameter [7:19-7:31]: prvic
      Atom [7:25-7:31]: STR
    Parameter [7:33-7:47]: drugic
      Atom [7:40-7:47]: INT
    Parameter [7:49-7:71]: cetrtic
      Array [7:57-7:71]
        [7]
        Atom [7:64-7:71]: LOG
    Atom [7:75-7:82]: INT
    Where [8:1-38:6]
      Defs [11:5-37:11]
        FunDef [11:5-37:11]: drugaFunkcija
          Parameter [11:23-11:36]: nekaj
            Atom [11:29-11:36]: INT
          Atom [11:38-11:45]: INT
          Binary [12:9-37:11]: ASSIGN
            IfThenElse [13:13-20:15]
              Name [13:18-13:22]: haha
              IfThenElse [13:28-19:18]
                Name [14:24-14:29]: nekaj
                Binary [14:35-14:51]: ADD
                  Binary [14:35-14:42]: SUB
                    Binary [14:35-14:40]: SUB
                      Binary [14:35-14:38]: ADD
                        Literal [14:35-14:36]: INT(5)
                        Literal [14:37-14:38]: INT(3)
                      Literal [14:39-14:40]: INT(1)
                    Literal [14:41-14:42]: INT(4)
                  Binary [14:43-14:51]: MUL
                    Literal [14:43-14:44]: INT(2)
                    Literal [14:47-14:51]: LOG(true)
                While [14:57-18:22]
                  Binary [15:31-15:67]: GEQ
                    Name [15:31-15:36]: nekaj
                    Binary [15:40-15:67]: ADD
                      Binary [15:40-15:58]: MUL
                        Name [15:40-15:45]: nekaj
                        Block [15:48-15:58]
                          Unary [15:49-15:57]: SUB
                            Literal [15:50-15:57]: INT(5323223)
                      Unary [15:61-15:67]: SUB
                        Literal [15:62-15:67]: INT(44545)
                  For [15:73-17:26]
                    Name [16:33-16:38]: nekaj
                    Literal [16:41-16:42]: INT(1)
                    Binary [16:44-16:61]: GEQ
                      Name [16:44-16:49]: nekaj
                      Unary [16:53-16:61]: SUB
                        Literal [16:54-16:61]: INT(9999999)
                    Binary [16:63-16:81]: SUB
                      Binary [16:63-16:79]: SUB
                        Binary [16:63-16:77]: SUB
                          Binary [16:63-16:75]: SUB
                            Name [16:63-16:73]: nekajnekaj
                            Literal [16:74-16:75]: INT(1)
                          Literal [16:76-16:77]: INT(1)
                        Literal [16:78-16:79]: INT(1)
                      Literal [16:80-16:81]: INT(1)
                    Name [16:82-16:87]: nekaj
            Where [21:11-36:14]
              Defs [22:13-35:18]
                FunDef [22:13-35:18]: epicBubblesort
                  Parameter [22:32-22:52]: polje
                    Array [22:38-22:52]
                      [10]
                      Atom [22:45-22:52]: INT
                  TypeName [22:54-22:58]: void
                  Binary [23:17-35:18]: AND
                    Binary [23:17-24:26]: AND
                      Binary [23:17-23:34]: EQ
                        Name [23:17-23:18]: n
                        Name [23:22-23:34]: dolzinaPolja
                      Binary [24:17-24:26]: EQ
                        Name [24:17-24:21]: temp
                        Literal [24:25-24:26]: INT(0)
                    For [24:29-35:18]
                      Name [25:21-25:22]: i
                      Literal [25:23-25:24]: INT(0)
                      Binary [25:25-25:28]: LT
                        Name [25:25-25:26]: i
                        Name [25:27-25:28]: n
                      Binary [25:29-25:34]: ADD
                        Name [25:29-25:30]: i
                        Literal [25:33-25:34]: INT(1)
                      For [26:21-34:22]
                        Name [26:27-26:28]: j
                        Literal [26:29-26:30]: INT(0)
                        Binary [26:32-26:39]: LT
                          Name [26:32-26:33]: j
                          Block [26:34-26:39]
                            Binary [26:35-26:38]: SUB
                              Name [26:35-26:36]: n
                              Name [26:37-26:38]: i
                        Binary [26:40-26:45]: ADD
                          Name [26:40-26:41]: j
                          Literal [26:44-26:45]: INT(1)
                        IfThenElse [27:25-33:26]
                          Binary [28:32-28:53]: GT
                            Binary [28:32-28:42]: ARR
                              Name [28:32-28:37]: polje
                              Binary [28:38-28:41]: SUB
                                Name [28:38-28:39]: j
                                Literal [28:40-28:41]: INT(1)
                            Binary [28:45-28:53]: ARR
                              Name [28:45-28:50]: polje
                              Name [28:51-28:52]: j
                          Binary [29:33-32:42]: AND
                            Binary [29:33-31:49]: AND
                              Binary [29:33-30:55]: AND
                                Binary [29:33-29:51]: EQ
                                  Name [29:33-29:37]: temp
                                  Binary [29:41-29:51]: ARR
                                    Name [29:41-29:46]: polje
                                    Binary [29:47-29:50]: SUB
                                      Name [29:47-29:48]: j
                                      Literal [29:49-29:50]: INT(1)
                                Binary [30:33-30:55]: EQ
                                  Binary [30:33-30:43]: ARR
                                    Name [30:33-30:38]: polje
                                    Binary [30:39-30:42]: SUB
                                      Name [30:39-30:40]: j
                                      Literal [30:41-30:42]: INT(1)
                                  Binary [30:47-30:55]: ARR
                                    Name [30:47-30:52]: polje
                                    Name [30:53-30:54]: j
                              Binary [31:33-31:49]: EQ
                                Binary [31:33-31:41]: ARR
                                  Name [31:33-31:38]: polje
                                  Name [31:39-31:40]: j
                                Name [31:45-31:49]: temp
                            Name [32:33-32:42]: blablabla
              Name [21:11-21:35]: celotenprogramtideluje69
      Binary [8:1-10:68]: OR
        Binary [8:1-10:19]: OR
          Binary [8:1-9:72]: OR
            Binary [8:1-9:49]: OR
              Binary [8:1-8:23]: AND
                Binary [8:1-8:16]: EQ
                  Binary [8:1-8:10]: SUB
                    Binary [8:1-8:6]: ADD
                      Literal [8:1-8:2]: INT(5)
                      Literal [8:5-8:6]: INT(3)
                    Literal [8:9-8:10]: INT(2)
                  Literal [8:14-8:16]: INT(10)
                Literal [8:19-8:23]: LOG(true)
              Binary [8:26-9:49]: NEQ
                Binary [8:26-8:79]: MOD
                  Binary [8:26-8:69]: MUL
                    Binary [8:26-8:46]: DIV
                      Name [8:26-8:32]: beseda
                      Name [8:35-8:46]: drugabeseda
                    Literal [8:49-8:69]: STR(danesjelepdanaaaaa)
                  Name [8:72-8:79]: cetrtic
                Binary [8:83-9:49]: SUB
                  Binary [8:83-9:41]: ADD
                    Binary [8:83-9:35]: ADD
                      Binary [8:83-9:8]: SUB
                        Binary [8:83-8:97]: SUB
                          Binary [8:83-8:92]: ADD
                            Name [8:83-8:88]: petic
                            Literal [8:91-8:92]: INT(5)
                          Unary [8:95-8:97]: NOT
                            Name [8:96-8:97]: e
                        Block [9:3-9:8]
                          Unary [9:4-9:7]: SUB
                            Literal [9:5-9:7]: INT(10)
                      Binary [9:11-9:35]: DIV
                        Block [9:11-9:22]
                          Unary [9:12-9:21]: ADD
                            Block [9:13-9:21]
                              Unary [9:14-9:20]: ADD
                                Block [9:15-9:20]
                                  Unary [9:16-9:19]: SUB
                                    Literal [9:17-9:19]: INT(10)
                        Block [9:25-9:35]
                          Unary [9:26-9:34]: NOT
                            Name [9:27-9:34]: cetrtic
                    Literal [9:38-9:41]: INT(100)
                  Literal [9:44-9:49]: LOG(false)
            Binary [9:52-9:72]: AND
              Name [9:52-9:57]: nekaj
              Binary [9:60-9:72]: EQ
                Name [9:60-9:65]: nekaj
                Name [9:69-9:72]: bla
          Binary [9:75-10:19]: AND
            Binary [9:75-9:90]: GEQ
              Name [9:75-9:80]: nekaj
              Name [9:84-9:90]: blabla
            Binary [10:1-10:19]: LEQ
              Name [10:1-10:6]: nekaj
              Name [10:10-10:19]: blablabla
        Binary [10:22-10:68]: AND
          Binary [10:22-10:41]: LT
            Binary [10:22-10:35]: ADD
              Name [10:22-10:27]: nekaj
              Name [10:30-10:35]: nekaj
            Name [10:38-10:41]: bla
          Binary [10:44-10:68]: GT
            Binary [10:44-10:57]: SUB
              Name [10:44-10:49]: nekaj
              Name [10:52-10:57]: nekaj
            Name [10:60-10:68]: nekajbla
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

# variable_definition
var k: string; var l: integer; var q: logical; var m: blabla; var n: arr[69] arr [420] string;

# Funkcije
# function_definition
fun firstfunction(x: integer, y: string, z: logical, n: arr[69] arr[420] string): integer =
	( {for i = 1, i <= 100, {i = i + 1}:
		{
			while j > (-10): {
				if j * (-10) + 20 + 30 + 10 - 40 / 50 % 100 -(-(-(-(-(+10))))) != 123 then
					break
				else
					firstfunction(123, -234, +456)
			}
		# komentar komentar komentar ----------------------------------------------
		}	},

		true & false | 10 | 'danes je lep dan''' & blablabla | firstfunction(blablabla, x + 10)
											>=
		+10 - -20 * !true - !false / (bla, ha, ne, da) - {x=10} + {y+x-z = 5 * true - false}
		| a > b+3 | a < c-2 | a >= d*3 | a <= e/7 | a != f & a & b & a / a == 1 + 10 + 20 + 30 / 40 / 50 / 60
		* 10 * 20 * 30 * 40 - 50 - 60 - 80 - 90  % 4 % 6 % 7 % 8 +++++++10,

		{
			if abc > abc - 10 + 10 * 10 / 10 - 10 + 10 + -10
				then
					print('danes je lep dan')
		},

		{
		for i =
			{
				while j: (j < 10)
			},
				{
				  if a * 10 >= 100 then {a = a - 10} else break
				},
					{ for j = 1, j < 10, {j=j*2}: print('blablabla') }:
						{blablablab[i+10-20*30][j-10-203] = null}
		}
	)
{ where fun secondfunction(x: integer):integer = 10+20|30-40&123==20 { where
fun thirdfunction(y: string):arr[10] string = ({10*20=xyz}) }
};

fun partition(stevila: arr[10] integer, begin: integer, end: integer): integer = (
	{pivot = stevila[end]},
	{i = (begin - 1)},

	{ for j = begin, j < end, {j = j + 1}:
		{ if stevila[j] <= pivot then
			({i = i + 1},
			{swapTemp = stevila[j]},
			{stevila[i] = stevila[j]},
			{stevila[j] = swampTemp})
		}
	},

	({swapTemp = stevila[i+1]},
	{stevila[i+1] = stevila[end]},
	{stevila[end] = swapTemp}),

	return(i + 1)
);

fun izpis(besedilo: string): void =
	print('Tvoj program je prestal celoten preizkus!')
!expected:
Defs [5:1-77:55]
  TypeDef [5:1-5:15]: x
    Atom [5:8-5:15]: INT
  TypeDef [6:1-6:14]: y
    Atom [6:8-6:14]: STR
  TypeDef [7:1-7:15]: z
    Atom [7:8-7:15]: LOG
  TypeDef [8:1-8:17]: i
    TypeName [8:8-8:17]: blablabla
  TypeDef [9:1-9:41]: j
    Array [9:8-9:41]
      [10]
      Array [9:16-9:41]
        [15]
        Array [9:24-9:41]
          [20]
          TypeName [9:32-9:41]: blablabla
  VarDef [12:1-12:14]: k
    Atom [12:8-12:14]: STR
  VarDef [12:16-12:30]: l
    Atom [12:23-12:30]: INT
  VarDef [12:32-12:46]: q
    Atom [12:39-12:46]: LOG
  VarDef [12:48-12:61]: m
    TypeName [12:55-12:61]: blabla
  VarDef [12:63-12:94]: n
    Array [12:70-12:94]
      [69]
      Array [12:78-12:94]
        [420]
        Atom [12:88-12:94]: STR
  FunDef [16:1-54:2]: firstfunction
    Parameter [16:19-16:29]: x
      Atom [16:22-16:29]: INT
    Parameter [16:31-16:40]: y
      Atom [16:34-16:40]: STR
    Parameter [16:42-16:52]: z
      Atom [16:45-16:52]: LOG
    Parameter [16:54-16:80]: n
      Array [16:57-16:80]
        [69]
        Array [16:65-16:80]
          [420]
          Atom [16:74-16:80]: STR
    Atom [16:83-16:90]: INT
    Where [17:5-54:2]
      Defs [52:9-53:62]
        FunDef [52:9-53:62]: secondfunction
          Parameter [52:28-52:38]: x
            Atom [52:31-52:38]: INT
          Atom [52:40-52:47]: INT
          Where [52:50-53:62]
            Defs [53:1-53:60]
              FunDef [53:1-53:60]: thirdfunction
                Parameter [53:19-53:28]: y
                  Atom [53:22-53:28]: STR
                Array [53:30-53:44]
                  [10]
                  Atom [53:38-53:44]: STR
                Block [53:47-53:60]
                  Binary [53:48-53:59]: ASSIGN
                    Binary [53:49-53:54]: MUL
                      Literal [53:49-53:51]: INT(10)
                      Literal [53:52-53:54]: INT(20)
                    Name [53:55-53:58]: xyz
            Binary [52:50-52:69]: OR
              Binary [52:50-52:55]: ADD
                Literal [52:50-52:52]: INT(10)
                Literal [52:53-52:55]: INT(20)
              Binary [52:56-52:69]: AND
                Binary [52:56-52:61]: SUB
                  Literal [52:56-52:58]: INT(30)
                  Literal [52:59-52:61]: INT(40)
                Binary [52:62-52:69]: EQ
                  Literal [52:62-52:65]: INT(123)
                  Literal [52:67-52:69]: INT(20)
      Block [17:5-51:6]
        For [17:7-26:15]
          Name [17:12-17:13]: i
          Literal [17:16-17:17]: INT(1)
          Binary [17:19-17:27]: LEQ
            Name [17:19-17:20]: i
            Literal [17:24-17:27]: INT(100)
          Binary [17:29-17:40]: ASSIGN
            Name [17:30-17:31]: i
            Binary [17:34-17:39]: ADD
              Name [17:34-17:35]: i
              Literal [17:38-17:39]: INT(1)
          While [18:9-26:10]
            Binary [19:19-19:28]: GT
              Name [19:19-19:20]: j
              Block [19:23-19:28]
                Unary [19:24-19:27]: SUB
                  Literal [19:25-19:27]: INT(10)
            IfThenElse [19:30-24:14]
              Binary [20:20-20:86]: NEQ
                Binary [20:20-20:79]: SUB
                  Binary [20:20-20:60]: SUB
                    Binary [20:20-20:44]: ADD
                      Binary [20:20-20:39]: ADD
                        Binary [20:20-20:34]: ADD
                          Binary [20:20-20:29]: MUL
                            Name [20:20-20:21]: j
                            Block [20:24-20:29]
                              Unary [20:25-20:28]: SUB
                                Literal [20:26-20:28]: INT(10)
                          Literal [20:32-20:34]: INT(20)
                        Literal [20:37-20:39]: INT(30)
                      Literal [20:42-20:44]: INT(10)
                    Binary [20:47-20:60]: MOD
                      Binary [20:47-20:54]: DIV
                        Literal [20:47-20:49]: INT(40)
                        Literal [20:52-20:54]: INT(50)
                      Literal [20:57-20:60]: INT(100)
                  Block [20:62-20:79]
                    Unary [20:63-20:78]: SUB
                      Block [20:64-20:78]
                        Unary [20:65-20:77]: SUB
                          Block [20:66-20:77]
                            Unary [20:67-20:76]: SUB
                              Block [20:68-20:76]
                                Unary [20:69-20:75]: SUB
                                  Block [20:70-20:75]
                                    Unary [20:71-20:74]: ADD
                                      Literal [20:72-20:74]: INT(10)
                Literal [20:83-20:86]: INT(123)
              Name [21:21-21:26]: break
              Call [23:21-23:51]: firstfunction
                Literal [23:35-23:38]: INT(123)
                Unary [23:40-23:44]: SUB
                  Literal [23:41-23:44]: INT(234)
                Unary [23:46-23:50]: ADD
                  Literal [23:47-23:50]: INT(456)
        Binary [28:9-32:75]: OR
          Binary [28:9-31:50]: OR
            Binary [28:9-31:39]: OR
              Binary [28:9-31:28]: OR
                Binary [28:9-31:18]: OR
                  Binary [28:9-30:93]: OR
                    Binary [28:9-28:61]: OR
                      Binary [28:9-28:26]: OR
                        Binary [28:9-28:21]: AND
                          Literal [28:9-28:13]: LOG(true)
                          Literal [28:16-28:21]: LOG(false)
                        Literal [28:24-28:26]: INT(10)
                      Binary [28:29-28:61]: AND
                        Literal [28:29-28:49]: STR(danes je lep dan')
                        Name [28:52-28:61]: blablabla
                    Binary [28:64-30:93]: GEQ
                      Call [28:64-28:96]: firstfunction
                        Name [28:78-28:87]: blablabla
                        Binary [28:89-28:95]: ADD
                          Name [28:89-28:90]: x
                          Literal [28:93-28:95]: INT(10)
                      Binary [30:9-30:93]: ADD
                        Binary [30:9-30:64]: SUB
                          Binary [30:9-30:55]: SUB
                            Binary [30:9-30:26]: SUB
                              Unary [30:9-30:12]: ADD
                                Literal [30:10-30:12]: INT(10)
                              Binary [30:15-30:26]: MUL
                                Unary [30:15-30:18]: SUB
                                  Literal [30:16-30:18]: INT(20)
                                Unary [30:21-30:26]: NOT
                                  Literal [30:22-30:26]: LOG(true)
                            Binary [30:29-30:55]: DIV
                              Unary [30:29-30:35]: NOT
                                Literal [30:30-30:35]: LOG(false)
                              Block [30:38-30:55]
                                Name [30:39-30:42]: bla
                                Name [30:44-30:46]: ha
                                Name [30:48-30:50]: ne
                                Name [30:52-30:54]: da
                          Binary [30:58-30:64]: ASSIGN
                            Name [30:59-30:60]: x
                            Literal [30:61-30:63]: INT(10)
                        Binary [30:67-30:93]: ASSIGN
                          Binary [30:68-30:73]: SUB
                            Binary [30:68-30:71]: ADD
                              Name [30:68-30:69]: y
                              Name [30:70-30:71]: x
                            Name [30:72-30:73]: z
                          Binary [30:76-30:92]: SUB
                            Binary [30:76-30:84]: MUL
                              Literal [30:76-30:77]: INT(5)
                              Literal [30:80-30:84]: LOG(true)
                            Literal [30:87-30:92]: LOG(false)
                  Binary [31:11-31:18]: GT
                    Name [31:11-31:12]: a
                    Binary [31:15-31:18]: ADD
                      Name [31:15-31:16]: b
                      Literal [31:17-31:18]: INT(3)
                Binary [31:21-31:28]: LT
                  Name [31:21-31:22]: a
                  Binary [31:25-31:28]: SUB
                    Name [31:25-31:26]: c
                    Literal [31:27-31:28]: INT(2)
              Binary [31:31-31:39]: GEQ
                Name [31:31-31:32]: a
                Binary [31:36-31:39]: MUL
                  Name [31:36-31:37]: d
                  Literal [31:38-31:39]: INT(3)
            Binary [31:42-31:50]: LEQ
              Name [31:42-31:43]: a
              Binary [31:47-31:50]: DIV
                Name [31:47-31:48]: e
                Literal [31:49-31:50]: INT(7)
          Binary [31:53-32:75]: AND
            Binary [31:53-31:67]: AND
              Binary [31:53-31:63]: AND
                Binary [31:53-31:59]: NEQ
                  Name [31:53-31:54]: a
                  Name [31:58-31:59]: f
                Name [31:62-31:63]: a
              Name [31:66-31:67]: b
            Binary [31:70-32:75]: EQ
              Binary [31:70-31:75]: DIV
                Name [31:70-31:71]: a
                Name [31:74-31:75]: a
              Binary [31:79-32:75]: ADD
                Binary [31:79-32:65]: SUB
                  Binary [31:79-32:43]: SUB
                    Binary [31:79-32:38]: SUB
                      Binary [31:79-32:33]: SUB
                        Binary [31:79-32:28]: ADD
                          Binary [31:79-31:90]: ADD
                            Binary [31:79-31:85]: ADD
                              Literal [31:79-31:80]: INT(1)
                              Literal [31:83-31:85]: INT(10)
                            Literal [31:88-31:90]: INT(20)
                          Binary [31:93-32:28]: MUL
                            Binary [31:93-32:23]: MUL
                              Binary [31:93-32:18]: MUL
                                Binary [31:93-32:13]: MUL
                                  Binary [31:93-31:110]: DIV
                                    Binary [31:93-31:105]: DIV
                                      Binary [31:93-31:100]: DIV
                                        Literal [31:93-31:95]: INT(30)
                                        Literal [31:98-31:100]: INT(40)
                                      Literal [31:103-31:105]: INT(50)
                                    Literal [31:108-31:110]: INT(60)
                                  Literal [32:11-32:13]: INT(10)
                                Literal [32:16-32:18]: INT(20)
                              Literal [32:21-32:23]: INT(30)
                            Literal [32:26-32:28]: INT(40)
                        Literal [32:31-32:33]: INT(50)
                      Literal [32:36-32:38]: INT(60)
                    Literal [32:41-32:43]: INT(80)
                  Binary [32:46-32:65]: MOD
                    Binary [32:46-32:61]: MOD
                      Binary [32:46-32:57]: MOD
                        Binary [32:46-32:53]: MOD
                          Literal [32:46-32:48]: INT(90)
                          Literal [32:52-32:53]: INT(4)
                        Literal [32:56-32:57]: INT(6)
                      Literal [32:60-32:61]: INT(7)
                    Literal [32:64-32:65]: INT(8)
                Unary [32:67-32:75]: ADD
                  Unary [32:68-32:75]: ADD
                    Unary [32:69-32:75]: ADD
                      Unary [32:70-32:75]: ADD
                        Unary [32:71-32:75]: ADD
                          Unary [32:72-32:75]: ADD
                            Literal [32:73-32:75]: INT(10)
        IfThenElse [34:9-38:10]
          Binary [35:16-35:61]: GT
            Name [35:16-35:19]: abc
            Binary [35:22-35:61]: ADD
              Binary [35:22-35:55]: ADD
                Binary [35:22-35:50]: SUB
                  Binary [35:22-35:45]: ADD
                    Binary [35:22-35:30]: SUB
                      Name [35:22-35:25]: abc
                      Literal [35:28-35:30]: INT(10)
                    Binary [35:33-35:45]: DIV
                      Binary [35:33-35:40]: MUL
                        Literal [35:33-35:35]: INT(10)
                        Literal [35:38-35:40]: INT(10)
                      Literal [35:43-35:45]: INT(10)
                  Literal [35:48-35:50]: INT(10)
                Literal [35:53-35:55]: INT(10)
              Unary [35:58-35:61]: SUB
                Literal [35:59-35:61]: INT(10)
          Call [37:21-37:46]: print
            Literal [37:27-37:45]: STR(danes je lep dan)
        For [40:9-50:10]
          Name [41:13-41:14]: i
          While [42:13-44:14]
            Name [43:23-43:24]: j
            Block [43:26-43:34]
              Binary [43:27-43:33]: LT
                Name [43:27-43:28]: j
                Literal [43:31-43:33]: INT(10)
          IfThenElse [45:17-47:18]
            Binary [46:22-46:35]: GEQ
              Binary [46:22-46:28]: MUL
                Name [46:22-46:23]: a
                Literal [46:26-46:28]: INT(10)
              Literal [46:32-46:35]: INT(100)
            Binary [46:41-46:53]: ASSIGN
              Name [46:42-46:43]: a
              Binary [46:46-46:52]: SUB
                Name [46:46-46:47]: a
                Literal [46:50-46:52]: INT(10)
            Name [46:59-46:64]: break
          For [48:21-48:71]
            Name [48:27-48:28]: j
            Literal [48:31-48:32]: INT(1)
            Binary [48:34-48:40]: LT
              Name [48:34-48:35]: j
              Literal [48:38-48:40]: INT(10)
            Binary [48:42-48:49]: ASSIGN
              Name [48:43-48:44]: j
              Binary [48:45-48:48]: MUL
                Name [48:45-48:46]: j
                Literal [48:47-48:48]: INT(2)
            Call [48:51-48:69]: print
              Literal [48:57-48:68]: STR(blablabla)
          Binary [49:25-49:66]: ASSIGN
            Binary [49:26-49:58]: ARR
              Binary [49:26-49:48]: ARR
                Name [49:26-49:36]: blablablab
                Binary [49:37-49:47]: SUB
                  Binary [49:37-49:41]: ADD
                    Name [49:37-49:38]: i
                    Literal [49:39-49:41]: INT(10)
                  Binary [49:42-49:47]: MUL
                    Literal [49:42-49:44]: INT(20)
                    Literal [49:45-49:47]: INT(30)
              Binary [49:49-49:57]: SUB
                Binary [49:49-49:53]: SUB
                  Name [49:49-49:50]: j
                  Literal [49:51-49:53]: INT(10)
                Literal [49:54-49:57]: INT(203)
            Name [49:61-49:65]: null
  FunDef [56:1-74:2]: partition
    Parameter [56:15-56:39]: stevila
      Array [56:24-56:39]
        [10]
        Atom [56:32-56:39]: INT
    Parameter [56:41-56:55]: begin
      Atom [56:48-56:55]: INT
    Parameter [56:57-56:69]: end
      Atom [56:62-56:69]: INT
    Atom [56:72-56:79]: INT
    Block [56:82-74:2]
      Binary [57:5-57:27]: ASSIGN
        Name [57:6-57:11]: pivot
        Binary [57:14-57:26]: ARR
          Name [57:14-57:21]: stevila
          Name [57:22-57:25]: end
      Binary [58:5-58:22]: ASSIGN
        Name [58:6-58:7]: i
        Block [58:10-58:21]
          Binary [58:11-58:20]: SUB
            Name [58:11-58:16]: begin
            Literal [58:19-58:20]: INT(1)
      For [60:5-67:6]
        Name [60:11-60:12]: j
        Name [60:15-60:20]: begin
        Binary [60:22-60:29]: LT
          Name [60:22-60:23]: j
          Name [60:26-60:29]: end
        Binary [60:31-60:42]: ASSIGN
          Name [60:32-60:33]: j
          Binary [60:36-60:41]: ADD
            Name [60:36-60:37]: j
            Literal [60:40-60:41]: INT(1)
        IfThenElse [61:9-66:10]
          Binary [61:14-61:33]: LEQ
            Binary [61:14-61:24]: ARR
              Name [61:14-61:21]: stevila
              Name [61:22-61:23]: j
            Name [61:28-61:33]: pivot
          Block [62:13-65:38]
            Binary [62:14-62:25]: ASSIGN
              Name [62:15-62:16]: i
              Binary [62:19-62:24]: ADD
                Name [62:19-62:20]: i
                Literal [62:23-62:24]: INT(1)
            Binary [63:13-63:36]: ASSIGN
              Name [63:14-63:22]: swapTemp
              Binary [63:25-63:35]: ARR
                Name [63:25-63:32]: stevila
                Name [63:33-63:34]: j
            Binary [64:13-64:38]: ASSIGN
              Binary [64:14-64:24]: ARR
                Name [64:14-64:21]: stevila
                Name [64:22-64:23]: i
              Binary [64:27-64:37]: ARR
                Name [64:27-64:34]: stevila
                Name [64:35-64:36]: j
            Binary [65:13-65:37]: ASSIGN
              Binary [65:14-65:24]: ARR
                Name [65:14-65:21]: stevila
                Name [65:22-65:23]: j
              Name [65:27-65:36]: swampTemp
      Block [69:5-71:31]
        Binary [69:6-69:31]: ASSIGN
          Name [69:7-69:15]: swapTemp
          Binary [69:18-69:30]: ARR
            Name [69:18-69:25]: stevila
            Binary [69:26-69:29]: ADD
              Name [69:26-69:27]: i
              Literal [69:28-69:29]: INT(1)
        Binary [70:5-70:34]: ASSIGN
          Binary [70:6-70:18]: ARR
            Name [70:6-70:13]: stevila
            Binary [70:14-70:17]: ADD
              Name [70:14-70:15]: i
              Literal [70:16-70:17]: INT(1)
          Binary [70:21-70:33]: ARR
            Name [70:21-70:28]: stevila
            Name [70:29-70:32]: end
        Binary [71:5-71:30]: ASSIGN
          Binary [71:6-71:18]: ARR
            Name [71:6-71:13]: stevila
            Name [71:14-71:17]: end
          Name [71:21-71:29]: swapTemp
      Call [73:5-73:18]: return
        Binary [73:12-73:17]: ADD
          Name [73:12-73:13]: i
          Literal [73:16-73:17]: INT(1)
  FunDef [76:1-77:55]: izpis
    Parameter [76:11-76:27]: besedilo
      Atom [76:21-76:27]: STR
    TypeName [76:30-76:34]: void
    Call [77:5-77:55]: print
      Literal [77:11-77:54]: STR(Tvoj program je prestal celoten preizkus!)
!end