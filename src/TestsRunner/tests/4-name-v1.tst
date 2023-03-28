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
          # defined at: [1:1-1:21]
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
          # defined at: [1:1-1:21]
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
          # defined at: [1:1-1:21]
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
            # defined at: [5:10-5:27]
          Binary [2:12-2:18]: ARR
            Name [2:12-2:15]: tab
              # defined at: [1:10-1:31]
            Name [2:16-2:17]: i
              # defined at: [1:33-1:44]
        Binary [3:5-3:22]: ASSIGN
          Binary [3:6-3:12]: ARR
            Name [3:6-3:9]: tab
              # defined at: [1:10-1:31]
            Name [3:10-3:11]: i
              # defined at: [1:33-1:44]
          Binary [3:15-3:21]: ARR
            Name [3:15-3:18]: tab
              # defined at: [1:10-1:31]
            Name [3:19-3:20]: j
              # defined at: [1:46-1:57]
        Binary [4:5-4:19]: ASSIGN
          Binary [4:6-4:12]: ARR
            Name [4:6-4:9]: tab
              # defined at: [1:10-1:31]
            Name [4:10-4:11]: j
              # defined at: [1:46-1:57]
          Name [4:15-4:18]: tmp
            # defined at: [5:10-5:27]
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
!failure:
99
!end


!code:
fun main(x: integer): integer = (
    ( {for i = 0, 10, 1:
        {if i / 2 == 1 | i & 2 != 0 | 1 == i & 'a' != 'b' & 1 != 3 then
            print(1)
        }
      }
    ) {where var i: integer}
);

fun print(s : string): integer = 0
!expected:
Defs [1:1-10:35]
  FunDef [1:1-8:2]: main
    Parameter [1:10-1:20]: x
      Atom [1:13-1:20]: INT
    Atom [1:23-1:30]: INT
    Block [1:33-8:2]
      Where [2:5-7:29]
        Defs [7:14-7:28]
          VarDef [7:14-7:28]: i
            Atom [7:21-7:28]: INT
        Block [2:5-7:6]
          For [2:7-6:8]
            Name [2:12-2:13]: i
              # defined at: [7:14-7:28]
            Literal [2:16-2:17]: INT(0)
            Literal [2:19-2:21]: INT(10)
            Literal [2:23-2:24]: INT(1)
            IfThenElse [3:9-5:10]
              Binary [3:13-3:67]: OR
                Binary [3:13-3:36]: OR
                  Binary [3:13-3:23]: EQ
                    Binary [3:13-3:18]: DIV
                      Name [3:13-3:14]: i
                        # defined at: [7:14-7:28]
                      Literal [3:17-3:18]: INT(2)
                    Literal [3:22-3:23]: INT(1)
                  Binary [3:26-3:36]: AND
                    Name [3:26-3:27]: i
                      # defined at: [7:14-7:28]
                    Binary [3:30-3:36]: NEQ
                      Literal [3:30-3:31]: INT(2)
                      Literal [3:35-3:36]: INT(0)
                Binary [3:39-3:67]: AND
                  Binary [3:39-3:58]: AND
                    Binary [3:39-3:45]: EQ
                      Literal [3:39-3:40]: INT(1)
                      Name [3:44-3:45]: i
                        # defined at: [7:14-7:28]
                    Binary [3:48-3:58]: NEQ
                      Literal [3:48-3:51]: STR(a)
                      Literal [3:55-3:58]: STR(b)
                  Binary [3:61-3:67]: NEQ
                    Literal [3:61-3:62]: INT(1)
                    Literal [3:66-3:67]: INT(3)
              Call [4:13-4:21]: print
                # defined at: [10:1-10:35]
                Literal [4:19-4:20]: INT(1)
  FunDef [10:1-10:35]: print
    Parameter [10:11-10:21]: s
      Atom [10:15-10:21]: STR
    Atom [10:24-10:31]: INT
    Literal [10:34-10:35]: INT(0)
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
    Parameter [1:11-1:21]: x
      Atom [1:14-1:21]: INT
    Atom [1:26-1:33]: INT
    Block [1:36-27:2]
      IfThenElse [2:5-26:6]
        Binary [2:10-2:43]: OR
          Binary [2:10-2:34]: OR
            Binary [2:10-2:25]: AND
              Binary [2:10-2:16]: EQ
                Literal [2:10-2:11]: INT(1)
                Literal [2:15-2:16]: INT(1)
              Binary [2:19-2:25]: EQ
                Literal [2:19-2:20]: INT(2)
                Literal [2:24-2:25]: INT(2)
            Binary [2:28-2:34]: EQ
              Literal [2:28-2:29]: INT(3)
              Literal [2:33-2:34]: INT(3)
          Binary [2:37-2:43]: EQ
            Literal [2:37-2:38]: INT(4)
            Literal [2:42-2:43]: INT(4)
        Where [4:9-15:10]
          Defs [13:13-14:28]
            VarDef [13:13-13:28]: k
              Atom [13:21-13:28]: INT
            VarDef [14:13-14:28]: i
              Atom [14:21-14:28]: INT
          Block [4:9-12:10]
            For [5:13-11:14]
              Name [5:19-5:20]: i
                # defined at: [14:13-14:28]
              Literal [5:23-5:24]: INT(0)
              Literal [5:26-5:28]: INT(10)
              Literal [5:30-5:31]: INT(1)
              Where [6:17-10:18]
                Defs [9:21-9:36]
                  VarDef [9:21-9:36]: k
                    Atom [9:29-9:36]: INT
                Block [6:17-8:18]
                  Binary [7:21-7:32]: ASSIGN
                    Name [7:22-7:23]: k
                      # defined at: [9:21-9:36]
                    Binary [7:26-7:31]: ADD
                      Name [7:26-7:27]: k
                        # defined at: [9:21-9:36]
                      Name [7:30-7:31]: i
                        # defined at: [14:13-14:28]
        Where [17:9-25:10]
          Defs [24:13-24:28]
            VarDef [24:13-24:28]: k
              Atom [24:21-24:28]: INT
          Block [17:9-23:10]
            While [18:13-22:14]
              Binary [18:21-18:27]: LT
                Name [18:21-18:22]: k
                  # defined at: [24:13-24:28]
                Literal [18:25-18:27]: INT(20)
              Block [19:17-21:18]
                Binary [20:21-20:32]: ASSIGN
                  Name [20:22-20:23]: k
                    # defined at: [24:13-24:28]
                  Binary [20:26-20:31]: ADD
                    Name [20:26-20:27]: k
                      # defined at: [24:13-24:28]
                    Literal [20:30-20:31]: INT(1)
      Literal [26:8-26:9]: INT(0)
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
            # defined at: [19:10-19:28]
          Binary [3:14-3:23]: ARR
            Name [3:14-3:17]: tab
              # defined at: [2:16-2:37]
            Name [3:18-3:22]: high
              # defined at: [2:53-2:66]
        Binary [4:5-4:20]: ASSIGN
          Name [4:6-4:7]: i
            # defined at: [19:30-19:44]
          Block [4:10-4:19]
            Binary [4:11-4:18]: SUB
              Name [4:11-4:14]: low
                # defined at: [2:39-2:51]
              Literal [4:17-4:18]: INT(1)
        Where [5:5-14:30]
          Defs [14:15-14:29]
            VarDef [14:15-14:29]: j
              Atom [14:22-14:29]: INT
          Block [5:5-14:7]
            For [5:6-14:6]
              Name [5:11-5:12]: j
                # defined at: [14:15-14:29]
              Name [5:15-5:18]: low
                # defined at: [2:39-2:51]
              Name [5:20-5:24]: high
                # defined at: [2:53-2:66]
              Name [5:26-5:27]: i
                # defined at: [19:30-19:44]
              IfThenElse [6:9-13:36]
                Binary [6:14-6:29]: LEQ
                  Binary [6:14-6:20]: ARR
                    Name [6:14-6:17]: tab
                      # defined at: [2:16-2:37]
                    Name [6:18-6:19]: j
                      # defined at: [14:15-14:29]
                  Name [6:24-6:29]: pivot
                    # defined at: [19:10-19:28]
                Where [6:35-13:35]
                  Defs [13:18-13:34]
                    VarDef [13:18-13:34]: tmp
                      Atom [13:27-13:34]: INT
                  Block [6:35-13:10]
                    Binary [8:13-8:22]: ASSIGN
                      Name [8:14-8:15]: i
                        # defined at: [19:30-19:44]
                      Binary [8:18-8:21]: ADD
                        Name [8:18-8:19]: i
                          # defined at: [19:30-19:44]
                        Literal [8:20-8:21]: INT(1)
                    Binary [9:13-9:27]: ASSIGN
                      Name [9:14-9:17]: tmp
                        # defined at: [13:18-13:34]
                      Binary [9:20-9:26]: ARR
                        Name [9:20-9:23]: tab
                          # defined at: [2:16-2:37]
                        Name [9:24-9:25]: i
                          # defined at: [19:30-19:44]
                    Binary [10:13-10:30]: ASSIGN
                      Binary [10:14-10:20]: ARR
                        Name [10:14-10:17]: tab
                          # defined at: [2:16-2:37]
                        Name [10:18-10:19]: j
                          # defined at: [14:15-14:29]
                      Binary [10:23-10:29]: ARR
                        Name [10:23-10:26]: tab
                          # defined at: [2:16-2:37]
                        Name [10:27-10:28]: j
                          # defined at: [14:15-14:29]
                    Binary [11:13-11:27]: ASSIGN
                      Binary [11:14-11:20]: ARR
                        Name [11:14-11:17]: tab
                          # defined at: [2:16-2:37]
                        Name [11:18-11:19]: j
                          # defined at: [14:15-14:29]
                      Name [11:23-11:26]: tmp
                        # defined at: [13:18-13:34]
        Binary [16:5-16:21]: ASSIGN
          Name [16:6-16:9]: tmp
            # defined at: [19:46-19:62]
          Binary [16:12-16:20]: ARR
            Name [16:12-16:15]: tab
              # defined at: [2:16-2:37]
            Binary [16:16-16:19]: ADD
              Name [16:16-16:17]: i
                # defined at: [19:30-19:44]
              Literal [16:18-16:19]: INT(1)
        Binary [17:5-17:27]: ASSIGN
          Binary [17:6-17:14]: ARR
            Name [17:6-17:9]: tab
              # defined at: [2:16-2:37]
            Binary [17:10-17:13]: ADD
              Name [17:10-17:11]: i
                # defined at: [19:30-19:44]
              Literal [17:12-17:13]: INT(1)
          Binary [17:17-17:26]: ARR
            Name [17:17-17:20]: tab
              # defined at: [2:16-2:37]
            Name [17:21-17:25]: high
              # defined at: [2:53-2:66]
        Binary [18:5-18:22]: ASSIGN
          Binary [18:6-18:15]: ARR
            Name [18:6-18:9]: tab
              # defined at: [2:16-2:37]
            Name [18:10-18:14]: high
              # defined at: [2:53-2:66]
          Name [18:18-18:21]: tmp
            # defined at: [19:46-19:62]
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
            # defined at: [21:33-21:45]
          Name [22:15-22:19]: high
            # defined at: [21:47-21:60]
        Where [22:25-26:30]
          Defs [26:14-26:29]
            VarDef [26:14-26:29]: pi
              Atom [26:22-26:29]: INT
          Block [22:25-26:6]
            Binary [23:9-23:41]: ASSIGN
              Name [23:10-23:12]: pi
                # defined at: [26:14-26:29]
              Call [23:15-23:40]: partition
                # defined at: [2:1-19:63]
                Name [23:25-23:28]: tab
                  # defined at: [21:10-21:31]
                Name [23:30-23:33]: low
                  # defined at: [21:33-21:45]
                Name [23:35-23:39]: high
                  # defined at: [21:47-21:60]
            Call [24:9-24:29]: sort
              # defined at: [21:1-27:2]
              Name [24:14-24:17]: tab
                # defined at: [21:10-21:31]
              Name [24:19-24:22]: low
                # defined at: [21:33-21:45]
              Binary [24:24-24:28]: SUB
                Name [24:24-24:26]: pi
                  # defined at: [26:14-26:29]
                Literal [24:27-24:28]: INT(1)
            Call [25:9-25:30]: sort
              # defined at: [21:1-27:2]
              Name [25:14-25:17]: tab
                # defined at: [21:10-21:31]
              Binary [25:19-25:23]: ADD
                Name [25:19-25:21]: pi
                  # defined at: [26:14-26:29]
                Literal [25:22-25:23]: INT(1)
              Name [25:25-25:29]: high
                # defined at: [21:47-21:60]
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

fun prvaFunkcija (prvic:string, drugic:integer, cetrtic:arr[7] logical) : integer =
5 + 3 - 2 == 10 & true | 'beseda' / 'drugabeseda' * 'danesjelepdanaaaaa' % cetrtic != petic + 5 - !'e'
- (-10) + (+(+(-10))) / (!cetrtic) + 100 - false | nekaj & nekaj == nekaj | nekaj >= nekaj &
nekaj <= blablabla | nekaj + nekaj < bla & nekaj - nekaj > nekajbla { where
    fun drugaFunkcija(nekaj:integer):integer=
        {
            ({ if haha then {
                    if nekaj then 5+3-1-4+2 * true else {
                        while nekaj >= nekaj * (-5323223) + -44545    : {
                            for nekaj = 1, nekaj >= -9999999, nekaj-nekaj-1-1-1-1:nekaj
                        }
                    }
                }
            }) { where var haha: integer }
        = celotenprogramtideluje69 { where
            fun epicBubblesort(polje:arr[10]integer):void = (
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
                }) { where var dolzinaPolja: integer; var n: integer; var temp: integer; var i: integer; var j: integer; var blablabla: string};
                var celotenprogramtideluje69: string
            }
         }
    }
#lololololol
!expected:
Defs [1:1-45:6]
  TypeDef [1:1-1:17]: prvic
    Atom [1:11-1:17]: STR
  TypeDef [2:1-2:21]: drugic
    Atom [2:14-2:21]: INT
  TypeDef [3:1-3:23]: tretjic
    TypeName [3:15-3:23]: besedilo
      # defined at: [6:1-6:21]
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
  TypeDef [6:1-6:21]: besedilo
    Atom [6:15-6:21]: STR
  TypeDef [7:1-7:18]: void
    Atom [7:11-7:18]: INT
  VarDef [8:1-8:18]: nekaj
    Atom [8:12-8:18]: STR
  VarDef [9:1-9:23]: blablabla
    Atom [9:16-9:23]: LOG
  VarDef [10:1-10:22]: nekajbla
    Atom [10:15-10:22]: LOG
  VarDef [11:1-11:17]: bla
    Atom [11:10-11:17]: LOG
  FunDef [13:1-45:6]: prvaFunkcija
    Parameter [13:19-13:31]: prvic
      Atom [13:25-13:31]: STR
    Parameter [13:33-13:47]: drugic
      Atom [13:40-13:47]: INT
    Parameter [13:49-13:71]: cetrtic
      Array [13:57-13:71]
        [7]
        Atom [13:64-13:71]: LOG
    Atom [13:75-13:82]: INT
    Where [14:1-45:6]
      Defs [17:5-44:11]
        FunDef [17:5-44:11]: drugaFunkcija
          Parameter [17:23-17:36]: nekaj
            Atom [17:29-17:36]: INT
          Atom [17:38-17:45]: INT
          Binary [18:9-44:11]: ASSIGN
            Where [19:13-26:43]
              Defs [26:24-26:41]
                VarDef [26:24-26:41]: haha
                  Atom [26:34-26:41]: INT
              Block [19:13-26:15]
                IfThenElse [19:14-26:14]
                  Name [19:19-19:23]: haha
                    # defined at: [26:24-26:41]
                  IfThenElse [19:29-25:18]
                    Name [20:24-20:29]: nekaj
                      # defined at: [17:23-17:36]
                    Binary [20:35-20:51]: ADD
                      Binary [20:35-20:42]: SUB
                        Binary [20:35-20:40]: SUB
                          Binary [20:35-20:38]: ADD
                            Literal [20:35-20:36]: INT(5)
                            Literal [20:37-20:38]: INT(3)
                          Literal [20:39-20:40]: INT(1)
                        Literal [20:41-20:42]: INT(4)
                      Binary [20:43-20:51]: MUL
                        Literal [20:43-20:44]: INT(2)
                        Literal [20:47-20:51]: LOG(true)
                    While [20:57-24:22]
                      Binary [21:31-21:67]: GEQ
                        Name [21:31-21:36]: nekaj
                          # defined at: [17:23-17:36]
                        Binary [21:40-21:67]: ADD
                          Binary [21:40-21:58]: MUL
                            Name [21:40-21:45]: nekaj
                              # defined at: [17:23-17:36]
                            Block [21:48-21:58]
                              Unary [21:49-21:57]: SUB
                                Literal [21:50-21:57]: INT(5323223)
                          Unary [21:61-21:67]: SUB
                            Literal [21:62-21:67]: INT(44545)
                      For [21:73-23:26]
                        Name [22:33-22:38]: nekaj
                          # defined at: [17:23-17:36]
                        Literal [22:41-22:42]: INT(1)
                        Binary [22:44-22:61]: GEQ
                          Name [22:44-22:49]: nekaj
                            # defined at: [17:23-17:36]
                          Unary [22:53-22:61]: SUB
                            Literal [22:54-22:61]: INT(9999999)
                        Binary [22:63-22:82]: SUB
                          Binary [22:63-22:80]: SUB
                            Binary [22:63-22:78]: SUB
                              Binary [22:63-22:76]: SUB
                                Binary [22:63-22:74]: SUB
                                  Name [22:63-22:68]: nekaj
                                    # defined at: [17:23-17:36]
                                  Name [22:69-22:74]: nekaj
                                    # defined at: [17:23-17:36]
                                Literal [22:75-22:76]: INT(1)
                              Literal [22:77-22:78]: INT(1)
                            Literal [22:79-22:80]: INT(1)
                          Literal [22:81-22:82]: INT(1)
                        Name [22:83-22:88]: nekaj
                          # defined at: [17:23-17:36]
            Where [27:11-43:14]
              Defs [28:13-42:53]
                FunDef [28:13-41:144]: epicBubblesort
                  Parameter [28:32-28:52]: polje
                    Array [28:38-28:52]
                      [10]
                      Atom [28:45-28:52]: INT
                  TypeName [28:54-28:58]: void
                    # defined at: [7:1-7:18]
                  Where [28:61-41:144]
                    Defs [41:28-41:143]
                      VarDef [41:28-41:53]: dolzinaPolja
                        Atom [41:46-41:53]: INT
                      VarDef [41:55-41:69]: n
                        Atom [41:62-41:69]: INT
                      VarDef [41:71-41:88]: temp
                        Atom [41:81-41:88]: INT
                      VarDef [41:90-41:104]: i
                        Atom [41:97-41:104]: INT
                      VarDef [41:106-41:120]: j
                        Atom [41:113-41:120]: INT
                      VarDef [41:122-41:143]: blablabla
                        Atom [41:137-41:143]: STR
                    Block [28:61-41:19]
                      Binary [29:17-41:18]: AND
                        Binary [29:17-30:26]: AND
                          Binary [29:17-29:34]: EQ
                            Name [29:17-29:18]: n
                              # defined at: [41:55-41:69]
                            Name [29:22-29:34]: dolzinaPolja
                              # defined at: [41:28-41:53]
                          Binary [30:17-30:26]: EQ
                            Name [30:17-30:21]: temp
                              # defined at: [41:71-41:88]
                            Literal [30:25-30:26]: INT(0)
                        For [30:29-41:18]
                          Name [31:21-31:22]: i
                            # defined at: [41:90-41:104]
                          Literal [31:23-31:24]: INT(0)
                          Binary [31:25-31:28]: LT
                            Name [31:25-31:26]: i
                              # defined at: [41:90-41:104]
                            Name [31:27-31:28]: n
                              # defined at: [41:55-41:69]
                          Binary [31:29-31:34]: ADD
                            Name [31:29-31:30]: i
                              # defined at: [41:90-41:104]
                            Literal [31:33-31:34]: INT(1)
                          For [32:21-40:22]
                            Name [32:27-32:28]: j
                              # defined at: [41:106-41:120]
                            Literal [32:29-32:30]: INT(0)
                            Binary [32:32-32:39]: LT
                              Name [32:32-32:33]: j
                                # defined at: [41:106-41:120]
                              Block [32:34-32:39]
                                Binary [32:35-32:38]: SUB
                                  Name [32:35-32:36]: n
                                    # defined at: [41:55-41:69]
                                  Name [32:37-32:38]: i
                                    # defined at: [41:90-41:104]
                            Binary [32:40-32:45]: ADD
                              Name [32:40-32:41]: j
                                # defined at: [41:106-41:120]
                              Literal [32:44-32:45]: INT(1)
                            IfThenElse [33:25-39:26]
                              Binary [34:32-34:53]: GT
                                Binary [34:32-34:42]: ARR
                                  Name [34:32-34:37]: polje
                                    # defined at: [28:32-28:52]
                                  Binary [34:38-34:41]: SUB
                                    Name [34:38-34:39]: j
                                      # defined at: [41:106-41:120]
                                    Literal [34:40-34:41]: INT(1)
                                Binary [34:45-34:53]: ARR
                                  Name [34:45-34:50]: polje
                                    # defined at: [28:32-28:52]
                                  Name [34:51-34:52]: j
                                    # defined at: [41:106-41:120]
                              Binary [35:33-38:42]: AND
                                Binary [35:33-37:49]: AND
                                  Binary [35:33-36:55]: AND
                                    Binary [35:33-35:51]: EQ
                                      Name [35:33-35:37]: temp
                                        # defined at: [41:71-41:88]
                                      Binary [35:41-35:51]: ARR
                                        Name [35:41-35:46]: polje
                                          # defined at: [28:32-28:52]
                                        Binary [35:47-35:50]: SUB
                                          Name [35:47-35:48]: j
                                            # defined at: [41:106-41:120]
                                          Literal [35:49-35:50]: INT(1)
                                    Binary [36:33-36:55]: EQ
                                      Binary [36:33-36:43]: ARR
                                        Name [36:33-36:38]: polje
                                          # defined at: [28:32-28:52]
                                        Binary [36:39-36:42]: SUB
                                          Name [36:39-36:40]: j
                                            # defined at: [41:106-41:120]
                                          Literal [36:41-36:42]: INT(1)
                                      Binary [36:47-36:55]: ARR
                                        Name [36:47-36:52]: polje
                                          # defined at: [28:32-28:52]
                                        Name [36:53-36:54]: j
                                          # defined at: [41:106-41:120]
                                  Binary [37:33-37:49]: EQ
                                    Binary [37:33-37:41]: ARR
                                      Name [37:33-37:38]: polje
                                        # defined at: [28:32-28:52]
                                      Name [37:39-37:40]: j
                                        # defined at: [41:106-41:120]
                                    Name [37:45-37:49]: temp
                                      # defined at: [41:71-41:88]
                                Name [38:33-38:42]: blablabla
                                  # defined at: [41:122-41:143]
                VarDef [42:17-42:53]: celotenprogramtideluje69
                  Atom [42:47-42:53]: STR
              Name [27:11-27:35]: celotenprogramtideluje69
                # defined at: [42:17-42:53]
      Binary [14:1-16:68]: OR
        Binary [14:1-16:19]: OR
          Binary [14:1-15:74]: OR
            Binary [14:1-15:49]: OR
              Binary [14:1-14:23]: AND
                Binary [14:1-14:16]: EQ
                  Binary [14:1-14:10]: SUB
                    Binary [14:1-14:6]: ADD
                      Literal [14:1-14:2]: INT(5)
                      Literal [14:5-14:6]: INT(3)
                    Literal [14:9-14:10]: INT(2)
                  Literal [14:14-14:16]: INT(10)
                Literal [14:19-14:23]: LOG(true)
              Binary [14:26-15:49]: NEQ
                Binary [14:26-14:83]: MOD
                  Binary [14:26-14:73]: MUL
                    Binary [14:26-14:50]: DIV
                      Literal [14:26-14:34]: STR(beseda)
                      Literal [14:37-14:50]: STR(drugabeseda)
                    Literal [14:53-14:73]: STR(danesjelepdanaaaaa)
                  Name [14:76-14:83]: cetrtic
                    # defined at: [13:49-13:71]
                Binary [14:87-15:49]: SUB
                  Binary [14:87-15:41]: ADD
                    Binary [14:87-15:35]: ADD
                      Binary [14:87-15:8]: SUB
                        Binary [14:87-14:103]: SUB
                          Binary [14:87-14:96]: ADD
                            Name [14:87-14:92]: petic
                              # defined at: [5:1-5:31]
                            Literal [14:95-14:96]: INT(5)
                          Unary [14:99-14:103]: NOT
                            Literal [14:100-14:103]: STR(e)
                        Block [15:3-15:8]
                          Unary [15:4-15:7]: SUB
                            Literal [15:5-15:7]: INT(10)
                      Binary [15:11-15:35]: DIV
                        Block [15:11-15:22]
                          Unary [15:12-15:21]: ADD
                            Block [15:13-15:21]
                              Unary [15:14-15:20]: ADD
                                Block [15:15-15:20]
                                  Unary [15:16-15:19]: SUB
                                    Literal [15:17-15:19]: INT(10)
                        Block [15:25-15:35]
                          Unary [15:26-15:34]: NOT
                            Name [15:27-15:34]: cetrtic
                              # defined at: [13:49-13:71]
                    Literal [15:38-15:41]: INT(100)
                  Literal [15:44-15:49]: LOG(false)
            Binary [15:52-15:74]: AND
              Name [15:52-15:57]: nekaj
                # defined at: [8:1-8:18]
              Binary [15:60-15:74]: EQ
                Name [15:60-15:65]: nekaj
                  # defined at: [8:1-8:18]
                Name [15:69-15:74]: nekaj
                  # defined at: [8:1-8:18]
          Binary [15:77-16:19]: AND
            Binary [15:77-15:91]: GEQ
              Name [15:77-15:82]: nekaj
                # defined at: [8:1-8:18]
              Name [15:86-15:91]: nekaj
                # defined at: [8:1-8:18]
            Binary [16:1-16:19]: LEQ
              Name [16:1-16:6]: nekaj
                # defined at: [8:1-8:18]
              Name [16:10-16:19]: blablabla
                # defined at: [9:1-9:23]
        Binary [16:22-16:68]: AND
          Binary [16:22-16:41]: LT
            Binary [16:22-16:35]: ADD
              Name [16:22-16:27]: nekaj
                # defined at: [8:1-8:18]
              Name [16:30-16:35]: nekaj
                # defined at: [8:1-8:18]
            Name [16:38-16:41]: bla
              # defined at: [11:1-11:17]
          Binary [16:44-16:68]: GT
            Binary [16:44-16:57]: SUB
              Name [16:44-16:49]: nekaj
                # defined at: [8:1-8:18]
              Name [16:52-16:57]: nekaj
                # defined at: [8:1-8:18]
            Name [16:60-16:68]: nekajbla
              # defined at: [10:1-10:22]
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
fun firstfunction(x: integer, y: string, z: logical, n: arr[69] arr[420] string): integer =
	( {for i = 1, i <= 100, {i = i + 1}:
		{
			while j > (-10): {
				if j * (-10) + 20 + 30 + 10 - 40 / 50 % 100 -(-(-(-(-(+10))))) != 123 then
				    12
				else
					firstfunction(123, -234, +456)
			}
		# komentar komentar komentar ----------------------------------------------
		}	},

		true & false | 10 | 'danes je lep dan''' & i | firstfunction(i, x + 10)
											>=
		+10 - -20 * !true - !false / (j, 'ha', 'ne', 'da') - {x=10} + {y+x-z = 5 * true - false}
		| i > j+3 | j < j-2 | i >= i*3 |  i <= j/7 | j != j & 'a' & 'b' & 'a' / 'a' == 1 + 10 + 20 + 30 / 40 / 50 / 60
		* 10 * 20 * 30 * 40 - 50 - 60 - 80 - 90  % 4 % 6 % 7 % 8 +++++++10,

		{
			if 'abc' > 'abc' - 10 + 10 * 10 / 10 - 10 + 10 + -10
				then
					firstfunction('danes je lep dan')
		},

		{
		for i =
			{
				while j: (j < 10)
			},
				{
				  if a * 10 >= 100 then {a = a - 10} else 0
				},
					{ for j = 1, j < 10, {j=j*2}: firstfunction('blablabla') }:
						{a[i+10-20*30][j-10-203] = 12}
		}
	)
{ where var i:integer; var a: logical; var j: integer; fun secondfunction(x: integer):integer = 10+20|30-40&123==20 { where
fun thirdfunction(y: string):arr[10] string = ({10*20=xyz}) {where var xyz: integer}}
};

fun partition(stevila: arr[10] integer, begin: integer, end: integer): integer = (
	{pivot = stevila[end]},
	{i = (begin - 1)},

	{ for j = begin, j < end, {j = j + 1}:
		{ if stevila[j] <= pivot then
			({i = i + 1},
			{swapTemp = stevila[j]},
			{stevila[i] = stevila[j]},
			{stevila[j] = swapTemp})
		}
	},

	({swapTemp = stevila[i+1]},
	{stevila[i+1] = stevila[end]},
	{stevila[end] = swapTemp}),

	(i + 1)
) {where
    var swapTemp: integer;
    var pivot: integer;
    var i: integer;
    var j: integer
};

fun izpis(besedilo: string): void =
	print('Tvoj program je prestal celoten preizkus!');

fun print(besedilo: string): string = besedilo
!expected:
Defs [5:1-87:47]
  TypeDef [5:1-5:15]: x
    Atom [5:8-5:15]: INT
  TypeDef [6:1-6:14]: y
    Atom [6:8-6:14]: STR
  TypeDef [7:1-7:15]: z
    Atom [7:8-7:15]: LOG
  TypeDef [8:1-8:17]: i
    TypeName [8:8-8:17]: blablabla
      # defined at: [10:1-10:23]
  TypeDef [9:1-9:41]: j
    Array [9:8-9:41]
      [10]
      Array [9:16-9:41]
        [15]
        Array [9:24-9:41]
          [20]
          TypeName [9:32-9:41]: blablabla
            # defined at: [10:1-10:23]
  TypeDef [10:1-10:23]: blablabla
    Atom [10:16-10:23]: INT
  TypeDef [11:1-11:22]: blabla
    TypeName [11:13-11:22]: blablabla
      # defined at: [10:1-10:23]
  TypeDef [12:1-12:18]: void
    Atom [12:11-12:18]: LOG
  VarDef [15:1-15:14]: k
    Atom [15:8-15:14]: STR
  VarDef [15:16-15:30]: l
    Atom [15:23-15:30]: INT
  VarDef [15:32-15:46]: q
    Atom [15:39-15:46]: LOG
  VarDef [15:48-15:61]: m
    TypeName [15:55-15:61]: blabla
      # defined at: [11:1-11:22]
  VarDef [15:63-15:94]: n
    Array [15:70-15:94]
      [69]
      Array [15:78-15:94]
        [420]
        Atom [15:88-15:94]: STR
  FunDef [19:1-57:2]: firstfunction
    Parameter [19:19-19:29]: x
      Atom [19:22-19:29]: INT
    Parameter [19:31-19:40]: y
      Atom [19:34-19:40]: STR
    Parameter [19:42-19:52]: z
      Atom [19:45-19:52]: LOG
    Parameter [19:54-19:80]: n
      Array [19:57-19:80]
        [69]
        Array [19:65-19:80]
          [420]
          Atom [19:74-19:80]: STR
    Atom [19:83-19:90]: INT
    Where [20:5-57:2]
      Defs [55:9-56:86]
        VarDef [55:9-55:22]: i
          Atom [55:15-55:22]: INT
        VarDef [55:24-55:38]: a
          Atom [55:31-55:38]: LOG
        VarDef [55:40-55:54]: j
          Atom [55:47-55:54]: INT
        FunDef [55:56-56:86]: secondfunction
          Parameter [55:75-55:85]: x
            Atom [55:78-55:85]: INT
          Atom [55:87-55:94]: INT
          Where [55:97-56:86]
            Defs [56:1-56:85]
              FunDef [56:1-56:85]: thirdfunction
                Parameter [56:19-56:28]: y
                  Atom [56:22-56:28]: STR
                Array [56:30-56:44]
                  [10]
                  Atom [56:38-56:44]: STR
                Where [56:47-56:85]
                  Defs [56:68-56:84]
                    VarDef [56:68-56:84]: xyz
                      Atom [56:77-56:84]: INT
                  Block [56:47-56:60]
                    Binary [56:48-56:59]: ASSIGN
                      Binary [56:49-56:54]: MUL
                        Literal [56:49-56:51]: INT(10)
                        Literal [56:52-56:54]: INT(20)
                      Name [56:55-56:58]: xyz
                        # defined at: [56:68-56:84]
            Binary [55:97-55:116]: OR
              Binary [55:97-55:102]: ADD
                Literal [55:97-55:99]: INT(10)
                Literal [55:100-55:102]: INT(20)
              Binary [55:103-55:116]: AND
                Binary [55:103-55:108]: SUB
                  Literal [55:103-55:105]: INT(30)
                  Literal [55:106-55:108]: INT(40)
                Binary [55:109-55:116]: EQ
                  Literal [55:109-55:112]: INT(123)
                  Literal [55:114-55:116]: INT(20)
      Block [20:5-54:6]
        For [20:7-29:15]
          Name [20:12-20:13]: i
            # defined at: [55:9-55:22]
          Literal [20:16-20:17]: INT(1)
          Binary [20:19-20:27]: LEQ
            Name [20:19-20:20]: i
              # defined at: [55:9-55:22]
            Literal [20:24-20:27]: INT(100)
          Binary [20:29-20:40]: ASSIGN
            Name [20:30-20:31]: i
              # defined at: [55:9-55:22]
            Binary [20:34-20:39]: ADD
              Name [20:34-20:35]: i
                # defined at: [55:9-55:22]
              Literal [20:38-20:39]: INT(1)
          While [21:9-29:10]
            Binary [22:19-22:28]: GT
              Name [22:19-22:20]: j
                # defined at: [55:40-55:54]
              Block [22:23-22:28]
                Unary [22:24-22:27]: SUB
                  Literal [22:25-22:27]: INT(10)
            IfThenElse [22:30-27:14]
              Binary [23:20-23:86]: NEQ
                Binary [23:20-23:79]: SUB
                  Binary [23:20-23:60]: SUB
                    Binary [23:20-23:44]: ADD
                      Binary [23:20-23:39]: ADD
                        Binary [23:20-23:34]: ADD
                          Binary [23:20-23:29]: MUL
                            Name [23:20-23:21]: j
                              # defined at: [55:40-55:54]
                            Block [23:24-23:29]
                              Unary [23:25-23:28]: SUB
                                Literal [23:26-23:28]: INT(10)
                          Literal [23:32-23:34]: INT(20)
                        Literal [23:37-23:39]: INT(30)
                      Literal [23:42-23:44]: INT(10)
                    Binary [23:47-23:60]: MOD
                      Binary [23:47-23:54]: DIV
                        Literal [23:47-23:49]: INT(40)
                        Literal [23:52-23:54]: INT(50)
                      Literal [23:57-23:60]: INT(100)
                  Block [23:62-23:79]
                    Unary [23:63-23:78]: SUB
                      Block [23:64-23:78]
                        Unary [23:65-23:77]: SUB
                          Block [23:66-23:77]
                            Unary [23:67-23:76]: SUB
                              Block [23:68-23:76]
                                Unary [23:69-23:75]: SUB
                                  Block [23:70-23:75]
                                    Unary [23:71-23:74]: ADD
                                      Literal [23:72-23:74]: INT(10)
                Literal [23:83-23:86]: INT(123)
              Literal [24:21-24:23]: INT(12)
              Call [26:21-26:51]: firstfunction
                # defined at: [19:1-57:2]
                Literal [26:35-26:38]: INT(123)
                Unary [26:40-26:44]: SUB
                  Literal [26:41-26:44]: INT(234)
                Unary [26:46-26:50]: ADD
                  Literal [26:47-26:50]: INT(456)
        Binary [31:9-35:75]: OR
          Binary [31:9-34:51]: OR
            Binary [31:9-34:39]: OR
              Binary [31:9-34:28]: OR
                Binary [31:9-34:18]: OR
                  Binary [31:9-33:97]: OR
                    Binary [31:9-31:53]: OR
                      Binary [31:9-31:26]: OR
                        Binary [31:9-31:21]: AND
                          Literal [31:9-31:13]: LOG(true)
                          Literal [31:16-31:21]: LOG(false)
                        Literal [31:24-31:26]: INT(10)
                      Binary [31:29-31:53]: AND
                        Literal [31:29-31:49]: STR(danes je lep dan')
                        Name [31:52-31:53]: i
                          # defined at: [55:9-55:22]
                    Binary [31:56-33:97]: GEQ
                      Call [31:56-31:80]: firstfunction
                        # defined at: [19:1-57:2]
                        Name [31:70-31:71]: i
                          # defined at: [55:9-55:22]
                        Binary [31:73-31:79]: ADD
                          Name [31:73-31:74]: x
                            # defined at: [19:19-19:29]
                          Literal [31:77-31:79]: INT(10)
                      Binary [33:9-33:97]: ADD
                        Binary [33:9-33:68]: SUB
                          Binary [33:9-33:59]: SUB
                            Binary [33:9-33:26]: SUB
                              Unary [33:9-33:12]: ADD
                                Literal [33:10-33:12]: INT(10)
                              Binary [33:15-33:26]: MUL
                                Unary [33:15-33:18]: SUB
                                  Literal [33:16-33:18]: INT(20)
                                Unary [33:21-33:26]: NOT
                                  Literal [33:22-33:26]: LOG(true)
                            Binary [33:29-33:59]: DIV
                              Unary [33:29-33:35]: NOT
                                Literal [33:30-33:35]: LOG(false)
                              Block [33:38-33:59]
                                Name [33:39-33:40]: j
                                  # defined at: [55:40-55:54]
                                Literal [33:42-33:46]: STR(ha)
                                Literal [33:48-33:52]: STR(ne)
                                Literal [33:54-33:58]: STR(da)
                          Binary [33:62-33:68]: ASSIGN
                            Name [33:63-33:64]: x
                              # defined at: [19:19-19:29]
                            Literal [33:65-33:67]: INT(10)
                        Binary [33:71-33:97]: ASSIGN
                          Binary [33:72-33:77]: SUB
                            Binary [33:72-33:75]: ADD
                              Name [33:72-33:73]: y
                                # defined at: [19:31-19:40]
                              Name [33:74-33:75]: x
                                # defined at: [19:19-19:29]
                            Name [33:76-33:77]: z
                              # defined at: [19:42-19:52]
                          Binary [33:80-33:96]: SUB
                            Binary [33:80-33:88]: MUL
                              Literal [33:80-33:81]: INT(5)
                              Literal [33:84-33:88]: LOG(true)
                            Literal [33:91-33:96]: LOG(false)
                  Binary [34:11-34:18]: GT
                    Name [34:11-34:12]: i
                      # defined at: [55:9-55:22]
                    Binary [34:15-34:18]: ADD
                      Name [34:15-34:16]: j
                        # defined at: [55:40-55:54]
                      Literal [34:17-34:18]: INT(3)
                Binary [34:21-34:28]: LT
                  Name [34:21-34:22]: j
                    # defined at: [55:40-55:54]
                  Binary [34:25-34:28]: SUB
                    Name [34:25-34:26]: j
                      # defined at: [55:40-55:54]
                    Literal [34:27-34:28]: INT(2)
              Binary [34:31-34:39]: GEQ
                Name [34:31-34:32]: i
                  # defined at: [55:9-55:22]
                Binary [34:36-34:39]: MUL
                  Name [34:36-34:37]: i
                    # defined at: [55:9-55:22]
                  Literal [34:38-34:39]: INT(3)
            Binary [34:43-34:51]: LEQ
              Name [34:43-34:44]: i
                # defined at: [55:9-55:22]
              Binary [34:48-34:51]: DIV
                Name [34:48-34:49]: j
                  # defined at: [55:40-55:54]
                Literal [34:50-34:51]: INT(7)
          Binary [34:54-35:75]: AND
            Binary [34:54-34:72]: AND
              Binary [34:54-34:66]: AND
                Binary [34:54-34:60]: NEQ
                  Name [34:54-34:55]: j
                    # defined at: [55:40-55:54]
                  Name [34:59-34:60]: j
                    # defined at: [55:40-55:54]
                Literal [34:63-34:66]: STR(a)
              Literal [34:69-34:72]: STR(b)
            Binary [34:75-35:75]: EQ
              Binary [34:75-34:84]: DIV
                Literal [34:75-34:78]: STR(a)
                Literal [34:81-34:84]: STR(a)
              Binary [34:88-35:75]: ADD
                Binary [34:88-35:65]: SUB
                  Binary [34:88-35:43]: SUB
                    Binary [34:88-35:38]: SUB
                      Binary [34:88-35:33]: SUB
                        Binary [34:88-35:28]: ADD
                          Binary [34:88-34:99]: ADD
                            Binary [34:88-34:94]: ADD
                              Literal [34:88-34:89]: INT(1)
                              Literal [34:92-34:94]: INT(10)
                            Literal [34:97-34:99]: INT(20)
                          Binary [34:102-35:28]: MUL
                            Binary [34:102-35:23]: MUL
                              Binary [34:102-35:18]: MUL
                                Binary [34:102-35:13]: MUL
                                  Binary [34:102-34:119]: DIV
                                    Binary [34:102-34:114]: DIV
                                      Binary [34:102-34:109]: DIV
                                        Literal [34:102-34:104]: INT(30)
                                        Literal [34:107-34:109]: INT(40)
                                      Literal [34:112-34:114]: INT(50)
                                    Literal [34:117-34:119]: INT(60)
                                  Literal [35:11-35:13]: INT(10)
                                Literal [35:16-35:18]: INT(20)
                              Literal [35:21-35:23]: INT(30)
                            Literal [35:26-35:28]: INT(40)
                        Literal [35:31-35:33]: INT(50)
                      Literal [35:36-35:38]: INT(60)
                    Literal [35:41-35:43]: INT(80)
                  Binary [35:46-35:65]: MOD
                    Binary [35:46-35:61]: MOD
                      Binary [35:46-35:57]: MOD
                        Binary [35:46-35:53]: MOD
                          Literal [35:46-35:48]: INT(90)
                          Literal [35:52-35:53]: INT(4)
                        Literal [35:56-35:57]: INT(6)
                      Literal [35:60-35:61]: INT(7)
                    Literal [35:64-35:65]: INT(8)
                Unary [35:67-35:75]: ADD
                  Unary [35:68-35:75]: ADD
                    Unary [35:69-35:75]: ADD
                      Unary [35:70-35:75]: ADD
                        Unary [35:71-35:75]: ADD
                          Unary [35:72-35:75]: ADD
                            Literal [35:73-35:75]: INT(10)
        IfThenElse [37:9-41:10]
          Binary [38:16-38:65]: GT
            Literal [38:16-38:21]: STR(abc)
            Binary [38:24-38:65]: ADD
              Binary [38:24-38:59]: ADD
                Binary [38:24-38:54]: SUB
                  Binary [38:24-38:49]: ADD
                    Binary [38:24-38:34]: SUB
                      Literal [38:24-38:29]: STR(abc)
                      Literal [38:32-38:34]: INT(10)
                    Binary [38:37-38:49]: DIV
                      Binary [38:37-38:44]: MUL
                        Literal [38:37-38:39]: INT(10)
                        Literal [38:42-38:44]: INT(10)
                      Literal [38:47-38:49]: INT(10)
                  Literal [38:52-38:54]: INT(10)
                Literal [38:57-38:59]: INT(10)
              Unary [38:62-38:65]: SUB
                Literal [38:63-38:65]: INT(10)
          Call [40:21-40:54]: firstfunction
            # defined at: [19:1-57:2]
            Literal [40:35-40:53]: STR(danes je lep dan)
        For [43:9-53:10]
          Name [44:13-44:14]: i
            # defined at: [55:9-55:22]
          While [45:13-47:14]
            Name [46:23-46:24]: j
              # defined at: [55:40-55:54]
            Block [46:26-46:34]
              Binary [46:27-46:33]: LT
                Name [46:27-46:28]: j
                  # defined at: [55:40-55:54]
                Literal [46:31-46:33]: INT(10)
          IfThenElse [48:17-50:18]
            Binary [49:22-49:35]: GEQ
              Binary [49:22-49:28]: MUL
                Name [49:22-49:23]: a
                  # defined at: [55:24-55:38]
                Literal [49:26-49:28]: INT(10)
              Literal [49:32-49:35]: INT(100)
            Binary [49:41-49:53]: ASSIGN
              Name [49:42-49:43]: a
                # defined at: [55:24-55:38]
              Binary [49:46-49:52]: SUB
                Name [49:46-49:47]: a
                  # defined at: [55:24-55:38]
                Literal [49:50-49:52]: INT(10)
            Literal [49:59-49:60]: INT(0)
          For [51:21-51:79]
            Name [51:27-51:28]: j
              # defined at: [55:40-55:54]
            Literal [51:31-51:32]: INT(1)
            Binary [51:34-51:40]: LT
              Name [51:34-51:35]: j
                # defined at: [55:40-55:54]
              Literal [51:38-51:40]: INT(10)
            Binary [51:42-51:49]: ASSIGN
              Name [51:43-51:44]: j
                # defined at: [55:40-55:54]
              Binary [51:45-51:48]: MUL
                Name [51:45-51:46]: j
                  # defined at: [55:40-55:54]
                Literal [51:47-51:48]: INT(2)
            Call [51:51-51:77]: firstfunction
              # defined at: [19:1-57:2]
              Literal [51:65-51:76]: STR(blablabla)
          Binary [52:25-52:55]: ASSIGN
            Binary [52:26-52:49]: ARR
              Binary [52:26-52:39]: ARR
                Name [52:26-52:27]: a
                  # defined at: [55:24-55:38]
                Binary [52:28-52:38]: SUB
                  Binary [52:28-52:32]: ADD
                    Name [52:28-52:29]: i
                      # defined at: [55:9-55:22]
                    Literal [52:30-52:32]: INT(10)
                  Binary [52:33-52:38]: MUL
                    Literal [52:33-52:35]: INT(20)
                    Literal [52:36-52:38]: INT(30)
              Binary [52:40-52:48]: SUB
                Binary [52:40-52:44]: SUB
                  Name [52:40-52:41]: j
                    # defined at: [55:40-55:54]
                  Literal [52:42-52:44]: INT(10)
                Literal [52:45-52:48]: INT(203)
            Literal [52:52-52:54]: INT(12)
  FunDef [59:1-82:2]: partition
    Parameter [59:15-59:39]: stevila
      Array [59:24-59:39]
        [10]
        Atom [59:32-59:39]: INT
    Parameter [59:41-59:55]: begin
      Atom [59:48-59:55]: INT
    Parameter [59:57-59:69]: end
      Atom [59:62-59:69]: INT
    Atom [59:72-59:79]: INT
    Where [59:82-82:2]
      Defs [78:5-81:19]
        VarDef [78:5-78:26]: swapTemp
          Atom [78:19-78:26]: INT
        VarDef [79:5-79:23]: pivot
          Atom [79:16-79:23]: INT
        VarDef [80:5-80:19]: i
          Atom [80:12-80:19]: INT
        VarDef [81:5-81:19]: j
          Atom [81:12-81:19]: INT
      Block [59:82-77:2]
        Binary [60:5-60:27]: ASSIGN
          Name [60:6-60:11]: pivot
            # defined at: [79:5-79:23]
          Binary [60:14-60:26]: ARR
            Name [60:14-60:21]: stevila
              # defined at: [59:15-59:39]
            Name [60:22-60:25]: end
              # defined at: [59:57-59:69]
        Binary [61:5-61:22]: ASSIGN
          Name [61:6-61:7]: i
            # defined at: [80:5-80:19]
          Block [61:10-61:21]
            Binary [61:11-61:20]: SUB
              Name [61:11-61:16]: begin
                # defined at: [59:41-59:55]
              Literal [61:19-61:20]: INT(1)
        For [63:5-70:6]
          Name [63:11-63:12]: j
            # defined at: [81:5-81:19]
          Name [63:15-63:20]: begin
            # defined at: [59:41-59:55]
          Binary [63:22-63:29]: LT
            Name [63:22-63:23]: j
              # defined at: [81:5-81:19]
            Name [63:26-63:29]: end
              # defined at: [59:57-59:69]
          Binary [63:31-63:42]: ASSIGN
            Name [63:32-63:33]: j
              # defined at: [81:5-81:19]
            Binary [63:36-63:41]: ADD
              Name [63:36-63:37]: j
                # defined at: [81:5-81:19]
              Literal [63:40-63:41]: INT(1)
          IfThenElse [64:9-69:10]
            Binary [64:14-64:33]: LEQ
              Binary [64:14-64:24]: ARR
                Name [64:14-64:21]: stevila
                  # defined at: [59:15-59:39]
                Name [64:22-64:23]: j
                  # defined at: [81:5-81:19]
              Name [64:28-64:33]: pivot
                # defined at: [79:5-79:23]
            Block [65:13-68:37]
              Binary [65:14-65:25]: ASSIGN
                Name [65:15-65:16]: i
                  # defined at: [80:5-80:19]
                Binary [65:19-65:24]: ADD
                  Name [65:19-65:20]: i
                    # defined at: [80:5-80:19]
                  Literal [65:23-65:24]: INT(1)
              Binary [66:13-66:36]: ASSIGN
                Name [66:14-66:22]: swapTemp
                  # defined at: [78:5-78:26]
                Binary [66:25-66:35]: ARR
                  Name [66:25-66:32]: stevila
                    # defined at: [59:15-59:39]
                  Name [66:33-66:34]: j
                    # defined at: [81:5-81:19]
              Binary [67:13-67:38]: ASSIGN
                Binary [67:14-67:24]: ARR
                  Name [67:14-67:21]: stevila
                    # defined at: [59:15-59:39]
                  Name [67:22-67:23]: i
                    # defined at: [80:5-80:19]
                Binary [67:27-67:37]: ARR
                  Name [67:27-67:34]: stevila
                    # defined at: [59:15-59:39]
                  Name [67:35-67:36]: j
                    # defined at: [81:5-81:19]
              Binary [68:13-68:36]: ASSIGN
                Binary [68:14-68:24]: ARR
                  Name [68:14-68:21]: stevila
                    # defined at: [59:15-59:39]
                  Name [68:22-68:23]: j
                    # defined at: [81:5-81:19]
                Name [68:27-68:35]: swapTemp
                  # defined at: [78:5-78:26]
        Block [72:5-74:31]
          Binary [72:6-72:31]: ASSIGN
            Name [72:7-72:15]: swapTemp
              # defined at: [78:5-78:26]
            Binary [72:18-72:30]: ARR
              Name [72:18-72:25]: stevila
                # defined at: [59:15-59:39]
              Binary [72:26-72:29]: ADD
                Name [72:26-72:27]: i
                  # defined at: [80:5-80:19]
                Literal [72:28-72:29]: INT(1)
          Binary [73:5-73:34]: ASSIGN
            Binary [73:6-73:18]: ARR
              Name [73:6-73:13]: stevila
                # defined at: [59:15-59:39]
              Binary [73:14-73:17]: ADD
                Name [73:14-73:15]: i
                  # defined at: [80:5-80:19]
                Literal [73:16-73:17]: INT(1)
            Binary [73:21-73:33]: ARR
              Name [73:21-73:28]: stevila
                # defined at: [59:15-59:39]
              Name [73:29-73:32]: end
                # defined at: [59:57-59:69]
          Binary [74:5-74:30]: ASSIGN
            Binary [74:6-74:18]: ARR
              Name [74:6-74:13]: stevila
                # defined at: [59:15-59:39]
              Name [74:14-74:17]: end
                # defined at: [59:57-59:69]
            Name [74:21-74:29]: swapTemp
              # defined at: [78:5-78:26]
        Block [76:5-76:12]
          Binary [76:6-76:11]: ADD
            Name [76:6-76:7]: i
              # defined at: [80:5-80:19]
            Literal [76:10-76:11]: INT(1)
  FunDef [84:1-85:55]: izpis
    Parameter [84:11-84:27]: besedilo
      Atom [84:21-84:27]: STR
    TypeName [84:30-84:34]: void
      # defined at: [12:1-12:18]
    Call [85:5-85:55]: print
      # defined at: [87:1-87:47]
      Literal [85:11-85:54]: STR(Tvoj program je prestal celoten preizkus!)
  FunDef [87:1-87:47]: print
    Parameter [87:11-87:27]: besedilo
      Atom [87:21-87:27]: STR
    Atom [87:30-87:36]: STR
    Name [87:39-87:47]: besedilo
      # defined at: [87:11-87:27]
!end


!code:
typ y: y;
typ x: integer;
fun f(x: x): integer = 1;
fun main(y: integer): integer = f(3)
!expected:
Defs [1:1-4:37]
  TypeDef [1:1-1:9]: y
    TypeName [1:8-1:9]: y
      # defined at: [1:1-1:9]
  TypeDef [2:1-2:15]: x
    Atom [2:8-2:15]: INT
  FunDef [3:1-3:25]: f
    Parameter [3:7-3:11]: x
      TypeName [3:10-3:11]: x
        # defined at: [2:1-2:15]
    Atom [3:14-3:21]: INT
    Literal [3:24-3:25]: INT(1)
  FunDef [4:1-4:37]: main
    Parameter [4:10-4:20]: y
      Atom [4:13-4:20]: INT
    Atom [4:23-4:30]: INT
    Call [4:33-4:37]: f
      # defined at: [3:1-3:25]
      Literal [4:35-4:36]: INT(3)
!end

!code: 
typ x: integer;
fun g(y: x): integer = (12) {
    where var x:x
}
!failure:
99
!end

!code:
typ x: integer;
fun g(x: x): integer = (12) {
    where var x: integer
}
!expected:
Defs [1:1-4:2]
  TypeDef [1:1-1:15]: x
    Atom [1:8-1:15]: INT
  FunDef [2:1-4:2]: g
    Parameter [2:7-2:11]: x
      TypeName [2:10-2:11]: x
        # defined at: [1:1-1:15]
    Atom [2:14-2:21]: INT
    Where [2:24-4:2]
      Defs [3:11-3:25]
        VarDef [3:11-3:25]: x
          Atom [3:18-3:25]: INT
      Block [2:24-2:28]
        Literal [2:25-2:27]: INT(12)
!end

