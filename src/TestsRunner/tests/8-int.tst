!compiler_flags: --dump INT --exec INT

!name: Nested functions and integers
!code: 
fun main(x: integer): integer = (
  { x = 1 },
  { w = 4 },
  g(2),
  print_str(str_test('aaaaa')),
  0
) { where
  var w: integer;
  fun g(y: integer): integer = (
    { ww = 5 },
    f(3),
    0
  ) { where
    var ww: integer;
    fun f(z: integer): integer = (
      print_int(x),
      print_int(y),
      print_int(z),
      print_int(w),
      print_int(ww),
      0
    )
  }
};


fun str_test(x: string): string = (
  { s = 'lolmao' },
  print_str(s),
  print_str(x),
  print_str('hello'),
  'bruh' 
) { where 
  var s: string
}
!expected:
1
2
3
4
5
"lolmao"
"aaaaa"
"hello"
"bruh"
!end

!name: Strings
!code:
fun main(x: integer): integer = (
    { s = 'str1' },
    print_str(s),
    print_str('str2'),
    print_str(f('str3')),
    0
) { where 
    var s: string;
    fun f(str: string): string = (
        print_str(str),
        'str4'
    )
}
!expected:
"str1"
"str2"
"str3"
"str4"
!end

!name: Boolean
!code:
var b1: logical;
var b2: logical;
var b3: logical;
var b4: logical;
var b5: logical;
var b6: logical;

fun main(x: integer): integer = (
    { b1 = true },
    { b2 = false },
    { b3 = b1 == b2 },
    { b4 = b1 & b2 },
    { b5 = b1 | b2 },
    { b6 = !b1 },
    print_log(b3),
    print_log(b4),
    print_log(b5),
    print_log(b6),
    print_log(b1 == !b2),
    f1(b1, !b2),
    x
); 

fun f1(b1: logical, b2: logical): logical = (
    { b3 = b1 == b2 },
    { b4 = b1 & b2 },
    { b5 = b1 | b2 },
    { b6 = !b1 },
    print_log(b3),
    print_log(b4),
    print_log(b5),
    print_log(b6),
    b1
) { where
    var b3: logical;
    var b4: logical;
    var b5: logical;
    var b6: logical
}
!expected:
false
false
true
false
true
true
true
true
false
!end

!name: While loop
!code:
typ int: integer;

fun main(x: integer): integer = (
    { i = 0 },
    { while i < 15: (
        print_int(i),
        { i = increment(i) }
    )},
    i
) { where 
    var i: int;
    fun increment(x: integer): integer = (
        { x = x + 1 },
        x
    )
}
!expected:
0
1
2
3
4
5
6
7
8
9
10
11
12
13
14
!end

!name: For loop 1
!code:
fun main(x: integer): integer = (
    { for x = 0, 15, 2: (
        print_int(x)
    )},
    0
)
!expected:
0
2
4
6
8
10
12
14
!end

!name: For loop 2
!code:
fun main(x: integer): integer = (
    { for x = 1, 130, x: (
        print_int(x)
    )},
    0
)
!expected:
1
2
4
8
16
32
64
128
!end

!name: For loop 3
!code:
fun main(x: integer): integer = (
    { for x = 1, 130, x * 2: (
        print_int(x)
    )},
    0
)
!expected:
1
3
9
27
81
!end

!name: If statement
!code:
fun main(x: integer): integer = (
    { if larger_than_10(5) then
        print_str('5 is larger than 10')
      else
        print_str('5 is not larger than 10')
    },
    { if larger_than_10(20) then
        print_str('20 is larger than 10')
      else
        print_str('20 is not larger than 10')
    },
    print_log(f(5, 10)),
    print_log(f(10, 5)),
    0
) { where
    typ boolean: logical;
    fun f(x1: integer, x2: integer): boolean = (
        { if x1 < x2 then
            print_str('x1 < x2')
        else
            print_str('x1 >= x2')
        },
        true
    );
    fun larger_than_10(x1: integer): boolean = (
        { larger = false },
        { if x1 > 10 then
            { larger = true }
        },
        larger
    ) { where
        var larger: logical
    }
}
!expected:
"5 is not larger than 10"
"20 is larger than 10"
"x1 < x2"
true
"x1 >= x2"
true
!end

!name: Global arrays
!code:
typ list: arr[10] integer;
var l: integer;
var array: list;

fun main(x: integer): integer = (
  { l = 10 },
  fill_array(array, l, 69),
  print_array(array, l),
  fill_array2(array, l, 0),
  print_array(array, l),
  0
);

fun fill_array(a: list, l: integer, x: integer): integer = (
  { i = 0 },
  { while i < l: (
      { a[i] = x },
      { i = i + 1 }
  )},
  0
) { where
  var i: integer
};

fun fill_array2(a: list, l: integer, x: integer): integer = (
  { for i = x, l, 1:
      { a[i] = i }
  },
  0
) { where
  var i: integer
};

fun print_array(a: list, l:integer): integer = (
  { for i = 0, l, 1:
      print_int(a[i])
  },
  0
) { where
  var i: integer
}
!expected:
69
69
69
69
69
69
69
69
69
69
0
1
2
3
4
5
6
7
8
9
!end

!name: Local arrays
!code:
typ list: arr[10] integer;
var l: integer;

fun main(x: integer): integer = (
  { l = 10 },
  fill_array(array, l, 69),
  print_array(array, l),
  fill_array2(array, l, 0),
  print_array(array, l),
  0
) { where
  var array: list
};

fun fill_array(a: list, l: integer, x: integer): integer = (
  { i = 0 },
  { while i < l: (
      { a[i] = x },
      { i = i + 1 }
  )},
  0
) { where
  var i: integer
};

fun fill_array2(a: list, l: integer, x: integer): integer = (
  { for i = x, l, 1:
      { a[i] = i }
  },
  0
) { where
  var i: integer
};

fun print_array(a: list, l:integer): integer = (
  { for i = 0, l, 1:
      print_int(a[i])
  },
  0
) { where
  var i: integer
}
!expected:
69
69
69
69
69
69
69
69
69
69
0
1
2
3
4
5
6
7
8
9
!end

!name: Local Arrays 2
!code:
typ list: arr[10] integer;
var l: integer;

fun main(x: integer): integer = (
  fill_array(10, 0),
  print_array(a, 10),
  0
) { where
  var a: list;
  fun fill_array(l: integer, x: integer): integer = (
    { for i = x, l, 1:
        { a[i] = i * 2 }
    },
    0
  ) { where
    var i: integer
  }
};

fun print_array(a: list, l:integer): integer = (
  { for i = 0, l, 1:
      print_int(a[i])
  },
  0
) { where
  var i: integer
}
!expected:
0
2
4
6
8
10
12
14
16
18
!end

!name: 2D Arrays
!code:
typ arr2dim: arr[2] arr[10] integer;
var a: arr2dim;

fun main(x: integer): integer = (
    fill_array(a[0], 0),
    fill_array(a[1], 10),
    print_arr(a),
    0
);

fun fill_array(a: arr[10] integer, x: integer): integer = (
    { for i = 0, 10, 1:
        { a[i] = x + i }
    },
    0
) { where 
    var i: integer
};

fun print_arr(a: arr2dim): integer = (
    { for i = 0, 2, 1:
        print_array(a[i])
    },
    0
) { where 
    var i: integer;
    fun print_array(a: arr[10] integer): integer = (
        { for i = 0, 10, 1:
            print_int(a[i])
        },
        0
    ) { where 
        var i: integer
    }
}
!expected:
0
1
2
3
4
5
6
7
8
9
10
11
12
13
14
15
16
17
18
19
!end

!name: 2D Arrays 2
!code:
typ arr2dim: arr[2] arr[10] integer;

fun main(x: integer): integer = (
    fill_array(a[0], 0),
    fill_array(a[1], 10),
    print_arr(a),
    0
) { where 
    var a: arr2dim
};

fun fill_array(a: arr[10] integer, x: integer): integer = (
    { for i = 0, 10, 1:
        { a[i] = x + i }
    },
    0
) { where 
    var i: integer
};

fun print_arr(a: arr2dim): integer = (
    { for i = 0, 2, 1:
        print_array(a[i])
    },
    0
) { where 
    var i: integer;
    fun print_array(a: arr[10] integer): integer = (
        { for i = 0, 10, 1:
            print_int(a[i])
        },
        0
    ) { where 
        var i: integer
    }
}
!expected:
0
1
2
3
4
5
6
7
8
9
10
11
12
13
14
15
16
17
18
19
!end

!name: 3D Arrays
!code:
typ arr3dim: arr[2] arr2dim;
typ arr2dim: arr[2] arr[10] integer;
var a: arr3dim;

fun main(x: integer): integer = (
    fill_array(a[0], 0),
    fill_array(a[1], 10),
    print_arr(a),
    0
);

fun fill_array(a: arr2dim, x: integer): integer = (
    { for i = 0, 10, 1:
      { for j = 0, 2, 1:
        { a[j][i] = i + j }
      }
    },
    0
) { where 
    var i: integer;
    var j: integer
};

fun print_arr(a: arr3dim): integer = (
    { for i = 0, 2, 1:
      { for j = 0, 2, 1:
          print_array(a[i][j])
      }
    },
    0
) { where 
    var i: integer;
    var j: integer;
    fun print_array(a: arr[10] integer): integer = (
        { for i = 0, 10, 1:
            print_int(a[i])
        },
        0
    ) { where 
        var i: integer
    }
}
!expected:
0
1
2
3
4
5
6
7
8
9
1
2
3
4
5
6
7
8
9
10
0
1
2
3
4
5
6
7
8
9
1
2
3
4
5
6
7
8
9
10
!end