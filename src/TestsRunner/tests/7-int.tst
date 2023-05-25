!compiler_flags: --dump INT --exec INT


!name: BubbleSort (1D Array Operations)
!code:
var array : arr[10] integer;

fun print_array(n: integer) : integer = (
	{ for i = 0, n, 1 : print_int(array[i]) } { where var i : integer },
	1
);

fun generate_numbers(n: integer) : integer = (
	seed(0),
	{ for i = 0, n, 1 : {array[i] = rand_int(0, 100)} } { where var i : integer },
	1
);

fun swap(index1 : integer, index2 : integer) : integer = (
	{ temp = array[index1] },
	{ array[index1] = array[index2] },
	{ array[index2] = temp },
	1
) { where var temp : integer };

fun bubble_sort(n: integer) : integer = (
	{
		for i = 0, n - 1, 1 : {
			for j = 0, n - 1, 1 : {
				if array[j] < array[j+1] then
					swap(j, j+1)
			}
		}
	} { where var i : integer; var j : integer },
	1
);

fun main(x: integer) : integer = (
   generate_numbers(10),
   bubble_sort(10),
   print_array(10)
)
!expected:
91
61
60
54
53
48
47
29
19
15
!end



!name: Recursive Fibonacci (1D Array Access & Recursive Calls)
!code:
fun fib(n: integer) : integer = (
	{
		if n <= 1 then
			{ result = n }
		else (
			{ fib1 = fib(n-1) },
			{ fib2 = fib(n-2) },
			{ result = fib1 + fib2 }
		)
	},
	result
) { where var result : integer; var fib1 : integer; var fib2 : integer };

fun main(x: integer) : integer = (
	print_int(fib(0)),
	print_int(fib(2)),
	print_int(fib(3)),
	print_int(fib(9)),
	print_int(fib(15))
)
!expected:
0
1
2
34
610
!end



!name: Basic Array Operations (1D Array Access & Array Reference Passing)
!code:
typ arrayTyp : arr[3] integer;

fun fill_array(array : arrayTyp, n : integer) : logical = (
	{ for i = 0, n, 1 : { array[i] = rand_int(1, 100) } } { where var i : integer },
	true
);

fun arrays_sum(array1 : arrayTyp, array2 : arrayTyp, output : arrayTyp, n : integer) : logical = (
	{ for i = 0, n, 1 : { output[i] = array1[i] + array2[i] } } { where var i : integer },
	true
);

fun array_sum(array : arrayTyp, n : integer) : integer = (
	{ sum = 0 },
	{ for i = 0, n, 1 : { sum = sum + array[i] }} { where var i : integer },
	sum
) { where var sum : integer };

fun print_array(array : arrayTyp, n : integer) : logical = (
	{ for i = 0, n, 1 : print_int(array[i]) } { where var i : integer },
	true
);

fun main(argc : integer) : integer = (
	{ n = 3 },

	seed(0),
	fill_array(array1, n),
	fill_array(array2, n),
	arrays_sum(array1, array2, output, n),

	print_str('Array 1:'),
	print_array(array1, n),

	print_str('Array 2:'),
	print_array(array2, n),

	print_str('Output:'),
	print_array(output, n),

	print_str('Output Sum:'),
	print_int(array_sum(output, n))
) { where var array1 : arrayTyp; var array2 : arrayTyp; var output : arrayTyp; var n : integer }
!expected:
"Array 1:"
34
62
86
"Array 2:"
39
18
3
"Output:"
73
80
89
"Output Sum:"
242
!end



!name: Basic Nested Array Operations
!code:
typ arrayTyp : arr[3] integer;

fun start_array_operations(array1 : arrayTyp, array2 : arrayTyp, output : arrayTyp, n : integer) : integer = (
	seed(0),
	fill_array(array1),
	fill_array(array2),
	arrays_sum(array1, array2),

	print_str('Array 1:'),
	print_array(array1),

	print_str('Array 2:'),
	print_array(array2),

	print_str('Output:'),
	print_array(output),

	print_str('Output Sum:'),
	print_int(array_sum(output))
) { where
	fun fill_array(array : arrayTyp) : logical = (
		{ for i = 0, n, 1 : { array[i] = rand_int(1, 100) } } { where var i : integer },
		true
	);

	fun arrays_sum(array1 : arrayTyp, array2 : arrayTyp) : logical = (
		{ for i = 0, n, 1 : { output[i] = array1[i] + array2[i] } } { where var i : integer },
		true
	);

	fun array_sum(array : arrayTyp) : integer = (
		{ sum = 0 },
		{ for i = 0, n, 1 : { sum = sum + array[i] }} { where var i : integer },
		sum
	) { where var sum : integer };

	fun print_array(array : arrayTyp) : logical = (
		{ for i = 0, n, 1 : print_int(array[i]) } { where var i : integer },
		true
	)
};

fun main(argc : integer) : integer = (
	{ n = 3 },
	start_array_operations(array1, array2, output, n)
) { where var array1 : arrayTyp; var array2 : arrayTyp; var output : arrayTyp; var n : integer }
!expected:
"Array 1:"
34
62
86
"Array 2:"
39
18
3
"Output:"
73
80
89
"Output Sum:"
242
!end



!name: Matrix Operations (2D Array Access & Array Reference Passing)
!code:
typ matrixTyp : arr[3] arr[3] integer;

fun matrix_generate(matrix : matrixTyp, n : integer) : logical = (
	{ for i = 0, n, 1 :
		{ for j = 0, n, 1 :
			{ matrix[i][j] = rand_int(0, 100) }
		} { where var j : integer }
	} { where var i : integer },
	true
);

fun matrix_zero(matrix : matrixTyp, n : integer) : logical = (
	{ for i = 0, n, 1 :
		{ for j = 0, n, 1 :
			{ matrix[i][j] = 0 }
		} { where var j : integer }
	} { where var i : integer },
	true
);

fun matrix_multiply(matrix1 : matrixTyp, matrix2 : matrixTyp, output : matrixTyp, n : integer) : logical = (
	{ for i = 0, n, 1 :
		{ for j = 0, n, 1 :
			{ for k = 0, n, 1 :
				{ output[i][j] = output[i][j] + matrix1[i][k] * matrix2[k][j] }
			} { where var k : integer }
		} { where var j : integer }
	} { where var i : integer },
	true
);

fun matrix_print(matrix : matrixTyp, n : integer) : logical = (
	{ for i = 0, n, 1 :
		{ for j = 0, n, 1 :
			print_int(matrix[i][j])
		} { where var j : integer }
	} { where var i : integer },
	true
);

fun main(x: integer) : logical = (
	{ n = 3 },

	seed(0),
	matrix_generate(matrix1, n),
	matrix_generate(matrix2, n),
	matrix_zero(output, n),
	matrix_multiply(matrix1, matrix2, output, n),

	print_str('Matrix 1:'),
	matrix_print(matrix1, n),

	print_str('Matrix 2:'),
	matrix_print(matrix2, n),

	print_str('Multiplication Output:'),
	matrix_print(output, n)
) { where var matrix1 : matrixTyp; var matrix2 : matrixTyp; var output : matrixTyp; var n : integer }
!expected:
"Matrix 1:"
60
48
29
47
15
53
91
61
19
"Matrix 2:"
54
77
77
73
62
95
44
84
75
"Multiplication Output:"
8020
10032
11355
5965
9001
9019
10203
12385
14227
!end



!name: Nested Functions (Simple Root Function Parameter Access)
!code:
fun nested_functions_print(n : integer) : integer = print_variable(0)
{ where
	fun print_variable(x : integer) : integer = print_variable2(0)
	{ where
		fun print_variable2(y : integer) : integer = print_int(n)
	}
};

fun main(argc : integer) : integer = (
	{ n = 8 },
	nested_functions_print(n)
) { where var n : integer }
!expected:
8
!end